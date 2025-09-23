open Effect
open Effect.Deep
open Eio


type _ Effect.t += Barrier_Action: unit -> unit Effect.t

let barrier_action f =
 match_with (fun() -> f )
 ()
 { effc = (fun (type c) (eff1: c Effect.t) ->
      match eff1 with
      | Barrier_Action  ()-> Some (fun (_: (c,_) continuation) ->
              Printf.printf "Barrier action";
          )
      | _ -> None
  );
  exnc = (function
        | e -> raise e
  );

  retc = (fun _ ->  ())
}


type round = {
	mutable count  :  int;
    wait_cond : Eio.Condition.t;
    breached_cond : Eio.Condition.t;
	mutable is_broken : bool
}

type cyclic_barrier = {
	participants :     int;
	barrier_action :  (unit -> unit);
    lock : Eio.Mutex.t;
	mutable round : round
}



let create_cyclic_barrier participants  =
	{
	 participants = participants;
     lock = Eio.Mutex.create();
 	 round = {
          count = 0;
          wait_cond = Eio.Condition.create ();
          breached_cond = Eio.Condition.create () ;
	        is_broken = false
    };
		barrier_action = (fun() -> perform (Barrier_Action() ))
	}
exception Barrier_breached of string

let break_barrier b wait_cond breached_cond should_lock =

    Eio.Mutex.use_rw ~protect:should_lock b.lock (
      fun () ->
       if not b.round.is_broken then
	   	b.round.is_broken <- true;
	   (*Broadcase change of condition  *)
       match wait_cond with
       | Some w ->
        Eio.Condition.broadcast w;
        (* Eio.Condition.broadcast br *)
       | None -> failwith "break barrier error"

    )

let resets b yes_or_no wait_cond breached_cond=
  Eio.Mutex.use_rw ~protect:true b.lock ( fun () ->

	if yes_or_no then(
       match wait_cond with
       | Some w ->
        Eio.Condition.broadcast w;
        (* Eio.Condition.broadcast br *)
       | None -> failwith "reset error"

  )
	else if b.round.count > 0 then
	   	break_barrier b wait_cond breached_cond true;

	b.round <- {           (* Start all over again *)

            count = 0;
            wait_cond = Eio.Condition.create ();
            breached_cond = Eio.Condition.create () ;
	        is_broken = false
		};
   )

let reset b wait_cond breached_cond =
   resets b true wait_cond breached_cond

let  get_participants b =
   b.participants

let  wait_list b =
  Eio.Switch.run @@ fun sw ->
  Eio.Mutex.use_rw ~protect:true b.lock (fun () ->  b.round.count)

let  is_broken b =

  Eio.Switch.run @@ fun sw ->
  Eio.Mutex.use_rw ~protect:true b.lock (fun () ->  b.round.is_broken)

let wait_cond = Fiber.create_key()
let breached_cond = Fiber.create_key()

let await b =

    try Fiber.check();

     let captured_round_result =
     Eio.Mutex.use_rw ~protect:true b.lock @@
      fun () ->
       let capture_round = b.round in
	     if capture_round.is_broken then(
          raise ( Barrier_breached "await\n" )
       )
       else
            capture_round.count <- capture_round.count + 1;
            let c = capture_round.count in
                Printf.printf "Count is %d\n" capture_round.count;
                if  c > b.participants then
                    failwith ("c > participants fatal")
                else
                if c < b.participants then(
                Fiber.any[ fun() ->
                 (match (Fiber.get wait_cond) with
                 | Some w ->
                  Eio.Condition.await w (* capture_round.wait_cond *) b.lock ;
                  false
                 | None -> failwith "await error")
                ]
                )
                else(
                  Fiber.any[(fun () -> true )]
                )
      in
      if captured_round_result  then(
        break_barrier b  (Fiber.get wait_cond) (Fiber.get breached_cond) true;
        reset b  (Fiber.get wait_cond) (Fiber.get breached_cond);
      )


     with | e ->
       let msg = Printexc.to_string e
       and stack = Printexc.get_backtrace () in
         Printf.eprintf "there was an error: %s%s\n" msg stack;
         break_barrier b (Fiber.get wait_cond) (Fiber.get breached_cond) true;
     ()

let spawn_fibers =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let b = create_cyclic_barrier 4 in
  Eio.Fiber.with_binding wait_cond b.round.wait_cond @@ fun() ->
  Eio.Fiber.with_binding breached_cond b.round.breached_cond @@ fun() ->
   for i = 1 to 4 do
      Fiber.fork ~sw (fun () ->
      Printf.printf "count is %d\n" i;
       await b
      )
    done
