package com.datastructure.skiplist;


import java.util.Random;

/**
 *
 * User: Mohan Radhakrishnan
 * Date: 10/21/12
 * Time: 1:19 PM
 *
 */
public class SkipList {

    /*Cap on the number of levels allowed
    * Not sure why this limit is required if not
    * to prevent a burgeoning tower*/
    private static final int MAXNUMBEROFLEVELS = 16;

    private Random r = new Random();

    NodeStack nodeStack = null;

    {
        Node n = newList();/*Need not return reference.Already present in NodeStack*/
    }

    public Node newList(){
        Node node = new Node(), n = null;

        node.value = NodeStack.HEADER_VAL;

        nodeStack = new NodeStack( node );
        node.forward = nodeStack.NIL;
        for( int i = 0 ; i < MAXNUMBEROFLEVELS - 1; i ++ ){
            n = new Node();
            n.value = NodeStack.HEADER_VAL;
            n.forward = nodeStack.NIL;
            nodeStack.stackNode( n );
        }
      return n;
    }
    
    public void insert( double d ){
        /*
          In order to build the tower we track the position of the
          new node at the bottom-most level.
         */
        int level = 1;
        
        Node prevNode = nodeStack.bottomMostnode;
        Node currentNode = nodeStack.bottomMostnode.forward;

        while( currentNode != nodeStack.NIL && d > currentNode.value ){
            prevNode = currentNode;
            currentNode = currentNode.forward;
            ++ level;
        }
        //Do not Allow Duplicates. How ?
        Node newNode = new Node();
        newNode.value = d;

        prevNode.forward = newNode;
        newNode.backward = prevNode;

        currentNode.backward = newNode;
        newNode.forward = currentNode;

        //track the position of the new node at the bottom-most level
        nodeStack.setLevel( level );

        buildTower( nodeStack.bottomMostnode,
                    newNode );
    }

    /* Probability 1/2 */
    public int coinflip(){
        return r.nextInt( 2 );
    }
    
    private void buildTower( Node bottomMostnode,
                             Node newNode){
        Node towerNode = null;

        Node prevNode,nextNode, towerLevel = nodeStack.bottomMostnode;

        while( coinflip() == 0 && ( MAXNUMBEROFLEVELS + ~1 - 1 ) > 0 ){
            //Move up a level
            towerLevel = towerLevel.next;
            prevNode = towerLevel;
            nextNode = prevNode.forward;

            //Create another node for the new level
            towerNode = new Node();
            towerNode.value = newNode.value;

            for(int i = 1 ; i < nodeStack.level ; i ++ ){
                prevNode = towerLevel;
                nextNode = towerLevel.forward;
            }

            prevNode.forward =  towerNode;
            towerNode.forward = nextNode;

            nextNode.backward = towerNode;
            towerNode.backward =  prevNode;

            towerNode.prev = newNode;
            newNode.next = towerNode;

            System.out.println(nodeStack.toStringOfTower(newNode));

            newNode = towerNode;

        }
    }
}
