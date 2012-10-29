package com.datastructure.skiplist;

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 *
 * User: Mohan Radhakrishnan
 * Date: 10/21/12
 * Time: 1:19 PM
 */
public class NodeStack {

    Node NIL;


    {
        NIL = new Node();
        NIL.value = NIL_VAL;
    }

     static Double NIL_VAL = Double.POSITIVE_INFINITY,
                   HEADER_VAL = Double.NEGATIVE_INFINITY;

    Node currentNode,prevNode;

    Node bottomMostnode;

    int level;


    public NodeStack( Node node ){
        bottomMostnode = node;
        currentNode = bottomMostnode;
   }

    public void stackNode( Node node ){
        assert ( null != node );

        prevNode = currentNode;

        currentNode.next = node;
        currentNode = currentNode.next;
        node.prev = prevNode;

    }

    public void setLevel( int level ){
         this.level = level;
    }
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        StringWriter sw = new StringWriter();
        sb.append("[ " + level + " ]");
        Node currentNode = bottomMostnode,prevNode = null;

        do{
            try {
                prevNode = currentNode;
                sb.append( "[ " + currentNode.value + " ]");
                currentNode = currentNode.forward;
            } catch (Exception e) {
                e.printStackTrace(new PrintWriter(sw));
                sb.append( sw.toString());
            }

        }while( null != currentNode);
        return sb.toString();
    }

    public String toStringOfTower( Node node ){
        StringBuilder sb = new StringBuilder();
        StringWriter sw = new StringWriter();
        Node currentNode = node,prevNode = null;

        do{
            try {
                prevNode = currentNode;
                System.out.println( "[ " + currentNode.value + " ]");
                currentNode = currentNode.next;
            } catch (Exception e) {
                e.printStackTrace(new PrintWriter(sw));
                sb.append( sw.toString());
            }

        }while( null != currentNode);
        return sb.toString();
    }

}

