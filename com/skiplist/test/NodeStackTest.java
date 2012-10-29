package com.skiplist.test;

import com.datastructure.skiplist.Node;
import com.datastructure.skiplist.NodeStack;
import com.datastructure.skiplist.SkipList;
import org.junit.Test;

import static junit.framework.Assert.*;



/**
 * User: Mohan Radhakrishnan
 * Date: 10/21/12
 * Time: 1:19 PM
 */
public class NodeStackTest {

    NodeStack testNode;

    @org.junit.Before
    public void setUp() throws Exception {
        Node node = new Node();
        node.value = 1.0;
        testNode = new NodeStack( node );

    }

    @org.junit.After
    public void tearDown() throws Exception {

    }
    
    @Test
    public void testNewNodeStack(){
        NodeStack ns = new NodeStack( new Node());
        assertNotNull(ns.bottomMostnode);
    }

    @Test
    public void testNILNodeStack(){
        NodeStack ns = new NodeStack( new Node());
        assertNotNull(ns.NIL);
        assertTrue(Double.isInfinite(ns.NIL.value));
    }

    @Test
    public void testNodeStackCreation(){
        assertNotNull(testNode.bottomMostnode);

        //Push test values into the stack
        setValues();
    }

    private void setValues(){
        Node node = new Node();
        node.value = 2.0;
        testNode.stackNode( node );

        Node node1 = new Node();
        node1.value = 3.0;
        testNode.stackNode( node1 );

        Node node2 = new Node();
        node2.value = 4.0;
        testNode.stackNode( node2 );


    }

    @Test
    public void testNewList(){
        SkipList sl = new SkipList();
        //Push test values into the stack
        setValues();
        sl.nodeStack.toStringOfTower( sl.nodeStack.bottomMostnode );
    }

    @Test
    public void testBottomInsert(){
        SkipList sl = new SkipList();
        sl.newList(); //Ignore return
        System.out.println( sl.nodeStack.toString() );
   }

   @Test
    public void testBinarySubtraction(){
       System.out.println( "BinarySubtraction (16-1 =" + ( 16 + ~1 + 1 ) + ")");
   }

    @Test
    public void testcoinflip(){
        SkipList sl = new SkipList();
        for(int i = 0 ; i < 10 ; i ++ )
            System.out.println( "coinflip " + sl.coinflip() );
    }

    @Test
    public void testBuildTower(){
        SkipList sl = new SkipList();
        sl.newList(); //Ignore return
        sl.insert( 1.0 );
        sl.insert( 2.0 );
        System.out.println( sl.nodeStack.toString() );
    }
}
