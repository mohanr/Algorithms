package com.datastructure;

import java.io.IOException;
import java.util.Scanner;

/**
 * Simple friendship graph that stores single digit numbers
 * denoting vertexes and connects the edges between them
 * 1,2
 * 2,3
 * 3,4.
 *
 * It also prints them.
 * Note. Type single digits upto 9 and ensure they connect.
 *
 * So, for example, this will store the vertexes and edges but
 * won't print properly.
 * 1,2
 * 3,4
 *
 * Weights are not used.
 */
public class Graph {

    public static final int MAXV = 1000;

    private static final String EMPTY_STRING = "";

    EdgeNode[] edges = new EdgeNode[MAXV+1]; /* adjacency info */

    int[] degree = new int[MAXV+1]; /* outdegree of each vertex */

    int vertices; /* number of vertices in graph */

    int countEdges; /* number of edges in graph */

    boolean directed; /* is the graph directed? */

    class EdgeNode{

        int weight, adjacencyInfo;
        EdgeNode next;

        public EdgeNode( int adjacencyInfo, int x){
            this.next = edges[x];
            this.adjacencyInfo = adjacencyInfo;

        }

        public EdgeNode() {

        }
    }

    private void insertEdge( int x, int y, boolean directed ){

        EdgeNode temporary = new EdgeNode(y,x);
        System.out.printf( "The edge %d is initialized", x );
        edges[x] = temporary;
        degree[x] ++;
        countEdges ++;
    }

    private void buildGraph() throws IOException {
        Scanner scanner = new Scanner(System.in);
        char[] details = new char[2];
        String line;
        while( !(line = scanner.nextLine()).isEmpty()){
            details = line.toCharArray();
            System.out.printf( "%d %d", Character.digit(details[0],10),Character.digit(details[2],10));
            insertEdge(Character.digit(details[0],10),
                       Character.digit(details[2],10),
                       true);
            vertices ++;
        }
    }

    private void printGraph(){
        EdgeNode temporary;
        System.out.println( "Size of vertices is " + vertices);

        for( int i = 1 ; i <=  vertices ; i ++){
            temporary = edges[i];
            while(temporary != null){
                System.out.println( temporary.adjacencyInfo);
                temporary = temporary.next;
            }
        }
    }

    public static void main( String... argv ) throws IOException {
        Graph g = new Graph();
        g.buildGraph();
        g.printGraph();
    }
}
