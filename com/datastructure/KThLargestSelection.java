package com.datastructure;

import com.google.common.primitives.Ints;

import java.util.ArrayList;
import java.util.Random;

import static java.util.Arrays.sort;

public class KThLargestSelection {

    private Random r = new Random();

    static int[] a1 = new int[]{ 0,2,7,3,4,9};

        public int findKThLargest(  int[] a, int k ){

                int pivot = getPivot(a);

                ArrayList<Integer> less = new ArrayList<>();
                ArrayList<Integer> equal = new ArrayList<>();
                ArrayList<Integer> great = new ArrayList<>();

                for( int value : a ){
                        if ( value < pivot){
                            less.add( value );
                        } else if ( value == pivot){
                            equal.add( value );
                        }else if ( value > pivot){
                            great.add( value );
                        }
                }
                        if (k <= less.size()) {
                            return findKThLargest (Ints.toArray( less ), k);
                        }else if (k <= less.size() + equal.size()){
                            return  pivot;
                        }else
                            return   findKThLargest (Ints.toArray( great  ), k - less.size() - equal.size());

        }

    private int getPivot( int[] data ){
        int[] highLow = new int[0];

            System.out.println("getPivot [" + data.length + "] " + toStringArray(data));
            highLow = new int[] { data[0],data[data.length - 1] };

        if( data.length >= 3){
            sort(new int[]{data[r.nextInt(2)],
                           data[r.nextInt(2)],
                           data[r.nextInt(2)]});
        }
        return highLow[ 1 ];
    }

    private String toStringArray(int[] data){
        StringBuilder sb = new StringBuilder();
        for( int i = 0 ; i < data.length ; i ++ ){
            sb.append( data[i] + " ");
        }
        return sb.toString();
    }
    
    public static void main( String... s){
        KThLargestSelection l = new KThLargestSelection();
        System.out.println( l.findKThLargest(a1, 1));
    }
}
