/*
 * HyperEllipsoide.java
 *
 * the "hyper ellipsoide function" f(x) := sum (x_i^2 *i)
 * where i starts from 1!
 * 
 * DeJongs Function from
 *  http://www.geatbx.com/docu/fcnindex-01.html#P99_3685
 *  
 * 23.07.2009, Joerg Ziegenhirt derived from Oliver Flasch's Ball-Function (joerg.ziegenhirt@fh-koeln.de)
 *
 */

package de.fhkoeln.spot.objectivefunctions;


public class HyperEllipsoide implements IObjectiveFunction {
    public double valueOf(final double[] r) {
        double sum = 0.0;
        for (int i = 0; i < r.length; i++) {
            sum += Math.pow(r[i], 2)*(i+1);
        }
        return sum;
    }

    public boolean isFeasible(final double[] x) { return true; } // defined on all of |R^n
}
