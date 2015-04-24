/*
 * Rosenbrock.java
 *
 * the Rosenbrock objective function
 *
 * 29.07.2009, Oliver Flasch (oliver.flasch@fh-koeln.de)
 *             based on code by Niko Hansen
 *
 */

package de.fhkoeln.spot.objectivefunctions;


public class Rosenbrock implements IObjectiveFunction {
    public double valueOf(final double[] x) {
        double res = 0;
        for (int i = 0; i < x.length-1; ++i) {
            res += 100 * (x[i]*x[i] - x[i+1]) * (x[i]*x[i] - x[i+1]) + 
                (x[i] - 1.) * (x[i] - 1.);
        }
        return res;
    }

    public boolean isFeasible(final double[] x) { return true; } // defined on all of |R^n
}