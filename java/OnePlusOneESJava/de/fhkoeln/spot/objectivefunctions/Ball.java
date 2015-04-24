/*
 * Ball.java
 *
 * the "ball function" f(x) := sum x_i^2
 *
 * 29.07.2009, Oliver Flasch (oliver.flasch@fh-koeln.de)
 *
 */

package de.fhkoeln.spot.objectivefunctions;


public class Ball implements IObjectiveFunction {
    public double valueOf(final double[] r) {
        double sum = 0.0;
        for (int i = 0; i < r.length; i++) {
            sum += Math.pow(r[i], 2);
        }
        return sum;
    }

    public boolean isFeasible(final double[] x) { return true; } // defined on all of |R^n
}
