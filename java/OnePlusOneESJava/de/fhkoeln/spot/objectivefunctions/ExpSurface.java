/*
 * ExpSurface.java
 *
 * f(x,y) := ( x^2 + 2.5*y^2 - y ) * exp( 1 - (x^2 + y^2) ) as an objective function
 *
 * 29.07.2009, Oliver Flasch (oliver.flasch@fh-koeln.de)
 *
 */

package de.fhkoeln.spot.objectivefunctions;


public class ExpSurface implements IObjectiveFunction {
    public double valueOf(final double[] r) {
        final double x = r[0], y = r[1];
        return (Math.pow(x,2) + 2.5*Math.pow(y,2) - y) * Math.exp(1 - (Math.pow(x,2) + Math.pow(y,2)));
    }

    public boolean isFeasible(final double[] x) { return x.length == 2; } // defined on all of |R^2
}
