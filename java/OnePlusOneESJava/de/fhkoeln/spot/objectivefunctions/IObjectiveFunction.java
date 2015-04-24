/*
 * IObjectiveFunction.java
 *
 * an inteface for objective functions f :: RealVector -> double
 *
 * 29.07.2009, Oliver Flasch (oliver.flasch@fh-koeln.de)
 *             based on code by Niko Hansen
 *
 */

package de.fhkoeln.spot.objectivefunctions;


public interface IObjectiveFunction {
    double valueOf(double[] x);
    boolean isFeasible(double[] x);
}
