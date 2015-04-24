/*
 * SimpleOnePlusOneES.java
 *
 * a really simple 1+1 ES
 *
 * 29.07.2009, Oliver Flasch (oliver.flasch@fh-koeln.de)
 *
 */

package de.fhkoeln.spot.algorithms;

import de.fhkoeln.spot.objectivefunctions.IObjectiveFunction;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Random;


public final class SimpleOnePlusOneES {
    // the ES evolution main loop
    public static IndividualFitnessPair evolution(final RealVector xp0, // starting point
                                                  final double sigma0, // initial step size
                                                  final double a, // step size multiplier
                                                  final int g, // history length
                                                  final IObjectiveFunction f, // objective function
                                                  final ITerminationCriterion terminationCriterion,
                                                  final Random rng,
                                                  final boolean verbose, // print status messages
                                                  final int px, // print individuals mode
                                                  final int py) // print objective function values mode
    {
        long step = 0;
        RealVector xp = xp0, xo;
        double yp = Double.MAX_VALUE , yo;
        double sigma = sigma0;
        final int n = xp0.dimension();
        final MutationHistory mutationHistory = new MutationHistory(g);

        if (verbose) System.out.print("1+1 ES evolution running"); // maybe print evolution start message
        while (!terminationCriterion.isFullfilled(step, yp)) {
            yp = f.isFeasible(xp.data) ? f.valueOf(xp.data) : Double.MAX_VALUE;

            // maybe print x(t) and y(t)...
            if (px == PRINT_ALL) System.out.println(xp);
            if (py == PRINT_ALL) System.out.println(yp);

            // mutation...
            xo = xp.plus(RealVector.gaussianRandom(n, rng).timesScalar(sigma));
            yo = f.isFeasible(xo.data) ? f.valueOf(xo.data) : Double.MAX_VALUE;

            if (yo < yp) {
                mutationHistory.recordSuccess();
                xp = xo; yp = yo;
                // maybe print t, x(t), and y(t)...
                if (px == PRINT_IMPROVING) System.out.println(step + " " + xp);
                if (py == PRINT_IMPROVING) System.out.println(step + " " + yp);
            } else {
                mutationHistory.recordFailure();
            }

            // sigma adaption...
            final double successRate = mutationHistory.successRate();
            if (successRate > 0.2) {
                sigma = sigma * a;
            } else if (successRate < 0.2) {
                sigma = sigma / a;
            } else {
                sigma = sigma; // sigma(t+1) := sigma(t)
            }

            // maybe print a progress indicator dot every 100 steps...
            if (verbose && step % 100 == 0) System.out.print(".");

            step++;
        }
        // maybe print x(steps) and y(steps)...
        if (px == PRINT_ALL) System.out.println(xp);
        if (py == PRINT_ALL) System.out.println(yp);
        if (verbose) System.out.println("DONE."); // maybe print evolution end message
        
        return new IndividualFitnessPair(xp, yp);
    }

    // a type for termination criteria (predicates on step number and fitness)
    public interface ITerminationCriterion {
        boolean isFullfilled(final long step, final double fitness);
    } 

    // a type for pairs of an individual and a fitness value
    public static final class IndividualFitnessPair {
        public final RealVector individual;
        public final double fitness;
        public IndividualFitnessPair(final RealVector individual, final double fitness) {
            this.individual = individual; this.fitness = fitness;
        }
        public String toString() { return "<individual: " + individual + "\n fitness: " + fitness +">"; }
    }

    // a tool class for keeping track of the mutation success rate
    public static final class MutationHistory {
        private final LinkedList<Integer> historyList = new LinkedList<Integer>();
        private final int g;

        public MutationHistory(final int g) { this.g = g; }

        public void record(final boolean successful) {
            historyList.addFirst(successful ? 1 : 0);
            if (historyList.size() > g) {
                historyList.removeLast();
            }
        }
        public void recordSuccess() { record(true); }
        public void recordFailure() { record(false); }

        public double successRate() {
            double gSuccessful = 0.0;
            for (int elt : historyList) { gSuccessful += elt; }
            return gSuccessful / historyList.size(); // TODO inefficient
        }
    }

    // a real vector type with addition and scalar multiplication
    public static final class RealVector {
        final double[] data;

        RealVector(final double[] data) { this.data = data; }

        int dimension() { return data.length; }

        RealVector plus(final RealVector other) {
            double[] sumData = new double[this.dimension()];
            for (int i = 0; i < this.dimension(); sumData[i] = other.data[i] + data[i], i++);
            return new RealVector(sumData);
        }

        RealVector timesScalar(final double scalar) {
            double[] scalarProductData = new double[this.dimension()];
            for (int i = 0; i < this.dimension(); scalarProductData[i] = scalar * data[i], i++);
            return new RealVector(scalarProductData);
        }

        static RealVector random(final int dimension, final Random rng) {
            double[] randomData = new double[dimension];
            for (int i = 0; i < dimension; randomData[i] = rng.nextDouble(), i++);
            return new RealVector(randomData);
        }

        static RealVector gaussianRandom(final int dimension, final Random rng) {
            double[] randomData = new double[dimension];
            for (int i = 0; i < dimension; randomData[i] = rng.nextGaussian(), i++);
            return new RealVector(randomData);
        }

        public double[] asArray() { return data; }

        public String toString() { return Arrays.toString(data); }
    }

    // print a help message to stdout
    static void printUsage(final String execName) {
        System.out.println("usage: " + execName + " seed steps target f n xp0 sigma0 a g px py"
                           + "\n  where"
                           + "\n    seed    random seed ( e.g. 12345 )"
                           + "\n    steps   maximum number of evolution steps ( e.g. 10000 )"
                           + "\n    target  objective function threshold for preliminary evolution end ( e.g. 0.0001 )"
                           + "\n    f       objective function class name ( e.g. de.fhkoeln.spot.objectivefunctions.Ball )"
                           + "\n    n       problem dimension ( e.g. 12 )"
                           + "\n    xp0     starting point"
                           + "\n            ( uniform             = uniformly distributed random vector from [0.0,1.0]^n,"
                           + "\n              gaussian            = normally distributed random vector from N(0,1),"
                           + "\n              c(xp0_0,...,xp0_n)  = the vector [xp0_0,...,xp0_n],"
                           + "\n              [xp0_0,...,xp0_n]   = the vector [xp0_0,...,xp0_n],"
                           + "\n              xp0_0,...,xp0_n     = the vector [xp0_0,...,xp0_n] )"
                           + "\n    sigma0  initial step size ( e.g. 1.0 )"
                           + "\n    a       step size muliplier ( e.g. 1.2239 )"
                           + "\n    g       history length ( e.g. 12 = n )"
                           + "\n    px      individual printing mode"
                           + "\n            ( 0 = do not print individuals,"
                           + "\n              1 = only print best individual,"
                           + "\n              2 = only print improving step numbers and individuals,"
                           + "\n              3 = print every individual )"
                           + "\n    py      objective function value printing mode"
                           + "\n            ( 0 = do not print objective function values,"
                           + "\n              1 = only print best objective function value,"
                           + "\n              2 = only print improving step numbers and objective function values,"
                           + "\n              3 = print every objective function value )"
                           + "\n");
    }

    static final RealVector parseRVector(final String s) {
        try {
						String[] componentStrings = null;
						if (s.toLowerCase().startsWith("c")) {
								componentStrings = s.toLowerCase().substring(2, s.length() - 1).split(",");
						} else if (s.toLowerCase().startsWith("[")) {
								componentStrings = s.toLowerCase().substring(1, s.length() - 1).split(",");
						} else {
								componentStrings = s.toLowerCase().split(",");
						}
            double[] components = new double[componentStrings.length];
            int i = 0;
            for (final String componentString : componentStrings) {
                components[i] = Double.parseDouble(componentString.trim());
                i++;
            }
            return new RealVector(components);
        } catch (final Exception e) {
            throw new RuntimeException("R vector syntax error: " + s);
        }
    }

    // trim a vector to n-dimensions by cutting or padding with zeros
    static final RealVector trimVectorToDimension(final RealVector r, final int n) {
        final double[] ra = r.asArray();
        final int rdim = r.dimension();
        final double[] tra = new double[n];
        for (int i = 0; i < n; i++) {
            tra[i] = i < rdim ? ra[i] : 0.0;
        }
        return new RealVector(tra);
    }

    static final int PRINT_NONE = 0, PRINT_BEST = 1, PRINT_IMPROVING = 2, PRINT_ALL = 3;

    // the main method
    public static void main(final String[] args) {
        boolean argsOK = false;
        try {
            final long seed = Long.parseLong(args[0]);
            final Random rng = new Random(seed);
            final long steps = Long.parseLong(args[1]);
            final double target = Double.parseDouble(args[2]);
            final String fClassName = args[3];
            final Class<?> f = Class.forName(fClassName);
            final int n = Integer.parseInt(args[4]);
            if (n <= 0) {
                throw new IllegalArgumentException("n must be a positive number");
            }
            final String xp0String = args[5].trim().toLowerCase();
            RealVector xp0 = null;
            if (xp0String.equals("uniform")) {
                xp0 = RealVector.random(n, rng);
            } else if (xp0String.equals("gaussian")) {
                xp0 = RealVector.gaussianRandom(n, rng);
            } else {
                xp0 = parseRVector(xp0String);
                if (xp0.dimension() != n) {
                    xp0 = trimVectorToDimension(xp0, n);
                    //System.err.println("Warning: dimension of xp0 not equal to n, setting xp0 to " + xp0);
                }
            }
            final double sigma0 = Double.parseDouble(args[6]);
            final double a = Double.parseDouble(args[7]);
            final int g = Integer.parseInt(args[8]);
            final int px = Integer.parseInt(args[9]);
            if (px < 0 || px > 3) throw new IllegalArgumentException("px must be 0, 1, 2, or 3");
            final int py = Integer.parseInt(args[10]);
            if (py < 0 || py > 3) throw new IllegalArgumentException("py must be 0, 1, 2, or 3");
            argsOK = true;

            final IndividualFitnessPair runResult =
                evolution(xp0,
                          sigma0,
                          a,
                          g,
                          (IObjectiveFunction) f.newInstance(),
                          new ITerminationCriterion() {
                              public boolean isFullfilled(final long step, final double fitness) {
                                  return step >= steps || fitness <= target;
                              }
                          },
                          rng,
                          false,
                          px,
                          py);
            
            if (px == PRINT_BEST) System.out.println(runResult.individual);
            if (py == PRINT_BEST) System.out.println(runResult.fitness);
            System.exit(0);
        } catch (final Exception e) {
            if (!argsOK) printUsage("simpleOnePlusOneES");
            System.err.println("Error: " + e + "\n");
            System.exit(1);
        }
    }
}
