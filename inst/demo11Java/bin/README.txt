README.txt

Ergaenzung vom 2. Juli 2009:
habe ich zur 1+1 ES 
alternative Syntaxen für Vektorliterale hinzugefügt:

usage: simpleOnePlusOneES seed steps target f n xp0 sigma0 a g px py
   where
[...]
     xp0     starting point
             ( uniform             = uniformly distributed random vector 
from [0.0,1.0]^n,
               gaussian            = normally distributed random vector 
from N(0,1),
               c(xp0_0,...,xp0_n)  = the vector [xp0_0,...,xp0_n],
               [xp0_0,...,xp0_n]   = the vector [xp0_0,...,xp0_n],
               xp0_0,...,xp0_n     = the vector [xp0_0,...,xp0_n] )
[...]

Du kannst nun z.B. den Vektor [1.0,2.5,-3.25] also auch als 
[1.0,2.5,-3.25] oder sogar als 1.0,2.5,-3.25 schreiben.



SimpleOnePlusOneES

a simple 1+1 ES, written in Java
29.07.2009, Oliver Flasch (oliver.flasch@fh-koeln.de)
            based on an example by Thomas Bartz-Beielstein and Mike Preuss
            some parts by Niko Hansen


* usage example
---

clear; java -jar simpleOnePlusOneES.jar 1 10000 1.0E-100 de.fhkoeln.spot.objectivefunctions.Ball 3 "c(1.0,1.0,1.0)" 1.0 1.2239 3 0 2

or:

java -jar simpleOnePlusOneES.jar 1 1000 1e-99 de.fhkoeln.spot.objectivefunctions.Ball 10 "100,  100,  100,  100,  100,  100,  100,  100,  100,  100" 1.75 1.4592 751 0 1

* command line parameters
---

usage: simpleOnePlusOneES seed steps target f n xp0 sigma0 a g px py
  where
    seed    random seed ( e.g. 12345 )
    steps   maximum number of evolution steps ( e.g. 10000 )
    target  objective function threshold for preliminary evolution end ( e.g. 0.0001 )
    f       objective function class name ( e.g. de.fhkoeln.spot.objectivefunctions.Ball )
    n       problem dimension ( e.g. 12 )
    xp0     starting point
            ( uniform             = uniformly distributed random vector from [0.0,1.0]^n,
              gaussian            = normally distributed random vector from N(0,1),
              c(xp0_0,...,xp0_n)  = the vector [xp0_0,...,xp0_n] )
    sigma0  initial step size ( e.g. 1.0 )
    a       step size muliplier ( e.g. 1.2239 )
    g       history length ( e.g. 12 = n )
    px      individual printing mode
            ( 0 = do not print individuals,
              1 = only print best individual,
              2 = only print improving step numbers and individuals,
              3 = print every individual )
    py      objective function value printing mode
            ( 0 = do not print objective function values,
              1 = only print best objective function value,
              2 = only print improving step numbers and objective function values,
              3 = print every objective function value )


* output
---

If both px and py are != 0, px will be printed before py.


* implemented objective functions
---

de.fhkoeln.spot.objectivefunctions.Ball
de.fhkoeln.spot.objectivefunctions.ExpSurface
de.fhkoeln.spot.objectivefunctions.Rosenbrock
