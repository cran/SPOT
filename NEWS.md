# SPOT

### 2.5.0
* CHANGES:
   * Miscellaeneuos functions and related R scripts moved to the new package SPOTMisc:
     * funExternal.R
     * funMarkovChain.R
     * mlrTools.R
     * modelMarkovChain.F
     * subgoups() from spotTools.R

### 2.4.8
* CHANGES
  * mlrTools:  package=TRUE -> package = "mlr"


### 2.4.6.
* NEW:
   * mlrTools.R (experimental)

### 2.4.4
* CHANGES:
  * Minor corrections in the documentation
  
### 2.4.2 
* CHANGES:
    * sequentialBifurcation example removed, because 
        * execution time might be too long or
        * example fails, because of missing data
        
### 2.4.0
* NEW:
  * Bayesian Optimization Model: buildBO

### 2.3.4
* CHANGES:
   * checks for most recent babsim.hospital version
   
### 2.3.2
* CHANGES:
   * spot() allows equal lower and upper bounds

### 2.3.0
* CHANGES
   * Cleanup vignettes

### 2.2.24
* CHANGES
  * Tests updated
  * region* data added

### 2.2.22
* CHANGES:
  * Examples check whether babsim.version is geq 11.7.

### 2.2.20
* FIXED:
   * Bug: failure (test.infillExpectedImprovement.R:26:5) fixed

### 2.2.18
* BUGS:
  * The following test was removed and should be checked:
     * Failure (test.infillExpectedImprovement.R:26:5): "check that infillExpectedImprovement has same result as target=ei".
  
### 2.2.16
* NEW:
  * funBabsimhospitalParallel() for parallel execution on each machine (affects unix systems only)

### 2.2.14
* CHANGES:
   * vignette updated

### 2.2.12
* CHANGES
   * directOpt accepts constraints (similar to optimizer). These can be specified via directOptControl
   * optimNLOPTR() returns x and y values from nloptr() optimization runs

### 2.2.10
* CHANGES:
   * Hybrid approach refined

### 2.2.8
* CHANGES:
  * selection of the final best solution: take the result from directOpt only if
  it is better than the surrogate solution
  
### 2.2.6
* CHANGES:
  * spotTools functions use vectors instead of matrices to define bounds
  
### 2.2.4
* NEWS:
   * reports added:
      * sensitivity analysis

## 2.2.2
* CHANGES:
   * donttest instead of dontrun

## 2.2.0
* CHANGES:
   * Some surrogate models can be updated: information whether the model is already build is stored in
    control$modelControl$modelInitialized 
    * objectiveFunctionEvaluation() returns NULL if xNew is empty
* NEW:    
    * funBabsimHospital()

## SPOT 2.1.12
* CHANGES:
   * plotly dependencies

## SPOT 2.1.10
* BUGFIX:
  * spot evaluates funEvals argument correctly

## SPOT 2.1.8
* CHANGES:
  * optimNLOPTR can handle additional fun arguments via "..." 

## SPOT 2.1.7
* NEWS:
  * optimNLOPTR extended

## SPOT 2.1.6
* NEWS:
  * direct mode extended
  
## SPOT 2.1.4
* NEWS:
  * spotControl(): option direct added

## SPOT 2.1.3
* NEWS:
  * buildLasso() added
  
## SPOT 2.1.2
* CHANGES:
  * reverse dependencies fixed

## SPOT 2.1.0
* CHANGES:
  * cleanup files
  
## SPOT 2.0.68
* CHANGES:
  * cleanup data

## SPOT 2.0.66
* CHANGES:
  * documentation improved
    
## SPOT 2.0.64

* CHANGES:
  * warnings are only issued if control$verbosity > 0
  * wrong upper and/or lower bounds result in stop (and not warning)
  * tests modified accordingly
  
* NEW:
  * test.repair

## SPOT 2.0.62

* NEW:
   * parseTunedRegionModel: return y-values 

## SPOT 2.0.60

* NEW:
  * plotSIRModel()
    
## SPOT 2.0.58

* evalMarkovChain: messages can be generated (verbosity = 1)

## SPOT 2.0.56

* spot: uses suppressWarnings(suppressMessages()

## SPOT 2.0.54

* Bug fix: 
  * preprocessCdeTestData adds slash to Region name

## SPOT 2.0.52

* generateMCPrediction: new argument startSimulation. 
  Default is the old internal value ("2020-01-22").

## SPOT 2.0.50

* new functions to handle ourworldindata.org coronavirus data:
   * preprocessCdeTestData 
   * preprocessCdeInputData

## SPOT 2.0.48

* Vignette cleanup

## SPOT 2.0.46

* New function preprocessCdeInputData

## SPOT 2.0.44

* New data sets: DEcde20200813 and cde20200813

## SPOT 2.0.42    

* funMarkovChain: new fitness function (log rmse) 

## SPOT 2.0.40    

* modelMarkovChain: run SIR model does not use a fixed seed any more: 
  run(model, seed = 123) -> run(model)
* modelMarkovChain:  new parameter n = number of nodes
* modelMarkovChain: dontrun example 
* funMarkovChain: dontrun example 

## SPOT 2.0.38

* tuneRegionModel: accepts lower and upper bounds 

## SPOT 2.0.36

* Bugfix in generateMCPrediction: Index corrected 

## SPOT 2.0.34

* Added a `NEWS.md` file to track changes to the package.
* internal data resTunedRegionModel
* vignette updated
* documentation
* examples cleaned


## SPOT  2.0.32
* extdata csv files converted to rda and moved to data
* generateMCPrediction() rewritten
  
## SPOT  2.0.30
* Covid Vignette and tools rewritten
* default: no warnings any more
* verbosity levels implemented

## SPOT  2.0.28
* new vignette added: nutshell
  
## SPOT  2.0.26
* import, importFrom fixed

## SPOT  2.0.24
* additional function set: select*
* first select* functions implemented:
   * "selectAll.R" (only dummy function)
   * "selectN.R" select the last N points only

## SPOT  2.0.22
* new implementation of funRosen (for Master AIT students)
  
## SPOT  2.0.21*
* new implementation of funMarkovChain and modellMarkovChain 

## SPOT  2.0.20
* new implementation of funMarkovChain and modellMarkovChain (still not tested!)

## SPOT  2.0.19
* sring test added

## SPOT  2.0.17
* Fixed data files generated with function funSring.R:
  * sringRes*.rda and resSpot.rda

## SPOT  2.0.16
* Fixed several bugs in function funSring.R

## SPOT  2.0.15
* new function funBBOBCall() to call BBOB functions using the smoof package

## SPOT  2.0.14
* example tuneRegionMmodel() runs faster
* example parseTunedRegionModel() runs faster
* new function buildTreeModel implemented (rpart)

## SPOT  2.0.13
* S-Ring vignette updated. spot runs added. 
* New data spotRes* and sringRes* added
* spotLoop() help updated

## SPOT  2.0.12
* S-Ring vignette updated. New data sringRes1 and sringRes2 added
  
## SPOT  2.0.11
* funSring implemented. Simple elevator simulator based on the sequential ring (S-Ring)

## SPOT  2.0.10
* visualizing  progress via "control options progress = TRUE"" prepared, but not fully implemented

## SPOT  2.0.8
* Markov Chain model implemented: funMarkovChain, modelMarkovChain
* cyclone.R moved to funCyclone.R 

## SPOT  2.0.6
* The Kriging model available via buildKriging now includes different options for simulation (via the simulate.kriging method)

## SPOT  2.0.5
* Typos corrected

## SPOT  2.0.4
 
* Various fixes to the documentation and help files
* The orientation of the surface plots in plot* functions with persp3d is now fixed
* design points generated in the initialization of the spot function are now
    also rounded if they are supposed to be integers
* Minor bug fixes

## SPOT  2.0.3
* Major bug fixed in the model produced by buildKriging. The bug occured when using 
		optimizeP=TRUE in the control, and affected the predict function.
* Before prediction with predict.kriging, the model will now round integer/factor variables to avoid
		issues caused by optimizers that produce numeric values.
* There is now a wrapper function "wrapFunction" available, to help use SPOT with simple, non.vectorized
		objective functions.
* A 3D plot was added to the plotFunction function (also available in plotModel and plotData).
	  It can be generated with type="persp3d"
	
## SPOT  2.0.2
* 3 new interfaces to existing optimization packages: optimDE, optimGenoud, optimNLOPTR
* An implementation of an evolution strategy was added: optimES
* Inequality constraints can now be handled (via the designLHD for designs, and optimNLOPTR for optimizers)
* buildSO was replaced by buildRSM
* As an additional model, the the LOESS model is now available via buildLOESS
* Several tools for creating simple surface plots from models/data/functions were added to the package: 
		plotData, plotModel, plotFunction.
* Various minor fixes for documentation and code

## SPOT  2.0.1
* This version is a complete rewrite of the SPOT package
    * A more modular architecture is provided, that allows the user to easily customize parts of the SPO procedure.
    * Core functions for modeling and optimization use interfaces more similar to functions from other packages / core-R, 
		 hence making them easier accessible for new users. Also, these can now be more easily used separately from the main SPO 
		 approach, e.g., only for modeling / DoE / optimization.
    * The unnecessarily large number of choices and parameters was reduced significantly.
    * Removal of extremely rarely used / un-used features, to reduce overall complexity of the framework.
    * Documentation and accessibility in were improved.
    * Some frequently used procedures received a speed-up.
* Please note that not all features of the older versions are available in SPOT.
* Please depend on "SPOT (<= 1.1.0)" if your code relies on the previous architecture,
	and consider to adapt to the new one. 
* Several examples for working with the rewritten framework are provided, e.g., in the documentation of the spot() function.
		
Release notes of older Versions:	
	
## SPOT  1.1.0
  1. Fixed Help Documentation
  2. Stability of forrBuilder improved
	3. Bugfix spotCreateDesignLhd: nested designs scaled correctly now
	4. Required maintenance (imports etc.)

## SPOT  1.0.5543
	1. SPOT may now handle NA values returned by target function (tuned algorithm)
		 in case a Kriging surrogate model is used.
		 See help of spotRepairMissingValues
	2. SPOT can now handle constraints during surrogate-model optimization, by 
		 using constraint optimization approaches in nloptr package.
		 Constraint function has to be supplied to spotConfig$alg.constraint.ineq,
		 and a nloptr algorithm has to be used with spotModelOptim.
	3. New demo: Tuning RGP with MAMP approach
	4. Code cleanup and several minor bugfixes
	5. Added source code for java GUI and java 1+1-ES to package
	6. Improvements to documentation, fixed links
	
## SPOT  1.0.4184
* Several bugfixes, including compatibility with rgp >= 0.3-4

## SPOT  1.0.4045
	1. Contour plot can now show all observations as crosses, when spotConfig$report.observations.all is set.
	2. The models spotPredictEarth and spotPredictRandomForest will now turn variables marked as "FACTOR" in the ROI into real factors with as.factor()
	3. There is now a new report function, for runs with spotPredictEarth only: spotReportEarth
	4. spotPredictForrester Models do not have to be rebuild if evaluated with different options
	5. Several infill criteria like Expected Improvement added, see spotInfill...
	6. Code cleanup and speed fixes
	7. Bugfix for multi criteria optimization: A crash has been fixed, that occurred when using sms-emoa for optimizing the surrogate model in certain situations.
	8. A R-port of the DACE-Kriging Toolbox was added to SPOT. See: daceBuilder and spotPredictDace
	10. SPOT runs with no file output that crashed or were interrupted by the user can now be continued, if spotConfig$spot.catch.error is set to TRUE. 
		Otherwise, only interrupted runs can be continued properly.
	11. Contour and 3d surface plots were adapted for multi objective optimization.
	13. Several examples of Model-Ensembles were added, e.g., spotPredictEnsemble***
	14. If cmaes is used as an optimizer on the model, it will use the vectorize option where possible.
	16. spotPredictLmOptim was removed, see the demo spotDemo14LmOptimSann for replacement functionality
	17. Added the Gaussian Landscape Generator to the package for the purpose of test function generation, see spotGlgCreate
	18. New easy to use interfaces to train surrogate models, or predict with them: spotModel.train, spotModel.predict and spotModel.func.
	19. Plotting tools for surfaces: spotSurf3d and spotSurfContour.
	20. By using the spotPredictMCO function one can use different models for each optimized objective, rather than model all objectives with the same model.
		For instance, for an optimization problem with three objectives, one can select Co-Kriging for the first objective, Kriging for the second and
		Random Forest for the third.
	21. Report functions for performing a Mixed Model analysis have been added, which are spotReportMAMP and spotReportSAMP. Examples are provided in demos 21 and 22.
	
## SPOT  1.0.2662
	Removed the dependency on the maptree package and changed the corresponding default report function spotReportDefault
	
## SPOT  1.0.2661
	1. Significant speed-up for spotPredictForrester, by factors of 2 or more (depending on dimensionality and number of observations)
	2. Fix in spotPredictForrester: Sometimes L-BFGS-B might violate boundaries, this is now checked and controlled by the calling function
	3. Better coloring in 3d plots (spotReport3d), both surface plot reports (spotReport3d and spotReportContour) use the same color scheme.
	4. Bugfix when using OCBA: RNG seeds are now determined in the right order
	5. Multi criteria optimization with SPOT now uses a more appropriate infill criterion: When estimating the hypervolume contribution of points suggested by a model, allready known points will also be considered.
	6. The Evolutionary Strategy used as a target algorithm for SPOT can now be called with an optim-like interface: esOptim
	7. For stability reasons, the nugget effect in the Kriging model spotPredictDice is now always on.
	
## SPOT  1.0.2257
	Version has changed from 0.1.* to 1.0.*, since now one of the most
	important predictors from the original Matlab package is implemented, 
	as well	as methods for Multi Criteria Optimization.
	More detailed changes are:
	1. Fixed auto.loop.nevals stopping criterion (one superfluous step)
	2. Added several additional examples of surrogate model interfaces: 
		spotPredictForrester (new, more stable Kriging model,
			based on Forrester's code as used with original Matlab-SPOT)
		spotPredictEarth (MARS, based on earth package)
		spotPredictGausspr (based on kernlab package gausspr)
		spotPredictKsvm (SVM based on kernlab package)
		spotPredictPsvm (Penalized SVM)
		spotPredictQrnn (Quantile regression neural network, from qrnn package) 
	3. Adaptation for R 2.14.X (Namespace and others)
	4. Dependencies changed, removed unused dependencies, moved optional 
		dependencies to suggests
	6. MSPOT: Spot can now do multi criteria optimization, see also 
		vignette("MultiCriteriaSPOT").
	7. Updated some demos to use new and better features
	9. Changed default settings in SANN and ES algorithm interfaces
	10. Bugfix to seed handling in target algorithm/function interfaces: 
		Will now be saved and loaded to make sure that seed of main SPOT 
		processes is undisturbed.
	11. spotPredictMlegp can be run with limited number of observations 
		(randomly sampled), to prevent exploding time consumption
	12. Documentation updated
	14. New demo for tuning symbolic regression (RGP)
	15. New report functions which create surface plots: 
		spotReport3d, spotReportContour
	16. Changes to spotGetOptions and related functions:
	* any .path variables in the spotConfig were removed. User should 
			make sure to load function to workspace.
	* Removed unused parameter settings
	* Settings of optional modules/functions will now be handled in 
			those very functions, not in spotGetOptions. That means, they 
			will only be in the list of parameters if actually used or 
			set by the user.
	17. Region of Interest will now use lower/upper instead of low/high 
		for column names of boundaries. A new constructor can be used to 
		create Region Of Interests: spotROI
	18. Some internal functions now use single arguments instead of 
		parameter list (spotWriteDes, spotWriteAroi, spotReadRoi)
	19. Updated the spotGui() function to fit to the changes (new version: 1.3)
	20. spot() and spotOptim() have now the argument "..." (dot-dot-dot) to 
		pass on additional parameters to a target function.

	
## SPOT  0.1.1550
	1. Fixed bugs with regard to problems with handling random number generator seed
	2. Changes to spotTask "meta" (spotStepMetaOpt): New structure, not fully documented yet.
	3. Sensitivity report will not use the same graphic window as the online plot any-more
	4. Changed/added suitable error messages
	3. some other smaller bug fixes
	
	
## SPOT  0.1.1375
	1. Important new function: 
		spotOptim: an optim like spot interface
		spotOcba: used with spotCreateSequentialDesignOcba. It is the new default, 
				but works only with noisy functions. See documentation. 
	2. Prediction functions should now return spotConfig. Both the model fit 
		(if available, used for the predict interface) and the y values 
		for the large design are returned inside spotConfig.
	3. Any model fit that works with the predict interface can now be optimized in 
		a additional function call in the sequential design step.
		Therefore a new parameter is introduced: spotConfig$seq.predictionOpt.func
		See also: spotPredictOptMulti
	4. Usability changes to the spotGui
	5. Updated documentation 
	6. Setting spot.seed or alg.seed to NULL will result into randomly chosen seeds.
	7. spot() and spotOptim() can be started without file input or output.
	8. New test functions
		spotFuncStartMexicanHat
		spotFuncStartRastrigin
		spotFuncStartRosenbrock
	9. Function handles can be passed to spot or spotOptim, instead of strings, for 
		the used objective function.
	10. re-added to the package: spotPredictKrig
	11. several bugfixes to predictors and other functions
	12. Where possible, verbosity of other packages used by spot is now in line with spot's verbosity
	
	Known major problems:
	spotpredictLmOptim does not work with interface changes (this also affects the corresponding demo)
		
	
## SPOT  0.1.1065
	1. New parameters in spotConfig:
		a.	spot.fileMode: Boolean, defines if 
			created data is logged in res/des/bst/aroi files
			If TRUE (default) files will be written, else
			the results will be written to spotConfig (see
			the following parameters)
		b.	alg.currentResult: Holds the results of the
			target algorithm or function that is optimized 
			by spot, as a data frame. (earlier only in .res file)
		c.	alg.aroi: this holds the aroi (earlier only in .aroi 
			file)
		d.	alg.currentBest: This holds a data frame with the best
			results (earlier only in .bst files)
		e.	alg.currentDesign: This holds a data frame with the 
			design to be evaluated by the next  run  task.(earlier
			only in .des files)

	2. Changes to user-defined functions:

		spotConfig$alg.func:
		This function now receives and returns only spotConfig.
		This means resFileName, apdFileName, and desFileName are no
		input arguments any more, they need to be read from 	spotConfig.
		The function needs to be changed to write the  result  data
		frame to the spotConfig$alg.currentResult.
		It can also be written to .res file, if wanted by the user.
		(check for spotConfig$spot.fileMode if needed)

		spotConfig$report.func
		The function now also returns spotConfig.
		To get the best value the following lines are be used now in the default report: 
		#################################	
		spotConfig=spotWriteBest(mergedData, spotConfig);
		C1=spotConfig$alg.currentBest[nrow(spotConfig$alg.currentBest),]
		#################################

		In general some spot functions you might use in your
		user-defined functions will now return spotConfig.

	3. spot() can now be called with a list of settings, e.g. spotConfig.
		example: spot( example.conf , auto ,spotConfig=list(auto.loop.nevals=150)).
		spot() will also return the spotConfig. This can then be feed into the next spot() run.

	4. Changes to the spotGui:
		a.	The spotGui layout was changed, a menu bar added.
		b.	New feature: Create your own test functions with
		the function generator

		
## SPOT  0.1.1016:
	1. New function: spotGui()
		Starts a java gui to work with SPOT
	2. New parameters for the  meta  task:
	* spotConfig$report.meta.func
	* spotConfig$report.meta.path
		Define name and path of a report that summarizes a meta experiment
	3. Fixed Documentation

	
Initial version: 0.1.888
