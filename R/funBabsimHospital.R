#' @title Optimization of the BaBSim.Hospital Simulator
#' 
#' @description \code{funBaBSimHospital} implements an interface to the 
#' \code{babsim.hospital} package.
#' \code{babsim.hospital} is a discrete-event simulation model 
#' for a hospital resource planning problem.  
#' The project is motivated by the challenges faced by health care institutions in 
#' the COVID-19 pandemic. 
#' It can be used by health departments to forecast demand for intensive care beds, 
#' ventilators, and staff resources.
#' \code{funBaBSimHospital} provides an interface to \code{\link[babsim.hospital]{getTrainTestObjFun}}.
#' 
#' @param x \code{matrix} of points to evaluate with the simulator. 
#' Rows for points and columns for dimension.
#' @param region \code{integer}. Represents the region code. Default: 5374 (Oberberg).
#' @param nCores \code{integer}. Defines the number of cores.
#' @param verbosity \code{integer}. Handles output. Default: 0
#' @param rkiEndDate \code{characters}. Last day of rki data. Default \code{"2020-12-09"}
#' @param icuEndDate \code{characters}. Last day of icu data. Default \code{"2020-12-09"}
#' @param trainingWeeksSimulator \code{integer}. Training period using rki data. Default: 10. Should be larger than \code{trainingWeeksField}.
#' @param trainingWeeksField \code{integer}. Training period using icu data. Default: 6. Should be smaller than \code{trainingWeeksSimulator}.
#' @param totalRepeats \code{integer}. Number of repeats for each configuration. Should be a multiple of \code{nCores}. Default: 10.
#' 
#' @return y \code{numeric} function value.
#' 
#' @importFrom parallel mclapply
#' 
#' @examples
#' ## babsim.hospital version must be greater equal 11.7:
#' # ver <-  unlist(packageVersion("babsim.hospital"))
#' # if( ver[1] >= 11 & ver[2] >= 7){
#' #    x <- matrix(as.numeric(babsim.hospital::getParaSet(5374)[1,-1]),1,)
#' #    funBaBSimHospital(x)
#' #    }
#' @export
funBaBSimHospital <- function(x,
                              region = 5374,
                              nCores = 2,
                              verbosity = 0,
                              rkiEndDate = "2020-12-09",
                              icuEndDate = "2020-12-09",
                              trainingWeeksSimulator = 10,
                              trainingWeeksField = 6,
                              totalRepeats = 10) {
  
  if(verbosity > 0 & nCores > 1){
    warning("verbosity in funBaBSimHospital only works with 'nCores = 1'. Turning verbosity off!")
    verbosity <- 0
  }
  
  makeFunBaBSimHospital  <- function(region,
                                     nCores,
                                     verbosity,
                                     rkiEndDate,
                                     icuEndDate,
                                     trainingWeeksSimulator,
                                     trainingWeeksField,
                                     totalRepeats) {
    singleRepeat <- function(index, x) {
      rkiwerte <- babsim.hospital::rkidata
      icuwerte <- babsim.hospital::icudata
      rkiwerte <- rkiwerte[rkiwerte$Refdatum <= as.Date(rkiEndDate), ]
      icuwerte <- icuwerte[icuwerte$daten_stand <= as.Date(icuEndDate), ]
      fun <- babsim.hospital::getTrainTestObjFun(
        region = region,
        rkiwerte = rkiwerte,
        icuwerte = icuwerte,
        TrainSimStartDate = as.Date(rkiEndDate) - trainingWeeksSimulator * 7,
        TrainFieldStartDate = as.Date(icuEndDate) - trainingWeeksField * 7,
        tryOnTestSet = FALSE
      )
      fun(x)
    }
    
    function(x) {
      res <- doParallel(1:totalRepeats,
                      singleRepeat,
                      nCores = nCores,
                      x)
      y <- as.numeric(unlist(res))
      median(y)
    }
  }
  
  matrix(apply(x, 1,
    makeFunBaBSimHospital(
      region = region,
      nCores = nCores,
      verbosity = verbosity,
      rkiEndDate = rkiEndDate,
      icuEndDate = icuEndDate,
      trainingWeeksSimulator = trainingWeeksSimulator,
      trainingWeeksField = trainingWeeksField,
      totalRepeats = totalRepeats
    )
  ),
  , 1) # number of columns
}