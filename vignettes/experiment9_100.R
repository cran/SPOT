# Determine the effect of subsetN

rm(list=ls())
library(SPOT)
library(babsim.hospital)
n <- 29 
reps <- 10
funEvals <- 10*n 
size <- 2*n
nCores <- 8

subsetN <- 100
FILENAME = "experiment9_100.RData"


progSpot <- matrix(NA, nrow = reps, ncol = 2*funEvals)
#x0 <- getStartParameter()
x0 <- matrix(as.numeric(babsim.hospital::getParaSet(5374)[1,-1]),1,)
bounds <- getBounds()
a <- bounds$lower
b <- bounds$upper
g <- function(x) {
  return(rbind(a[1] - x[1], x[1] - b[1], a[2] - x[2], x[2] - b[2], 
               a[3] - x[3], x[3] - b[3], a[4] - x[4], x[4] - b[4], 
               a[5] - x[5], x[5] - b[5], a[6] - x[6], x[6] - b[6], 
               a[7] - x[7], x[7] - b[7], a[8] - x[8], x[8] - b[8], 
               a[9] - x[9], x[9] - b[9], a[10] - x[10], x[10] - b[10],
               a[11] - x[11], x[11] - b[11], a[12] - x[12],  x[12] - b[12],
               a[13] - x[13], x[13] - b[13], a[14] - x[14],  x[14] - b[14],
               a[15] - x[15], x[15] - b[15], a[16] - x[16],  x[16] - b[16],
               a[17] - x[17], x[17] - b[17], a[18] - x[18],  x[18] - b[18],
               a[19] - x[19], x[19] - b[19], a[20] - x[20],  x[20] - b[20],
               a[21] - x[21], x[21] - b[21], a[22] - x[22],  x[22] - b[22],
               a[23] - x[23], x[23] - b[23], a[24] - x[24],  x[24] - b[24],
               a[25] - x[25], x[25] - b[25], a[26] - x[26],  x[26] - b[26],
               a[27] - x[27], x[27] - b[27], x[15] + x[16] - 1, 
               x[17] + x[18] + x[19] - 1, x[20] + x[21] - 1, x[23] + x[29] - 1)
  )
}
for(r in 1:reps){
  set.seed(r)
  print(r)
  sol <- spot(x = x0,
              fun = funBaBSimHospital,
              lower = a,
              upper = b,
              verbosity = 0,
              nCores = nCores,
              control = list(funEvals = 2*funEvals,
                             noise = TRUE,
                             designControl = list(
                               # inequalityConstraint = g,
                               size = size,
                               retries = 1000),
                             optimizer = optimNLOPTR,
                             optimizerControl = list(
                               opts = list(algorithm = "NLOPT_GN_ISRES"),
                               eval_g_ineq = g),
                             model =  buildKriging,
                             plots = FALSE,
                             progress = TRUE,
                             directOpt = optimNLOPTR,
                             directOptControl = list(
                               funEvals = 0),
                             eval_g_ineq = g,
                             subsetSelect = selectN,
                             subsetControl = list(N = subsetN))
  )
  progSpot[r, ] <- bov(sol$y, 2*funEvals)
}

matplot(t(progSpot), type="l", col="red", lty=1, xlab="n: blackbox evaluations", ylab="best objective value", log="y")
abline(v=size, lty=2)

save(progSpot, file=FILENAME)



