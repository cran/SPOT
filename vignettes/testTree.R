library("SPOT")
library("SimInf")
library("parallel")
funEvals = 10000
low <- c(1e-07, 1e-03, 1e-03, 1e-02)
up <-  c(1e-05, 0.5 , 0.5 , 0.5)
deData <- preprocessCdeInputData(DEcde20200813[1:220,])
deList <- lapply(X = deData, 
                 FUN = tuneRegionModel, 
                 pops = NULL, 
                 lower = low,
                 upper = up,
                 control=list(funEvals= funEvals, 
                              model = buildRandomForest,
                              optimizer=optimLBFGSB,
                              noise = FALSE,
                              replicates = 1,
                              subsetSelect= selectN, 
                              subsetControl = list(N=5000),
                              progress = TRUE,
                              plots = FALSE,
                              verbosity = 1))
save(deList, file="deTunedRegionListTest.Rdata")
deParsedList <- parseTunedRegionModel(deList)
save(deParsedList, file="deParsedListTest.Rdata")
deModels <- deParsedList$models
dePops <- deParsedList$pops
save(dePops, file="dePopsTest.Rdata")
save(deModels, file="deModelsTest.Rdata")
# Test
deTestData <- preprocessCdeTestData(DEcde20200813[221:227,])
deSubmit <- generateMCPrediction(testData = deTestData, startSimulation = "2020-01-22", models = deParsedList$models, write = FALSE)
dePredict <- cbind(deSubmit, deTestData$Date, deTestData$Region)
names(dePredict) <- c("ForecastID", "confirmed", "fatalities", "date", "region")
save(dePredict, file="dePredictTest.Rdata")
