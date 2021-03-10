options(spot.run.full.test = FALSE)

if(!getOption("spot.run.full.test")){
    skip("Skipping shorter Tests due to option spot.run.full.tests = FALSE in test.000TestSetup.R")
}