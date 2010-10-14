## Load package
require("SPOT")
## (for calls from demo():) disable interactive confirmation of plots
old.demo.ask <- options("demo.ask" = FALSE)
## get path of test project
testPath<-.find.package("SPOT")
testPath<-file.path(testPath,"demo05TgpBranin")
## show path
testPath
## run example
testFile<-file.path(testPath,"demo05TgpBranin.conf")
spotConfig=spot(testFile)
## (for calls from demo():) reset confirmation of plots to previous setting
options("demo.ask" = old.demo.ask)
