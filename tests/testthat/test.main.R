test_that("check that spot function works without problems", {
  res <- spot(,funSphere,c(-2,-3),c(1,2))
  res <- spot(,funSphere,c(-2,-3),c(1,2),control=list(model=buildRandomForest))	
  res <- spot(,funSphere,c(-2,-3),c(1,2),control=list(model=buildLM))	
  res <- spot(,funSphere,c(-2,-3),c(1,2),control=list(funEvals=11,model=buildKrigingDACE))	
  res <- spot(,funSphere,c(-2,-3),c(1,2),control=list(optimizer=optimLBFGSB))	
  res <- spot(,funSphere,c(-2,-3),c(1,2),control=list(design=designUniformRandom))	
	res <- spot(,funSphere,c(-2,-3),c(1,2),control=list(modelControl=list(target="ei")))
})