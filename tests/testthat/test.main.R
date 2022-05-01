
context("SPOT general")

test_that("check that spot function works without problems", {
  res <- spot(x=NULL,funSphere,c(-2,-3),c(1,2))
	expect_equal(c(nrow(res$x),length(res$y)), c(20,20))
  #res <- spot(x=NULL,funSphere,c(-2,-3),c(1,2),control=list(model=buildRandomForest))	
  res <- spot(x=NULL,funSphere,c(-2,-3),c(1,2),control=list(model=buildLM))	
	expect_equal(c(nrow(res$x),length(res$y)), c(20,20))
  res <- spot(x=NULL,funSphere,c(-2,-3),c(1,2),control=list(funEvals=11,model=buildKrigingDACE))	
	expect_equal(c(nrow(res$x),length(res$y)), c(11,11))
  res <- spot(x=NULL,funSphere,c(-2,-3),c(1,2),control=list(optimizer=optimLBFGSB))	
	expect_equal(c(nrow(res$x),length(res$y)), c(20,20))
  res <- spot(x=NULL,funSphere,c(-2,-3),c(1,2),control=list(design=designUniformRandom))	
	expect_equal(c(nrow(res$x),length(res$y)), c(20,20))
	res <- spot(x=NULL,funSphere,c(-2,-3),c(1,2),control=list(modelControl=list(target="ei")))
	expect_equal(c(nrow(res$x),length(res$y)), c(20,20))
	res <- spot(x=NULL,funSphere,c(0,0),c(1,1),control=list(subsetSelect = selectAll, subsetControl=list(N=1)))
	expect_equal(c(nrow(res$x),length(res$y)), c(20,20))
	res <- spot(x=NULL,funSphere,c(0,0),c(1,1),control=list(funEvals = 30, designControl = list(size=10), subsetSelect = selectN, subsetControl=list(N=9)))
	expect_equal(c(nrow(res$x),length(res$y)), c(30,30))
	# check whether additional arguments can be passed to the objective function, here: parameter a for shifting funSphere from x to (x-a)
	# offset:
	a <- 2.34
	# dim:
	n <- 10
	#search space (only one point)
	upper <- lower <- rep(a,n)
	res <- spot(x = NULL,
	            funShiftedSphere,
	            lower, 
	            upper, 
	            control=list(funEvals = 1, 
	                         designControl = list(size=1)),
	            a = a)
	expect_equal(as.double(res$ybest), 0)
})






