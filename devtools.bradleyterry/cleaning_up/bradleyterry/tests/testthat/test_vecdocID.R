context("Estimation of Lambda for a vector of Doc ID")


#This is the test==================================================
test_that("Dataframe lambda output",{
  expect_that(bradleyterry(1,1,c(1,2,3),lambda,dataset), equals(6/11))
})

