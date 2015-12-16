library(testthat)
library(phyloTop)
library(ape)

############################
# create some test objects
############################

tree_a <- rtree4(100)

############################
# test that evaluating at lambda immediately, or via the function, gives the same result
############################

test_that("cherries should equal nConfig evaluated at 2", {
  expect_equal(cherries(tree_a),nConfig(tree_a,2))
})
