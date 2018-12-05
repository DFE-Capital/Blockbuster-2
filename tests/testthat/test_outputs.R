context("Testing blockbuster output: expected proportion by row counts")

test_that("Output contains expected proportions for all grades", {

  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5,
                        elementid = 1:6, buildingid = 1, B.repair.cost = 1,
                        C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2,
                        D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  expect_named(Blockbuster(element), "expected blocks with external wall at E")
})

test_that("Expected proportions some to 1 (with a tolerance)", {
  #TODO
})

test_that("Specific test data produces the same as hand calculated results",{
  #TODO
})

#------------------------------------------------------------------------------#

context("Testing blockbuster output: expected proportion by area")

test_that("Output contains expected proportions for all grades", {
  #TODO
})

test_that("Expected proportions is sensible, e.g. sum to 1 (with a tolerance)", {
  #TODO
})

test_that("Specific test data produces the same as hand calculated results",{
  #TODO
})

#------------------------------------------------------------------------------#

context("Testing blockbuster output: expected decommissioned blocks")

test_that("Output contains expected decommissioned blocks", {
  #TODO
})

test_that("Expected decommissioned blocks is sensible", {
  #TODO
})

test_that("Specific test data produces the same as hand calculated results",{
  #TODO
})

#------------------------------------------------------------------------------#

context("Testing blockbuster output: expected decommissioned schools")

test_that("Output contains expected decommissioned schools", {
  #TODO
})

test_that("Expected decommissioned schools is sensible", {
  #TODO
})

test_that("Specific test data produces the same as hand calculated results",{
  #TODO
})

