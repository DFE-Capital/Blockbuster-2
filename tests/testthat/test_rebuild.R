context("Testing Rebuild function")

test_that("Check toy example for correct decisions",
          {
            # construct toy element
            block <- data.frame(buildingid = 1:4,
                                block.rebuild.cost = 0:3,
                                ratio = 0:3)
            element <- data.frame(buildingid = 1:4,
                                  elementid = 1,
                                  A = 9, # this value is never checked
                                  B = 9,
                                  C = 9,
                                  D = 9,
                                  E = 9)

            expect_equal(Rebuild(element, block, 0), element, check.attributes = FALSE) # no rebuild ( block 1 has no cost so is not eligble for rebuilding)
            expect_equal(Rebuild(element, block, 1)$A, c(9, 1, 9, 9), check.attributes = FALSE) # block 2 rebuilt (first affordable)
            expect_equal(Rebuild(element, block, 2)$A, c(9, 9, 1, 9), check.attributes = FALSE) # block 3 rebuilt (first affordable)
            expect_equal(Rebuild(element, block, 3)$A, c(9, 9, 9, 1), check.attributes = FALSE) # block 4 rebuilt (first affordable)
            expect_equal(Rebuild(element, block, 4)$A, c(9, 1, 9, 1), check.attributes = FALSE) # block 2, 4 rebuilt (first affordable, can't afford 3, can afford 2)
            expect_equal(Rebuild(element, block, 5)$A, c(9, 9, 1, 1), check.attributes = FALSE) # block 3, 4 rebuilt (first affordable, can afford 3, can't afford 2)
            expect_equal(Rebuild(element, block, 6)$A, c(9, 1, 1, 1), check.attributes = FALSE) # block 2, 3, 4 rebuilt (afford 2, 3, and 4)
            expect_equal(Rebuild(element, block, 7)$A, c(9, 1, 1, 1), check.attributes = FALSE) # block 2, 3, 4 rebuilt (even with more money, block 1 has no rebuild cost so is not rebuilt)
          })

test_that("Check toy example with nothing to rebuild for correct decisions",
          {
            # construct toy element
            block <- data.frame(buildingid = 1:4,
                                block.rebuild.cost = 0,
                                ratio = 0:3)
            element <- data.frame(buildingid = 1:4,
                                  elementid = 1,
                                  A = 9, # this value is never checked
                                  B = 9,
                                  C = 9,
                                  D = 9,
                                  E = 9)

            expect_equal(Rebuild(element, block, 0), element, check.attributes = FALSE) # no rebuild
            expect_equal(Rebuild(element, block, 1), element, check.attributes = FALSE) # no rebuild
            expect_equal(Rebuild(element, block, 2), element, check.attributes = FALSE) # no rebuild
            expect_equal(Rebuild(element, block, 3), element, check.attributes = FALSE) # no rebuild
            expect_equal(Rebuild(element, block, 4), element, check.attributes = FALSE) # no rebuild
            expect_equal(Rebuild(element, block, 5), element, check.attributes = FALSE) # no rebuild
            expect_equal(Rebuild(element, block, 6), element, check.attributes = FALSE) # no rebuild
            expect_equal(Rebuild(element, block, 7), element, check.attributes = FALSE) # no rebuild
          })

test_that("Check toy example with two tied blocks - it should fix the one with the highest ratio first",
          {
            # construct toy element
            block <- data.frame(buildingid = 1:2,
                                block.rebuild.cost = 1,
                                ratio = c(2, 1))
            element <- data.frame(buildingid = 1:2,
                                  elementid = 1,
                                  A = 9, # this value is never checked
                                  B = 9,
                                  C = 9,
                                  D = 9,
                                  E = 9)

            expect_equal(Rebuild(element, block, 0), element, check.attributes = FALSE) # no rebuild
            expect_equal(Rebuild(element, block, 1)$A, c(1, 9), check.attributes = FALSE) # rebuild 1 as it has higest ratio
            expect_equal(Rebuild(element, block, 2)$A, c(1, 1), check.attributes = FALSE) # rebuild 2 and 1
          })

test_that("Rebuild is reporting number of rebuilds correctly",
          {
            # construct toy element
            block <- data.frame(buildingid = 1:4,
                                block.rebuild.cost = 0:3,
                                ratio = 0:3)
            element <- data.frame(buildingid = 1:4,
                                  elementid = 1,
                                  A = 9, # this value is never checked
                                  B = 9,
                                  C = 9,
                                  D = 9,
                                  E = 9)
            expect_equal(attr(Rebuild(element, block, 1), "No. of rebuilds"), 1)
            expect_equal(attr(Rebuild(element, block, 2), "No. of rebuilds"), 1)
            expect_equal(attr(Rebuild(element, block, 3), "No. of rebuilds"), 1)
            expect_equal(attr(Rebuild(element, block, 4), "No. of rebuilds"), 2)
            expect_equal(attr(Rebuild(element, block, 5), "No. of rebuilds"), 2)
            expect_equal(attr(Rebuild(element, block, 6), "No. of rebuilds"), 3)
          })

context("Testing RebuildBlock")

test_that("RebuildBlock correctly updates the appropriate block", {
  element <- data.frame(buildingid = 1:2,
                        A = 9, B = 9, C = 9, D = 9, E = 9)

  expect_equal(RebuildBlock(element, 0), element) # no change as building not in data
  expect_equal(RebuildBlock(element, 1),
               data.frame(buildingid = 1:2, A = c(1, 9), B = c(0, 9), C = c(0, 9), D = c(0, 9), E = c(0,9))) # rebuilds building 1 only
  expect_equal(RebuildBlock(element, 2),
               data.frame(buildingid = 1:2, A = c(9, 1), B = c(9, 0), C = c(9, 0), D = c(9, 0), E = c(9, 0))) # rebuilds building 1 only
  expect_equal(RebuildBlock(element, 1:2),
               data.frame(buildingid = 1:2, A = 1, B = 0, C = 0, D = 0, E = 0)) # rebuilds both buildings
})

context("Testing RecursiveBudgeting function")

test_that("RecursiveBudgeting correctly identifies blocks to rebuild.",
          {
            expect_equal(RecursiveBudgeting(rep(1,5), 1:5, 1)$state, 1)
            expect_equal(RecursiveBudgeting(rep(1,5), 1:5, 2)$state, 1:2)
            expect_equal(RecursiveBudgeting(rep(1,5), 1:5, 3)$state, 1:3)
            expect_equal(RecursiveBudgeting(rep(1,5), 1:5, 4)$state, 1:4)
            expect_equal(RecursiveBudgeting(rep(1,5), 1:5, 5)$state, 1:5)
            expect_equal(RecursiveBudgeting(1:5, 1:5, 1)$state, 1)
            expect_equal(RecursiveBudgeting(1:5, 1:5, 2)$state, 1)
            expect_equal(RecursiveBudgeting(1:5, 1:5, 3)$state, 1:2)
            expect_equal(RecursiveBudgeting(5:1, 1:5, 1)$state, 5)
            expect_equal(RecursiveBudgeting(5:1, 1:5, 2)$state, 4)
            expect_equal(RecursiveBudgeting(5:1, 1:5, 3)$state, 3)
            expect_equal(RecursiveBudgeting(5:1, 1:5, 8)$state, c(1,3))
            expect_equal(RecursiveBudgeting(5:1, 1:5, 10)$state, c(1,2,5))
            expect_lt(length(RecursiveBudgeting(1:5, 1:5, 0)$state), 1)
          })



test_that("Nothing is rebuilt when there is less money than the cheapest block",
          {
            # 1 block
            A <- c(1, 0, 0, 0, 0)
            B <- c(0, 1, 0, 0, 0)
            C <- c(0, 0, 1, 0, 0)
            D <- c(0, 0, 0, 1, 0)
            E <- c(0, 0, 0, 0, 1)
            element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:5, buildingid = 1,
                                  B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                                  B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                                  gifa = 1, unit_area = 1)

            block <- data.frame(buildingid = 1,
                                block.rebuild.cost = 10,
                                B.block.repair.cost = 1,
                                C.block.repair.cost = 2,
                                D.block.repair.cost = 3,
                                E.block.repair.cost = 4,
                                ratio = 1)

            expect_equal(Rebuild(element, block, 0), element, check.attributes = FALSE)
            expect_equal(Rebuild(element, block, 5), element, check.attributes = FALSE)
            })

test_that("Blocks are rebuilt in the element.data",
          {
            # 1 block
            A <- c(1, 0, 0, 0, 0)
            B <- c(0, 1, 0, 0, 0)
            C <- c(0, 0, 1, 0, 0)
            D <- c(0, 0, 0, 1, 0)
            E <- c(0, 0, 0, 0, 1)
            element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:5, buildingid = 1,
                                  B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                                  B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                                  gifa = 1, unit_area = 1)

            block <- data.frame(buildingid = 1,
                                block.rebuild.cost = 10,
                                B.block.repair.cost = 1,
                                C.block.repair.cost = 2,
                                D.block.repair.cost = 3,
                                E.block.repair.cost = 4,
                                ratio = 1)

            funds <- 10 # enough to rebuild block

            # function to count number of elements that are at grade A (i.e. have
            # been rebuilt)
            count_grade <- function(element, grade){
              grade <- enquo(grade)
              element %>% filter(UQ(grade) == 1) %>% nrow
            }

            expect_equal(count_grade(Rebuild(element, block, funds), A),
                         5) # all elements rebuilt
            expect_equal(count_grade(Rebuild(element, block, funds), B),
                         0)
            expect_equal(count_grade(Rebuild(element, block, funds), C),
                         0)
            expect_equal(count_grade(Rebuild(element, block, funds), D),
                         0)
            expect_equal(count_grade(Rebuild(element, block, funds), E),
                         0)

          })


test_that("Blocks are not rebuilt in the element.data when there isn't enough money",
          {
            # 1 block
            A <- c(1, 0, 0, 0, 0)
            B <- c(0, 1, 0, 0, 0)
            C <- c(0, 0, 1, 0, 0)
            D <- c(0, 0, 0, 1, 0)
            E <- c(0, 0, 0, 0, 1)
            element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:5, buildingid = 1,
                                  B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                                  B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                                  gifa = 1, unit_area = 1)

            block <- data.frame(buildingid = 1,
                                block.rebuild.cost = 10,
                                B.block.repair.cost = 1,
                                C.block.repair.cost = 2,
                                D.block.repair.cost = 3,
                                E.block.repair.cost = 4,
                                ratio = 1)

            funds <- 5 # not enough to rebuild block
            # function to count number of elements that are at grade A (i.e. have
            # been rebuilt)
            count_grade <- function(element, grade){
              grade <- enquo(grade)
              element %>% filter(UQ(grade) == 1) %>% nrow
            }

            expect_equal(count_grade(Rebuild(element, block, funds), A),
                         1)
            expect_equal(count_grade(Rebuild(element, block, funds), B),
                         1)
            expect_equal(count_grade(Rebuild(element, block, funds), C),
                         1)
            expect_equal(count_grade(Rebuild(element, block, funds), D),
                         1)
            expect_equal(count_grade(Rebuild(element, block, funds), E),
                         1)

          })
