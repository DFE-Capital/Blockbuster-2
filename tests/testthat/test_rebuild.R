context("Testing Rebuild function")

# RebuildBlock tests
# create block and element objects for testing
# block should have ratio and block.rebuild.cost
# element has buildingid, A, B, C, D, E

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

            expect_equal(Rebuild(element, block, 0), element) # no rebuild ( block 1 has no cost so is not eligble for rebuilding)
            expect_equal(Rebuild(element, block, 1)$A, c(9, 1, 9, 9)) # block 2 rebuilt (first affordable)
            expect_equal(Rebuild(element, block, 2)$A, c(9, 9, 1, 9)) # block 3 rebuilt (first affordable)
            expect_equal(Rebuild(element, block, 3)$A, c(9, 9, 9, 1)) # block 4 rebuilt (first affordable)
            expect_equal(Rebuild(element, block, 4)$A, c(9, 1, 9, 1)) # block 2, 4 rebuilt (first affordable, can't afford 3, can afford 2)
            expect_equal(Rebuild(element, block, 5)$A, c(9, 9, 1, 1)) # block 3, 4 rebuilt (first affordable, can afford 3, can't afford 2)
            expect_equal(Rebuild(element, block, 6)$A, c(9, 1, 1, 1)) # block 2, 3, 4 rebuilt (afford 2, 3, and 4)
            expect_equal(Rebuild(element, block, 7)$A, c(9, 1, 1, 1)) # block 2, 3, 4 rebuilt (even with more money, block 1 has no rebuild cost so is not rebuilt)
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

            expect_equal(Rebuild(element, block, 0), element) # no rebuild
            expect_equal(Rebuild(element, block, 1), element) # no rebuild
            expect_equal(Rebuild(element, block, 2), element) # no rebuild
            expect_equal(Rebuild(element, block, 3), element) # no rebuild
            expect_equal(Rebuild(element, block, 4), element) # no rebuild
            expect_equal(Rebuild(element, block, 5), element) # no rebuild
            expect_equal(Rebuild(element, block, 6), element) # no rebuild
            expect_equal(Rebuild(element, block, 7), element) # no rebuild
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

            expect_equal(Rebuild(element, block, 0), element) # no rebuild
            expect_equal(Rebuild(element, block, 1)$A, c(1, 9)) # rebuild 1 as it has higest ratio
            expect_equal(Rebuild(element, block, 2)$A, c(1, 1)) # rebuild 2 and 1
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

context("Recursive rebuilding identifier")

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


# run tests on random blocks from PDS.data
context("Testing rebuilding on randomly sampled blocks")

test_that("Nothing is rebuilt there is less money than the cheapest block",
          {
            # sample 5 blocks
            block_numbers <- sample(unique(PDS.block$buildingid), 5)
            element <- PDS.element %>% filter(buildingid %in% block_numbers)
            block <- PDS.block %>% filter(buildingid %in% block_numbers)

            expect_equal(Rebuild(element, block, 0), element)
            expect_equal(Rebuild(element, block, min(block$block.rebuild.cost) / 2), element)
            })

test_that("Blocks are rebuilt in the element.data",
          {
            # sample 5 blocks
            block_numbers <- sample(unique(PDS.block$buildingid), 5)
            element <- PDS.element %>% filter(buildingid %in% block_numbers)
            block <- PDS.block %>% filter(buildingid %in% block_numbers)
            funds <- max(block$block.rebuild.cost) # enough to rebuild at least one block

            # function to count number of elements that are at grade A (i.e. have
            # been rebuilt)
            count_grade <- function(element, grade){
              grade <- enquo(grade)
              element %>% filter(UQ(grade) == 1) %>% nrow
            }

            expect_gte(count_grade(Rebuild(element, block, funds), "A"),
                       count_grade(element, "A"))
            expect_lte(count_grade(Rebuild(element, block, funds), "B"),
                       count_grade(element, "B"))
            expect_lte(count_grade(Rebuild(element, block, funds), "C"),
                       count_grade(element, "C"))
            expect_lte(count_grade(Rebuild(element, block, funds), "D"),
                       count_grade(element, "D"))
            expect_lte(count_grade(Rebuild(element, block, funds), "E"),
                       count_grade(element, "E"))

          })

test_that("Blocks are not rebuilt in the element.data when there isn't enough money",
          {
            # sample 5 blocks
            block_numbers <- sample(unique(PDS.block$buildingid), 5)
            element <- PDS.element %>% filter(buildingid %in% block_numbers)
            block <- PDS.block %>% filter(buildingid %in% block_numbers)
            funds <- min(block$block.rebuild.cost) - 1 # not enough to rebuild any block

            # function to count number of elements that are at grade A (i.e. have
            # been rebuilt)
            count_grade <- function(element, grade){
              grade <- enquo(grade)
              element %>% filter(UQ(grade) == 1) %>% nrow
            }

            expect_equal(count_grade(Rebuild(element, block, funds), "A"),
                       count_grade(element, "A"))
            expect_equal(count_grade(Rebuild(element, block, funds), "B"),
                       count_grade(element, "B"))
            expect_equal(count_grade(Rebuild(element, block, funds), "C"),
                       count_grade(element, "C"))
            expect_equal(count_grade(Rebuild(element, block, funds), "D"),
                       count_grade(element, "D"))
            expect_equal(count_grade(Rebuild(element, block, funds), "E"),
                       count_grade(element, "E"))

          })
