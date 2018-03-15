context("Rebuilding")

# Note that the minimum rebuild in the 10% PDS sample is ?20,520
# The total is $13,474,822,127 (13 trillion)

# After one rebuild step:
#
# 1: ...the total repair cost must have gone down if blocks are rebuilt
# 2: ...the total repair cost must not change if no blocks are rebuilt
# 3: ...the number of N grade must increase
blocks <- sample(unique(PDS.block$buildingid), 5)
test.block <- PDS.block %>% filter(buildingid %in% blocks) %>%
  BlockLevel
test.element <- PDS.element %>% filter(buildingid %in% blocks) %>%
  ElementLevel
total <- sum(test.block$block.rebuild.cost)
minimum <- min(test.block$block.rebuild.cost[test.block$block.rebuild.cost > 0])

test_that("The total rebuild cost must have gone down if something is rebuilt",
          {
            x <- Rebuild(test.element, test.block, minimum * 10) %>%
              UpdateElementRepairs
            x <- UpdateBlockRepairs(test.block, x)
            expect_less_than(sum(x$B.block.repair.cost + x$C.block.repair.cost + x$D.block.repair.cost),
                             sum(test.block$B.block.repair.cost + test.block$C.block.repair.cost + test.block$D.block.repair.cost))
          })

test_that("The total repair cost must not change when there is less money than the cheapest block",
          {
            x <- Rebuild(test.element, test.block, minimum / 2) %>%
              UpdateElementRepairs
            x <- UpdateBlockRepairs(test.block, x)
            expect_equal(sum(x$B.block.repair.cost + x$C.block.repair.cost + x$D.block.repair.cost),
                         sum(test.block$B.block.repair.cost + test.block$C.block.repair.cost + test.block$D.block.repair.cost))
          })

test_that("Blocks are rebuilt in the element.data",
          {
            x <- Rebuild(test.element, test.block, minimum * 10)
            expect_more_than(sum(x$A), sum(test.element$A))
          })

test_that("Blocks are not rebuilt in the element.data when there isn't enough money",
          {
            x <- Rebuild(test.element, test.block, minimum / 2)
            expect_equal(sum(x$A), sum(test.element$A))
          })

test_that("Areas are consistent after rebuild",
          {
           expect_equal(sum(Rebuild(test.element, test.block, minimum / 2)$unit_area), sum(test.element$unit_area))
           expect_equal(sum(Rebuild(test.element, test.block, minimum * 10)$unit_area), sum(test.element$unit_area))
           expect_equal(sum(Rebuild(test.element, test.block, total)$unit_area), sum(test.element$unit_area))
          })

context("RebuildBlock updates the data in the correct way.")

test_that("RebuildBlock changes only the appropriate block.",
          {
            to.rebuild <- test.element %>%
              filter(buildingid == blocks[1])
            not.rebuild <- test.element %>%
              filter(buildingid %in% blocks[2:5])
            rebuilt <- RebuildBlock(test.element, blocks[1])
            done <- rebuilt %>% filter(buildingid == blocks[1])
            not.done <- rebuilt %>% filter(buildingid %in% blocks[2:5])
            expect_equal(done$A, rep(1, nrow(to.rebuild)))
            expect_equal(done$B, rep(0, nrow(to.rebuild)))
            expect_equal(done$C, rep(0, nrow(to.rebuild)))
            expect_equal(done$D, rep(0, nrow(to.rebuild)))
            expect_equal(not.done$A, not.rebuild$A)
            expect_equal(not.done$B, not.rebuild$B)
            expect_equal(not.done$C, not.rebuild$C)
            expect_equal(not.done$D, not.rebuild$D)
            })

test_that("RebuildBlock changes only the appropriate blocks.",
          {
            to.rebuild <- test.element %>%
              filter(buildingid %in% blocks[1:2])
            not.rebuild <- test.element %>%
              filter(buildingid %in% blocks[3:5])
            rebuilt <- RebuildBlock(test.element, blocks[1:2])
            done <- rebuilt %>% filter(buildingid %in% blocks[1:2])
            not.done <- rebuilt %>% filter(buildingid %in% blocks[3:5])
            expect_equal(done$A, rep(1, nrow(to.rebuild)))
            expect_equal(done$B, rep(0, nrow(to.rebuild)))
            expect_equal(done$C, rep(0, nrow(to.rebuild)))
            expect_equal(done$D, rep(0, nrow(to.rebuild)))
            expect_equal(not.done$A, not.rebuild$A)
            expect_equal(not.done$B, not.rebuild$B)
            expect_equal(not.done$C, not.rebuild$C)
            expect_equal(not.done$D, not.rebuild$D)
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
