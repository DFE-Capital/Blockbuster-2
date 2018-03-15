context("Repairing")

# After repairing the estate
#
# 1 The total cost of repairs must have gone down
# 2. No repairs if not enough money.
#
# total cost of repairs in 10% pds sample is £735,688,604
# cheapest repairs in 10% pds sample is 0.205


# sample 5 blocks to test
blocks <- sample(unique(PDS.block$buildingid), 5)
test.block <- PDS.block %>% filter(buildingid %in% blocks) %>%
  BlockLevel
test.element <- PDS.element %>% filter(buildingid %in% blocks) %>%
  ElementLevel
total <- sum(test.block$B.block.repair.cost + test.block$C.block.repair.cost +
               test.block$D.block.repair.cost)
minimum <- 0.1 # this is smallest than the smallest repair cost in the 10% sample


test_that("Total backlog has decreased and by less than funding", {
  x <- Repair(test.element, test.block, 10000) %>%
    UpdateElementRepairs %>%
    ungroup %>%
    summarise(backlog = sum(B.repair.total + C.repair.total + D.repair.total)) %>% as.numeric
  expect_lt(x, total)
  expect_lt(total - x, 10000)
})

test_that("Total backlog has decreased and by less than funding", {
  x <- Repair(test.element, test.block, 1000) %>%
    UpdateElementRepairs %>%
    ungroup %>%
    summarise(backlog = sum(B.repair.total + C.repair.total + D.repair.total)) %>% as.numeric
  expect_lt(x, total)
  expect_lt(total - x, 1000)
})

test_that("Total backlog has decreased and by less than funding", {
  x <- Repair(test.element, test.block, 100000) %>%
    UpdateElementRepairs %>%
    ungroup %>%
    summarise(backlog = sum(B.repair.total + C.repair.total + D.repair.total)) %>% as.numeric
  expect_lt(x, total)
  expect_lt(total - x, 100000)
})

test_that("Total backlog does not change with zero funding", {
  x <- Repair(test.element, test.block, 0) %>%
    UpdateElementRepairs %>%
    ungroup %>%
    summarise(backlog = sum(B.repair.total + C.repair.total + D.repair.total)) %>% as.numeric
  expect_equal(x, total)
  })

test_that("Proportions remain consistent after repair",
          {
            expect_equal(Repair(test.element,test.block, 0) %>% mutate(total = A+B+C+D) %>% .$total, rep(1, nrow(test.element)))
            expect_equal(Repair(test.element,test.block, 100000) %>% mutate(total = A+B+C+D) %>% .$total, rep(1, nrow(test.element)))
            expect_equal(Repair(test.element,test.block, total) %>% mutate(total = A+B+C+D) %>% .$total, rep(1, nrow(test.element)))
          })

