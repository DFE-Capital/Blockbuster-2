context("Checking inflation functions")

test_that("Inflation is applied correctly to blocks", {
  # test inflation <1, 1 and >1

  # minimal data required by rebuild inflation, should include grades B to E and
  # block.rebuild.cost
  block_test <- data.frame(block.rebuild.cost = c(50, 100),
                           B.block.repair.cost = c(10, 0),
                           C.block.repair.cost = c(20, 10),
                           D.block.repair.cost = c(30, 50),
                           E.block.repair.cost = c(40, 100))

  # expected results for inflation = 0.5
  half <- data.frame(block.rebuild.cost = c(25, 50),
                     B.block.repair.cost = c(5, 0),
                     C.block.repair.cost = c(10, 5),
                     D.block.repair.cost = c(15, 25),
                     E.block.repair.cost = c(20, 50))

  # expected results for inflation = 1
  one <- block_test

  # expected results for inflation = 1.5
  onehalf <- data.frame(block.rebuild.cost = c(75, 150),
                   B.block.repair.cost = c(15, 0),
                   C.block.repair.cost = c(30, 15),
                   D.block.repair.cost = c(45, 75),
                   E.block.repair.cost = c(60, 150))

  expect_equal(InflateRebuild(block_test, 0.5)$block.rebuild.cost, half$block.rebuild.cost)
  expect_equal(InflateRebuild(block_test, 1), block_test)
  expect_equal(InflateRebuild(block_test, 1.5)$block.rebuild.cost, onehalf$block.rebuild.cost)

  expect_equal(InflateRepairBlock(block_test, 0.5) %>% select(-block.rebuild.cost),
               half %>% select(-block.rebuild.cost))
  expect_equal(InflateRepairBlock(block_test, 1), block_test)
  expect_equal(InflateRepairBlock(block_test, 1.5) %>% select(-block.rebuild.cost),
               onehalf %>% select(-block.rebuild.cost))

  expect_equal(block_test %>% InflateRebuild(0.5) %>% InflateRepairBlock(0.5), half)
  expect_equal(block_test %>% InflateRebuild(1) %>% InflateRepairBlock(1), one)
  expect_equal(block_test %>% InflateRebuild(1.5) %>% InflateRepairBlock(1.5), onehalf)

})

test_that("Inflation is applied correctly to elements", {
  # test inflation <1, 1 and >1

  # minimal data required by rebuild inflation - should include grades B to E
  element_test <- data.frame(B.repair.cost = c(10, 0),
                             C.repair.cost = c(40, 40),
                             D.repair.cost = c(160, 320),
                             E.repair.cost = c(640, 320),
                             B.repair.total = c(10, 0),
                             C.repair.total = c(40, 40),
                             D.repair.total = c(160, 320),
                             E.repair.total = c(640, 320))

  # expected results for inflation = 0.5
  half <- data.frame(B.repair.cost = c(5, 0),
                      C.repair.cost = c(20, 20),
                      D.repair.cost = c(80, 160),
                      E.repair.cost = c(320, 160),
                      B.repair.total = c(5, 0),
                      C.repair.total = c(20, 20),
                      D.repair.total = c(80, 160),
                      E.repair.total = c(320, 160))

  # expected results for inflation = 1
  one <- element_test

  # expected results for inflation = 1.5
  onehalf <- data.frame(B.repair.cost = c(15, 0),
                        C.repair.cost = c(60, 60),
                        D.repair.cost = c(240, 480),
                        E.repair.cost = c(960, 480),
                        B.repair.total = c(15, 0),
                        C.repair.total = c(60, 60),
                        D.repair.total = c(240, 480),
                        E.repair.total = c(960, 480))

  expect_equal(InflateRepairElement(element_test, 0.5), half)
  expect_equal(InflateRepairElement(element_test, 1), one)
  expect_equal(InflateRepairElement(element_test, 1.5), onehalf)

})
