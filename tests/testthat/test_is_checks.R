context("Testing object validity checks")

test_that("is_element_level correctly identifies element_level objects",{
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

  expect_true(is_element_level(element))
  expect_false(is_element_level(element %>% select(-gifa)))
  expect_false(is_element_level(element %>% select(ab, bc, B.repair.total)))
  expect_false(is_element_level(1))
  expect_false(is_element_level(data.frame(1:10,1:10)))
})

test_that("is_block_level correctly identifies block_level objects",{
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

  block <- ConvertPdsToBlock(element, 1)

  expect_true(is_block_level(block))
  expect_false(is_block_level(block %>% select(-ratio)))
  expect_false(is_block_level(block %>% select(ratio)))
  expect_false(is_block_level(1))
  expect_false(is_block_level(data.frame(1:10,1:10)))
})
