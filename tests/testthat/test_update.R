context("Testing UpdateElementRepairs")

test_that("Updated figures are correct", {
  # 1 block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = 9, C.repair.total = 3, D.repair.total = 1, E.repair.total = 7, # note the incorrect values
                        gifa = 1, unit_area = 1)
  correct <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2, D.repair.total = D * 3, E.repair.total = E * 4, # correct figures
                        gifa = 1, unit_area = 1)
  correct10 <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B * 10, C.repair.total = C * 20, D.repair.total = D * 30, E.repair.total = E * 40, # correct figures
                        gifa = 1, unit_area = 10)
  element10 <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = 9, C.repair.total = 3, D.repair.total = 1, E.repair.total = 7, # note the incorrect values
                        gifa = 1, unit_area = 10)
  expect_equal(UpdateElementRepairs(element), correct)
  expect_equal(UpdateElementRepairs(element10), correct10)
})

context("Testing UpdateBlockRepairs")

test_that("Updated figures are correct", {
  # 1 block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2, D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)
  block <- data.frame(buildingid = 1,
                      block.rebuild.cost = 100,
                      B.block.repair.cost = 1,
                      C.block.repair.cost = 1,
                      D.block.repair.cost = 1,
                      E.block.repair.cost = 1,
                      ratio = 1)  # note incorrect repair and ratio values
  correct <- data.frame(buildingid = 1,
                        block.rebuild.cost = 100,
                        B.block.repair.cost = 1.25,
                        C.block.repair.cost = 2.3,
                        D.block.repair.cost = 3.21,
                        E.block.repair.cost = 4.12,
                        ratio = 10.88/ 100)
  expect_equal(UpdateBlockRepairs(block, element), correct)
})
