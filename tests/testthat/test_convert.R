context("Testing ConvertPDSToBlock")

test_that("ConvertPdsToBlock correctly summarises element-level data", {

  # toy data for two identical blocks, each with five components.  Four components
  # have a backlog of one for exactly one of the grades each, the fifth has backlog five for all of the grades.
  # The second building has twice the area so the rebuild cost should be doubled, but the backlog remains the same.
  sim_elements <- data.frame(buildingid = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
                             gifa = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
                             B.repair.total = rep(c(5, 0, 0, 0, 1), 2),
                             C.repair.total = rep(c(0, 5, 0, 0, 1), 2),
                             D.repair.total = rep(c(0, 0, 5, 0, 1), 2),
                             E.repair.total = rep(c(0, 0, 0, 5, 1), 2))
  # expected output when block.rebuild.cost is 12.
  sim_block <- data.frame(buildingid = c(1, 2),
                          block.rebuild.cost = c(12, 24),
                          B.block.repair.cost = c(6, 6),
                          C.block.repair.cost = c(6, 6),
                          D.block.repair.cost = c(6, 6),
                          E.block.repair.cost = c(6, 6),
                          ratio = c(2, 1))

  expect_equal(ConvertPdsToBlock(sim_elements, 12), sim_block)
})
