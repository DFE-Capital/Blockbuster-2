context("Testing backlog functions")

test_that("single_backlog returns error with invalid grade", {
  # simulated block
  simulated_block <- tibble(buildingid = 1:5,
                            block.rebuild.cost = 1:5,
                            B.block.repair.cost = c(1, 0, 0, 0, 1),
                            C.block.repair.cost = c(0, 1, 0, 0, 1),
                            D.block.repair.cost = c(0, 0, 1, 0, 1),
                            E.block.repair.cost = c(0, 0, 0, 1, 1),
                            ratio = c(1, 1/2, 1/3, 1/4, 4/5))
  expect_error(single_backlog(simulated_block, "A"),
               "When pulling the backlog from block data, the grade must be either 'B', 'C', 'D' or 'E'. single_backlog() was supplied with A",
               fixed = TRUE)
  })

test_that("Backlog functions return correct value",
          {
            # simulated block
            simulated_block <- tibble(buildingid = 1:5,
                                      block.rebuild.cost = 1:5,
                                      B.block.repair.cost = c(1, 0, 0, 0, 1),
                                      C.block.repair.cost = c(0, 1, 0, 0, 1),
                                      D.block.repair.cost = c(0, 0, 1, 0, 1),
                                      E.block.repair.cost = c(0, 0, 0, 1, 1),
                                      ratio = c(1, 1/2, 1/3, 1/4, 4/5)
            )

            # one block
            expect_equal(backlog(simulated_block[1, ], "B"), 1)
            expect_equal(backlog(simulated_block[2, ], "B"), 0)
            expect_equal(backlog(simulated_block[1, ], "C"), 0)
            expect_equal(backlog(simulated_block[2, ], "C"), 1)
            expect_equal(backlog(simulated_block[1, ], "D"), 0)
            expect_equal(backlog(simulated_block[3, ], "D"), 1)
            expect_equal(backlog(simulated_block[1, ], "E"), 0)
            expect_equal(backlog(simulated_block[4, ], "E"), 1)
            expect_equal(backlog(simulated_block[1, ]), 1)
            expect_equal(backlog(simulated_block[5, ]), 4)

            # five blocks
            expect_equal(backlog(simulated_block, "B"), 2)
            expect_equal(backlog(simulated_block, "C"), 2)
            expect_equal(backlog(simulated_block, "D"), 2)
            expect_equal(backlog(simulated_block, "E"), 2)
            expect_equal(backlog(simulated_block), 8)

          }
)
