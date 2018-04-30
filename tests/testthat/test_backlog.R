context("Testing backlog functions")

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
            expect_equal(BacklogB(simulated_block[1, ]), 1)
            expect_equal(BacklogB(simulated_block[2, ]), 0)
            expect_equal(BacklogC(simulated_block[1, ]), 0)
            expect_equal(BacklogC(simulated_block[2, ]), 1)
            expect_equal(BacklogD(simulated_block[1, ]), 0)
            expect_equal(BacklogD(simulated_block[3, ]), 1)
            expect_equal(BacklogE(simulated_block[1, ]), 0)
            expect_equal(BacklogE(simulated_block[4, ]), 1)

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
            expect_equal(BacklogB(simulated_block), 2)
            expect_equal(BacklogC(simulated_block), 2)
            expect_equal(BacklogD(simulated_block), 2)
            expect_equal(BacklogE(simulated_block), 2)

            expect_equal(backlog(simulated_block, "B"), 2)
            expect_equal(backlog(simulated_block, "C"), 2)
            expect_equal(backlog(simulated_block, "D"), 2)
            expect_equal(backlog(simulated_block, "E"), 2)
            expect_equal(backlog(simulated_block), 8)

          }
)
#
#
#
##
#
# NUMBER <- 1
# LIST <- list(1)
# DATAFRAME <- data.frame(1)
# BLOCK <- test.block
# ELEMENT <- test.element[1, ]
# BLOCKLIST <- BlockLevelList(list(test.block, test.block))
# ELEMENTLIST <- ElementLevelList(list(test.element, test.element))
# BLOCKBUSTER <- vector("list", 2)
# BLOCKBUSTER[[1]]$block.data <- BLOCK
# BLOCKBUSTER[[1]]$element.data <- ELEMENT
# BLOCKBUSTER[[2]] <- BLOCKBUSTER[[1]]
# BLOCKBUSTER <- BlockbusterOutput(BLOCKBUSTER)
# context("Generating long format backlog")
#
# test_that("Inputs succeed or fail as expected.",
#           {
#             # should work
#             expect_is(LongRepairBacklog(BLOCK), "repair.backlog")
#             expect_is(LongRepairBacklog(BLOCKBUSTER), "repair.backlog")
#             expect_is(LongRepairBacklog(BLOCKLIST), "repair.backlog")
#
#             # should not
#             expect_error(LongRepairBacklog(NUMBER))
#             expect_error(LongRepairBacklog(LIST))
#             expect_error(LongRepairBacklog(DATAFRAME))
#           })
#
# test_that("Timesteps are added correctly to the output.",
#           {
#             grade <- sample(c("B", "C", "D"), 1)
#             data1 <- LongRepairBacklog(BLOCK) %>% filter(grade == grade[1])
#             data2 <- LongRepairBacklog(BLOCKBUSTER) %>% filter(grade == grade[1])
#
#             expect_equal(data1$timestep, 0)
#             expect_equal(data2$timestep, 0:1)
#           })
#
# test_that("Totals add up correctly",
#           {
#             # TODO
#           })
#
