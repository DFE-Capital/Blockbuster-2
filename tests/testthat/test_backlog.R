context("Checking backlog computations.")

value <- runif(1, 0, 10000)
test.data <- test.block[1:2, ] %>%
  mutate(B.block.repair.cost = value,
         C.block.repair.cost = value,
         D.block.repair.cost = value
         )

NUMBER <- 1
LIST <- list(1)
DATAFRAME <- data.frame(1)
BLOCK <- test.block
ELEMENT <- test.element
BLOCKLIST <- BlockLevelList(list(test.block, test.block))
ELEMENTLIST <- ElementLevelList(list(test.element, test.element))
BLOCKBUSTER <- vector("list", 2)
BLOCKBUSTER[[1]]$block.data <- BLOCK
BLOCKBUSTER[[1]]$element.data <- ELEMENT
BLOCKBUSTER[[2]] <- BLOCKBUSTER[[1]]
BLOCKBUSTER <- BlockbusterOutput(BLOCKBUSTER)


# checking backlog computation
test_that("Backlog functions return correct value",
          {
            # one block
            expect_equal(BacklogB(test.data[1, ]), value)
            expect_equal(BacklogC(test.data[1, ]), value)
            expect_equal(BacklogD(test.data[1, ]), value)

            # two blocks
            expect_equal(BacklogB(test.data), value * 2)
            expect_equal(BacklogC(test.data), value * 2)
            expect_equal(BacklogD(test.data), value * 2)

          }
)

context("Generating long format backlog")

test_that("Inputs succeed or fail as expected.",
          {
            # should work
            expect_is(LongRepairBacklog(BLOCK), "repair.backlog")
            expect_is(LongRepairBacklog(BLOCKBUSTER), "repair.backlog")
            expect_is(LongRepairBacklog(BLOCKLIST), "repair.backlog")

            # should not
            expect_error(LongRepairBacklog(NUMBER))
            expect_error(LongRepairBacklog(LIST))
            expect_error(LongRepairBacklog(DATAFRAME))
          })

test_that("Timesteps are added correctly to the output.",
          {
            grade <- sample(c("B", "C", "D"), 1)
            data1 <- LongRepairBacklog(BLOCK) %>% filter(grade == grade[1])
            data2 <- LongRepairBacklog(BLOCKBUSTER) %>% filter(grade == grade[1])

            expect_equal(data1$timestep, 0)
            expect_equal(data2$timestep, 0:1)
          })

test_that("Totals add up correctly",
          {
            # TODO
          })

