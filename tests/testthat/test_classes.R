context("Methods and classes")


testthat::test_that("All class generating functions throw errors with
                    incompatible inputs",
                    {
                      NUMBER <- 1
                      LIST <- list(1)
                      DATAFRAME <- data.frame(1)
                      DATAFRAMEWITHMISSINGCOL <- data.frame(buildingid = 1,
                                                            elementid = 2)
                      LISTOFLISTS <- list(list(1, 2))



                      # element
                      expect_error(ElementLevel(NUMBER))
                      expect_error(ElementLevel(DATAFRAME))
                      expect_error(ElementLevel(DATAFRAMEWITHMISSINGCOL))
                      expect_error(ElementLevel(LISTOFLISTS))

                      # block
                      expect_error(BlockLevel(NUMBER))
                      expect_error(BlockLevel(DATAFRAME))
                      expect_error(BlockLevel(DATAFRAMEWITHMISSINGCOL))
                      expect_error(BlockLevel(LISTOFLISTS))

                      # element.list
                      expect_error(ElementLevelList(NUMBER))
                      expect_error(ElementLevelList(DATAFRAME))
                      expect_error(ElementLevelList(DATAFRAMEWITHMISSING))
                      expect_error(ElementLevelList(LISTOFLISTS))

                      # block.list
                      expect_error(BlockLevelList(NUMBER))
                      expect_error(BlockLevelList(DATAFRAME))
                      expect_error(BlockLevelList(DATAFRAMEWITHMISSINGCOL))
                      expect_error(BlockLevelList(LISTOFLISTS))

                      # blockbuster
                      expect_error(BlockbusterOutput(NUMBER))
                      expect_error(BlockbusterOutput(DATAFRAME))
                      expect_error(BlockbusterOutput(DATAFRAMEWITHMISSINGCOL))
                      expect_error(BlockbusterOutput(LISTOFLISTS))

                      # repair.backlog
                      expect_error(RepairBacklogClass(NUMBER))
                      expect_error(RepairBacklogClass(DATAFRAME))
                      expect_error(RepairBacklogClass(DATAFRAMEWITHMISSINGCOL))
                      expect_error(RepairBacklogClass(LISTOFLISTS))
                    })
