context("Blockbuster modelling")

random.block <- sample(unique(PDS.block$buildingid), 1)
test.elements <- PDS.element %>%
  filter(buildingid == random.block) %>% # select block elements
  ElementLevel() # set class
test.elements$D <- 0.2
test.elements$C <- 0.3
test.elements$B <- 0.3
test.elements$A <- 0.2
test.elements <- test.elements %>% UpdateElementRepairs()
test.block <- PDS.block %>%
  filter(buildingid == random.block) %>%
  BlockLevel() %>% # set class
  UpdateBlockRepairs(test.elements)

test.funding <- Blockbuster(element.data = test.elements,
                               forecast.horizon = 1,
                               rebuild.money = 100000,
                               repair.money = 10000,
                               block.rebuild.cost = 0,
                               inflation = 1,
                               path = "./tests/testthat/test_outputs/",
                               filelabel = "testthat_funded")
funding.backlog <- GenerateRepairBacklog(test.funding)

test.no.funding <- Blockbuster(element.data = test.elements,
                               forecast.horizon = 1,
                               rebuild.money = 0,
                               repair.money = 0,
                               block.rebuild.cost = 0,
                               inflation = 1,
                               path = "./tests/testthat/test_outputs/",
                               filelabel = "testthat_no_funds")
no.funding.backlog <- GenerateRepairBacklog(test.no.funding)

test.block.file <- readRDS("./tests/testthat/test_outputs/testthat_funded_block_1.rds")
test.element.file <- readRDS("./tests/testthat/test_outputs/testthat_funded_element_1.rds")
test.output.file <- readRDS("./tests/testthat/test_outputs/testthat_funded_output.rds")


test.no.rebuild <- Blockbuster(element.data = test.elements,
                     forecast.horizon = 1,
                     rebuild.money = 0,
                     repair.money = 10000,
                     block.rebuild.cost = 0,
                     inflation = 1,
                     path = "./tests/testthat/test_outputs/",
                     filelabel = "testthat_no_rebuild")
no.rebuild.backlog <- GenerateRepairBacklog(test.no.rebuild)

test.no.repair <- Blockbuster(element.data = test.elements,
                               forecast.horizon = 1,
                               rebuild.money = 50000000,
                               repair.money = 0,
                               block.rebuild.cost = 0,
                               inflation = 1,
                               path = "./tests/testthat/test_outputs/",
                               filelabel = "testthat_no_repair")
no.repair.backlog <- GenerateRepairBacklog(test.no.repair)

test.inflation <- Blockbuster(element.data = test.elements,
                              forecast.horizon = 1,
                              rebuild.money = 0,
                              repair.money = 0,
                              block.rebuild.cost = 0,
                              inflation = 2,
                              path = "./tests/testthat/test_outputs/",
                              filelabel = "testthat_inflation")

test.grades <- Blockbuster(element.data = test.elements,
                              forecast.horizon = 1,
                              rebuild.money = 0,
                              repair.money = 0,
                              block.rebuild.cost = 0,
                              inflation = 2,
                              path = "./tests/testthat/test_outputs/",
                              filelabel = "testthat_grades"
                              )


test_that("Blockbuster output is of the correct class and length",
          {
          expect_is(test.no.funding, "blockbuster")
          expect_is(test.no.repair, "blockbuster")
          expect_is(test.no.rebuild, "blockbuster")

          forecast <- sample(1:10, 1)
          expect_length(Blockbuster(element.data = test.elements,
                                    forecast.horizon = forecast,
                                    rebuild.money = rep(0, forecast),
                                    repair.money = rep(0, forecast),
                                    block.rebuild.cost = 0,
                                    inflation = 1), forecast + 1)
          })

test_that("Saved files are of the correct class", {
          expect_is(test.output.file, "blockbuster")
          expect_is(test.block.file, "block")
          expect_is(test.element.file, "element")
})

test_that("Repairs and rebuilds are occuring correctly.",
          {
          # no repair and no rebuild means backlog increase due to deterioration.
          expect_lte(sum(no.funding.backlog[1, ]), sum(no.funding.backlog[2, ]))
          # no funding backlog should be larger than funded backlog
          expect_lte(sum(funding.backlog[2, ]), sum(no.funding.backlog[2, ]))

          # repair and no rebuild reduces repair costs and does not increase
          # grade N
          expect_lte(sum(no.rebuild.backlog[1, ]), sum(no.rebuild.backlog[2, ]))
          expect_lte(sum(test.no.rebuild[[2]]$element.data$N),
                     sum(test.no.rebuild[[1]]$element.data$N))
          #rebuild and no repair increases grade N
          expect_lte(sum(test.no.repair[[1]]$element.data$N),
                     sum(test.no.repair[[2]]$element.data$N))
          })

test_that("Incorrect inputs produce the appropriate warnings and errors",
          {
            # incorrect element.data
            expect_error(Blockbuster(element.data = test.block,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 0,
                                     inflation = 1))
            #incorrect block.data
            expect_error(Blockbuster(element.data = test.elements,
                                     block.data = test.elements,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 0,
                                     inflation = 1))
            # invalid forecast.horizon
            expect_error(Blockbuster(element.data = test.elements,
                                     forecast.horizon = 0,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 0,
                                     inflation = 1))
            # negative monies
            expect_error(Blockbuster(element.data = test.elements,
                                     forecast.horizon = 1,
                                     rebuild.money = -100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 0,
                                     inflation = 1))
            expect_error(Blockbuster(element.data = test.elements,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = -10000,
                                     block.rebuild.cost = 0,
                                     inflation = 1))
            expect_error(Blockbuster(element.data = test.elements,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = -123,
                                     inflation = 1))
            # too many values
            expect_warning(Blockbuster(element.data = test.elements,
                                     forecast.horizon = 1,
                                     rebuild.money = 1:10,
                                     repair.money = 1,
                                     block.rebuild.cost = 0,
                                     inflation = 1))
            expect_warning(Blockbuster(element.data = test.elements,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 1:10000,
                                     block.rebuild.cost = 0,
                                     inflation = 1))
            expect_warning(Blockbuster(element.data = test.elements,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 1:10,
                                     inflation = 1))
            expect_warning(Blockbuster(element.data = test.elements,
                                       forecast.horizon = 1,
                                       rebuild.money = 100000,
                                       repair.money = 10000,
                                       block.rebuild.cost = 0,
                                       inflation = 1:10))
            # recycling inflation
            expect_warning(Blockbuster(element.data = test.elements,
                                     forecast.horizon = 2,
                                     rebuild.money = 1:2,
                                     repair.money = 1:2,
                                     block.rebuild.cost = 0,
                                     inflation = 1))
          })

test_that("Inflation is occuring",
          {
            expect_equal(test.inflation[[2]]$block$block.rebuild.cost,
                         test.inflation[[1]]$block$block.rebuild.cost * 2)
            expect_equal(test.inflation[[2]]$element$B.repair.cost,
                         test.inflation[[1]]$element$B.repair.cost * 2)
            expect_equal(test.inflation[[2]]$element$C.repair.cost,
                         test.inflation[[1]]$element$C.repair.cost * 2)
            expect_equal(test.inflation[[2]]$element$D.repair.cost,
                         test.inflation[[1]]$element$D.repair.cost * 2)
            expect_equal(test.inflation[[2]]$element$E.repair.cost,
                         test.inflation[[1]]$element$E.repair.cost * 2)
          })

test_that("Blockbuster works with single inputs by repeating values.",
          {
            expect_warning(Blockbuster(test.elements, test.block,
                                     forecast.horizon = 2, rebuild.money = 0,
                                     repair.money = c(0,0), inflation = c(1,1)))

            expect_warning(Blockbuster(test.elements, test.block,
                                     forecast.horizon = 2, rebuild.money = c(0,0),
                                     repair.money = 0, inflation = c(1,1)))
            expect_warning(Blockbuster(test.elements, test.block,
                                       forecast.horizon = 2, rebuild.money = c(0,0),
                                       repair.money = c(0, 0), inflation = 1))
          })

test_that("Blockbuster converts element.data into block form if block.data not
          supplied",
          {
            expect_message(Blockbuster(test.elements,
                                       forecast.horizon = 1, rebuild.money = 0,
                                       repair.money = 0, inflation = 1))
            expect_is(Blockbuster(test.elements,
                                  forecast.horizon = 1, rebuild.money = 0,
                                  repair.money = 0, inflation = 1), "blockbuster")
          })

context("Loading blockbuster outputs from saved files")

test_that("LoadBlockbusterOutput produces the correct class of object",
          {
            expect_is(LoadBlockbusterOutput(1,
                                            path = "./tests/testthat/test_outputs/",
                                            filelabel = "testthat_funded"),
                      "blockbuster")
          })


