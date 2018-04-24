context("Testing main Blockbuster function")

test_that("Blockbuster doesn't fail", {
  # sample 5 blocks
  block_number <- sample(unique(PDS.block$buildingid), 1)
  random_elements <- PDS.element %>%
    filter(buildingid %in% block_number)

  # There should be no errors or warnings
  expect_failure(expect_error(Blockbuster(random_elements)))
  expect_failure(expect_warning(Blockbuster(random_elements)))
})

test_that("Blockbuster creates block data  when not supplied", {

  # sample 1 block
  block_number <- sample(unique(PDS.block$buildingid), 1)
  random_elements <- PDS.element %>%
    filter(buildingid %in% block_number)
  random_block <- PDS.block %>%
    filter(buildingid %in% block_number) %>%
    select(-block.rebuild.cost, - ratio)  # removing these as they should not really be in stored data in the first place - they depend on the block rebuild cost passed to the function

  # run Blockbuster so it generates initial block data (saved in the first list
  #  element of the output and called `block``)
  simulated_block <- Blockbuster(random_elements)[[1]]$block %>%
    select(-block.rebuild.cost, -ratio) # removing these as they should not really be in stored data in the first place - they depend on the block rebuild cost passed to the function

  # The simulated block should be the same as the original.
  expect_equal(simulated_block,
               random_block)
  })

test_that("Blockbuster models correct timespan", {
  # sample 1 element
  random_elements <- PDS.element %>%
    slice(sample(1:nrow(PDS.element), 1))

  # run simulations for a variety of years and check the output is the same length
  # (plus one for the original input)
  # warnings are suppressed as the input checks throw warnings as they recycle default inputs over each year
  expect_equal(suppressWarnings(Blockbuster(random_elements, forecast.horizon = 1)) %>% length, 1 + 1)
  expect_equal(suppressWarnings(Blockbuster(random_elements, forecast.horizon = 2)) %>% length, 2 + 1)
  expect_equal(suppressWarnings(Blockbuster(random_elements, forecast.horizon = 3)) %>% length, 3 + 1)
  expect_equal(suppressWarnings(Blockbuster(random_elements, forecast.horizon = 4)) %>% length, 4 + 1)
  expect_equal(suppressWarnings(Blockbuster(random_elements, forecast.horizon = 5)) %>% length, 5 + 1)
  expect_equal(suppressWarnings(Blockbuster(random_elements, forecast.horizon = 10)) %>% length, 10 + 1)
  expect_equal(suppressWarnings(Blockbuster(random_elements, forecast.horizon = 20)) %>% length, 20 + 1)

})

test_that("Blockbuster rebuilds when there are funds", {

   # sample 5 blocks
  block_number <- sample(unique(PDS.block$buildingid), 5)
  random_element <- PDS.element %>%
    filter(buildingid %in% block_number) %>%
    # turn off deterioration by setting rates to zero
    mutate(ab = 0, bc = 0, cd = 0, de = 0)
  random_block <- PDS.block %>%
    filter(buildingid %in% block_number)

  # compute backlogs after one year with no repair
  original_backlog <- backlog(random_block)
  alt_model    <- Blockbuster(random_element, random_block, rebuild.money = random_block$block.rebuild.cost[1] * 2) # rebuild money more than that needed to rebuild at least one block
  alt_backlog  <- backlog(alt_model[[2]]$block)

  expect_lt(alt_backlog, original_backlog)
})

test_that("Blockbuster does not rebuild when there is no money", {

  # sample 5 block
  block_number <- sample(unique(PDS.block$buildingid), 5)
  random_element <- PDS.element %>%
    filter(buildingid %in% block_number) %>%
    # turn off deterioration by setting rates to zero
    mutate(ab = 0, bc = 0, cd = 0, de = 0)
  random_block <- PDS.block %>%
    filter(buildingid %in% block_number)

  # compute original backlog and backlog after 1 year with no rebuild money
  original_backlog <- backlog(random_block)
  base_model   <- Blockbuster(random_element, random_block, rebuild.money = 0)
  base_backlog <- backlog(base_model[[2]]$block)

  expect_equal(original_backlog, base_backlog)
})

test_that("Blockbuster repairs when there are funds", {

  # sample 5 blocks
  block_number <- sample(unique(PDS.block$buildingid), 5)
  random_element <- PDS.element %>%
    filter(buildingid %in% block_number) %>%
    # turn off deterioration by setting rates to zero
    mutate(ab = 0, bc = 0, cd = 0, de = 0)
  random_block <- PDS.block %>%
    filter(buildingid %in% block_number)

  # compute original backlog and backlog after 1 year with enough funds to repair
  # original backlog
  original_backlog <- backlog(random_block)
  base_model   <- Blockbuster(random_element, random_block, repair.money = original_backlog)
  base_backlog <- backlog(base_model[[2]]$block)

  expect_lt(base_backlog, original_backlog)

})

test_that("Blockbuster does not repair when there is no money", {

  # sample 5 blocks
  block_number <- sample(unique(PDS.block$buildingid), 5)
  random_element <- PDS.element %>%
    filter(buildingid %in% block_number) %>%
    # turn off deterioration by setting rates to zero
    mutate(ab = 0, bc = 0, cd = 0, de = 0)
  random_block <- PDS.block %>%
    filter(buildingid %in% block_number)

  # compute original backlog and backlog after 1 year with no repair
  original_backlog <- backlog(random_block)
  base_model   <- Blockbuster(random_element, random_block, repair.money = 0)
  base_backlog <- backlog(base_model[[2]]$block)

  expect_equal(base_backlog, original_backlog)

})

test_that("Blockbuster correctly adjusts rebuild costs when block.rebuild.cost argument is changed", {
  # sample 1 block
  block_number <- sample(unique(PDS.block$buildingid), 1)
  random_element <- PDS.element %>%
    filter(buildingid %in% block_number)

  # run simulations to create new block data which contains block rebuild values
  # for different values of block.rebuild.cost
  first_sim  <- Blockbuster(random_element, block.rebuild.cost = 1000)
  first_sim_rebuild_cost <- first_sim[[2]]$block$block.rebuild.cost
  second_sim <- Blockbuster(random_element, block.rebuild.cost = 10000)
  second_sim_rebuild_cost <- second_sim[[2]]$block$block.rebuild.cost

  # check second_sim rebuild costs are ten times the first
  expect_equal(first_sim_rebuild_cost, second_sim_rebuild_cost / 10)

  # check that the cost is correctly computed from gifa
  expect_equal(random_element$gifa[1] * 1000, first_sim_rebuild_cost)
  expect_equal(random_element$gifa[1] * 10000, second_sim_rebuild_cost)

})

test_that("Blockbuster correctly applies inflation", {
  # sample 5 blocks
  block_number <- sample(unique(PDS.block$buildingid), 5)
  random_element <- PDS.element %>%
    filter(buildingid %in% block_number) %>%
    # turn off deterioration by setting rates to zero
    mutate(ab = 0, bc = 0, cd = 0, de = 0)
  random_block <- PDS.block %>%
    filter(buildingid %in% block_number)

  # compute original backlog and backlog after 1 year with no repair
  double_model   <- Blockbuster(random_element, random_block, inflation = 2)
  original_backlog <- backlog(double_model[[1]]$block)
  double_backlog <- backlog(double_model[[2]]$block)
  half_model   <- Blockbuster(random_element, random_block, inflation = 1/2)
  half_backlog <- backlog(half_model[[2]]$block)

  # as there is no deterioration, repair or rebuilds then the backlog should change in proportion to inflation
  expect_equal(original_backlog, double_backlog / 2)
  expect_equal(original_backlog, half_backlog * 2)

  # the repair costs should show inflation
  # check the repair costs for the first element to see if this is true
  expect_equal(random_element[1, ]$B.repair.cost, double_model[[2]]$element[1, ]$B.repair.cost / 2)
  expect_equal(random_element[1, ]$C.repair.cost, double_model[[2]]$element[1, ]$C.repair.cost / 2)
  expect_equal(random_element[1, ]$D.repair.cost, double_model[[2]]$element[1, ]$D.repair.cost / 2)
  expect_equal(random_element[1, ]$E.repair.cost, double_model[[2]]$element[1, ]$E.repair.cost / 2)

  expect_equal(random_element[1, ]$B.repair.cost, half_model[[2]]$element[1, ]$B.repair.cost * 2)
  expect_equal(random_element[1, ]$C.repair.cost, half_model[[2]]$element[1, ]$C.repair.cost * 2)
  expect_equal(random_element[1, ]$D.repair.cost, half_model[[2]]$element[1, ]$D.repair.cost * 2)
  expect_equal(random_element[1, ]$E.repair.cost, half_model[[2]]$element[1, ]$E.repair.cost * 2)

})

test_that("Blockbuster saves output in correct place", {

  # sample 1 element
  random_elements <- PDS.element %>%
    slice(sample(1:nrow(PDS.element), 1))

  # run simulation with a random number as the save tag
  dir_tag <- "test_outputs"
  file_tag <- "Yah"
  Blockbuster(random_elements, filelabel = file_tag, path = dir_tag)

  # check file is there
  expect_true(file.exists(paste0(file.path(dir_tag, file_tag), "_output.rds")))

  # remove test files
  file.remove(paste0(file.path(dir_tag, file_tag), "_output.rds"))
  file.remove(paste0(file.path(dir_tag, file_tag), "_element_0.rds"))
  file.remove(paste0(file.path(dir_tag, file_tag), "_element_1.rds"))
  file.remove(paste0(file.path(dir_tag, file_tag), "_block_0.rds"))
  file.remove(paste0(file.path(dir_tag, file_tag), "_block_1.rds"))
})

test_that("Blockbuster makes same decisions on runs with the same inputs", {

  # sample 1 element
  random_elements <- PDS.element %>%
    slice(sample(1:nrow(PDS.element), 1))

  # run two simulations, both with same input
  run_one <- Blockbuster(random_elements)
  run_two <- Blockbuster(random_elements)

  # outputs should match
  expect_equal(run_one, run_two)
})

test_that("Blockbuster makes different repair decisions when the grade order is changed", {

  # sample 5 blocks with components at grades B, C, and D
  candidate_blocks <- PDS.block %>%
    filter(B.block.repair.cost > 0,
           C.block.repair.cost > 0,
           D.block.repair.cost > 0) %>%
    pull(buildingid)

  block_number <- sample(candidate_blocks, 5)
  random_element <- PDS.element %>%
    filter(buildingid %in% block_number) %>%
    # turn off deterioration by setting rates to zero
    mutate(ab = 0, bc = 0, cd = 0, de = 0)
  random_block <- PDS.block %>%
    filter(buildingid %in% block_number)

  # identify amount that will allow all of a grade to be repaired, no matter what grade
  funds <- max(random_block$B.block.repair.cost,
               random_block$C.block.repair.cost,
               random_block$D.block.repair.cost)

  # compute original backlogs at each grade
  original_B_backlog <- backlog(random_block, "B")
  original_C_backlog <- backlog(random_block, "C")
  original_D_backlog <- backlog(random_block, "D")

  # run simulations with different orders
  B_sim <- Blockbuster(random_element, random_block,
                       repair.money = funds,
                       grade.order = c("B", "C", "D"))
  B_backlog <- sum(B_sim[[2]]$block$B.block.repair.cost)
  C_sim <- Blockbuster(random_element, random_block,
                       repair.money = funds,
                       grade.order = c("C", "D", "B"))
  C_backlog <- sum(C_sim[[2]]$block$C.block.repair.cost)
  D_sim <- Blockbuster(random_element, random_block,
                       repair.money = funds,grade.order = c("D", "B", "C"))
  D_backlog <- sum(D_sim[[2]]$block$D.block.repair.cost)

  expect_lt(B_backlog, original_B_backlog)
  expect_lt(C_backlog, original_C_backlog)
  expect_lt(D_backlog, original_D_backlog)
})


test_that("Blockbuster works with single inputs by repeating values.",
          {
            # sample 1 block1
            block_number <- sample(unique(PDS.block$buildingid), 1)
            random_element <- PDS.element %>%
              filter(buildingid %in% block_number) %>%
              # turn off deterioration by setting rates to zero
              mutate(ab = 0, bc = 0, cd = 0, de = 0)
            random_block <- PDS.block %>%
              filter(buildingid %in% block_number)

            expect_warning(Blockbuster(random_element, random_block,
                                     forecast.horizon = 2, rebuild.money = 0,
                                     repair.money = c(0,0), inflation = c(1,1)))

            expect_warning(Blockbuster(random_element, random_block,
                                     forecast.horizon = 2, rebuild.money = c(0,0),
                                     repair.money = 0, inflation = c(1,1)))
            expect_message(Blockbuster(random_element, random_block,
                                       forecast.horizon = 2, rebuild.money = c(0,0),
                                       repair.money = c(0, 0), inflation = 1))
          })


#
test_that("Incorrect inputs produce the appropriate warnings and errors",
          {
            # sample 1 block1
            block_number <- sample(unique(PDS.block$buildingid), 1)
            random_element <- PDS.element %>%
              filter(buildingid %in% block_number) %>%
              # turn off deterioration by setting rates to zero
              mutate(ab = 0, bc = 0, cd = 0, de = 0)
            random_block <- PDS.block %>%
              filter(buildingid %in% block_number)


            # incorrect element.data
            expect_error(Blockbuster(element.data = random_block,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            #incorrect block.data
            expect_error(Blockbuster(element.data = random_element,
                                     block.data = random_element,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            # invalid forecast.horizon
            expect_error(Blockbuster(element.data = random_element,
                                     forecast.horizon = 0,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            # negative monies
            expect_error(Blockbuster(element.data = random_element,
                                     forecast.horizon = 1,
                                     rebuild.money = -100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            expect_error(Blockbuster(element.data = random_element,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = -10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            expect_error(Blockbuster(element.data = random_element,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = -123,
                                     inflation = 1))
            # too many values
            expect_warning(Blockbuster(element.data = random_element,
                                     forecast.horizon = 1,
                                     rebuild.money = 1:10,
                                     repair.money = 1,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            expect_warning(Blockbuster(element.data = random_element,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 1:10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            expect_warning(Blockbuster(element.data = random_element,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 1:10,
                                     inflation = 1))
            expect_warning(Blockbuster(element.data = random_element,
                                       forecast.horizon = 1,
                                       rebuild.money = 100000,
                                       repair.money = 10000,
                                       block.rebuild.cost = 100,
                                       inflation = 1:10))
            # recycling inflation
            expect_message(Blockbuster(element.data = random_element,
                                     forecast.horizon = 2,
                                     rebuild.money = 1:2,
                                     repair.money = 1:2,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
          })



#
# # FUNCTIONALITY NOT YET IMPLEMENTED
# # test_that("Blockbuster correctly applies user-supplied deterioration rates")
#
# # FUNCTIONALITY NOT YET IMPLEMENTED
# # test_that("Blockbuster correctly applies user-supplied repair costs")
#
# # FUNCTIONALITY NOT YET IMPLEMENTED
# # test_that("Blockbuster toggles grade E as specified")
#
