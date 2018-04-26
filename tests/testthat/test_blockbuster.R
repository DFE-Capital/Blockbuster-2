context("Testing main Blockbuster function")

test_that("Blockbuster doesn't fail", {
  #  one block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = 1, C.repair.total = 1, D.repair.total = 1, E.repair.total = 1,
                        gifa = 1, unit_area = 1)

  # There should be no errors or warnings
  expect_failure(expect_error(Blockbuster(element)))
  expect_failure(expect_warning(Blockbuster(element)))
})

test_that("Blockbuster creates block data  when not supplied", {

  # 1 block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                        gifa = 1, unit_area = 1)

  block <- data.frame(buildingid = 1,
                      B.block.repair.cost = 1,
                      C.block.repair.cost = 2,
                      D.block.repair.cost = 3,
                      E.block.repair.cost = 4)

  # run Blockbuster so it generates initial block data (saved in the first list
  #  element of the output and called `block``)
  simulated_block <- Blockbuster(element)[[1]]$block %>%
    select(-block.rebuild.cost, -ratio) # removing these as they should not really be in stored data in the first place - they depend on the block rebuild cost passed to the function

  # The simulated block should be the same as the original.
  expect_equal(simulated_block,
               block)
  })

test_that("Blockbuster models correct timespan", {

  # 1 block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                        gifa = 1, unit_area = 1)


  # run simulations for a variety of years and check the output is the same length
  # (plus one for the original input)
  # warnings are suppressed as the input checks throw warnings as they recycle default inputs over each year
  expect_equal(suppressWarnings(Blockbuster(element, forecast.horizon = 1)) %>% length, 1 + 1)
  expect_equal(suppressWarnings(Blockbuster(element, forecast.horizon = 2)) %>% length, 2 + 1)
  expect_equal(suppressWarnings(Blockbuster(element, forecast.horizon = 3)) %>% length, 3 + 1)
  expect_equal(suppressWarnings(Blockbuster(element, forecast.horizon = 4)) %>% length, 4 + 1)
  expect_equal(suppressWarnings(Blockbuster(element, forecast.horizon = 5)) %>% length, 5 + 1)
  expect_equal(suppressWarnings(Blockbuster(element, forecast.horizon = 10)) %>% length, 10 + 1)
  expect_equal(suppressWarnings(Blockbuster(element, forecast.horizon = 20)) %>% length, 20 + 1)

})

test_that("Blockbuster rebuilds when there are funds", {

  # 1 block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                        gifa = 1, unit_area = 1)

  expect_equal(Blockbuster(element, rebuild.money = 2000)[[2]]$block$B.block.repair.cost, 0)
  expect_equal(Blockbuster(element, rebuild.money = 2000)[[2]]$block$C.block.repair.cost, 0)
  expect_equal(Blockbuster(element, rebuild.money = 2000)[[2]]$block$D.block.repair.cost, 0)
  expect_equal(Blockbuster(element, rebuild.money = 2000)[[2]]$block$E.block.repair.cost, 0)
})

test_that("Blockbuster does not rebuild or repair when there is no money", {

  # 1 block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E,
                        ab = 0, bc = 0, cd = 0, de = 0, # turns off deterioration
                        elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = c(0, 1, 0, 0, 0),
                        C.repair.total = c(0, 0, 1, 0, 0),
                        D.repair.total = c(0, 0, 0, 1, 0),
                        E.repair.total = c(0, 0, 0, 0, 1),
                        gifa = 1, unit_area = 1)

  expect_equal(Blockbuster(element)[[2]]$element,
               element)
})

test_that("Blockbuster repairs when there are funds", {

  # 1 block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E,
                        ab = 0, bc = 0, cd = 0, de = 0, # turns off deterioration
                        elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = c(0, 1, 0, 0, 0),
                        C.repair.total = c(0, 0, 1, 0, 0),
                        D.repair.total = c(0, 0, 0, 1, 0),
                        E.repair.total = c(0, 0, 0, 0, 1),
                        gifa = 1, unit_area = 1)


  expect_equal(Blockbuster(element, repair.money = 1)[[2]]$block %>%
                 select(B.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 1)[[2]]$block %>%
                 select(C.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 1)[[2]]$block %>%
                 select(D.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 1)[[2]]$block %>%
                 select(E.block.repair.cost) %>% pull,
               1)

  expect_equal(Blockbuster(element, repair.money = 2)[[2]]$block %>%
                 select(B.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 2)[[2]]$block %>%
                 select(C.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 2)[[2]]$block %>%
                 select(D.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 2)[[2]]$block %>%
                 select(E.block.repair.cost) %>% pull,
               1)

  expect_equal(Blockbuster(element, repair.money = 3)[[2]]$block %>%
                 select(B.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 3)[[2]]$block %>%
                 select(C.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 3)[[2]]$block %>%
                 select(D.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 3)[[2]]$block %>%
                 select(E.block.repair.cost) %>% pull,
               1)

  })

test_that("Blockbuster correctly adjusts rebuild costs when block.rebuild.cost argument is changed", {
  # 1 block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E,
                        ab = 0, bc = 0, cd = 0, de = 0, # turns off deterioration
                        elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = c(0, 1, 0, 0, 0),
                        C.repair.total = c(0, 0, 1, 0, 0),
                        D.repair.total = c(0, 0, 0, 1, 0),
                        E.repair.total = c(0, 0, 0, 0, 1),
                        gifa = 3, unit_area = 1)

  # check second_sim rebuild costs are ten times the first
  expect_equal(Blockbuster(element, block.rebuild.cost = 1)[[2]]$block %>%
                 select(block.rebuild.cost) %>% pull,
               3)

  expect_equal(Blockbuster(element, block.rebuild.cost = 10)[[2]]$block %>%
                 select(block.rebuild.cost) %>% pull,
               30)
})

test_that("Blockbuster correctly applies inflation", {
  # 1 block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E,
                        ab = 0, bc = 0, cd = 0, de = 0, # turns off deterioration
                        elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = c(0, 1, 0, 0, 0),
                        C.repair.total = c(0, 0, 1, 0, 0),
                        D.repair.total = c(0, 0, 0, 1, 0),
                        E.repair.total = c(0, 0, 0, 0, 1),
                        gifa = 3, unit_area = 1)

  # as there is no deterioration, repair or rebuilds then the backlog should change in proportion to inflation
  expect_equal(Blockbuster(element, inflation = 1)[[2]]$block %>% select(B.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, inflation = 2)[[2]]$block %>% select(B.block.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, inflation = 0.5)[[2]]$block %>% select(B.block.repair.cost) %>% pull,
               0.5)

  # the repair costs should show inflation
  # check the repair costs for the first element to see if this is true
  expect_equal(Blockbuster(element, inflation = 1)[[2]]$element %>%
                 slice(1) %>% select(B.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, inflation = 2)[[2]]$element %>%
                 slice(1) %>% select(B.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, inflation = 0.5)[[2]]$element %>%
                 slice(1) %>% select(B.repair.cost) %>% pull,
               0.5)
  expect_equal(Blockbuster(element, inflation = 1)[[2]]$element %>%
                 slice(1) %>% select(C.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, inflation = 2)[[2]]$element %>%
                 slice(1) %>% select(C.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, inflation = 0.5)[[2]]$element %>%
                 slice(1) %>% select(C.repair.cost) %>% pull,
               0.5)
  expect_equal(Blockbuster(element, inflation = 1)[[2]]$element %>%
                 slice(1) %>% select(D.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, inflation = 2)[[2]]$element %>%
                 slice(1) %>% select(D.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, inflation = 0.5)[[2]]$element %>%
                 slice(1) %>% select(D.repair.cost) %>% pull,
               0.5)
  expect_equal(Blockbuster(element, inflation = 1)[[2]]$element %>%
                 slice(1) %>% select(E.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, inflation = 2)[[2]]$element %>%
                 slice(1) %>% select(E.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, inflation = 0.5)[[2]]$element %>%
                 slice(1) %>% select(E.repair.cost) %>% pull,
               0.5)
  })

test_that("Blockbuster saves output in correct place", {

  # 1 block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E,
                        ab = 0, bc = 0, cd = 0, de = 0, # turns off deterioration
                        elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = c(0, 1, 0, 0, 0),
                        C.repair.total = c(0, 0, 1, 0, 0),
                        D.repair.total = c(0, 0, 0, 1, 0),
                        E.repair.total = c(0, 0, 0, 0, 1),
                        gifa = 3, unit_area = 1)

  # run simulation with a random number as the save tag
  dir_tag <- "test_outputs"
  file_tag <- "test"
  Blockbuster(element, filelabel = file_tag, path = dir_tag)

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

  # 1 block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E,
                        ab = 0, bc = 0, cd = 0, de = 0, # turns off deterioration
                        elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = c(0, 1, 0, 0, 0),
                        C.repair.total = c(0, 0, 1, 0, 0),
                        D.repair.total = c(0, 0, 0, 1, 0),
                        E.repair.total = c(0, 0, 0, 0, 1),
                        gifa = 3, unit_area = 1)

  # outputs should match
  expect_equal(Blockbuster(element),
               Blockbuster(element))
})

test_that("Blockbuster makes different repair decisions when the grade order is changed", {

  # 1 block
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E,
                        ab = 0, bc = 0, cd = 0, de = 0, # turns off deterioration
                        elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = c(0, 1, 0, 0, 0),
                        C.repair.total = c(0, 0, 1, 0, 0),
                        D.repair.total = c(0, 0, 0, 1, 0),
                        E.repair.total = c(0, 0, 0, 0, 1),
                        gifa = 3, unit_area = 1)

  expect_equal(Blockbuster(element, repair.money = 1)[[2]]$block %>%
                 select(B.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 1)[[2]]$block %>%
                 select(C.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 1)[[2]]$block %>%
                 select(D.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 1)[[2]]$block %>%
                 select(E.block.repair.cost) %>% pull,
               1)

  expect_equal(Blockbuster(element, repair.money = 1, grade.order = c("B", "C", "D"))[[2]]$block %>%
                 select(B.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 1, grade.order = c("B", "C", "D"))[[2]]$block %>%
                 select(C.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 1, grade.order = c("B", "C", "D"))[[2]]$block %>%
                 select(D.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 1, grade.order = c("B", "C", "D"))[[2]]$block %>%
                 select(E.block.repair.cost) %>% pull,
               1)

  expect_equal(Blockbuster(element, repair.money = 1, grade.order = c("C", "D", "B"))[[2]]$block %>%
                 select(B.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 1, grade.order = c("C", "D", "B"))[[2]]$block %>%
                 select(C.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 1, grade.order = c("C", "D", "B"))[[2]]$block %>%
                 select(D.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 1, grade.order = c("C", "D", "B"))[[2]]$block %>%
                 select(E.block.repair.cost) %>% pull,
               1)

})


test_that("Blockbuster works with single inputs by repeating values.",
          {

            # 1 block
            A <- c(1, 0, 0, 0, 0)
            B <- c(0, 1, 0, 0, 0)
            C <- c(0, 0, 1, 0, 0)
            D <- c(0, 0, 0, 1, 0)
            E <- c(0, 0, 0, 0, 1)
            element <- data.frame(A, B, C, D, E,
                                  ab = 0, bc = 0, cd = 0, de = 0, # turns off deterioration
                                  elementid = 1:5, buildingid = 1,
                                  B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                                  B.repair.total = c(0, 1, 0, 0, 0),
                                  C.repair.total = c(0, 0, 1, 0, 0),
                                  D.repair.total = c(0, 0, 0, 1, 0),
                                  E.repair.total = c(0, 0, 0, 0, 1),
                                  gifa = 3, unit_area = 1)
            block <- data.frame(buildingid = 1,
                                block.rebuild.cost = 1,
                                ratio = 1,
                                B.block.repair.cost = 1,
                                C.block.repair.cost = 2,
                                D.block.repair.cost = 3,
                                E.block.repair.cost = 4)


            expect_warning(Blockbuster(element,
                                     forecast.horizon = 2, rebuild.money = 0,
                                     repair.money = c(0,0), inflation = c(1,1)),
                           "You have only provided a single value for rebuild.money. It will be used as the available funds for each forecast timestep.")

            expect_warning(Blockbuster(element,
                                     forecast.horizon = 2, rebuild.money = c(0,0),
                                     repair.money = 0, inflation = c(1,1)),
                           "You have only provided a single value for repair.money. It will be used as the available funds for each forecast timestep.")
            expect_message(Blockbuster(element, block,
                                       forecast.horizon = 2, rebuild.money = c(1, 1),
                                       repair.money = c(1, 1), inflation = 1),
                           "Inflation will be applied as a constant rate each timestep")
          })


#
test_that("Incorrect inputs produce the appropriate warnings and errors",
          {

            # 1 block
            A <- c(1, 0, 0, 0, 0)
            B <- c(0, 1, 0, 0, 0)
            C <- c(0, 0, 1, 0, 0)
            D <- c(0, 0, 0, 1, 0)
            E <- c(0, 0, 0, 0, 1)
            element <- data.frame(A, B, C, D, E,
                                  ab = 0, bc = 0, cd = 0, de = 0, # turns off deterioration
                                  elementid = 1:5, buildingid = 1,
                                  B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                                  B.repair.total = c(0, 1, 0, 0, 0),
                                  C.repair.total = c(0, 0, 1, 0, 0),
                                  D.repair.total = c(0, 0, 0, 1, 0),
                                  E.repair.total = c(0, 0, 0, 0, 1),
                                  gifa = 3, unit_area = 1)
            block <- data.frame(buildingid = 1,
                                block.rebuild.cost = 1,
                                ratio = 1,
                                B.block.repair.cost = 1,
                                C.block.repair.cost = 2,
                                D.block.repair.cost = 3,
                                E.block.repair.cost = 4)


            # incorrect element.data
            expect_error(Blockbuster(element.data = block,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            #incorrect block.data
            expect_error(Blockbuster(element.data = element,
                                     block.data = element,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            # invalid forecast.horizon
            expect_error(Blockbuster(element.data = element,
                                     forecast.horizon = 0,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            # negative monies
            expect_error(Blockbuster(element.data = element,
                                     forecast.horizon = 1,
                                     rebuild.money = -100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            expect_error(Blockbuster(element.data = element,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = -10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            expect_error(Blockbuster(element.data = element,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = -123,
                                     inflation = 1))
            # too many values
            expect_warning(Blockbuster(element.data = element,
                                     forecast.horizon = 1,
                                     rebuild.money = 1:10,
                                     repair.money = 1,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            expect_warning(Blockbuster(element.data = element,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 1:10000,
                                     block.rebuild.cost = 100,
                                     inflation = 1))
            expect_warning(Blockbuster(element.data = element,
                                     forecast.horizon = 1,
                                     rebuild.money = 100000,
                                     repair.money = 10000,
                                     block.rebuild.cost = 1:10,
                                     inflation = 1))
            expect_warning(Blockbuster(element.data = element,
                                       forecast.horizon = 1,
                                       rebuild.money = 100000,
                                       repair.money = 10000,
                                       block.rebuild.cost = 100,
                                       inflation = 1:10))
            # recycling inflation
            expect_message(Blockbuster(element.data = element,
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
