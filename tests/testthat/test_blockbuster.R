context("Testing main Blockbuster function")


test_that("Blockbuster doesn't fail", {

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

  # There should be no errors or warnings
  expect_failure(expect_error(Blockbuster(element)))
  expect_failure(expect_warning(Blockbuster(element)))
})

test_that("Blockbuster creates block data when not supplied", {

  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5,
                        elementid = 1:6, buildingid = 1, B.repair.cost = 1,
                        C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2,
                        D.repair.total = D * 3, E.repair.total = E * 4, # correct figures
                        gifa = 1, unit_area = 1)

  expect_message(Blockbuster(element), "Constructing block summary from element.data.")
  })

test_that("Blockbuster models correct timespan", {

  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5,
                        elementid = 1:6, buildingid = 1, B.repair.cost = 1,
                        C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2,
                        D.repair.total = D * 3, E.repair.total = E * 4, # correct figures
                        gifa = 1, unit_area = 1)

  # define expectation that checks the maximum year in the output matches the
  # forecast.horizon
  expect_year <- function(element, year){
    result <- suppressWarnings(Blockbuster(element, forecast.horizon = year))$"element summary"$year %>%
      max()
    expect_equal(year, result)
  }

  # check against a variety of years
  # warnings are suppressed as the input checks throw warnings as they recycle default inputs over each year
  expect_year(element, 1)
  expect_year(element, 2)
  expect_year(element, 3)
  expect_year(element, 4)
  expect_year(element, 5)
  expect_year(element, 10)
  expect_year(element, 20)
})

test_that("Blockbuster rebuilds when there are funds", {


  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5,
                        elementid = 1:6, buildingid = 1, B.repair.cost = 1,
                        C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2,
                        D.repair.total = D * 3, E.repair.total = E * 4, # correct figures
                        gifa = 1, unit_area = 1)

  expect_zero_backlog <- function(blockbuster_output){
    blockbuster_output$"building summary" %>%
      filter(year == 1) %>%
      pull(backlog) %>%
      sum(na.rm = TRUE) %>%
      expect_equal(0)
  }

  expect_zero_backlog(Blockbuster(element, rebuild.money = 2000))
})

test_that("Blockbuster does not rebuild or repair when there is no money", {


  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E,
                        # turn off deterioration with zero rates
                        ab = 0, bc = 0, cd = 0, de = 0,
                        elementid = 1:6, buildingid = 1, B.repair.cost = 1,
                        C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2,
                        D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  expect_equal(Blockbuster(element)$element,
               element)
})

test_that("Blockbuster repairs when there are funds", {


  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)

  element <- data.frame(A, B, C, D, E,
                        # turn off deterioration with zero rates
                        ab = 0, bc = 0, cd = 0, de = 0,
                        elementid = 1:5, buildingid = 1, B.repair.cost = 1,
                        C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2,
                        D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  # note that repair totals are B = 1, C = 2, D = 3, E = 4

  # with 1 money, only B is repaired
  expect_equal(Blockbuster(element, repair.money = 1)$block %>%
                 select(B.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 1)$block %>%
                 select(C.block.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, repair.money = 1)$block %>%
                 select(D.block.repair.cost) %>% pull,
               3)
  expect_equal(Blockbuster(element, repair.money = 1)$block %>%
                 select(E.block.repair.cost) %>% pull,
               4)

  # with 2 money, C is repaired only
  expect_equal(Blockbuster(element, repair.money = 2)$block %>%
                 select(B.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 2)$block %>%
                 select(C.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 2)$block %>%
                 select(D.block.repair.cost) %>% pull,
               3)
  expect_equal(Blockbuster(element, repair.money = 2)$block %>%
                 select(E.block.repair.cost) %>% pull,
               4)

  # with 8 money B, D and E are repaired
  expect_equal(Blockbuster(element, repair.money = 8)$block %>%
                 select(B.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 8)$block %>%
                 select(C.block.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, repair.money = 8)$block %>%
                 select(D.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 8)$block %>%
                 select(E.block.repair.cost) %>% pull,
               0)

  })

test_that("Blockbuster correctly adjusts rebuild costs when block.rebuild.cost argument is changed", {

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

  expect_equal(Blockbuster(element, block.rebuild.cost = 1)$block %>%
                 select(block.rebuild.cost) %>% pull,
               1)

  expect_equal(Blockbuster(element, block.rebuild.cost = 10)$block %>%
                 select(block.rebuild.cost) %>% pull,
               10)
})

test_that("Blockbuster correctly applies inflation", {

  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E,
                        # turn off deterioration with zero rates
                        ab = 0, bc = 0, cd = 0, de = 0,
                        elementid = 1:6, buildingid = 1, B.repair.cost = 1,
                        C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2,
                        D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  # as there is no deterioration, repair or rebuilds then the backlog should
  # change in proportion to inflation
  expect_equal(Blockbuster(element, inflation = 1)$block %>%
                 select(B.block.repair.cost) %>% pull,
               1.25)
  expect_equal(Blockbuster(element, inflation = 2)$block %>%
                 select(B.block.repair.cost) %>% pull,
               2.5)
  expect_equal(Blockbuster(element, inflation = 0.5)$block %>%
                 select(B.block.repair.cost) %>% pull,
               0.625)

  # the repair costs should show inflation
  # check the repair costs for the first element to see if this is true
  expect_equal(Blockbuster(element, inflation = 1)$element %>%
                 slice(1) %>% select(B.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, inflation = 2)$element %>%
                 slice(1) %>% select(B.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, inflation = 0.5)$element %>%
                 slice(1) %>% select(B.repair.cost) %>% pull,
               0.5)
  expect_equal(Blockbuster(element, inflation = 1)$element %>%
                 slice(1) %>% select(C.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, inflation = 2)$element %>%
                 slice(1) %>% select(C.repair.cost) %>% pull,
               4)
  expect_equal(Blockbuster(element, inflation = 0.5)$element %>%
                 slice(1) %>% select(C.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, inflation = 1)$element %>%
                 slice(1) %>% select(D.repair.cost) %>% pull,
               3)
  expect_equal(Blockbuster(element, inflation = 2)$element %>%
                 slice(1) %>% select(D.repair.cost) %>% pull,
               6)
  expect_equal(Blockbuster(element, inflation = 0.5)$element %>%
                 slice(1) %>% select(D.repair.cost) %>% pull,
               1.5)
  expect_equal(Blockbuster(element, inflation = 1)$element %>%
                 slice(1) %>% select(E.repair.cost) %>% pull,
               4)
  expect_equal(Blockbuster(element, inflation = 2)$element %>%
                 slice(1) %>% select(E.repair.cost) %>% pull,
               8)
  expect_equal(Blockbuster(element, inflation = 0.5)$element %>%
                 slice(1) %>% select(E.repair.cost) %>% pull,
               2)
  })

test_that("Blockbuster saves output in correct place", {


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

  # run simulation with interim states saved
  dir_tag <- "test_outputs"
  file_tag <- "test"
  Blockbuster(element, save = TRUE, filelabel = file_tag, path = dir_tag)

  # check files are all there - there should be two element files and two block
  # files
  expect_true(file.exists(paste0(file.path(dir_tag, file_tag), "_element_0.rds")))
  expect_true(file.exists(paste0(file.path(dir_tag, file_tag), "_element_1.rds")))
  expect_true(file.exists(paste0(file.path(dir_tag, file_tag), "_block_0.rds")))
  expect_true(file.exists(paste0(file.path(dir_tag, file_tag), "_block_1.rds")))
})

test_that("Blockbuster makes same decisions on runs with the same inputs", {

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

  # outputs should match
  expect_equal(Blockbuster(element),
               Blockbuster(element))
})

test_that("Blockbuster makes different repair decisions when the grade order is changed", {

  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)

  element <- data.frame(A, B, C, D, E,
                        # turn off deterioration with zero rates
                        ab = 0, bc = 0, cd = 0, de = 0,
                        elementid = 1:5, buildingid = 1, B.repair.cost = 1,
                        C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2,
                        D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  # with 4 money, E is repaired by default
  expect_equal(Blockbuster(element, repair.money = 4)$block %>%
                 select(B.block.repair.cost) %>% pull,
               1)
  expect_equal(Blockbuster(element, repair.money = 4)$block %>%
                 select(C.block.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, repair.money = 4)$block %>%
                 select(D.block.repair.cost) %>% pull,
               3)
  expect_equal(Blockbuster(element, repair.money = 4)$block %>%
                 select(E.block.repair.cost) %>% pull,
               0)

  # if we set B,C,D, E as the order then B and C are repaired
  expect_equal(Blockbuster(element, repair.money = 4,
                           grade.order = c("B", "C", "D", "E"))$block %>%
                 select(B.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 4,
                           grade.order = c("B", "C", "D"))$block %>%
                 select(C.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 4,
                           grade.order = c("B", "C", "D"))$block %>%
                 select(D.block.repair.cost) %>% pull,
               3)
  expect_equal(Blockbuster(element, repair.money = 4,
                           grade.order = c("B", "C", "D"))$block %>%
                 select(E.block.repair.cost) %>% pull,
               4)

  # if D,C,B is the order then D and B are repaired as E is ignored
  expect_equal(Blockbuster(element, repair.money = 4,
                           grade.order = c("D", "C", "B"))$block %>%
                 select(B.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 4,
                           grade.order = c("D", "C", "B"))$block %>%
                 select(C.block.repair.cost) %>% pull,
               2)
  expect_equal(Blockbuster(element, repair.money = 4,
                           grade.order = c("D", "C", "B"))$block %>%
                 select(D.block.repair.cost) %>% pull,
               0)
  expect_equal(Blockbuster(element, repair.money = 4,
                           grade.order = c("D", "C", "B"))$block %>%
                 select(E.block.repair.cost) %>% pull,
               4)

})


test_that("Blockbuster works with single inputs by repeating values.", {

  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6,de = 0.5,
                        elementid = 1:6, buildingid = 1, B.repair.cost = 1,
                        C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2,
                        D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  expect_warning(Blockbuster(element,
                             forecast.horizon = 2, rebuild.money = 0,
                             repair.money = c(0,0), inflation = c(1,1)),
                 "You have only provided a single value for rebuild.money. It will be used as the available funds for each forecast timestep.")
  expect_warning(Blockbuster(element, forecast.horizon = 2,
                             rebuild.money = c(0,0), repair.money = 0,
                             inflation = c(1,1)),
                 "You have only provided a single value for repair.money. It will be used as the available funds for each forecast timestep.")
  expect_message(Blockbuster(element, forecast.horizon = 2,
                             rebuild.money = c(1, 1), repair.money = c(1, 1),
                             inflation = 1),
                 "Inflation will be applied as a constant rate each timestep")
  })



test_that("Incorrect inputs produce the appropriate warnings and errors", {
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
  block <- data.frame(buildingid = 1,
                      block.rebuild.cost = 1,
                      ratio = 1,
                      B.block.repair.cost = 1,
                      C.block.repair.cost = 2,
                      D.block.repair.cost = 3,
                      E.block.repair.cost = 4)
  # incorrect element.data
  expect_error(Blockbuster(element.data = block))
            #incorrect block.data
            expect_error(Blockbuster(element.data = element,
                                     block.data = element))
            # invalid forecast.horizon
            expect_error(Blockbuster(element.data = element,
                                     forecast.horizon = 0))
            # negative monies
            expect_error(Blockbuster(element.data = element,
                                     rebuild.money = -100000))
            expect_error(Blockbuster(element.data = element,
                                     repair.money = -10000))
            expect_error(Blockbuster(element.data = element,
                                     block.rebuild.cost = -123))
            # too many values
            expect_warning(Blockbuster(element.data = element,
                                     forecast.horizon = 1,
                                     rebuild.money = 1:10))
            expect_warning(Blockbuster(element.data = element,
                                     forecast.horizon = 1,
                                     repair.money = 1:10000))
            expect_warning(Blockbuster(element.data = element,
                                     block.rebuild.cost = 1:10))
            expect_warning(Blockbuster(element.data = element,
                                       forecast.horizon = 1,
                                       inflation = 1:10))
            # recycling inflation
            expect_message(Blockbuster(element.data = element,
                                     forecast.horizon = 2,
                                     inflation = 1))
          })

test_that("No files are saved when save = FALSE",{
  # 1 block

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
  block <- data.frame(buildingid = 1,
                      block.rebuild.cost = 1,
                      ratio = 1,
                      B.block.repair.cost = 1,
                      C.block.repair.cost = 2,
                      D.block.repair.cost = 3,
                      E.block.repair.cost = 4)

  # run simulation with a random number as the save tag
  dir_tag <- "test_save_outputs"
  file_tag <- "test"
  Blockbuster(element, filelabel = file_tag, path = dir_tag,
              save = FALSE)

  # check file is there
  expect_false(file.exists(paste0(file.path(dir_tag, file_tag), "_output.rds")))
})

test_that("Blockbuster outputs contains area and backlog summaries for all years",{
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

  test_output <- Blockbuster(element, forecast.horizon = 2, save = FALSE)

  expect_named(test_output,
               c("element summary", "building summary", "element", "block", "building failures",
                 "Number of buildings in need of rebuilding", "Number of rebuilds", "Cost of rebuilding in need buildings"),
               ignore.order = TRUE)
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
