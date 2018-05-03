context("Testing Blockbuster input checks")

test_that("Incorrect inputs cause errors", {

  # Define useful function for testing

  #' Run a valid input_check but with one user-defined argument
  #'
  #' This function calls the input_check function with all valid inputs, apart
  #' from one user-defined one.  This allows for simplified testing of inputs, by
  #' replacing each input in turn with a variety of invalid inputs.
  #'
  #' @param input Character. One of "element.data", "block.data", "forecast.horizon",
  #'   "rebuild.money", "repair.money", "block.rebuild.cost" or "inflation"
  #' @param value Anything
  #'
  #' @return The return from the input_check call.
  check_input_checks <- function(input, value){

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
                        block.rebuild.cost = 10,
                        B.block.repair.cost = 1,
                        C.block.repair.cost = 2,
                        D.block.repair.cost = 3,
                        E.block.repair.cost = 4,
                        ratio = 1)

    # valid arguments for input_checks
    args <- list(element.data = element,
                 block.data = block,
                 forecast.horizon = 10,
                 rebuild.money = rep(1, 10),
                 repair.money = rep(1, 10),
                 block.rebuild.cost = 1,
                 inflation = 1)
    # populate inputs with valid arguments
    inputs <- list()
    for (i in which(!names(args) %in% input))
      `[[`(inputs, names(args)[i]) <- args[[i]]
    # include user-defined argument in input
    `[[`(inputs, input) <- value
    #run input_checks
    do.call("input_checks", inputs)
  }

  # input types to test
  list_of_five <- rep(1, 5)
  list_of_ten <- rep(1, 10)
  list_of_twenty <- rep(1, 20)
  list_of_ten_char <- rep("a", 10)
  num <- 10
  zero <- 0
  neg_num <- -10
  char <- "a"
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
                      block.rebuild.cost = 10,
                      B.block.repair.cost = 1,
                      C.block.repair.cost = 2,
                      D.block.repair.cost = 3,
                      E.block.repair.cost = 4,
                      ratio = 1)

  # TESTS

  # forecast.horizon
  expect_error(check_input_checks("forecast.horizon", list_of_five), "The forecast horizon must be a single number.")
  expect_error(check_input_checks("forecast.horizon", list_of_ten), "The forecast horizon must be a single number.")
  expect_error(check_input_checks("forecast.horizon", list_of_twenty), "The forecast horizon must be a single number.")
  expect_error(check_input_checks("forecast.horizon", list_of_ten_char), "The forecast horizon must be a single number.")
  expect_error(check_input_checks("forecast.horizon", zero), "The forecast horizon must be a positive number.")
  expect_error(check_input_checks("forecast.horizon", neg_num), "The forecast horizon must be a positive number.")
  expect_error(check_input_checks("forecast.horizon", char), "The forecast horizon must be a single number.")
  expect_error(check_input_checks("forecast.horizon", element), "The forecast horizon must be a single number.")
  expect_error(check_input_checks("forecast.horizon", block), "The forecast horizon must be a single number.")

  # rebuild.money
  expect_error(check_input_checks("rebuild.money", list_of_five),
               "The length of the vector passed to rebuild.money must match the number passed to forecast.horizon.")
  expect_error(check_input_checks("rebuild.money", list_of_ten_char),
               "The rebuild.money argument must be numeric.")
  expect_warning(check_input_checks("rebuild.money", list_of_twenty),
                 "You have provided rebuild money for years outside the forecast horizon.  The extra values will be ignored.")
  expect_message(suppressWarnings(check_input_checks("rebuild.money", zero)),
                 "There will be no rebuilding as rebuild.money is zero for all years.")
  expect_error(suppressWarnings(check_input_checks("rebuild.money", neg_num)),
               "You have supplied negative values for the rebuild budget.")
  expect_warning(check_input_checks("rebuild.money", num),
                 "You have only provided a single value for rebuild.money. It will be used as the available funds for each forecast timestep.")
  expect_error(check_input_checks("rebuild.money", char),
               "The rebuild.money argument must be numeric.")
  expect_error(check_input_checks("rebuild.money", element),
               "The rebuild.money argument must be numeric.")
  expect_error(check_input_checks("rebuild.money", block),
               "The rebuild.money argument must be numeric.")

  # repair.money
  expect_error(check_input_checks("repair.money", list_of_five),
               "The length of the vector passed to repair.money must match the number passed to forecast.horizon.")
  expect_error(check_input_checks("repair.money", list_of_ten_char),
               "The repair.money argument must be numeric.")
  expect_warning(check_input_checks("repair.money", list_of_twenty),
                 "You have provided repair money for years outside the forecast horizon.  The extra values will be ignored.")
  expect_message(suppressWarnings(check_input_checks("repair.money", zero)),
                 "There will be no repairing as repair.money is zero for all years.")
  expect_error(suppressWarnings(check_input_checks("repair.money", neg_num)),
               "You have supplied negative values for the repair budget.")
  expect_warning(check_input_checks("repair.money", num),
                 "You have only provided a single value for repair.money. It will be used as the available funds for each forecast timestep.")
  expect_error(check_input_checks("repair.money", char),
               "The repair.money argument must be numeric.")
  expect_error(check_input_checks("repair.money", element),
               "The repair.money argument must be numeric.")
  expect_error(check_input_checks("repair.money", block),
               "The repair.money argument must be numeric.")

  # block.rebuild.cost
  expect_warning(check_input_checks("block.rebuild.cost", list_of_five),
                 "Only the first value entered for block.rebuild.cost will be used.")
  expect_warning(check_input_checks("block.rebuild.cost", list_of_ten),
                 "Only the first value entered for block.rebuild.cost will be used.")
  expect_error(check_input_checks("block.rebuild.cost", list_of_ten_char),
               "The unit block rebuild cost must be a positive number.")
  expect_warning(check_input_checks("block.rebuild.cost", list_of_twenty),
                 "Only the first value entered for block.rebuild.cost will be used.")
  expect_error(check_input_checks("block.rebuild.cost", zero),
                 "The unit block rebuild cost must be a positive number.")
  expect_error(check_input_checks("block.rebuild.cost", neg_num),
               "The unit block rebuild cost must be a positive number.")
  expect_failure(expect_warning(check_input_checks("block.rebuild.cost", num)))
  expect_error(check_input_checks("block.rebuild.cost", char),
               "The unit block rebuild cost must be a positive number.")
  expect_error(check_input_checks("block.rebuild.cost", element),
               "The unit block rebuild cost must be a positive number.")
  expect_error(check_input_checks("block.rebuild.cost", block),
               "The unit block rebuild cost must be a positive number.")

  # inflation
  expect_error(check_input_checks("inflation", list_of_five),
                 "The inflation argument should be a single number or a vector with a value for each timestep.")
  expect_failure(expect_warning(check_input_checks("inflation", list_of_ten),
                 "The inflation argument should be a single number or a vector with a value for each timestep."))
  expect_error(check_input_checks("inflation", list_of_ten_char),
               "The inflation argument should be a single number or a vector with a value for each timestep.")
  expect_warning(check_input_checks("inflation", list_of_twenty),
                 "You have provided inflation rates for years outside the forecast horizon.  The extra values will be ignored.")
  expect_error(check_input_checks("inflation", zero),
                 "All inflation values should be positive")
  expect_error(check_input_checks("inflation", neg_num),
               "All inflation values should be positive")
  expect_message(check_input_checks("inflation", num),
                 "Inflation will be applied as a constant rate each timestep")
  expect_error(check_input_checks("inflation", char),
               "The inflation argument should be a single number or a vector with a value for each timestep.")
  expect_error(check_input_checks("inflation", element),
               "The inflation argument should be a single number or a vector with a value for each timestep.")
  expect_error(check_input_checks("inflation", block),
               "The inflation argument should be a single number or a vector with a value for each timestep.")

  })

