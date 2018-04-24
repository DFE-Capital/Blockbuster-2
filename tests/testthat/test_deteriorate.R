context("Testing Deterioration process")

test_that("Deteriorate throws errors with incorrect inputs",
          {
            # incorrect inputs
            expect_error(Deteriorate(PDS.block))
            expect_error(Deteriorate(1))

            # correct input
            expect_silent(Deteriorate(PDS.element))
          }

)

test_that("Elements deteriorate correctly through
          Deteriorate",
          {
            # test data is five rows, each at a different grade
            A <- c(1, 0, 0, 0, 0)
            B <- c(0, 1, 0, 0, 0)
            C <- c(0, 0, 1, 0, 0)
            D <- c(0, 0, 0, 1, 0)
            E <- c(0, 0, 0, 0, 1)
            test_data <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5)
            output <- Deteriorate(test_data) %>%
              select(A, B, C, D, E)

            #expected output (explicitly computed)
            A <- c(1 - 0.8, 0, 0, 0, 0)
            B <- c(0.8, 1 - 0.7, 0, 0, 0)
            C <- c(0, 0.7, 1 - 0.6, 0, 0)
            D <- c(0, 0, 0.6, 1 - 0.5, 0)
            E <- c(0, 0, 0, 1 - 0.5, 1)
            exp_output <- data.frame(A, B, C, D, E)

            expect_equal(output, exp_output)

            # test data is five rows, with different combinations of probabilities
            # four pairs of probabilities
            # one with equal probabilities over all grades
            A <- c(0.5, 0.5, 0.5, 0.5, 0.2)
            B <- c(0.5, 0, 0, 0, 0.2)
            C <- c(0, 0.5, 0, 0.5, 0.2)
            D <- c(0, 0, 0.5, 0, 0.2)
            E <- c(0, 0, 0, 0.5, 0.2)
            test_data <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5)
            output <- Deteriorate(test_data) %>%
              select(A, B, C, D, E)

            #expected output
            A <- c(0.5 * (1 - 0.8), 0.5 * (1 - 0.8), 0.5 * (1 - 0.8), 0.5 * (1 - 0.8), 0.2 * (1 - 0.8))
            B <- c(0.5 * 0.8 + 0.5 * (1 - 0.7), 0.5 * 0.8, 0.5 * 0.8, 0.5 * 0.8, 0.2 * 0.8 + 0.2 * (1 - 0.7))
            C <- c(0.5 * 0.7, 0.5 * (1 - 0.6), 0, 0.5 * (1 - 0.6), 0.2 * 0.7 + 0.2 * (1 - 0.6))
            D <- c(0, 0.5 * 0.6, 0.5 * (1 - 0.5), 0.5 * 0.6, 0.2 * 0.6 + 0.2 * (1 - 0.5))
            E <- c(0, 0, 0.5 * 0.5, 0.5, 0.2 * 0.5 + 0.2)
            exp_output <- data.frame(A, B, C, D, E)

            expect_equal(output, exp_output)

            })

test_that("Setting de to 0 removes grade E from model (as long as there is no E grade originally)", {
  # test data is five rows, with random probabilities but no grade E
  A <- runif(5)
  B <- runif(5)
  C <- runif(5)
  D <- runif(5)
  E <- rep(0, 5)
  test_data <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0)
  output <- Deteriorate(test_data) %>%
    select(A, B, C, D, E)

  expect_equal(output$E, c(0, 0, 0, 0, 0))
})

