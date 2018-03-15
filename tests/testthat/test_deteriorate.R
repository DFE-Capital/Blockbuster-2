context("Element deterioration")

test_that("Deteriorate throws errors with incorrect inputs",
          {
            test.data <- PDS.block
            class(test.data) <- class(test.data)[1:3] # remove block class

            # incorrect inputs
            expect_error(Deteriorate(PDS.block))
            expect_error(Deteriorate(1))
            expect_error(Deteriorate(test.data))

            # correct input
            expect_silent(Deteriorate(PDS.element))
          }

)

test_that("Deteriorate returns the correct class",
          {
            expect_is(Deteriorate(PDS.element), "element")
          })

test_that("Random elements set to a specific grade deteriorate correctly through
          Deteriorate",
          {
            test.data <- PDS.element[sample(1:nrow(PDS.element), 4), ]
            test.data <- ElementLevel(test.data)
            test.data$A <- c(1, 0, 0, 0)
            test.data$B <- c(0, 1, 0, 0)
            test.data$C <- c(0, 0, 1, 0)
            test.data$D <- c(0, 0, 0, 1)
            output <- Deteriorate(test.data) %>%
              select(A, B, C, D) %>% as.matrix

            expect_equivalent(output[1, ],  # grade A
                         c(1 - test.data$ab[1], test.data$ab[1], 0, 0))
            expect_equivalent(output[2, ],
                         c(0, 1 - test.data$bc[2], test.data$bc[2], 0))
            expect_equivalent(output[3, ],
                         c(0, 0, 1 - test.data$cd[3], test.data$cd[3]))
            })
