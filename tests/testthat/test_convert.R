# Conversion tests

context("Converting element data into block data")

test_that("ConvertElementToBlock throws an error with the wrong input.",
          {
            expect_error(ConvertElementToBlock(1))
          })

test_that("ConvertElementToBlock output is a block object",
          {
            expect_is(ConvertElementToBlock(PDS.element.data), "block")
          })

test_that("Number of rows in output matches number of unique buildingids in
          input",
          {
            expect_equal(nrow(ConvertElementToBlock(PDS.element.data)),
                         length(unique(PDS.element.data$buildingid)))
          })

test_that("Total block repair costs match the totals from the input",
          {
            expect_equal(sum(ConvertElementToBlock(PDS.element.data)$B.block.repair.cost),
                         sum(PDS.element.data$B.repair.total))
            expect_equal(sum(ConvertElementToBlock(PDS.element.data)$C.block.repair.cost),
                         sum(PDS.element.data$C.repair.total))
            expect_equal(sum(ConvertElementToBlock(PDS.element.data)$D.block.repair.cost),
                         sum(PDS.element.data$D.repair.total))
          })

test_that("Converting PDS.element.data recreates PDS.block.data.  Note that the
    PDS.block.data was created from the long format data so is a different route.")
#context("Is the conversion from long to wide format working correctly")
#
#test_that("ConvertOutput combines rows of the same building/element into one row with the correct proportions",
#         {
#           # create object
#           z <- runif(1)
#           x <- blockbuster_pds[1:2, ]
#           x$unit_area <- c(z, 1 - z)
#           x$grade <- c("A", "B")
#           x$elementid <- 1700
#           y <- ConvertOutput(x)
#           expect_equal(c(y$A, y$B), x$unit_area)
#         })
#
#test_that("ConvertOutput combines rows of the same building/element into one row with the correct area",
#          {
#            # create object
#            z <- runif(1)
#            x <- blockbuster_pds[1:2, ]
#            x$unit_area <- c(z, 1 - z)
#            x$grade <- c("A", "B")
#            x$elementid <- 1700
#            y <- ConvertOutput(x)
#            expect_equal(1, y$unit_area)
#          })
#
#test_that("ConvertOutput proportions add to 1", {
#          x <- blockbuster_pds %>%
#            filter(buildingid %in% sample(unique(blockbuster_pds$buildingid), 1))
#          y <- ConvertOutput(x)
#          expect_equal(rep(1, nrow(y)), rowSums(y[, 4:9]))
#          }
#)
