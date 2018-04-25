# context("Updating repair totals on element objects")
#
# test_that("UpdateElementRepairs returns errors with incorrect inputs", {
#   x <- PDS.element
#   class(x) <- class(x)[1:3]
#
#   expect_error(UpdateElementRepairs(1))
#   expect_error(UpdateElementRepairs(PDS.block.data))
#   expect_error(UpdateElementRepairs(x)) # PDS.element.data but no as an element object.
# })
#
# test_that("UpdateElementRepairs output is correct class and no repair totals are
#           more than their theoretical maximum",
#           {
#           expect_all_less_than <- function(x, y)
#             all(x <= y)
#
#           output <- UpdateElementRepairs(PDS.element)
#           base <- PDS.element %>%
#             mutate(B.max <- unit_area * B.repair.cost,
#               C.max <- unit_area * C.repair.cost,
#               D.max <- unit_area * D.repair.cost)
#
#           expect_is(output, "element")
#           expect_all_less_than (output$B.repair.total, base$B.max)
#           expect_all_less_than (output$C.repair.total, base$C.max)
#           expect_all_less_than (output$D.repair.total, base$D.max)
#           })
#
# #------------------------------------------------------------------------------#
#
# context("Updating repair totals on block objects")
#
# test_that("UpdateBlockRepairs returns errors with incorrect inputs", {
#   x <- PDS.block
#   class(x) <- class(x)[1:3] # remove block class
#
#   y <- PDS.element
#   class(y) <- class(y)[1:3] # remove element class
#
#   expect_error(UpdateBlockRepairs(1, PDS.element.data))
#   expect_error(UpdateBlockRepairs(PDS.element.data, PDS.element.data))
#   expect_error(UpdateElementRepairs(x, PDS.element.data)) # PDS.block.data but not as a block object.
#
#   expect_error(UpdateBlockRepairs(PDS.block.data, 1))
#   expect_error(UpdateBlockRepairs(PDS.block.data, PDS.block.data))
#   expect_error(UpdateBlockRepairs(PDS.block.data, y)) # PDS.element.data but no as an element object.
#   })
#
# test_that("UpdateBlockRepairs output is correct class with no dropped blocks and
#           no negative ratios or totals",
#           {
#           expect_all_gt<- function(x, y) all(x > y)
#           output <- UpdateBlockRepairs(PDS.block, PDS.element)
#
#           expect_is(output, "block")
#           expect_equal(nrow(output), length(unique(PDS.element$buildingid)))
#           expect_all_gt(output$B.block.repair.cost, 0)
#           expect_all_gt(output$C.block.repair.cost, 0)
#           expect_all_gt(output$D.block.repair.cost, 0)
#           expect_all_gt(output$ratio, 0)
#           })
#
# context("Looking up repair costs")
#
# # test_that("blockcoster_lookup does not return negative values",
# #           {
# #             random.elementid <- sample(PDS.element$elementid, 1)
# #             random.grade <- sample(c("B", "C", "D"))
# #             expect_gte(blockcoster_lookup(the_elementid = random.elementid,
# #                                           grade = random.grade),
# #                        0)
# #           })
# #
# # test_that("blockcoster_lookup returns 0 cost and warning if repair cost not found.",
# #           {
# #             expect_equal(blockcoster_lookup(the_elementid = 0, the_grade = "C"), 0)
# #             expect_equal(blockcoster_lookup(the_elementid = 1701, the_grade = "N"), 0)
# #             expect_equal(blockcoster_lookup(the_elementid = 0, the_grade = "N"), 0)
# #             expect_warning(blockcoster_lookup(the_elementid = 0, the_grade = "C"))
# #             expect_warning(blockcoster_lookup(the_elementid = 1701, the_grade = "N"))
# #             expect_warning(blockcoster_lookup(the_elementid = 0, the_grade = "N"))
# #           })
