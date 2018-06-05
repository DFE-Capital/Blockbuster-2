context("Summarise functions working correctly")

test_that("Summarises area by component",{

  # set up test block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0.5, 0, 0.3)
  C <- c(0, 0, 1, 0, 0, 0.2)
  D <- c(0, 0, 0, 0.5, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E,
                        elementid = c(1, 2, 3, 2, 3, 4),
                        buildingid = c(1, 1, 1, 2, 2, 2),
                        B.repair.cost = c(1, 1, 1, 1, 1, 1),
                        C.repair.cost = c(2, 2, 3, 2, 3, 4),
                        D.repair.cost = c(3, 4, 6, 4, 6, 8),
                        E.repair.cost = c(4, 5, 7, 5, 7, 10),
                        B.repair.total = c(0, 1, 0, 5, 0, 9),
                        C.repair.total = c(0, 0, 6, 0, 0, 24),
                        D.repair.total = c(0, 0, 0, 20, 0, 1.68),
                        E.repair.total = c(0, 0, 0, 0, 140, 9),
                        gifa = 1,
                        unit_area = c(1, 1, 2, 10, 20, 30))

  correct <- data.frame(grade = c("A", "B", "D", "C", "E", "A", "B", "C", "D", "E"),
                        elementid = c(1, 2, 2, 3, 3, 4, 4, 4, 4, 4),
                        area = c(1, 6, 5, 2, 20, 12, 9, 6, 2.1, 0.9),
                        stringsAsFactors = FALSE)

  expect_equal(as.data.frame(element_summarise_area(element, elementid)), # need to coerce result into data frame as expect equal tolerances don't work with tibbles.
                   correct)
})

test_that("Summarises backlog by component",{

  # set up test block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0.5, 0, 0.3)
  C <- c(0, 0, 1, 0, 0, 0.2)
  D <- c(0, 0, 0, 0.5, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E,
                        elementid = c(1, 2, 3, 2, 3, 4),
                        buildingid = c(1, 1, 1, 2, 2, 2),
                        B.repair.cost = c(1, 1, 1, 1, 1, 1),
                        C.repair.cost = c(2, 2, 3, 2, 3, 4),
                        D.repair.cost = c(3, 4, 6, 4, 6, 8),
                        E.repair.cost = c(4, 5, 7, 5, 7, 10),
                        B.repair.total = c(0, 1, 0, 5, 0, 9),
                        C.repair.total = c(0, 0, 6, 0, 0, 24),
                        D.repair.total = c(0, 0, 0, 20, 0, 1.68),
                        E.repair.total = c(0, 0, 0, 0, 140, 9),
                        gifa = 1,
                        unit_area = c(1, 1, 2, 10, 20, 30))

  correct <- data.frame(grade = c("B", "D", "C", "E", "B", "C", "D", "E"),
                        elementid = c(2, 2, 3, 3, 4, 4, 4, 4),
                        backlog = c(6, 20, 6, 140, 9, 24, 1.68, 9),
                        stringsAsFactors = FALSE)

  expect_equal(as.data.frame(element_summarise_backlog(element, elementid)), # need to coerce result into data frame as expect equal tolerances don't work with tibbles.
               correct)
})

test_that("Summarises backlog by block",{

  # set up test block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0.5, 0, 0.3)
  C <- c(0, 0, 1, 0, 0, 0.2)
  D <- c(0, 0, 0, 0.5, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E,
                        elementid = c(1, 2, 3, 2, 3, 4),
                        buildingid = c(1, 1, 1, 2, 2, 2),
                        B.repair.cost = c(1, 1, 1, 1, 1, 1),
                        C.repair.cost = c(2, 2, 3, 2, 3, 4),
                        D.repair.cost = c(3, 4, 6, 4, 6, 8),
                        E.repair.cost = c(4, 5, 7, 5, 7, 10),
                        B.repair.total = c(0, 1, 0, 5, 0, 9),
                        C.repair.total = c(0, 0, 6, 0, 0, 24),
                        D.repair.total = c(0, 0, 0, 20, 0, 1.68),
                        E.repair.total = c(0, 0, 0, 0, 140, 9),
                        gifa = 1,
                        unit_area = c(1, 1, 2, 10, 20, 30))

  correct <- data.frame(grade = c("B", "C", "B", "C", "D", "E"),
                        buildingid = c(1, 1, 2, 2, 2, 2),
                        backlog = c(1, 6, 14, 24, 21.68, 149),
                        stringsAsFactors = FALSE)

  expect_equal(as.data.frame(element_summarise_backlog(element, buildingid)), # need to coerce result into data frame as expect equal tolerances don't work with tibbles.
               correct)
})

test_that("Summarises area by block",{

  # set up test block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0.5, 0, 0.3)
  C <- c(0, 0, 1, 0, 0, 0.2)
  D <- c(0, 0, 0, 0.5, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)

  element <- data.frame(A, B, C, D, E,
                        elementid = c(1, 2, 3, 2, 3, 4),
                        buildingid = c(1, 1, 1, 2, 2, 2),
                        B.repair.cost = c(1, 1, 1, 1, 1, 1),
                        C.repair.cost = c(2, 2, 3, 2, 3, 4),
                        D.repair.cost = c(3, 4, 6, 4, 6, 8),
                        E.repair.cost = c(4, 5, 7, 5, 7, 10),
                        B.repair.total = c(0, 1, 0, 5, 0, 9),
                        C.repair.total = c(0, 0, 6, 0, 0, 24),
                        D.repair.total = c(0, 0, 0, 20, 0, 1.68),
                        E.repair.total = c(0, 0, 0, 0, 140, 9),
                        gifa = 1,
                        unit_area = c(1, 1, 2, 10, 20, 30))

  correct <- data.frame(grade = c("A", "B", "C", "A", "B", "C", "D", "E"),
                        buildingid = c(1, 1, 1, 2, 2, 2, 2, 2),
                        area = c(1, 1, 2, 12, 14, 6, 7.1, 20.9),
                        stringsAsFactors = FALSE)

  expect_equal(as.data.frame(element_summarise_area(element, buildingid)), # need to coerce result into data frame as expect equal tolerances don't work with tibbles.
               correct)

})

