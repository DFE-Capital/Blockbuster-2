context("Testing repairComponent function")

# repairComponent ---------------------------------------------------------

test_that("Only the correct row is changed", {
          # 1 block
          A <- c(1, 0, 0, 0, 0, 0.4)
          B <- c(0, 1, 0, 0, 0, 0.25)
          C <- c(0, 0, 1, 0, 0, 0.15)
          D <- c(0, 0, 0, 1, 0, 0.07)
          E <- c(0, 0, 0, 0, 1, 0.03)
          element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                                B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                                B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                                gifa = 1, unit_area = 1)

          expect_failure(expect_equal(repairComponent(element, 2, "B")[2, ], element[2, ])) # compare repaired row
          expect_equal(repairComponent(element, 2, "B")[6, ], element[6, ]) # compare row that shouldn't change

          expect_failure(expect_equal(repairComponent(element, 6, "B")[6, ], element[2, ])) # compare repaired row
          expect_equal(repairComponent(element, 6, "B")[2, ], element[2, ]) # compare row that shouldn't change

})

test_that("Only the correct grades are changed", {
  # 1 block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 1, D.repair.cost = 1, E.repair.cost = 1,
                        B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                        gifa = 1, unit_area = 1)

  expect_failure(expect_equal(repairComponent(element, 6, "B")[["B"]], element[["B"]])) # compare column that changes
  expect_failure(expect_equal(repairComponent(element, 6, "B")[["A"]], element[["A"]])) # compare column that changes
  expect_equal(repairComponent(element, 6, "B")[["D"]], element[["D"]]) # compare column that doesn't change

  expect_failure(expect_equal(repairComponent(element, 2, "B")[["B"]], element[["B"]])) # compare column that changes
  expect_failure(expect_equal(repairComponent(element, 2, "B")[["A"]], element[["A"]])) # compare column that changes
  expect_equal(repairComponent(element, 2, "B")[["D"]], element[["D"]]) # compare column that doesn't change

})


test_that("The updated values are correct", {
  # 1 block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                        gifa = 1, unit_area = 1)

          expect_equal(repairComponent(element, 3, "C")$C[3], 0) #repaired
          expect_equal(repairComponent(element, 3, "C")$A[3], 1) #includes repaired

          expect_equal(repairComponent(element, 2, "B")$B[2], 0) #repaired
          expect_equal(repairComponent(element, 2, "B")$A[2], 1) # includes repaired

          expect_equal(repairComponent(element, 4, "D")$A[4], 1) # includes repaired
          expect_equal(repairComponent(element, 4, "D")$D[4], 0) # repaired

          expect_equal(repairComponent(element, 5, "E")$A[5], 1)
          expect_equal(repairComponent(element, 5, "E")$E[5], 0)

          expect_equal(repairComponent(element, 6, "B")$A[6], 0.65)
          expect_equal(repairComponent(element, 6, "B")$B[6], 0)

          expect_equal(repairComponent(element, 6, "C")$A[6], 0.55)
          expect_equal(repairComponent(element, 6, "C")$C[6], 0)
          expect_equal(repairComponent(element, 6, "D")$A[6], 0.47)
          expect_equal(repairComponent(element, 6, "D")$D[6], 0)

          expect_equal(repairComponent(element, 6, "E")$A[6], 0.43)
          expect_equal(repairComponent(element, 6, "E")$E[6], 0)

})



# repairGrade -------------------------------------------------------------

context("Testing repairGrade function")

test_that("Doesn't repair when there is no money", {
  # 1 block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2, D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  attr(element, "repair_money") <- 0
  expect_equal(repairGrade(element, "A"), element)
  expect_equal(repairGrade(element, "B"), element)
  expect_equal(repairGrade(element, "C"), element)
  expect_equal(repairGrade(element, "D"), element)
  expect_equal(repairGrade(element, "E"), element)

  attr(element, "repair_money") <- -10
  expect_equal(repairGrade(element, "A"), element)
  expect_equal(repairGrade(element, "B"), element)
  expect_equal(repairGrade(element, "C"), element)
  expect_equal(repairGrade(element, "D"), element)
  expect_equal(repairGrade(element, "E"), element)

})

test_that("Components are repaired when there is money", {
  # 1 block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2, D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  attr(element, "repair_money") <- 1
  expect_equal(repairGrade(element, "A"), element)
  expect_failure(expect_equal(repairGrade(element, "B"), element))
  expect_failure(expect_equal(repairGrade(element, "C"), element))
  expect_failure(expect_equal(repairGrade(element, "D"), element))
  expect_failure(expect_equal(repairGrade(element, "E"), element))

  attr(element, "repair_money") <- 10
  expect_equal(repairGrade(element, "A"), element)
  expect_failure(expect_equal(repairGrade(element, "B"), element))
  expect_failure(expect_equal(repairGrade(element, "C"), element))
  expect_failure(expect_equal(repairGrade(element, "D"), element))
  expect_failure(expect_equal(repairGrade(element, "E"), element))
})


test_that("all (and no more) possible components are repaired", {
  # 1 block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2, D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  matched_rows <- function(element1, element2){
   which(apply(element1 == element2, 1, all))
  }

  expect_matched_rows <- function(test1, test2, matched_rows){
    expect_equal(matched_rows(test1, test2), matched_rows)
  }

  attr(element, "repair_money") <- 0.1
  expect_matched_rows(repairGrade(element, "B"), element, c(1, 2, 3, 4, 5, 6))
  expect_matched_rows(repairGrade(element, "C"), element, c(1, 2, 3, 4, 5, 6))
  expect_matched_rows(repairGrade(element, "D"), element, c(1, 2, 3, 4, 5, 6))
  expect_matched_rows(repairGrade(element, "E"), element, c(1, 2, 3, 4, 5, 6))

  attr(element, "repair_money") <- 0.25
  expect_matched_rows(repairGrade(element, "B"), element, c(1, 2, 3, 4, 5))
  expect_matched_rows(repairGrade(element, "C"), element, c(1, 2, 3, 4, 5, 6))
  expect_matched_rows(repairGrade(element, "D"), element, c(1, 2, 3, 4, 5))
  expect_matched_rows(repairGrade(element, "E"), element, c(1, 2, 3, 4, 5))

  attr(element, "repair_money") <- 5
  expect_matched_rows(repairGrade(element, "B"), element, c(1, 3, 4, 5))
  expect_matched_rows(repairGrade(element, "C"), element, c(1, 2, 4, 5))
  expect_matched_rows(repairGrade(element, "D"), element, c(1, 2, 3, 5))
  expect_matched_rows(repairGrade(element, "E"), element, c(1, 2, 3, 4))
})

test_that("Is faster when there is no money (it doesn't use the algorithm)", {
  # 1 block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2, D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  benchmark <- function(element, money){
    t1 <- Sys.time()
    attr(element, "repair_money") <- money
    repairGrade(element, "B")
    t2 <- Sys.time()
    as.numeric(t2 - t1)
  }

  expect_lte(benchmark(element, 0),
             benchmark(element, 10))
  expect_lte(benchmark(element, -10),
             benchmark(element, 10))
})

test_that("Correct grade is repaired", {
  # 1 block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2, D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  matched_cols <- function(element1, element2){
    names(which(apply(element1[c("A", "B", "C", "D", "E")] == element2[c("A", "B", "C", "D", "E")], 2, all)))
  }

  expect_matched_cols <- function(test1, test2, matched_cols){
    expect_equal(matched_cols(test1, test2), matched_cols)
  }

  attr(element, "repair_money") <- 5
  expect_matched_cols(repairGrade(element, "B"), element, c("C", "D", "E"))
  expect_matched_cols(repairGrade(element, "C"), element, c("B", "D", "E"))
  expect_matched_cols(repairGrade(element, "D"), element, c("B", "C", "E"))
  expect_matched_cols(repairGrade(element, "E"), element, c("B", "C", "D"))

})


# Repair ------------------------------------------------------------------

context("Testing Repair function")

test_that("No repairs if there is no money, or negative money", {
  # construct toy element
  element <- data.frame(buildingid = 1:4,
                        elementid = 1,
                        A = 9, # this value is never checked
                        B = 9,
                        C = 9,
                        D = 9,
                        E = 9)

  expect_equal(Repair(element, 0), element)
  expect_equal(Repair(element, -1), element)
})

test_that("Faster if there is no funds and therefore no checking", {

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


  benchmark <- function(element, money){
    t1 <- Sys.time()
    Repair(element, money)
    t2 <- Sys.time()
    as.numeric(t2 - t1)
  }

  expect_lte(benchmark(element, 0),
             benchmark(element, 10))
  expect_lte(benchmark(element, -10),
             benchmark(element, 10))

          })

test_that("Things are repaired if there is money", {
          # 1 block
          A <- c(1, 0, 0, 0, 0)
          B <- c(0, 1, 0, 0, 0)
          C <- c(0, 0, 1, 0, 0)
          D <- c(0, 0, 0, 1, 0)
          E <- c(0, 0, 0, 0, 1)
          element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:5, buildingid = 1,
                                B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                                B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                                gifa = 1, unit_area = 1)

          # 1 can only repair B grade component
          expect_equal(Repair(element, 1)$A, c(1, 1, 0, 0, 0))
          expect_equal(Repair(element, 1)$B, c(0, 0, 0, 0, 0))
          expect_equal(Repair(element, 1)$C, c(0, 0, 1, 0, 0))
          expect_equal(Repair(element, 1)$D, c(0, 0, 0, 1, 0))
          expect_equal(Repair(element, 1)$E, c(0, 0, 0, 0, 1))

          # 2 can only repair C grade component
          expect_equal(Repair(element, 2)$A, c(1, 0, 1, 0, 0))
          expect_equal(Repair(element, 2)$B, c(0, 1, 0, 0, 0))
          expect_equal(Repair(element, 2)$C, c(0, 0, 0, 0, 0))
          expect_equal(Repair(element, 2)$D, c(0, 0, 0, 1, 0))
          expect_equal(Repair(element, 2)$E, c(0, 0, 0, 0, 1))

          # 3 can only repair D grade component
          expect_equal(Repair(element, 3)$A, c(1, 0, 0, 1, 0))
          expect_equal(Repair(element, 3)$B, c(0, 1, 0, 0, 0))
          expect_equal(Repair(element, 3)$C, c(0, 0, 1, 0, 0))
          expect_equal(Repair(element, 3)$D, c(0, 0, 0, 0, 0))
          expect_equal(Repair(element, 3)$E, c(0, 0, 0, 0, 1))

          # 4 can only repair E grade component
          expect_equal(Repair(element, 4)$A, c(1, 0, 0, 0, 1))
          expect_equal(Repair(element, 4)$B, c(0, 1, 0, 0, 0))
          expect_equal(Repair(element, 4)$C, c(0, 0, 1, 0, 0))
          expect_equal(Repair(element, 4)$D, c(0, 0, 0, 1, 0))
          expect_equal(Repair(element, 4)$E, c(0, 0, 0, 0, 0))

          # 5 can only repair E and B grade component
          expect_equal(Repair(element, 5)$A, c(1, 1, 0, 0, 1))
          expect_equal(Repair(element, 5)$B, c(0, 0, 0, 0, 0))
          expect_equal(Repair(element, 5)$C, c(0, 0, 1, 0, 0))
          expect_equal(Repair(element, 5)$D, c(0, 0, 0, 1, 0))
          expect_equal(Repair(element, 5)$E, c(0, 0, 0, 0, 0))

          # 6 can only repair E and C grade component
          expect_equal(Repair(element, 6)$A, c(1, 0, 1, 0, 1))
          expect_equal(Repair(element, 6)$B, c(0, 1, 0, 0, 0))
          expect_equal(Repair(element, 6)$C, c(0, 0, 0, 0, 0))
          expect_equal(Repair(element, 6)$D, c(0, 0, 0, 1, 0))
          expect_equal(Repair(element, 6)$E, c(0, 0, 0, 0, 0))

          # 10 repairs all
          expect_equal(Repair(element, 10)$A, c(1, 1, 1, 1, 1))
          expect_equal(Repair(element, 10)$B, c(0, 0, 0, 0, 0))
          expect_equal(Repair(element, 10)$C, c(0, 0, 0, 0, 0))
          expect_equal(Repair(element, 10)$D, c(0, 0, 0, 0, 0))
          expect_equal(Repair(element, 10)$E, c(0, 0, 0, 0, 0))
})

test_that("Changing the grade order produces the correct decisions", {
  # 1 block
  A <- c(1, 0, 0, 0, 0, 0.4)
  B <- c(0, 1, 0, 0, 0, 0.25)
  C <- c(0, 0, 1, 0, 0, 0.15)
  D <- c(0, 0, 0, 1, 0, 0.07)
  E <- c(0, 0, 0, 0, 1, 0.03)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:6, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = B, C.repair.total = C * 2, D.repair.total = D * 3, E.repair.total = E * 4,
                        gifa = 1, unit_area = 1)

  matched_rows <- function(element1, element2){
    which(apply(element1 == element2, 1, all))
  }

  expect_matched_rows <- function(test1, test2, matched_rows){
    expect_equal(matched_rows(test1, test2), matched_rows)
  }

  expect_matched_rows(Repair(element, 1, grade.order = c("B")), element,      c(1, 3, 4, 5, 6))
  expect_matched_rows(Repair(element, 1, grade.order = c("C")), element,      c(1, 2, 3, 4, 5))
  expect_matched_rows(Repair(element, 1, grade.order = c("B", "C")), element, c(1, 3, 4, 5, 6))
  expect_matched_rows(Repair(element, 1, grade.order = c("C", "B")), element, c(1, 2, 3, 4, 5))
  expect_matched_rows(Repair(element, 5, grade.order = c("C", "E", "D", "B")), element , c(1, 4, 5))


})


test_that("The output object has same structure as the input", {
  A <- c(1, 0, 0, 0, 0)
  B <- c(0, 1, 0, 0, 0)
  C <- c(0, 0, 1, 0, 0)
  D <- c(0, 0, 0, 1, 0)
  E <- c(0, 0, 0, 0, 1)
  element <- data.frame(A, B, C, D, E, ab = 0.8, bc = 0.7, cd = 0.6, de = 0.5, elementid = 1:5, buildingid = 1,
                        B.repair.cost = 1, C.repair.cost = 2, D.repair.cost = 3, E.repair.cost = 4,
                        B.repair.total = 1, C.repair.total = 2, D.repair.total = 3, E.repair.total = 4,
                        gifa = 1, unit_area = 1)

  expect_equal(names(element), names(Repair(element, 1)))
  expect_equal(names(element), names(Repair(element, 1)))
  expect_equal(names(element), names(Repair(element, 100)))
})

