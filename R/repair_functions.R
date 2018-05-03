#' Repair elements by expected cost at a grade
#'
#' Given the current state of the buildings, this function outputs the element-
#' level status after repairing components within budget.
#'
#' The repairing gives priority to repairing those components with the highest
#' expected cost at grade E, then grade D, then grade C, then grade B, although
#' this order can be adjusted using the arguement \code{grade_order}. It calls
#' the same \code{\link{RecursiveBudgeting}} recursive algorithm used by
#' \code{\link{Rebuild()}}function to efficiently identify the most at risk
#' components within budget.
#'
#' This function does NOT update the repair costs, only the grade proportions.
#' Therefore \code{\link{UpdateElementRepairs}} and
#' \code{\link{UpdateBlockRepairs}} should be called on the element- and block-
#' level objects afterwards to ensure the repair costs and totals are correct.
#'
#' @param element.data An \code{\link{element}} class object.
#' @param repair.money A number.
#' @param grade.order (Optional) A vector of character strings from \code{"E"},
#'  \code{"D"}, \code{"C"} and \code{"B"} in any order.  This determines the
#'  priority for repairs.  The first character gives the first grade that will
#'  be repaired. By default, E is repaired first, then D, then C, then B.
#'
#' @return An \code{\link{element}} class object with the grade proportions
#' amended accordingly.
Repair <- function(element.data, repair.money,
                   grade.order = c("E", "D", "C", "B")){

  if (!is.numeric(repair.money)) stop("The repair money must be a number.")
  if (length(repair.money) > 1) warning("Only the first value in repair.money
                                        will be used.")
  # If there is no money then no need to repair.
  if (repair.money <= 0) return(element.data)

  attr(element.data, "repair_money") <- repair.money

  for(i in 1:length(grade.order)){

    element.data <- repairGrade(element.data, grade = grade.order[i])

  }

  return(element.data) # Note that repair costs haven't been updated.
}




#' Repair components of a particular grade
#'
#' This function is used by \code{\link{Repair}} to repair all components at a
#' particular grade.
#'
#' Components are sorted by their proportion at the given grade.  They are then
#' repaired in descending order until the money given by \code{budget} runs out.
#'
#' @param element.data An \code{\link{element}} object, with a repair_money attribute indicating the money allowed for repairs
#' @param budget Numeric. The amount of funds available for repair.
#' @param grade Either \code{"B"}, \code{"C"}, \code{"D"} or \code{"E"}.  The grade to
#' repair
#'
#' @return The \code{\link{element}} object with repairs
#' completed, and a numeric amount stating the remaining funds as an attribute.
repairGrade <- function(element.data, grade){

  if(grade == "A") return(element.data) # no change as A isn't repaired

  repair.money <- attr(element.data, "repair_money")

  # no repairs needed if there is no money
  if (repair.money <= 0) return(element.data)

  # identify candidate rows (non zero probability)
  candidates <- which(element.data[grade] > 0)
  # compute expected repair cost (repair.cost * probability of grade)
  cost <- (element.data[[grade]] * element.data[[paste0(grade, ".repair.cost")]] * element.data[["unit_area"]])[candidates]
  # arrange row indices and costs in order of expected repair cost
  ord <- order(cost, decreasing = TRUE)
  cost <- cost[ord]
  candidates <- candidates[ord]
  # pass to function that identifies which will be repaired
  repairing <- RecursiveBudgeting(cost, candidates, repair.money)
  # update element.data with repairs by calling repairComponent
  element.data <- repairComponent(element.data, repairing$state, grade)
  attr(element.data, "repair_money") <- repairing$budget
  return(element.data)
}

#' Repairs the component in the given rows at the given grade
#'
#' @param element.data
#' @param row numeric.  A vector of row indices to update
#' @param grade
#'
#' @return The updated element.data
repairComponent <- function(element.data, rows, grade){
  if(grade == "A") return(element.data) # no change as "A" isn't valid for repair
  element.data[rows, ]$A <- element.data$A[rows] + element.data[[grade]][rows]
  element.data[[grade]][rows] <- 0
  return(element.data)
}

