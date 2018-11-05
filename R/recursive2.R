
#' Find blocks to rebuild within budget recursively.
#'
#' An efficient recursive algorithm used by \code{\link{Rebuild}} that outputs
#' the \code{keys} corresponding to the unique combination of elements in the
#' ordered vector \code{vec} that sum to less than \code{budget}.
#'
#' The cumulative sum of the repair costs are computed and the vector split at
#' the largest amount that does not go over the budgeted amount.  The first
#' part is tagged to be fixed, and the second part is fed back into the
#' function with the remaining budget.
#'
#' @param vec A numeric vector. When called by \code{\link{Rebuild}} this
#' contains the cost of rebuilding blocks.
#' @param keys A vector. When called by \code{\link{Rebuild}} this contains the
#' \code{buildingids} associated with the costs in \code{vec}.
#' @param budget A number.
#' @param state (optional) This holds the selected keys during recursion.
#' @return A list.  \code{.$state} holds the selected keys while \code{.$budget}
#' holds the unused budget.
#' @examples
#' # Ten buildings have rebuild costs between one to ten. Spend 12 to rebuild
#' # the first four
#' blockbuster2:::RecursiveBudgeting(1:10, 1:10, 12)
#' # If the costs are reversed, the decision changes as the vector is ordered
#' blockbuster2:::RecursiveBudgeting(10:1, 1:10, 12)
RecursiveBudgeting2 <- function(vec, keys, budget, state = NULL){


  # remove components that cost more than the budget
  ind <- vec <= budget
  vec <- vec[ind]
  keys <- keys[ind]

  # stopping rules
  if (length(vec) < 1) return (list(state = state, budget = budget))


  # split in half, run on first half, then on second.
  csum <- cumsum(vec)
  ind <- which(csum <= budget) # indices of csums less than budget
  n <- ind[length(ind)] # the split point is the last index (the greatest number as vec is ordered)

  # return all keys, reduce budget
    budget <- budget - csum[n]
    state <- c(state, keys[1:n])
    if (budget == 0) return(list(state = state, budget = budget)) # finish if nothing else to spend.

  # SECOND PART
  # invoke recursion
      res <- RecursiveBudgeting2(vec[-(1:n)], keys[-(1:n)], budget, state)
      budget <- res$budget
      state  <- res$state
      if (budget == 0) return(list(state = state, budget = budget))

  return(list(state = state, budget = budget))
}
