#' Rebuild blocks by adjusting the block- and element-level data frames
#'
#' This function simulates rebuilding by changing all component blocks to grade
#' A in rebuilt blocks.  The output is an element-level data frame and only the
#' grade probabilities are changed. The repair costs are not updated in this
#' function. It is therefore important to run \code{\link{UpdateElementRepairs}}
#' and \code{\link{UpdateBlockRepairs}} on the element- and block-level data
#' frames respectively after calling this function, and in that order too.
#'
#' The decision about which blocks to rebuild is made based on a measure of
#' cost-effectiveness, the ratio between the cost to repair all components in a
#' block and the cost to rebuild the entire block.  The rebuild cost is
#' estimated using the unit rebuild cost times the Gross Internal Floor Area
#' (GIFA) of the block.
#'
#' After eliminating all zero rebuild cost block from consideration (to save
#' computational time), the blocks are ordered in descending order of
#' cost effectiveness.  If there is enough money then the first block is
#' repaired, then the second, and so on, skipping blocks if there are not
#' enough funds.  This continues until either there are no blocks left to
#' rebuild or not enough money to rebuild any remaining blocks.
#'
#' @param element.data An element-level data-frame.
#' @param block.data A block-level data-frame.
#' @param rebuild.money A number
#'
#' @return An element-level data frame with updated grade proportions for
#' rebuilt blocks.  Note that repair costs are not updated, nor the
#' block-level data frame holding block-level information so
#' \code{\link{UpdateElementRepairs}} and \code{\link{UpdateBlockRepairs}}
#' should be run afterwards.
#' @examples
#' # Rebuild blocks with budget of £1M
#' rebuilt_elements <- blockbuster2:::Rebuild(simulated_elements, simulated_blocks, 1e6)
#' # IMPORTANT - This does not update the repair costs so you need to pass
#' # the results to UpdateElementRepairs
#' rebuilt_elements <- blockbuster2:::UpdateElementRepairs(rebuilt_elements)
#' # Also update the block-level costs
#' rebuild_blocks <- blockbuster2:::UpdateBlockRepairs(simulated_blocks, rebuilt_elements)
Rebuild <- function(element.data, block.data, rebuild.money){

  if (!is.numeric(rebuild.money)) stop("The repair money must be a number.")
  if (rebuild.money < 0) stop("The repair money must be a positive number.")
  if (length(rebuild.money) > 1) warning("Only the first value in repair.money
                                         will be used.")

  # If there is no money then no need to rebuild.
  if (rebuild.money == 0) return(element.data)

  # identify blocks which need repairs and arrange in descending ratio order.
  # Then send relevant columns to the recursive function that outputs the
  # building ids for blocks to rebuild.
  # This will be used as an index when amending the element.data object.
  rebuild.order <- block.data %>%
    filter(block.rebuild.cost != 0) %>%
    arrange(desc(ratio))
  buildings <- RecursiveBudgeting(rebuild.order$block.rebuild.cost,
                                 rebuild.order$buildingid,
                                 rebuild.money)$state

  if(length(buildings) < 1) return(element.data)

  element.data <- RebuildBlock(element.data, buildings)

  return(element.data)
}

#' Rebuild all elements in the given blocks.
#'
#' This is a helper function used by \code{\link{Rebuild}}.  It sets grade A to
#' 1 for all blocks identified in the vector \code{buildingids} and sets all
#' grades to 0 while not changing the grades of blocks not in
#' \code{buildingids}.
#' @param element.data An element-level data frame.
#' @param buildingids A numeric vector identifying the \code{buildingid} keys of
#' the blocks to rebuild.
#'
#' @return An element-level data frame with updated grades.
#' @examples
#' # Rebuild the block with buildingid 11
#' blockbuster2:::RebuildBlock(simulated_elements, 11)
RebuildBlock <- function(element.data, buildingids){
  ind <- which(element.data$buildingid %in% buildingids)
  element.data$A[ind] <- 1
  element.data$B[ind] <- 0
  element.data$C[ind] <- 0
  element.data$D[ind] <- 0
  element.data$E[ind] <- 0
  return(element.data)
}

#' Find blocks to rebuild within budget recursively.
#'
#' An efficient recursive algorithm used by \code{\link{Rebuild}} that outputs
#' the \code{keys} corresponding to the unique combination of elements in the
#' ordered vector \code{vec} that sum to less than \code{budget}.
#'
#' The \code{vec} and \code{keys} arguments are split into two.  The sum of the
#' first half of \code{vec} is compared to the budget.  If it is less then all
#' keys are selected, otherwise, the argument is split in half again and checked,
#' and so on.  Afterwards, the second half is checked if there are still monies
#' to spend.
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
RecursiveBudgeting <- function(vec, keys, budget, state = NULL){
  # stopping rules
  if (length(vec) < 1) return (list(state = state, budget = budget))
  n <- ceiling(length(vec) / 2)

    # split in half, run on first half, then on second.

  # FIRST HALF
  # if cost < budget, return all keys, reduce budget
  cost <- sum(vec[1:n])
  if (cost <= budget){
    budget <- budget - cost
    state <- c(state, keys[1:n])
    if (budget == 0) return(list(state = state, budget = budget))
  } else {
    # if not enough to rebuild everything then invoke recursion
    if(length(vec) != n){
      res <- RecursiveBudgeting(vec[1:n], keys[1:n], budget, state)
      budget <- res$budget
      state  <- res$state
      if (budget == 0) return(list(state = state, budget = budget))
    }
  }

  # SECOND HALF
  # if cost < budget, return all keys, reduce budget
  cost <- sum(vec[-(1:n)])
  if (cost <= budget){
    budget <- budget - cost
    state <- c(state, keys[-(1:n)])
    if (budget == 0) return(list(state = state, budget = budget))
  } else {
    if(length(vec) != n){
      # if not enough to rebuild everything then invoke recursion
      res <- RecursiveBudgeting(vec[-(1:n)], keys[-(1:n)], budget, state)
      budget <- res$budget
      state  <- res$state
      if (budget == 0) return(list(state = state, budget = budget))
    }
  }
  return(list(state = state, budget = budget))
}
