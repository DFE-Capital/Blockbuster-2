#' Repair elements in a block
#'
#' Given the current state of the buildings, this function outputs the element-
#' level status after repairing components within budget.
#'
#' The available funding for repairs, input as \code{repair.money}, is split
#' evenly between blocks.  Within each block the components are repaired in
#' descending order of cost. Note that this method can result in a large amount
#' of wasted fund allocation.  For example, a block with only N or A grade
#' components will be allocated a proportion of the funds, but needs none.
#'
#' This function does NOT update the repair costs, only the grade proportions.
#' Therefore \code{\link{UpdateElementRepairs}} and
#' \code{\link{UpdateBlockRepairs}} should be called on the element- and block-
#' level objects afterwards to ensure the repair costs and totals are correct.
#'
#' @param element.data An \code{\link{element}} class object.
#' @param block.data A \code{\link{block}} class object.
#' @param repair.money A number
#'
#' @return An \code{\link{element}} class object with relevant components
#' repaired, i.e. set to grade A.
# RepairOld <- function(element.data, block.data, repair.money){
#
#   # Check integrity of inputs.
#   if (!is.element(element.data)) stop("The element.data argument needs to be an
#                                       element class data table.")
#   if (!is.block(block.data)) stop("The block.data argument needs to be a
#                                       block class data table.")
#   if (!is.numeric(repair.money)) stop("The repair money must be a number.")
#   if (length(repair.money) > 1) warning("Only the first value in repair.money
#                                         will be used.")
#
#   # If there is no money then no need to repair.
#   if (repair.money == 0) return(element.data)
#
#   number.of.blocks <- nrow(block.data)
#
#   # identify blocks with zero cost to speed things up
#   zero.cost.blocks <- block.data$buildingid[
#     (block.data$B.block.repair.cost + block.data$C.block.repair.cost +
#     block.data$D.block.repair.cost + block.data$E.block.repair.cost) == 0]
#
#   blocks.to.repair <- setdiff(block.data$buildingid, zero.cost.blocks)
#   number.of.blocks <- length(blocks.to.repair)
#
#   # To speed decision making, blocks where everything is in budget are repaired
#   # first.
#
#   # vector of buildingids where repairing everything is in budget with non-zero
#   # cost
#   repaired.blocks   <- block.data$buildingid[
#     (block.data$B.block.repair.cost + block.data$C.block.repair.cost +
#        block.data$D.block.repair.cost + block.data$E.block.repair.cost) <= (repair.money / number.of.blocks)
#     ]
#   repaired.blocks <- setdiff(repaired.blocks, zero.cost.blocks) # remove zeroes
#
#   # cost of repairing in-budget blocks
#   cost <- block.data %>%
#     filter(buildingid %in% repaired.blocks) %>% # use only block in budget
#     ungroup %>% # ungroup to avoid summarise problems if data is already grouped
#     summarise(cost = sum(B.block.repair.cost + C.block.repair.cost +
#                          D.block.repair.cost + E.block.repair.cost)) # total repair cost of blocks
#
#   # repair everything in blocks where it is within budget
#   in.budget.element.data <- element.data %>%
#     filter(buildingid %in% repaired.blocks) %>%
#     # the following sets all B,C,D grade proportions to 0 and adds them to A
#     mutate(A = A + B + C + D, B = 0, C = 0, D = 0, E = 0)
#
#   # adjust money available to other blocks
#   repair.money <- as.numeric(repair.money - cost)
#
#   blocks.to.repair <- setdiff(blocks.to.repair, repaired.blocks)
#   number.of.blocks <- length(blocks.to.repair)
#
#   # Other blocks are sent to RepairBlock to make decisions about what components
#   # to repair.
#
#   # repair in other blocks
#   out.of.budget.element.data <- element.data %>%
#     # filter out repaired blocks and zero cost
#     filter(!buildingid %in% repaired.blocks) %>%
#     filter(!buildingid %in% zero.cost.blocks)
#
#   # need to send separately to RepairBlock or lapply depending on number of
#   # blocks
#   if (number.of.blocks == 1){
#     out.of.budget.element.data <- out.of.budget.element.data %>%
#       ElementLevel %>%
#       RepairBlock(., repair.money)
#   } else {
#   out.of.budget.element.data <- out.of.budget.element.data %>%
#     split(.$buildingid) %>% # split by block
#     as.element.list %>% # set class
#     lapply(., RepairBlock, repair.money / number.of.blocks) # repair
#   }
#
#   # recombine the two repaired sections into one.
#   element.data <- bind_rows(in.budget.element.data, out.of.budget.element.data)
#   element.data <- ElementLevel(element.data) # set class
#   return(element.data) # Note that repair costs haven't been updated.
# }
#

#' Repair components in element data when there is not enough money to repair
#' them all.
#'
#' This function is called by \code{\link{Repair}} when it needs to make
#' decisions about what to spend money on repairing.  Despite the name, it can
#' be applied to element-level data that contains components from more than one
#' block. A greedy algorithm is applied to spend the available repair money.
#' Components are repaired in order of descending cost by grade, starting with
#' grade D, then C, then B.
#'
#' This function does NOT update the repair costs, only the grade proportions.
#' Therefore \code{\link{UpdateElementRepairs}} and
#' \code{\link{UpdateBlockRepairs}} should be called on the element- and block-
#' level objects afterwards to ensure the repair costs and totals are correct.
#'
#' @param element.data An \code{\link{element}} class object.
#' @param repair.money A number.
#'
#' @return An \code{\link{element}} class object with the grade proportions
#' amended accordingly.
# RepairBlock <- function(element.data, repair.money){
#
#
#   # input integrity checks
#   if (!is.element(element.data)) stop("The element.data argument needs to be an
#                                       element class data table.")
#   if (!is.numeric(repair.money)) stop("The repair money must be a number.")
#   if (length(repair.money) > 1) warning("Only the first value in repair.money
#                                         will be used.")
#
#   # If there is no money then no need to repair
#   if(repair.money == 0) return(element.data)
#
#   funds <- repair.money
#
#   # repair D grade first
#
#   # order elements according to D grade repair cost
#   element.data <- element.data %>%
#     arrange(desc(D.repair.total))
#   ind <- which(element.data$D.repair.total > 0)
#
#   if (length(ind) > 0){ #repair grade D only if there is something to repair
#
#     cost <- element.data[ind, ]$D.repair.total
#     min.cost <- min(cost)
#     i <- 1
#
#     # A while loop is used instead of a for loop to save time when we cannot
#     # afford to repair anything else
#     while (funds > min(cost)){
#
#       if(funds >= cost[i]){ # if we can afford to repair...
#
#         # add D grade to A grade
#         element.data[ind[i], "A"] <- element.data[ind[i], "A"] +
#           element.data[ind[i], "D"]
#         # set D grade to zero
#         element.data[ind[i], "D"] <- 0
#
#         # update funds
#         funds <- funds - element.data[ind[i], "D.repair.total"]
#       }
#       # move onto the next row
#       ifelse(i == length(cost), break, i <- i + 1)
#     }
#   }
#
#   # repair C grade
#
#   # order elements according to C grade repair cost
#   element.data <- element.data %>%
#     arrange(desc(C.repair.total))
#   ind <- which(element.data$C.repair.total > 0)
#
#   if (length(ind) > 0){ #repair grade C only if there is something to repair
#
#     cost <- element.data[ind, ]$C.repair.total
#     min.cost <- min(cost)
#     i <- 1
#
#     # A while loop is used instead of a for loop to save time when we cannot
#     # afford to repair anything else
#     while (funds > min(cost)){
#
#       if(funds >= cost[i]){ # if we can afford to repair...
#
#         # add C grade to A grade
#         element.data[ind[i], "A"] <- element.data[ind[i], "A"] +
#           element.data[ind[i], "C"]
#         # set C grade to zero
#         element.data[ind[i], "C"] <- 0
#
#         # update funds
#         funds <- funds - element.data[ind[i], "C.repair.total"]
#       }
#       # move onto the next row
#       ifelse(i == length(cost), break, i <- i + 1)
#     }
#
#   }
#
#   # repair B grade
#
#   # order elements according to B grade repair cost
#   element.data <- element.data %>%
#     arrange(desc(B.repair.total))
#   ind <- which(element.data$B.repair.total > 0)
#
#   if (length(ind) > 0){ #repair grade B only if there is something to repair
#
#     cost <- element.data[ind, ]$B.repair.total
#     min.cost <- min(cost)
#     i <- 1
#
#     # A while loop is used instead of a for loop to save time when we cannot
#     # afford to repair anything else
#     while (funds > min(cost)){
#
#       if(funds >= cost[i]){ # if we can afford to repair...
#
#         # add B grade to A grade
#         element.data[ind[i], "A"] <- element.data[ind[i], "A"] +
#                                               element.data[ind[i], "B"]
#         # set B grade to zero
#         element.data[ind[i], "B"] <- 0
#
#         # update funds
#         funds <- funds - element.data[ind[i], "B.repair.total"]
#       }
#       # move onto the next row
#       ifelse(i == length(cost), break, i <- i + 1)
#     }
#
#   }
#   element.data <- ElementLevel(element.data) # set class
#   return(element.data)
# }

#' Repair elements in a block
#'
#' Given the current state of the buildings, this function outputs the element-
#' level status after repairing components within budget.
#'
#' The repairing gives priority to repairing those components at greatest risk
#' of being grade D, then grade C, then grade B. It calls the same
#' \code{\link{RecursiveBudgeting}} recursive algorithm used by
#' \code{\link{Rebuild()}}function to efficiently identify the most at risk
#' components within budget.
#'
#' The argument \code{block.data} is deprecated and not used in this function.
#' It is left in for modularity purposes as at a later data it may be that
#' several repair decision algorithms are selectable and some may require
#' the block-level information.
#'
#' This function does NOT update the repair costs, only the grade proportions.
#' Therefore \code{\link{UpdateElementRepairs}} and
#' \code{\link{UpdateBlockRepairs}} should be called on the element- and block-
#' level objects afterwards to ensure the repair costs and totals are correct.
#'
#' @param element.data An \code{\link{element}} class object.
#' @param repair.money A number.
#' @param grade.order (Optional) A vector of character strings from \code{"E"}, \code{"D"},
#'  \code{"C"} and \code{"B"} in any order.  This determines the priority for
#'  repairs.  The first character gives the first grade that will be repaired.
#'  By default, E is repaired first, then D, then C, then B.
#'
#' @return An \code{\link{element}} class object with the grade proportions
#' amended accordingly.
Repair <- function(element.data, repair.money,
                   grade.order = c("E", "D", "C", "B")){

  # Check integrity of inputs.
  #if (!is.element(element.data)) stop("The element.data argument needs to be an
  #                                    element class data table.")
  #if (!is.block(block.data)) stop("The block.data argument needs to be a
  #                                block class data table.")
  if (!is.numeric(repair.money)) stop("The repair money must be a number.")
  if (length(repair.money) > 1) warning("Only the first value in repair.money
                                        will be used.")
  # If there is no money then no need to repair.
  if (repair.money <= 0) return(element.data)

  attr(element.data, "repair_money") <- repair.money

  for(i in 1:length(grade.order)){

    element.data <- repairGrade(element.data, grade = grade.order[i])

  }

 # element.data <- ElementLevel(element.data) # set class
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

