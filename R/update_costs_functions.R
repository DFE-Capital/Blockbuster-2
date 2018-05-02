
#' Update the repair costs for components in an element object.
#'
#' Computes the total cost of repairing each component at each grade using the
#' formula unit area * unit repair cost * proportion at grade.
#'
#' @param element.data An \code{\link{element}} class object.
#'
#' @return An \code{\link{element}} object with updated repair totals.
UpdateElementRepairs <- function(element.data){

  # input integrity
  #if(!is.element(element.data)) stop("Argument needs to be an element object.")

  # update element.data with the new repair totals.
  element.data <- element.data %>%
    mutate(B.repair.total = unit_area * B * B.repair.cost,
           C.repair.total = unit_area * C * C.repair.cost,
           D.repair.total = unit_area * D * D.repair.cost,
           E.repair.total = unit_area * E * E.repair.cost)

  return(element.data)
}


#' Updates the repair costs of each grade in a block object
#'
#' Computes the total repair costs at each grade for all blocks by summing the
#' totals from the related \code{\link{element}} object.  It is good practise to
#' run \code{\link{UpdateElementRepairs}} on the \code{\link{element}} first to
#' avoid incorrect repair totals.  When computing the repair/rebuild ratio used
#' to determine which blocks are rebuilt first, B grade repair costs are ignored
#' so as to prioritise poor condition blocks.
#'
#' @param block.data A \code{\link{block}} object.
#' @param element.data The \code{\link{element}} object containing the component
#' information of the blocks.
#'
#' @return A \code{\link{block}} object with updated repair costs and
#' repair/rebuild ratio
UpdateBlockRepairs <- function(block.data, element.data){

  # Input integrity
  #if(!is.block(block.data)) stop("block.data argument needs to be a block object.")
  #if(!is.element(element.data)) stop("element.data argument needs to be an element object.")

  # total repair costs for each building at each grade.
  repairs <- element.data %>%
    group_by(buildingid) %>%
    summarise(B = sum(B.repair.total),
              C = sum(C.repair.total),
              D = sum(D.repair.total),
              E = sum(E.repair.total))
  # load total repair costs into block.data and compute repair/rebuild ratios.
  block.data <- block.data %>%
    left_join(., repairs, by = "buildingid") %>%
    mutate(B.block.repair.cost = B,
           C.block.repair.cost = C,
           D.block.repair.cost = D,
           E.block.repair.cost = E,
           ratio = case_when(block.rebuild.cost == 0 ~ 0,
                             TRUE ~ (B + C + D + E) / block.rebuild.cost)) %>%
    select(-B, -C, -D, -E)
  #block.data <- BlockLevel(block.data)
  return(block.data)
}
