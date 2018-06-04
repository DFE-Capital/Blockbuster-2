#' Update the repair costs for components in an element-level data frame.
#'
#' Computes the total cost of repairing each component at each grade using the
#' formula unit area * unit repair cost * proportion at grade.
#'
#' @param element.data An element-level data frame.
#'
#' @return An element-level data frame with updated repair totals.
#'
#' @examples
#' blockbuster2:::UpdateElementRepairs(simulated_elements)
UpdateElementRepairs <- function(element.data){

  # update element.data with the new repair totals.
  element.data <- element.data %>%
    mutate(B.repair.total = unit_area * B * B.repair.cost,
           C.repair.total = unit_area * C * C.repair.cost,
           D.repair.total = unit_area * D * D.repair.cost,
           E.repair.total = unit_area * E * E.repair.cost)

  return(element.data)
}


#' Updates the repair costs of each grade in a block-level data frame
#'
#' Computes the total repair costs at each grade for all blocks by summing the
#' totals from the related element-level data frame.  It is good practise to
#' run \code{\link{UpdateElementRepairs}} on the element-level data frame first
#' to avoid incorrect repair totals.  When computing the repair/rebuild ratio
#' used to determine which blocks are rebuilt first, B grade repair costs are
#' ignored so as to prioritise poor condition blocks.
#'
#' @param block.data A block-level data frame.
#' @param element.data The element_level data frame containing the component
#' information of the blocks.
#'
#' @return A block-level data frame with updated repair costs and
#' repair/rebuild ratio
#' @examples
#' blockbuster2:::UpdateBlockRepairs(simulated_blocks, simulated_elements)
UpdateBlockRepairs <- function(block.data, element.data){

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
                             TRUE ~ (C + D + E) / block.rebuild.cost)) %>%
    select(-B, -C, -D, -E)

  return(block.data)
}
