
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
  if(!is.element(element.data)) stop("Argument needs to be an element object.")

  # update element.data with the new repair totals.
  element.data <- element.data %>%
    mutate(B.repair.total = unit_area * B * B.repair.cost,
           C.repair.total = unit_area * C * C.repair.cost,
           D.repair.total = unit_area * D * D.repair.cost,
           E.repair.total = unit_area * E * E.repair.cost)

  return(ElementLevel(element.data))
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
  if(!is.block(block.data)) stop("block.data argument needs to be a block object.")
  if(!is.element(element.data)) stop("element.data argument needs to be an element object.")

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
           # B repairs are not considered as they are not urgent.
           ratio = case_when(block.rebuild.cost == 0 ~ 0,
                             TRUE ~ (C + D + E) / block.rebuild.cost)) %>%
    select(-B, -C, -D, -E)

  return(BlockLevel(block.data))
}

#' A lookup table for repair cost rate given a building component and its grade.
#'
#' An internal function in \code{\link{blockbuster}} to select the correct
#' repair cost constant for a building component and its condition grade.
#'
#' The default is to use the repair cost estimates for the PDS plus
#' an E grade cost estimator based on D grade cost plus 5% (removal costs etc.). This
#' is likely to be a source of error as the replacement of a decommisioned component
#' depends on the component as well as other complexities. This should be addressed in later
#' improvements of the \code{\link{blockbuster}}.
#'
#' @param the_elementid the building component unique identifier found in the PDS.
#' @param the_grade the grade of a blockbuster tibble single row (a factor with 6 levels).
#' @param costs_lookup the relevant costs look up table, default is derived from PDS 2016 costs
#' @return a numeric constant for the repair cost of building component by grade, in pounds per unit_area.
#' @seealso \code{\link{blockbuster_pds_repair_costs}}
#' @export
#' @examples
#' x <- blockcoster_lookup(
#' blockbuster_pds[1:10, ]$elementid,
#'  blockbuster_pds[1:10, ]$grade)

blockcoster_lookup <- function(
  the_elementid, the_grade, costs_lookup = blockbuster_pds_repair_costs
) {
  output <- filter(costs_lookup,
                   elementid == the_elementid & grade == the_grade)$repair_cost
  if (length(output) <1 ){
    warning("No repair cost has been found. Please double check the elementid
            and grade are correct.")
    output <- 0
  }

  return (output)
}
