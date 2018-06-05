#' Convert blockbuster_pds_data sample into block level wide format
#'
#' Provide a block-level summary of an element-level data frame.  Repair costs
#' at each grade are summed and the cost-effectiveness ratio (repair/rebuild) is
#' computed.
#' @param pds A data frame containing the element-level data.
#' @param block.rebuild.cost The unit cost to rebuild 1 sq.m.
#' @details block.rebuild.cost is used to compute the estimated cost of
#' rebuilding.  The formula used is block.rebuild.cost x gifa (Gross Internal
#' Floor Area).
#'
#' @return A block level wide format table for use with blockbuster v2
#' @examples
#' # Summarise the element-level PDS.data at block level with a £5,000 unit
#' # rebuild cost.
#' blockbuster2:::ConvertPdsToBlock(simulated_elements, 5000)
ConvertPdsToBlock <- function(pds, block.rebuild.cost){
  pds %>%
    group_by(buildingid) %>%
    summarise(block.rebuild.cost = max(block.rebuild.cost * gifa),
              B.block.repair.cost = sum(B.repair.total),
              C.block.repair.cost = sum(C.repair.total),
              D.block.repair.cost = sum(D.repair.total),
              E.block.repair.cost = sum(E.repair.total),
              ratio = (B.block.repair.cost + C.block.repair.cost + D.block.repair.cost + E.block.repair.cost) / block.rebuild.cost)
  }

