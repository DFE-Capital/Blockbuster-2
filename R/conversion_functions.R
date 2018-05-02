#' Convert blockbuster_pds_data sample into block level wide format
#'
#' @param pds blockbuster_det_data (or equivalent)
#' @param block.rebuild.cost A number
#' @details block.rebuild.cost is used to compute the estimated cost of rebuilding
#' each building as block.rebuild.cost x gifa (Gross Internal Floor Area).
#'
#' @return A block level wide format table for use with blockbuster v2
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

