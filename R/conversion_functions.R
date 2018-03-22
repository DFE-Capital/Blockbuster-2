# functions to convert blockbuster_pds into the two formats of table the new
# functions need.  These functions are not generally needed so I'll leave them
# undocumented/checked.


#' Convert a block_tibble into a row per component.
#'
#' @param block_tibble
#'
#' @return A tibble with one row per component, and the probability
#'  for each grade (A<B<C<D) as variables
ConvertOutput <- function(block_tibble){

  # change all zero unit_area to one.
  # The intention is to preserve repair costs and avoid divide by 0 issues or
  # strange decision making
  output <- block_tibble %>%
    mutate(unit_area = ifelse(unit_area == 0, 1, unit_area))

  # put area of each grade in the appropriate variable
  output <- output %>%
    dplyr::mutate(., A = ifelse(grade == "A", unit_area, 0)) %>%
    dplyr::mutate(., B = ifelse(grade == "B", unit_area, 0)) %>%
    dplyr::mutate(., C = ifelse(grade == "C", unit_area, 0)) %>%
    dplyr::mutate(., D = ifelse(grade == "D", unit_area, 0)) %>%
    dplyr::group_by(., buildingid, elementid) %>%
    dplyr::summarise_at(., c("unit_area", "A", "B", "C", "D"), sum)

  # convert to proportions
  output$A <- output$A / output$unit_area
  output$B <- output$B / output$unit_area
  output$C <- output$C / output$unit_area
  output$D <- output$D / output$unit_area
  # the following was to avoid divide by zero NAs, but there should no longer
  # be zero areas so I have commented it out.
  #output$A[is.na(output$A)] <- 0
  #output$B[is.na(output$B)] <- 0
  #output$C[is.na(output$C)] <- 0
  #output$D[is.na(output$D)] <- 0
  return(output)
}

#' Convert blockbuster_pds_data sample into element level wide format
#'
#' @param pds A slice from blockbuster_pds_data (or equivalent)
#' @param deterioration.rates blockbuster_det_data (or equivalent)
#'
#' @description Deterioration rates are pulled from the default blockbuster_det_data file
#' and are used to populate the appropriate columns of the output.
#'
#' Columns are: TODO
#'
#' @return A data.table with the appropriate rows for use by blockbuster v2
ConvertPdsToWide <- function(pds, deterioration.rates = blockbuster_det_data){
  element.data <- pds %>%
    ConvertOutput %>% # Convert to wide format with proportions at each grade
    ungroup()
  # pull deterioration rates
  det.data <- deterioration.rates %>% select(., elementid, ab, bc, cd)
  # append deterioration rates to wide format
  element.data <- left_join(element.data, det.data, by = "elementid")
  # append costs to wide format
  element.data <- element.data %>%
    mutate(B.repair.cost = sapply(elementid, blockcoster_lookup, the_grade = "B"),
           C.repair.cost = sapply(elementid, blockcoster_lookup, the_grade = "C"),
           D.repair.cost = sapply(elementid, blockcoster_lookup, the_grade = "D"),
           B.repair.total = B.repair.cost * B * unit_area,
           C.repair.total = C.repair.cost * C * unit_area,
           D.repair.total = D.repair.cost * D * unit_area
           )
}

# Deprecated ConvertPdsToBlock since it includes B grades in repair costs.  I
# only want to consider C,D costs when rebuilding.
#' Convert blockbuster_pds_data sample into block level wide format
#'
#' @param pds blockbuster_det_data (or equivalent)
#' @param block.rebuild.cost A number
#' @details block.rebuild.cost is used to compute the estimated cost of rebuilding
#' each building as block.rebuild.cost x gifa (Gross Internal Floor Area).
#'
#' @return A block level wide format table for use with blockbuster v2
#ConvertPdsToBlockOld <- function(pds, block.rebuild.cost){
#  pds %>%
#    group_by(., buildingid) %>%
#    summarise(., gifa = max(gifa), block.repair.cost = sum(cost)) %>%
#    mutate(., block.rebuild.cost = block.rebuild.cost * gifa,
#           ratio = ifelse(block.rebuild.cost == 0, 0, block.repair.cost / block.rebuild.cost))
#}



# NEW VERSION
# Deprecated ConvertPdsToBlock since it includes B grades in repair costs.  I
# only want to consider C,D,E costs when making a decision on rebuilding.
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
              ratio = (B.block.repair.cost + C.block.repair.cost + D.block.repair.cost + E.block.repair.cost) / block.rebuild.cost) %>%
    BlockLevel
}

