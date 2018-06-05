
#' Apply inflation to the block rebuild costs in the block level data
#'
#' @param block.data A block-level data frame.
#' @param inflation A number. An inflation rate of 4.5% should be entered as
#' 1.045.
#'
#' @return A block-level data frame with the block rebuild costs inflated by the
#' appropriate amount.
#' @examples
#' # Apply 3% inflation to rebuild costs in block-level data
#' blockbuster2:::InflateRebuild(simulated_blocks, 1.03)
InflateRebuild <- function(block.data, inflation){

  if (!is.numeric(inflation)) stop("Inflation must be a number.")
  if(inflation > 2 | inflation < 0.5) {
    warning(
      paste("Inflation is usually expected to lie in a small interval around 1.
            You have entered ", inflation, ". Are you sure this is correct?",
            sep=""))}

  # new rebuild cost is the old rebuild cost multiplied by inflation
  block.data %>%
    mutate(block.rebuild.cost = block.rebuild.cost * inflation)
}

#' Apply inflation to the block repair costs in the block level data
#'
#' @param block.data A block-level data frame.
#' @param inflation A number. An inflation rate of 4.5% should be entered as
#' 1.045.
#'
#' @return A block-level data frame with the block repair costs inflated by the
#' appropriate amount.
#' @examples
#' # Apply 3% inflation to block-level repair costs
#' blockbuster2:::InflateRepairBlock(simulated_blocks, 1.03)
InflateRepairBlock <- function(block.data, inflation){

  if (!is.numeric(inflation)) stop("Inflation must be a number.")
  if(inflation > 2 | inflation < 0.5) {
    warning(
      paste("Inflation is usually expected to   lie in a small interval around
            1. You have entered ", inflation, ". Are you sure this is correct?",
            sep=""))}

  # Inflate all repair costs
  block.data %>%
    mutate(C.block.repair.cost = C.block.repair.cost * inflation,
           D.block.repair.cost = D.block.repair.cost * inflation,
           B.block.repair.cost = B.block.repair.cost * inflation,
           E.block.repair.cost = E.block.repair.cost * inflation)
}

#' Apply inflation to the element repair costs in the element level data
#'
#' @param element.data An element-level data frame.
#' @param inflation A number. An inflation rate of 4.5% should be entered as
#' 1.045.
#'
#' @return An element-level data frame with the repair costs and totals inflated
#' by the appropriate amount.
#' @examples
#' # Apply 3% inflation to the element-level repair costs and totals.
#' blockbuster2:::InflateRepairElement(simulated_elements, 1.03)
InflateRepairElement <- function(element.data, inflation){

  if (!is.numeric(inflation)) stop("Inflation must be a number.")
  if(inflation > 2 | inflation < 0.5){
    warning(
      paste("Inflation is usually expected to lie in a small interval around 1.
            You have entered ", inflation, ". Are you sure this is correct?",
            sep=""))}

  # Inflate the repair costs and totals.
  element.data %>%
    mutate(B.repair.cost = B.repair.cost * inflation,
           C.repair.cost = C.repair.cost * inflation,
           D.repair.cost = D.repair.cost * inflation,
           E.repair.cost = E.repair.cost * inflation,
           B.repair.total = B.repair.total * inflation,
           C.repair.total = C.repair.total * inflation,
           D.repair.total = D.repair.total * inflation,
           E.repair.total = E.repair.total * inflation)
}

