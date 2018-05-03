
#' Apply inflation to the block rebuild costs in the block level data
#'
#' An inflation rate of 4.5% should be entered as 1.045.
#'
#' @param block.data A \code{\link{block}} class object.
#' @param inflation A number.
#'
#' @return A \code{\link{block}} class object with the block rebuild costs
#' inflated by the appropriate amount.
InflateRebuild <- function(block.data, inflation){

  # Check integrity of input.
  #if (!is.block(block.data)) stop("Inflating total block rebuild costs can
  #                                only be done on block level data.")
  if (!is.numeric(inflation)) stop("Inflation must be a number.")
  if(inflation > 2 | inflation < 0.5) warning(paste("Inflation is usually expected to
                                                    lie in a small interval around 1.
                                                    You have entered ", inflation,
                                                    ". Are you sure this is correct?",
                                                    sep=""))

  # new rebuild cost is the old rebuild cost multiplied by inflation
  block.data %>%
    mutate(block.rebuild.cost = block.rebuild.cost * inflation)# %>%
    #BlockLevel # set class
}

#' Apply inflation to the block repair costs in the block level data
#'
#' An inflation rate of 4.5% should be entered as 1.045.
#'
#' @param block.data A \code{\link{block}} class object.
#' @param inflation A number.
#'
#' @return A \code{\link{block}} class object with the block repair costs
#' inflated by the appropriate amount.
InflateRepairBlock <- function(block.data, inflation){

  # Check integrity of input
  #if (!is.block(block.data)) stop("Inflating block repair costs can
  #                                only be done on block level data.")
  if (!is.numeric(inflation)) stop("Inflation must be a number.")
  if(inflation > 2 | inflation < 0.5) warning(paste("Inflation is usually expected to
                                                    lie in a small interval around 1.
                                                    You have entered ", inflation,
                                                    ". Are you sure this is correct?",
                                                    sep=""))


  # Inflate all repair costs
  block.data %>%
    mutate(C.block.repair.cost = C.block.repair.cost * inflation,
           D.block.repair.cost = D.block.repair.cost * inflation,
           B.block.repair.cost = B.block.repair.cost * inflation,
           E.block.repair.cost = E.block.repair.cost * inflation)# %>%
   # BlockLevel # set class
}

#' Apply inflation to the element repair costs in the element level data
#'
#' An inflation rate of 4.5% should be entered as 1.045.
#'
#' @param element.data An \code{\link{element}} class object.
#' @param inflation A number.
#'
#' @return An \code{\link{element}} class object with the repair costs and
#' totals inflated by the appropriate amount.
InflateRepairElement <- function(element.data, inflation){

  # Check integrity of input
  #if (!is.element(element.data)) stop("Inflating component repair costs can only
  #                                    be done on element level data.")
  if (!is.numeric(inflation)) stop("Inflation must be a number.")
  if(inflation > 2 | inflation < 0.5) warning(paste("Inflation is usually expected to
                                                    lie in a small interval around 1.
                                                    You have entered ", inflation,
                                                    ". Are you sure this is correct?",
                                                    sep=""))

  # Inflate the repair costs and totals.
  element.data %>%
    mutate(B.repair.cost = B.repair.cost * inflation,
           C.repair.cost = C.repair.cost * inflation,
           D.repair.cost = D.repair.cost * inflation,
           E.repair.cost = E.repair.cost * inflation,
           B.repair.total = B.repair.total * inflation,
           C.repair.total = C.repair.total * inflation,
           D.repair.total = D.repair.total * inflation,
           E.repair.total = E.repair.total * inflation) #%>%
    #ElementLevel # set class
}

