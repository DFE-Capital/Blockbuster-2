#' Pull the backlog column for a given grade from block data
#'
#' @param block A \code{\link{block}} class object.
#' @param grade Character.  Either \code{"B"}, \code{"C"}, \code{"D"}, or
#'  \code{"E"}. Anything else will return an error.
#'
#' @return A vector of the repair costs for the appropriate grade.
single_backlog <- function(block, grade){
  if(grade %in% c("B", "C", "D", "E")){
    return (eval(parse(text = paste(grade, ".block.repair.cost", sep = "")), block))
  } else {
    stop(paste("When pulling the backlog from block data, the grade must be either 'B', 'C', 'D' or 'E'. single_backlog() was supplied with", grade))
  }
}

#' Sums the backlog in block data
#'
#' @param block A \code{\link{block}} class object.
#' @param grade Character vector. Elements in the vector must be either
#'  \code{"B"}, \code{"C"}, \code{"D"}, or \code{"E"}. Anything else will return
#'   an error. Set as \code{c("B", "C", "D", "E")} by default.
#'
#' @return Numeric. The sum of the repair costs for all grades specified in
#'  \code{grade}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Total grade B backlog
#' backlog(block_data, "B")
#' # Total grade B and C backlog
#' backlog(block_data, c("B", "C"))
#' # All backlog
#' backlog(block_data)
#' }
backlog <- function(block, grade = c("B", "C", "D", "E")){
  sum(sapply(unique(grade), single_backlog, block = block))
}
