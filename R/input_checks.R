#' Check input to Blockbuster function is valid.
#'
#' This function contains all the input validity checks used by the
#' \code{\link{Blockbuster}} function.  It produces messages and warnings about
#' common scenarios, and errors for invalid data inputs. Currently this function
#' will only report the first error that occurs.
#'
#' @param element.data An element-level data frame.
#' @param block.data A block-level data frame.
#' @param forecast.horizon An integer.
#' @param rebuild.money A numeric vector
#' @param repair.money A numeric vector
#' @param block.rebuild.cost A number
#' @param inflation A numeric vector
#'
#' @details If \code{block.data} is not provided, this function will generate
#' the block-level data.frame.
#'
#' \code{forecast.horizon} must be a single positive number.
#'
#' \code{inflation}, \code{rebuild.money} and \code{repair.money} must be
#' numeric.  If either is  a single number then a message is displayed stating
#' that it will be used for each year. If vectors are passed to these arguments,
#' their length must be equal or greater than \code{forecast.horizon}, with the
#' additional values being ignored and a message displayed.  Negative values
#' will throw an error. A message is displayed if \code{inflation} seems
#' excessive (over 200%).
#'
#' \code{block.rebuild.cost} is checked to be a positive number.  If a vector is
#' passed, the values beyond the first will be ignored and a message displayed.
#'
#' @return The input is returned as is, with the addition of a block-level data
#' frame if \code{block.data = NULL}.
#' @examples
#' # run input checks on a 10 year simulation with no rebuilding, £1M repair
#' # budget each year with £2,000 unit rebuild cost and no inflation. The block-
#' # level data frame is not supplied and will be generated.
#' blockbuster2:::input_checks(element.data = simulated_elements,
#'                block.data = NULL,
#'                forecast.horizon = 10,
#'                rebuild.money = 0,
#'                repair.money = 1000000,
#'                block.rebuild.cost = 2000,
#'                inflation = 1)
input_checks <- function(element.data = NULL,
                         block.data = NULL,
                         forecast.horizon = NULL,
                         rebuild.money = NULL,
                         repair.money = NULL,
                         block.rebuild.cost = NULL,
                         inflation = NULL
                         ){

  if(length(block.data) < 1){
    message("Constructing block summary from element.data.")
    block.data <- ConvertPdsToBlock(element.data, block.rebuild.cost)
  }

  if(length(forecast.horizon) > 1 | !is.numeric(forecast.horizon)){
    stop("The forecast horizon must be a single number.")}
  if(forecast.horizon < 1){
    stop("The forecast horizon must be a positive number.")
  }

  if(!is.numeric(rebuild.money)){
    stop ("The rebuild.money argument must be numeric.")
  }
  if(length(rebuild.money) == 1 & forecast.horizon != 1){
    warning("You have only provided a single value for rebuild.money. It will be
            used as the available funds for each forecast timestep.")
    rebuild.money <- rep(rebuild.money, forecast.horizon)
  }
  if(length(rebuild.money) < forecast.horizon){
    stop("The length of the vector passed to rebuild.money must match the number
         passed to forecast.horizon.")
  }
  if(length(rebuild.money) > forecast.horizon) {
    warning("You have provided rebuild money for years outside the forecast
            horizon.  The extra values will be ignored.")
  }
  if(any(rebuild.money < 0)){
    stop("You have supplied negative values for the rebuild budget.")
  }
  if(all(rebuild.money == 0)){
    message("There will be no rebuilding as rebuild.money is zero for all years.")
  }

  if(!is.numeric(repair.money)){
    stop ("The repair.money argument must be numeric.")
  }
  if(length(repair.money) == 1 & forecast.horizon != 1){
    warning("You have only provided a single value for repair.money. It will be
            used as the available funds for each forecast timestep.")
    repair.money <- rep(repair.money, forecast.horizon)
  }
  if(length(repair.money) < forecast.horizon){
    stop("The length of the vector passed to repair.money must match the number
         passed to forecast.horizon.")
  }
  if(length(repair.money) > forecast.horizon) {
    warning("You have provided repair money for years outside the forecast
            horizon.  The extra values will be ignored.")
  }
  if(any(repair.money < 0)){
    stop("You have supplied negative values for the repair budget.")
  }
  if(all(repair.money == 0)){
    message("There will be no repairing as repair.money is zero for all years.")
  }
  if(!is.numeric(block.rebuild.cost)){
    stop("The unit block rebuild cost must be a positive number.")
  }
  if(any(block.rebuild.cost <= 0)){
    stop("The unit block rebuild cost must be a positive number.")
  }
  if(length(block.rebuild.cost) > 1) {
    warning("Only the first value entered for block.rebuild.cost will be used.")
  }
  if(!is.numeric(inflation)) {
    stop("The inflation argument should be a single number or a vector with a
         value for each timestep.")
  }
  if(length(inflation) == 1 & forecast.horizon != 1){
    inflation <- rep(inflation, forecast.horizon)
    message("Inflation will be applied as a constant rate each timestep")
  }
  if(length(inflation) < forecast.horizon){
    stop("The inflation argument should be a single number or a vector with a
         value for each timestep.")
  }
  if(length(inflation) > forecast.horizon) {
    warning("You have provided inflation rates for years outside the forecast
            horizon.  The extra values will be ignored.")
  }

  if(any(inflation <= 0)) stop("All inflation values should be positive")
  if(any(inflation > 2)){
    message("For at least one year, inflation is set above 200%. Are you sure
            this is right.  A value of 1 represents 100% inflation, i.e. no
            change.")
  }

  return(list(element.data = element.data,
              block.data = block.data,
              forecast.horizon = forecast.horizon,
              rebuild.money = rebuild.money,
              repair.money = repair.money,
              block.rebuild.cost = block.rebuild.cost,
              inflation = inflation)
  )
  }

