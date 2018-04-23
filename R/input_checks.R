input_checks <- function(element.data = NULL,
                         block.data = NULL,
                         forecast.horizon = NULL,
                         rebuild.money = NULL,
                         repair.money = NULL,
                         block.rebuild.cost = NULL,
                         inflation = NULL
                         ){

#    if(!is.element(element.data)) stop("The element.data argument must be an element
 #                                    object.")

  if(length(block.data) < 1){
    message("Constructing block summary from element.data.")
    block.data <- ConvertPdsToBlock(element.data, block.rebuild.cost)
  }
#  if(!is.block(block.data)) stop("The block.data argument must be a block
#                                 object.")
  if(!is.numeric(forecast.horizon)) stop("The forecast horizon must be number.")
  if(forecast.horizon < 1) stop("The forecast horizon must be a positive
                                number.")

  if(!is.numeric(rebuild.money)) stop ("The rebuild.money argument must be
                                       numeric.")
  if(length(rebuild.money) == 1 & forecast.horizon != 1){
    warning("You have only provided a single value for rebuild.money. It will be
            used as the available funds for each forecast timestep.")
    rebuild.money <- rep(rebuild.money, forecast.horizon)
  }
  if(length(rebuild.money) < forecast.horizon){
    stop("The length of the vector passed to rebuild.money must match the
         number passed to forecast.horizon.")
  }
  if(length(rebuild.money) > forecast.horizon) {
    warning("You have provided rebuild money for years outside the forecast
            horizon.  The extra values will be ignored.")
  }
  if(any(rebuild.money < 0)) stop("You have supplied negative values for the
                                  rebuild budget.")

  if(!is.numeric(repair.money)) stop ("The repair.money argument must be
                                      numeric.")
  if(length(repair.money) == 1 & forecast.horizon != 1){
    warning("You have only provided a single value for repair.money. It will be
            used as the available funds for each forecast timestep.")
    repair.money <- rep(repair.money, forecast.horizon)
  }
  if(length(repair.money) < forecast.horizon){
    stop("The length of the vector passed to repair.money must match the
         number passed to forecast.horizon.")
  }
  if(length(repair.money) > forecast.horizon) {
    warning("You have provided repair money for years outside the forecast
            horizon.  The extra values will be ignored.")
  }
  if(any(repair.money < 0)) stop("You have supplied negative values for the
                                 repair budget.")
  if(!is.numeric(block.rebuild.cost)) stop("block.rebuild.cost must be a number.")
  if(any(block.rebuild.cost < 0)) stop("You have supplied negative values for the
                                       unit block rebuild cost.")
  if(length(block.rebuild.cost) > 1) warning("Only the first value entered for
                                             block.rebuild.cost will be used.")
  if(!is.numeric(inflation)) stop("Inflation should be numeric.")
  if(length(inflation) == 1 & forecast.horizon != 1){
    inflation <- rep(inflation, forecast.horizon)
    warning("Inflation will be applied as a constant rate each timestep")
  }
  if(length(inflation) < forecast.horizon){
    stop("The inflation argument should be a single number or a vector with a
         value for each timestep.")
  }
  if(length(inflation) > forecast.horizon) {
    warning("You have provided inflation rates for years outside the forecast
            horizon.  The extra values will be ignored.")
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

