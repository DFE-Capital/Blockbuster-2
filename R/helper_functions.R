#' Find the most appropriate format for the axis of a backlog plot
#'
#' @param backlog A \code{\link{backlog}} class object.
#'
#' @return A list containing the magnitude by which to divide the currency,
#' and the suffix for the plot axis.
CurrencyFormat <- function(backlog){
  magnitude <- backlog %>%
    .$backlog %>% # select values
    log10 %>% # find magnitude
    max(na.rm=TRUE) %>% # maximum magnitude
    `/`(3) %>% # name level
    floor #round it off
  levels <- c("", "K", "M", "B", "Tr")
  return(list(magnitude = magnitude * 3, level = levels[magnitude + 1]))
}


pounds <- function(money){
  mag <- (log10(money) / 3) %>% floor
  paste(round(money / 10 ^ (mag * 3), 1), c("", "K", "M", "B", "Tr")[mag + 1], sep = "")
}

percent <- function(percent){
  paste(round(percent, 1), "%", sep = "")
}
