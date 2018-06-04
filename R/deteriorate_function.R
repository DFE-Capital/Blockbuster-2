#' Deteriorate all the elements in an element-level data frame.
#'
#' Takes an element-level data frame and amends the grade proportions for each
#' row according to the deterioration rate in each row. This function is used by
#' the \code{\link{Blockbuster}} function to compute the deterioration at each
#' timestep.
#'
#' The deterioration rates are based on lifetime estimates from EC Harris and
#' other expert opinion.
#'
#' @param element.data An element-level data frame.
#'
#' @return An element-level data frame.
#' @examples
#' blockbuster2:::Deteriorate(simulated_elements)
Deteriorate <- function(element.data){

  # Adjust all grade proportions according to the appropriate deterioration
  # rate. Since each computation affects the current grade and the grade below,
  # computations are performed in reverse order.
  element.data %>%
    mutate(E = D * de + E,
           D = C * cd + D * (1 - de),
           C = B * bc + C * (1 - cd),
           B = A * ab + B * (1 - bc),
           A = A * (1 - ab))

}
