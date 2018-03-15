#' Deteriorate all the elements in an \code{\link{element}} class blockbuster
#' object.
#'
#' Takes an \code{\link{element}} class object and amends the grade
#' proportions for each row according to the deterioration rate in each row.
#' This function is used by the \code{\link{Blockbuster}} function to
#' compute the deterioration at each timestep.
#'
#' The deterioration rates are based on lifetime estimates from EC Harris and
#' other expert opinion.
#'
#' @param element.data An \code{\link{element}} object.
#'
#' @return An \code{\link{element}} object.
Deteriorate <- function(element.data){

  # Check integrity of the input.
  if (!is.element(element.data)) stop("Deterioration can only be performed on
                                      an element class object.")

  # Adjust all grade proporitions according to the appropriate deterioration
  # rate. Since each computation affects the current grade and the grade below,
  # computations are performed in reverse order.
  element.data %>%
    mutate(E = D* de + E,
           D = C * cd + D * (1 - de),
           C = B * bc + C * (1 - cd),
           B = A * ab + B * (1 - bc),
           A = A * (1 - ab)) %>%
    ElementLevel
}
