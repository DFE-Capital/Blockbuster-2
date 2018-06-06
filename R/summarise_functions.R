#' Summarise the area at each grade for an element-level dataset
#'
#' @param element_data An element-level data frame.
#' @param by An expression that identifies the column name used to group the
#' summary.  Should be either \code{buildingid} or \code{elementid}.
#'
#' @return A data frame in tidy format with grade and whatever was supplied to
#' the \code{by} argument as variables and the area as the value.
#' @examples
#' # Summarising area by component type
#' element_summarise_area(simulated_elements, elementid)
#' # Summarising area by block
#' element_summarise_area(simulated_elements, buildingid)
element_summarise_area <- function(element_data, by = elementid){
  by <- enquo(by)
  element_data <- element_data %>%
    mutate(A = A * unit_area,
           B = B * unit_area,
           C = C * unit_area,
           D = D * unit_area,
           E = E * unit_area) %>%
    gather("grade", "area", A, B, C, D, E) %>%
    group_by(!!by, grade) %>%
    summarise(area = sum(area)) %>%
    filter(area > 0) %>%
    ungroup %>%
    select(grade, !!by, area)
  return(element_data)
}

#' Summarise the backlog at each grade for an element-level dataset
#'
#' @param element_data An element-level data frame.
#' @param by An expression that identifies the column name used to group the
#' summary.  Should be either \code{buildingid} or \code{elementid}.
#'
#' @return A data frame in tidy format with grade and whatever was supplied to
#' the \code{by} argument as variables and the backlog as the value.
#' @examples
#' # Summarising backlog by component type
#' element_summarise_backlog(simulated_elements, elementid)
#' # Summarising backlog by block
#' element_summarise_backlog(simulated_elements, buildingid)
element_summarise_backlog <- function(element_data, by = elementid){
  by <- enquo(by)
  element_data <- element_data %>%
    select(-B, -C, -D, -E) %>%
    rename(B = B.repair.total,
           C = C.repair.total,
           D = D.repair.total,
           E = E.repair.total) %>%
    gather("grade", "backlog", B, C, D, E) %>%
    group_by(!!by, grade) %>%
    summarise(backlog = sum(backlog)) %>%
    filter(backlog > 0) %>%
    ungroup %>%
    select(grade, !!by, backlog)
  return(element_data)
}


