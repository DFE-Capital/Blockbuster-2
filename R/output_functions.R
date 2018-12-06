#' Load data from the blockbuster output files.
#'
#' This function loads the blockbuster outputs into memory from the saved files
#' produced during a call to \code{\link{Blockbuster}}.
#'
#' @param forecast.horizon The number of timesteps to collate.
#' @param filelabel Character. The file label on the output files. This
#' corresponds to the \code{filelabel} argument passed to \code{Blockbuster}.
#' Default is "blockbuster_output".
#' @param path Character. The path to the folder containing the saved files.
#' This corresponds to the \code{path} argument passed to \code{Blockbuster}.
#' Default is "./output/".
#'
#' @details The files are expected to end in "_element_i" and "block_i", with i
#' being the timestep plus one. If the data input to \code{\link{Blockbuster}}
#' is large then R may run out of memory when creating the final output.  For
#' this reason the state of the estate can be saved to disc and the final output
#' reconstructed at the end of the process, or outside of the
#' \code{\link{Blockbuster}} call.
#' @return A list of lists.  There is one list entry for each timestep, so
#' the output will be of length \code{forecast.horizon + 1} as the first will
#' contain the initial state.  Each list entry contains a block-level and
#' element-level data frame named \code{block} and \code{element} respectively.
#' @examples
#' \dontrun{
#' # Load the first ten years of output from the default folder
#' LoadBlockbusterOutput(10)
#' }
LoadBlockbusterOutput <- function(forecast.horizon,
                                  filelabel = "blockbuster_output",
                                  path = "./output/"){
  # integrity check of input
  if (!is.numeric(forecast.horizon)) stop("forecast.horizon must be a number.")

  # initialize empty list of correct length
  output <- vector("list", forecast.horizon + 1)

  # load saved files into list
  for (i in 0:forecast.horizon){

      output[[i + 1]]$element <- readRDS(
        paste0(path, "/", filelabel, "_element_", i, ".rds")
        )
      output[[i + 1]]$block <- readRDS(
        paste0(path, "/", filelabel, "_block_", i, ".rds")
        )
  }

  return(output)
}

#' Collate element- and block-level outputs from Blockbuster outputs
#'
#' A set of convenience functions for managing the raw output of a
#' \code{\link{Blockbuster}} function call.
#'
#' The functions \code{pull_Block_data} and \code{pull_Element_data} take the
#' output from a \code{\link{Blockbuster}} call and combine the appropriate data
#' into a single dataframe with a column for year.
#'
#' For large datasets (for example, the PDS dataset), R can hit memory limits
#' when the \code{\link{Blockbuster}} function tries to construct the single
#' output object from the saved interim files. \code{load_Block_Data} and
#' \code{load_Element_Data} loads only the relevant outputs from the saved
#' files.
#'
#' These functions can also provide area and backlog summaries in tidy format
#' suitable for passing to \code{ggplot2}.
#'
#' @describeIn pull_Element_Data Pulls out the element level data from the
#'  output of a blockbuster function call
#' @param blockbuster_output The output from a \code{\link{Blockbuster}} call.
#'
#' @param type character. Selects whether the output contains tidy data about
#'  \code{"area"} or \code{"backlog"}.  If left as default, raw element data
#'   is output.
#'
#' @return A data.frame containing the appropriate data from the output, with a
#'  year column.  If \code{type = "area"} then the output will contain the grade
#'  and component area in tidy format. If \code{type = "backlog"} then the
#'  output will contain the grade and backlog in tidy format.
#' @export
#' @examples
#' \dontrun{
#' # Collate the first ten years of output from an object output by
#' # Blockbuster
#' output <- Blockbuster(simulated_elements, forecast.horizon = 15)
#' pull_Block_Data(output, 10)
#' pull_Element_Data(output, 10)
#' }
pull_Element_Data <- function(blockbuster_output, type = ""){

  # initialize output object
  element.data <- vector("list", length(blockbuster_output))

  # populate output list
  for (i in 1:length(blockbuster_output)){
    element.data[[i]] <- blockbuster_output[[i]]$element %>%
      mutate(year = i - 1)
  }

  element.data <- bind_rows(element.data)

  if(type == "backlog"){
    element.data <- element.data %>%
      select(-B, -C, -D, -E) %>% # remove these columns as I want their names
      rename(B = B.repair.total,
             C = C.repair.total,
             D = D.repair.total,
             E = E.repair.total) %>%
      gather("grade", "backlog", B, C, D, E) %>%
      select("buildingid", "elementid", year, grade, backlog)
  }

  if (type == "area"){
    element.data <- element.data %>%
      mutate(A = A * unit_area,
             B = B * unit_area,
             C = C * unit_area,
             D = D * unit_area,
             E = E * unit_area) %>%
      gather("grade", "area", A, B, C, D, E) %>%
      select("buildingid", "elementid", year, grade, area)
  }

  return(element.data)
}

#' @describeIn pull_Element_Data Pulls out block data from the output from a
#' blockbuster function call
#'
#' @export
pull_Block_Data <- function(blockbuster_output){

  # initialize output
  block.data <- vector("list", length(blockbuster_output))

  # put data into list
  for (i in 1:length(blockbuster_output)){
    block.data[[i]] <- blockbuster_output[[i]]$block %>% mutate(year = i - 1)
  }

  block.data <- bind_rows(block.data) %>%
    rename(B = B.block.repair.cost,
           C = C.block.repair.cost,
           D = D.block.repair.cost,
           E = E.block.repair.cost) %>%
    gather("grade", "backlog", B, C, D, E)

  return(block.data)
}

#' @describeIn pull_Element_Data Loads the element-level output from a
#' blockbuster call
#'
#' @param forecast.horizon integer.  This specifies how many years of the saved
#' output will be loaded.
#' @param path character. This should be the same as the argument passed to the
#' \code{\link{Blockbuster}} function.
#' @param filelabel character. This should be the same as the argument passed to
#' the \code{\link{Blockbuster}} function.
#'
#' @export
#' @examples
#' # loading outputs from saved files
#' \dontrun{
#' Blockbuster(simulated_elements, path = "output", filelabel = "example")
#' load_Element_data(1, path = "output", filelabel = "example")
#' load_Block_data(1, path = "output", filelabel = "example")
#' }
load_Element_Data <- function(forecast.horizon, path = "./output/",
                              filelabel = "blockbuster_output", type = ""){

  file <- file.path(path, filelabel)

  # set up output
    output <- vector("list", forecast.horizon + 1)

    # loop over forecast.horizon
    for (i in 0:forecast.horizon){
      # load data
       output[[i + 1]] <- readRDS(paste0(file, "_element_", i, ".rds")) %>%
         mutate(year = i)
    }

    output <- bind_rows(output)

    if(type == "backlog"){
      output <- output %>%
        select(-B, -C, -D, -E) %>% # remove these columns as I want their names
        rename(B = B.repair.total,
               C = C.repair.total,
               D = D.repair.total,
               E = E.repair.total) %>%
        gather("grade", "backlog", B, C, D, E) %>%
        select("buildingid", "elementid", year, grade, backlog)
    }

    if (type == "area"){
      output <- output %>%
        mutate(A = A * unit_area,
               B = B * unit_area,
               C = C * unit_area,
               D = D * unit_area,
               E = E * unit_area) %>%
        gather("grade", "area", A, B, C, D, E) %>%
        select("buildingid", "elementid", year, grade, area)
    }

    return(output)
  }


#' @describeIn pull_Element_Data Loads the element-level output from a
#' blockbuster call
#' @export
load_Block_Data <- function(forecast.horizon, path = "./output/",
                            filelabel = "blockbuster_output"){
  file <- file.path(path, filelabel)

  # set up output
  output <- vector("list", forecast.horizon + 1)

  # loop over forecast.horizon
  for (i in 0:forecast.horizon){
    # load data
    output[[i + 1]] <- readRDS(paste0(file, "_block_", i, ".rds")) %>%
      mutate(year = i)
  }

  output <- bind_rows(output) %>%
    rename(B = B.block.repair.cost,
           C = C.block.repair.cost,
           D = D.block.repair.cost,
           E = E.block.repair.cost) %>%
    gather("grade", "backlog", B, C, D, E)

  return(output)
}


#' Count the number of expected building failures
#'
#' @param element Data frame. Element level data. Any data frame will work as
#'  long as it contains \code{buildingid}, \code{elementid} and \code{E} columns.
#' @param critical Numeric.  A vector of numbers indicating the
#'  \code{elementid} values used to filter \code{element}.
#'
#' @return The expected number of buildings that will fail.  A failed
#' building is defined as one in which at least one of the critical elements has
#' reached grade E.  If $p_i$ is the probability that the $i$th component has
#' failed then the probability a building with $n$ critical components has
#' failed is $1-\prod_{i=1}^n\left(1-p_i\right)$.
#'
#' @examples
#' critical_elements <- c(1700, 1701, 1706, 1707, 1708, 1752, 1756, 1769, 1773,
#'                        1774, 1775, 1786, 1787, 1788, 1826, 1827, 1828, 1829,
#'                        1830, 1883, 1885, 1887, 1889, 1892, 1893, 1900, 1903,
#'                        1905, 1983, 1984, 1985)
#' blockbuster2:::buildings_expected_failures(simulated_elements, critical_elements)
buildings_expected_failures <- function(element, critical){
  element %>%
    filter(elementid %in% critical) %>%
    group_by(buildingid) %>%
    summarise(p_failure = 1 - prod(1-E)) %>%
    summarise(Expected_building_failures = sum(p_failure))
}
