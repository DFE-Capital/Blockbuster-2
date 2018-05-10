
#' Load data from the blockbuster output files.
#'
#' Create a \code{\link{blockbuster}}, \code{\link{element.list}} or
#' \code{\link{block.list}} object from the files saved during a call to
#' \code{\link{Blockbuster}}.
#'
#' The files are expected to end in "_element_i" and "block_i", with i being the
#' timestep plus one. If the data input to \code{\link{Blockbuster}} is large
#' then R may run out of memory when creating the final output.  For this reason
#' the state of the estate can be saved to disc and the final output
#' reconstructed at the end of the process, or outside of the
#' \code{\link{Blockbuster}} call.
#'
#' If \code{type} is set to \code{"block"} or \code{"block.list"} then a
#' \code{\link{block.list}} object will be created containing on the block-level
#' summary.
#'
#' If \code{type} is set to \code{"element"} or \code{"element.list"} then an
#' \code{\link{element.list}} object is created containing the element-level
#' information.
#'
#' The default behavior is \code{type = "blockbuster"} which creates the
#' \code{\link{blockbuster}} object containing the element and block level
#' information for all timesteps.
#'
#' For large datasets, it is not unusual for \code{type = "block.list"} and
#' \code{type = "blockbuster"} to fail due to lack of memory.  The functions
#' \code{\link{LoadLongArea}} and \code{\link{LoadRepairBacklog}} will then be
#' useful for constructing the required output summaries to pass to
#' \code{ggplot2}.
#'
#' @param forecast.horizon The number of timesteps to collate.
#' @param filelabel Character. Default is "blockbuster_output".
#' @param path Character. Default is "./output/".
#'
#' @return A \code{\link{blockbuster}} object.
LoadBlockbusterOutput <- function(forecast.horizon,
                                  filelabel = "blockbuster_output",
                                  # type = "blockbuster",
                                  path = "./output/"){
  # integrity check of input
  if (!is.numeric(forecast.horizon)) stop("forecast.horizon must be a number.")

  # initialize empty list of correct length
  output <- vector("list", forecast.horizon + 1)



  # load saved files into list
  for (i in 0:forecast.horizon){

      output[[i + 1]]$element <- readRDS(paste0(path, "/", filelabel, "_element_", i, ".rds"))
      output[[i + 1]]$block <- readRDS(paste0(path, "/", filelabel, "_block_", i, ".rds"))
  }

  return(output)
}

#' Collate element- and block-level outputs from Blockbuster outputs
#'
#' A set of convenience functions for managing the raw output of a \code{\link{Blockbuster}} function call.
#'
#' The functions \code{pull_Block_data} and \code{pull_Element_data} combine the appropriate data into a single dataframe with a column for year.
#'
#' For large datasets (for example, the PDS dataset), R can hit memory limits when the \code{\link{Blockbuster}} function tries to construct the single output object from the saved interim files. \code{load_Block_Data} and \code{load_Element_Data} loads the relevant outputs from the saved files.
#'
#' @describeIn pull_Element_Data Pulls out the element level data from the output of a blockbuster function call
#' @param blockbuster_output The output from a \code{\link{Blockbuster}} call.
#'
#' @param type character. Selects whether the output contains tidy data about \code{"area"} or \code{"backlog"}.  If left as default, raw element data is output.
#'
#' @return A data.frame containing the appropriate data from the output, with a year column.  If \code{type = "area"} then the data will contain the grade and component area in tidy format. If \code{type = "backlog"} then the data will contain the grade and backlog in tidy format.
#' @export
#' @examples
#' \dontrun{
#' output <- Blockbuster(simulated_elements, forecast_horizon = 10)
#' pull_Block_Data(output, 10)
#' pull_Element_Data(output, 10)
#' }
pull_Element_Data <- function(blockbuster_output, type = ""){

  # initialize output object
  element.data <- vector("list", length(blockbuster_output))

  # populate output list
  for (i in 1:length(blockbuster_output)){
    element.data[[i]] <- blockbuster_output[[i]]$element %>% mutate(year = i - 1)
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



#' @describeIn pull_Element_Data Pulls out the block data from the output from a blockbuster function call
#'
#' @export
#' @examples
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

#' @describeIn pull_Element_Data Loads the element-level output from a blockbuster call
#'
#' @param forecast.horizon integer.  This specifies how many years of the saved
#' output will be loaded.
#' @param path character. This should be the same as the argument passed to the \code{\link{Blockbuster}} function.
#' @param filelabel character. This should be the same as the argument passed to the \code{\link{Blockbuster}} function.
#'
#' @export
#' @examples
#' # loading outputs from saved files
#' \dontrun{
#' Blockbuster(simulated elements, path = "output", filelabel = "example")
#' load_Element_data(1, path = "output", filelabel = "example")
#' load_Block_data(1, path = "output", filelabel = "example")
#' }
load_Element_Data <- function(forecast.horizon, path = "./output/", filelabel = "blockbuster_output", type = ""){

  file <- file.path(path, filelabel)

  # set up output
    output <- vector("list", forecast.horizon + 1)

    # loop over forecast.horizon
    for (i in 0:forecast.horizon){
      # load data
       output[[i + 1]] <- readRDS(paste0(file, "_element_", i, ".rds")) %>% mutate(year = i)
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


#' @describeIn pull_Element_Data Loads the element-level output from a blockbuster call
#' @export
load_Block_Data <- function(forecast.horizon, path = "./output/", filelabel = "blockbuster_output"){
  file <- file.path(path, filelabel)

  # set up output
  output <- vector("list", forecast.horizon + 1)

  # loop over forecast.horizon
  for (i in 0:forecast.horizon){
    # load data
    output[[i + 1]] <- readRDS(paste0(file, "_block_", i, ".rds")) %>% mutate(year = i)
  }

  output <- bind_rows(output) %>%
    rename(B = B.block.repair.cost,
           C = C.block.repair.cost,
           D = D.block.repair.cost,
           E = E.block.repair.cost) %>%
    gather("grade", "backlog", B, C, D, E)

  return(output)
}
