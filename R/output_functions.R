
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

    # if(type == "blockbuster"){
      # load element and block files into list
      output[[i + 1]]$element <- readRDS(paste0(path, "/", filelabel, "_element_", i, ".rds"))
      output[[i + 1]]$block <- readRDS(paste0(path, "/", filelabel, "_block_", i, ".rds"))
      #check integrity
      #if (!is.element(output[[i + 1]]$element)){
      #  stop(paste0(path,filelabel, "_element_", i, ".rds is not an element object."))
      #}
      #if (!is.block(output[[i + 1]]$block)){
      #  stop(paste0(path,filelabel, "_block_", i, ".rds is not a block object."))
      #}
    # }

    # if(type == "element" | type == "element.list"){
    #   output[[i+1]] <- readRDS(paste0(path, "/", filelabel, "_element_", i, ".rds"))
    #   #check integrity
    #   #if (!is.element(output[[i + 1]])){
    #   #  stop(paste0(path,filelabel, "_element_", i, ".rds is not an element object."))
    #   #}
    # }
    # if(type == "block" | type == "block.list"){
    #   output[[i+1]] <- readRDS(paste0(path, "/", filelabel, "_block_", i, ".rds"))
    #   #if (!is.block(output[[i + 1]])){
    #    # stop(paste0(path,filelabel, "_block_", i, ".rds is not a block object."))
    #   #}
    # }
  }

  # set class and return
  #if (type == "blockbuster") output <- BlockbusterOutput(output)
  #if(type == "element" | type == "element.list") output <- ElementLevelList(output)
  #if(type == "block" | type == "block.list") output <- BlockLevelList(output)
  return(output)
}


# LoadLongArea <- function(forecast.horizon, path = "./output/", filelabel = "blockbuster_output"){
#   # set up output
#   output <- vector("list", forecast.horizon + 1)
#
#   # loop over forecast.horizon
#   for (i in 0:forecast.horizon){
#     # load data
#     data <- readRDS(paste0(path, filelabel, "_element_", i, ".rds"))
#     # pull area
#     output[[i + 1]] <- LongArea(data)
#     output[[i + 1]]$timestep <- rep(i, 4)
#   }
#   output <- bind_rows(output)
# }
#
#
# LoadLongRepairBacklog <- function(forecast.horizon, path = "./output/", filelabel = "blockbuster_output"){
#   # set up output
#   output <- vector("list", forecast.horizon + 1)
#
#   # loop over forecast.horizon
#   for (i in 0:forecast.horizon){
#     # load data
#     data <- readRDS(paste0(path, filelabel, "_block_", i, ".rds"))
#     # pull area
#     output[[i + 1]] <- LongRepairBacklog(data)
#     output[[i + 1]]$timestep <- rep(i, 3)
#   }
#   output <- bind_rows(output)
# }
