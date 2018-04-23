#' Blockbuster model of school estate deterioration
#'
#' High level modelling of element and block objects through time that considers
#' repairs and rebuilding interventions on the condition of the modelled
#' building components. It is composed of many smaller functions that
#' deteriorate, rebuild and repair the components.

#' Outputs a list of block and element objects detailing the condition of the
#' modelled blocks and components at each timestep.  Information is stored as
#' \code{\link{block}} and \code{\link{element}} objects. The states at each
#' timestep and the final output are saved as files specified by the
#' \code{filelabel} and \code{path} parameters.  The saving of outputs to disc is
#' mandatory.  It saves memory space for large samples and simulations and the
#' final output is collated from the individual files rather than being stored
#' in memory.
#'
#' At each timestep the components are degraded at the appropriate rates.
#' Inflation (set by the user using the \code{inflation} parameter) is then
#' applied to repair and rebuild costs.  After this, blocks are
#' rebuilt in an order determined by a cost benefit calculation which weighs the
#' cost of repairing all components in a block with the cost of rebuilding the
#' block. Once this is done, the model then decides on which components need
#' repairing.  Grade D components are repaired first, in order of descending
#' cost.  Then Grade C components are repaired and then, if the budget allows,
#' Grade B.
#'
#' The degradation rates of a component are based on transition rates
#' from EC Harris and expert opinion within DfE.  Alternate transition rates can
#' be passed to the function using the \code{block.repair.costs} parameter after
#' manually editing the block_det_data object created by the library.
#'
#' Repair costs per unit area for each component are pulled from the
#' \code{link{blockbuster_pds_repair_costs}} data table created by the
#' blockbuster package.  Repair costs can be manually passed to the blockbuster
#' function using the \code{block.repair.costs} argument, respecting the format
#' of the default parameter.
#'
#' Rebuild costs per unit area are user-input as parameter
#' \code{block.rebuild.cost} and individiual block rebuild costs generated from
#' this and the GIFA (Gross Internal Floor Area) of the block.
#'
#' The \code{rebuild.money} and \code{repair.money} arguments provide the
#' budgets for rebuilding and repairing during each timestep respectively.
#'
#' @param element.data An \code{\link{element}} object containing the initial
#' state of the block components.
#' @param block.data (optional) A \code{\link{block}} object containing the
#' initial state of the blocks. If not supplied, this will be created from the
#' element.data.
#' @param forecast.horizon A number. How many timesteps are to be simulated.
#' Default value is 1.
#' @param rebuild.money A vector of numbers indicating the available money to
#' spend on rebuilding blocks each timestep. Default value is zero.
#' @param repair.money A vector of numbers indicating the available money to
#' spend on repairing components each timestep. Default value is zero
#' @param block.rebuild.cost (optional) The unit cost (per m^2^) of rebuilding blocks.
#' Default value is 2000. This is required if the \code{block.data} argument is not
#' supplied.
#' @param inflation A vector of numbers indicating the inflation rate to apply
#' to repair and rebuild costs each timestep.  If a vector of length one is
#' supplied it will be used as the inflation rate for all timesteps. Default
#' is 1, i.e. no inflation.
#' @param block.det.data (optional) An object of the same form as
#' \code{block_det_data} that is used to provide deterioration rates.
#' @param block.repair.costs (optional) an object of the same form as
#' \code{\link{blockbuster_pds_repair_costs}} that is used to provide repair
#' costs.
#' @param save (optional) Logical. If \code{TRUE} (the default behaviour), then
#' the block and element states are saved to disc at each timestep.
#' @param filelabel (optional) Character. The start of the file names used to save
#' the interim outputs. Default is "blockbuster_output".
#' @param path (optional) Character. The path to the folder which stores the
#' outputs. If the folder this points to does not exist then the simulation will
#' fail. Default is "./output/".
#' @param grade.order (Optional) A list of the three character strings \code{"D"},
#'  \code{"C"} and \code{"B"} in any order.  This determines the priority for
#'  repairs.  The first character gives the first grade that will be repaired.
#'  By default, D is repaired first, then C, then B.
#'
#' @return A list.  Each list entry contains the \code{\link{block}} and
#' \code{\link{element}} objects that store the condition of the blocks and
#' components at each timestep, with the initial conditions stored in the first
#' list entry.
#'
#' @export
#'
#' @examples
#' # TODO
Blockbuster <- function(element.data, block.data = NULL,
                        forecast.horizon = 1,
                        rebuild.money = 0,
                        repair.money = 0,
                        block.rebuild.cost = 2000,
                        inflation = 1,
                        #block.det.data = blockbuster_det_data,
                        #block.repair.costs = blockbuster_pds_repair_costs,
                        save = TRUE,
                        filelabel = "blockbuster_output",
                        path = "./output/",
                        grade.order = c("D", "C", "B")
){

  # input integrity checks -------------------------------------------------
  message ("Checking inputs...")
  inputs <- input_checks(element.data,
                         block.data,
                         forecast.horizon,
                         rebuild.money,
                         repair.money,
                         block.rebuild.cost,
                         inflation)
  # inputs_checks may have changed some inputs (e.g. repeating single values)
  block.data <- inputs$block.data
  rebuild.money <- inputs$rebuild.money
  repair.money <- inputs$repair.money
  inflation <- inputs$inflation

  # set filepath
  savefile <- file.path(path, filelabel)



  # save initial state ------------------------------------------------------

  if(save){
    message("Saving initial state to file.")
    # set up output directory if necessary
    if(!dir.exists(path)) dir.create(path)

    # save initial state (this may seem odd since it is an input, but it is useful
    # to have the initial state saved as part of the complete output).
    saveRDS(element.data, file = paste0(savefile, "_element_0.rds"))
    saveRDS(block.data, file = paste0(savefile, "_block_0.rds"))
  } else {
    # set up output list.
    message("Setting up output.")
    output <- vector("list", forecast.horizon + 1)
    output[[1]]$element <- element.data
    output[[1]]$block <- block.data
  }

  # LOOP ----
  for (i in 1:forecast.horizon){
    # deteriorate ----
    message(paste0("Deteriorating at time step ", i,"."))
    element.data <- Deteriorate(element.data = element.data)

    # update repair costs ----
    message("Updating element repair totals.")
    element.data <- UpdateElementRepairs(element.data)
    block.data <- UpdateBlockRepairs(block.data, element.data)

    # inflate all repair/rebuild costs ----
    message("Inflating rebuild and repair costs.")
    block.data <- InflateRebuild(block.data = block.data,
                                 inflation = inflation[i])

    block.data <- InflateRepairBlock(block.data = block.data,
                                     inflation = inflation[i])

    element.data <- InflateRepairElement(element.data = element.data,
                                         inflation = inflation[i])

    # rebuild ----
    message("Rebuilding blocks.")
    element.data <- Rebuild(element.data = element.data,
                            block.data = block.data,
                            rebuild.money = rebuild.money[i])

    # update repair costs ----
    element.data <- UpdateElementRepairs(element.data)
    block.data <- UpdateBlockRepairs(block.data, element.data)

    # repair ----
    message("Repairing blocks.")
    element.data <- Repair(element.data = element.data,
                           block.data = block.data,
                           repair.money = repair.money[i],
                           grade.order = grade.order)

    # update repair costs ----
    element.data <- UpdateElementRepairs(element.data)
    block.data <- UpdateBlockRepairs(block.data, element.data)

    # save current state ----
    if(save){
      message(paste0("Saving state at timestep ", i, " to file."))
      # save current state
      saveRDS(element.data, file = paste0(savefile, "_element_", i, ".rds"))
      saveRDS(block.data, file = paste0(savefile, "_block_", i, ".rds"))
    } else {
      output[[i + 1]]$element <- element.data
      output[[i + 1]]$block <- block.data
    }

    # LOOP END ----

  }

  # OUTPUT ----
  if(save){
    # collate states, save (a backup!)
    message(paste0("Saving output to file ", savefile, "_output.rds"))
    output <- LoadBlockbusterOutput(forecast.horizon = forecast.horizon,
                                    filelabel = filelabel,
                                    path = path)
    #output <- BlockbusterOutput(output)
    saveRDS(output, file = paste0(savefile, "_output.rds"))
  } else {
    message("Preparing output.")
    #output <- BlockbusterOutput(output)
  }

  return(output)
}

#'
#' # TODO - this needs to be able to modify the output folder.
#' #' Run Blockbuster simulations on a variety of spending strategies.
#' #'
#' #' Given a data.frame of spending scenarios, run the scenario with the
#' #' \code{strat} argument in the strategy column.  This function is useful for
#' #' passing to lapply with a list of strategy names as the first argument.
#' #'
#' #' @param strat Character.  A single character string corresponding with one
#' #' of the entries in the strategy column of \code{scenarios}.
#' #' @param scenarios Data frame. Strategies are named in the last column and each
#' #' strategy is associated with two rows, the first containing the rebuild budget,
#' #' the second containing the repair budget.
#' #' @param element.data An \code{\link{element}} object containing the initial
#' #' state.
#' #' @param block.data A \code{\link{block}} object containing the initial state.
#' #' @param forecast.horizon A number indicating the number of timesteps to
#' #' simulate.
#' #' @param inflation  A vector of numbers indicating the inflation rate to apply
#' #' to repair and rebuild costs each timestep.  If a vector of length one is
#' #' supplied it will be used as the inflation rate for all timesteps.
#' #' @param block.rebuild.cost The unit cost (per m^2^) of rebuilding blocks.
#' #'
#' #' @return NULL.  This function does not return an object, but instead saves all
#' #' outputs (and interim states) to files in the "./output/" folder.
#' MultiBlockbuster <- function(strat, scenarios, element.data, block.data,
#'                              forecast.horizon, inflation, block.rebuild.cost){
#'
#'   # pull the appropriate spending scenario for the given strategy
#'   monies <- scenarios %>% filter(strategy == strat)
#'
#'   # Run blockbuster with the given spending
#'   Blockbuster(element.data = element.data,
#'               block.data = block.data,
#'               forecast.horizon = forecast.horizon,
#'               rebuild.money = monies[1, ],
#'               repair.money = monies[2, ],
#'               block.rebuild.cost = block.rebuild.cost,
#'               inflation = inflation,
#'               filelabel = strat)
#' }
