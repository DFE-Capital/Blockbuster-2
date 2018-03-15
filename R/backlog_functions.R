#' Creates a block.list object from a blockbuster object
#'
#' Creates a list of class \code{\link{block.list}} containing all the block-
#' level information from the output from a \code{\link{Blockbuster}} model.
#' @param blockbuster A \code{\link{blockbuster}} object.
#'
#' @return A list of block-level information as a \code{\link{block.list}}
#' object
PullBlockData <- function(blockbuster){

  # Check input integrity
  if (!is.blockbuster(blockbuster)) stop("PullBlockData requires a blockbuster
                                         class input.")

  # initialize output
  block.data.list <- vector("list", length(blockbuster))

  # put data into list
  for (i in 1:length(blockbuster)){
    block.data.list[[i]] <- blockbuster[[i]]$block
  }

  # set class and return
  block.data.list <- BlockLevelList(block.data.list)
  return(block.data.list)
}

#' Creates an element.list object from a blockbuster object
#'
#' Creates a list of class \code{\link{element.list}} containing all the
#' element-level information from the output from a \code{\link{Blockbuster}}
#' model.
#' @param blockbuster A \code{\link{blockbuster}} object.
#'
#' @return A list of element-level information as an \code{\link{element.list}}
#' object
PullElementData <- function(blockbuster){

  # Check input integrity
  if (!is.blockbuster(blockbuster)) stop("PullElementData requires a blockbuster
                                         class input.")

  # initialize output object
  element.data.list <- vector("list", length(blockbuster))

  # populate output list
  for (i in 1:length(blockbuster)){
    element.data.list[[i]] <- blockbuster[[i]]$element
  }

  # set class and return
  element.data.list <- ElementLevelList(element.data.list)
  return(element.data.list)
}

#' Compute total repair cost of blocks at grade B using block-level data.
#'
#' @param block.data A \code{\link{block}} object.
#' @return A number.
BacklogB <- function(block.data){
  sum(block.data$B.block.repair.cost)
}

#' Compute total repair cost of blocks at grade C using block-level data.
#'
#' @param block.data A \code{\link{block}} object.
#' @return A number.
BacklogC <- function(block.data)
  sum(block.data$C.block.repair.cost)

#' Compute total repair cost of blocks at grade D using block-level data.
#'
#' @param block.data A \code{\link{block}} object.
#' @return A number.
BacklogD <- function(block.data)
  sum(block.data$D.block.repair.cost)

#' Create a table containing the total repair cost of blocks at all grades and
#' timesteps.
#'
#' The input can either be a \code{\link{block.list}}, \code{\link{block}} or
#' \code{\link{blockbuster}} object.#'
#'
#' @param block.data A \code{\link{block.list}}, \code{\link{block}} or
#' \code{\link{blockbuster}} object.
#' @return A table with the grade B, C, and D repair backlogs as columns with
#' each row representing a timestep.
GenerateRepairBacklog <- function(blockbuster){

  # check input integrity and modify blockbuster object if necessary.
  if (is.blockbuster(blockbuster)) blockbuster <- PullBlockData(blockbuster)
  if (is.block(blockbuster)) blockbuster <- BlockLevelList(list(blockbuster))
  if (!is.block.list(blockbuster)) stop("GenerateRepairBacklog requires either
                                        a blockbuster, a block.list, or a block
                                        class object as an input.")

  # compute backlogs for each grade
  B <- sapply(blockbuster, BacklogB) %>% unlist
  C <- sapply(blockbuster, BacklogC) %>% unlist
  D <- sapply(blockbuster, BacklogD) %>% unlist
  data.table::data.table(B, C, D)
}

#' Compute the mean proportion of building components at each grade.
#'
#' @param element.data An \code{\link{element}} object.
#' @return A data table containing the mean proportions at each grade.
PercentageAtGrade <- function(element.data){

  # check input integrity
  if (!is.element(element.data)) stop("PercentageAtGrade requires an element
                                      class object.")

  element.data %>%
    dplyr::ungroup %>%
    dplyr::summarise_at(c("A", "B", "C", "D"), mean)
}

#' Compute the mean proportion of elements at each grade per timestep.
#'
#' This function accepts both \code{\link{element.list}} and
#' \code{\link{blockbuster}} objects as inputs. If a \code{\link{blockbuster}}
#' object is provided the appropraite \code{\link{element.list}} information is
#' gathered using an internal call to \code{\link{PullElementData}}.
#'
#' @param input An \code{\link{element}} or \code{\link{blockbuster}} object.
#'
#' @return A table where rows are timesteps and columns are grades.
GeneratePercentageAtGrade <- function(input){

  # if input is a blockbuster object then pull the element-level data from it.
  if (is.blockbuster(input)){
    input <- PullElementData(input)
  }

  if (!is.element.list(input)) stop("GeneratePercentageAtGrade requires either
                                    an element.list object or blockbuster object")

  # compute percentage at each timestep and output
  t(sapply(input, PercentageAtGrade)) %>%
    data.frame
}


LongRepairBacklog <- function(block.data){

  # Input integrity
  if (is.blockbuster(block.data)) block.data <- PullBlockData(block.data)
  if (is.block(block.data)) block.data <- BlockLevelList(list(block.data))
  if (!is.block.list(block.data)) stop("LongRepairBacklog requires either
                                        a blockbuster, a block.list, or a block
                                        class object as an input.")

  block.data <- GenerateRepairBacklog(block.data) %>%
    tidyr::gather(grade, backlog, c("B", "C", "D")) %>%
    dplyr::mutate(timestep = as.integer(rep(1:length(block.data) - 1, 3)))

  block.data <- RepairBacklogClass(block.data)
  return(block.data)
}


GenerateArea <- function(element.data){
  element.data %>% ungroup %>%
    dplyr::mutate(A = A * unit_area,
                  B = B * unit_area,
                  C = C * unit_area,
                  D = D * unit_area) %>%
    dplyr::summarise_at(c("A", "B", "C", "D"), sum) %>%
    tidyr::gather(grade, area, c("A", "B", "C", "D"))
}

LongArea <- function(element.data){

  if(is.blockbuster(element.data)) element.data <- PullElementData(element.data)
  if(is.element(element.data)){
    element.data <- ElementLevelList(list(element.data))
  }
  if(!is.element.list(element.data)) stop("LongArea requires either a
                                          blockuster, element or element.list
                                          class object as an input.")

  res <- lapply(element.data, GenerateArea) %>%
    bind_rows %>%
    mutate(timestep = as.integer(rep(1:length(element.data) - 1, 1,
                                     each = 4))) %>%
    AreaClass
}


GenerateElementBacklog <- function(element){
  if(!is.element(element)) stop("Input should be an element class object.")

  element <- element %>%
    tidyr::gather(grade, backlog, c("B.repair.total", "C.repair.total",
                                    "D.repair.total")) %>%
    mutate(grade = case_when(grade == "B.repair.total" ~ "B",
                             grade == "C.repair.total" ~ "C",
                             grade == "D.repair.total" ~ "D")) %>%
    select(elementid, buildingid, grade, backlog)
  element$grade <- as.factor(element$grade)
  return(element)
}

CreateElementBacklog <- function(element.data){
  # accepts blockbuster, element.list and element
  # TODO make it accept blockbuster element
  if(is.blockbuster(element.data)) element.data <- PullElementData(element.data)
  if(is.element(element.data)){
    element.data <- ElementLevelList(list(element.data))
  }
  if(!is.element.list(element.data)) stop("LongArea requires either a
                                          blockuster, element or element.list
                                          class object as an input.")

  res <- lapply(element.data, GenerateElementBacklog)
  len <- nrow(res[[1]])
  res <- res %>%
    bind_rows %>%
    mutate(timestep = as.integer(rep(1:length(element.data) - 1, 1,
                                     each = len))) %>%
    ElementBacklog
}

