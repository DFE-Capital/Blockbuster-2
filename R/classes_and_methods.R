## New S3 classes and methods

#' #' Defines element class objects
#' #'
#' #' An element class object is used to hold element-level data.  It identifies
#' #' the building/element and contains information about the unit area, the
#' #' proportion of area at each grade, the associated unit cost of repairing each
#' #' grade, and the total cost to repair the proportion of each element at each
#' #' grade.
#' #'
#' #' @param element.level A data.frame containing the needed variables.
#' ElementLevel <- function(element.level){
#'
#'   # Integrity check
#'   #if(!is.data.frame(element.level)){
#'   #  stop("element class objects can only be constructed from a data.frame.")
#'   #}
#'   # Does it contain all necessary columns?
#'   cols <- c("buildingid", "elementid", "unit_area", "A", "B",
#'            "C", "D", "ab", "bc", "cd",
#'            "B.repair.cost", "C.repair.cost", "D.repair.cost",
#'            "B.repair.total", "C.repair.total",
#'            "D.repair.total", "gifa")
#'   missing.cols <- cols[!cols %in% names(element.level)]
#'
#'   if (length(missing.cols) > 0) {
#'     stop (paste("This object is missing the following columns:\n",
#'                  paste(missing.cols, collapse = ", ")))
#'   }
#'
#'   # set class
#'   class(element.level) <- append("element", class(element.level))
#'   return(element.level)
#' }

#' Defines block class objects
#'
#' A block class object is used to hold block-level data. It identifies the
#' building by \code{buildingid} and contains information about the cost to
#' rebuild the entire block, the cost to repair all components at each grade,
#' and the \code{ratio} between repair and rebuild costs used to determine
#' which blocks are rebuilt each timestep.
#'
#' @param block.level A data.frame containing the necessary variables.
# #BlockLevel <- function(block.level){
#
#    # Integrity check
#   # Is it a data.frame?
#   #if(!is.data.frame(block.level)){
#   #  stop("block class objects can only be constructed from a data.frame.")
#   #}
#
#   # Does it contain all necessary columns?
#   cols <- c("buildingid", "block.rebuild.cost", "B.block.repair.cost",
#             "C.block.repair.cost", "D.block.repair.cost",
#             "ratio")
#   missing.cols <- cols[!cols %in% names(block.level)]
#
#   if (length(missing.cols) > 0) {
#     stop (paste("This object is missing the following columns:\n",
#                 paste(missing.cols, collapse = ", ")))
#   }
#
#   # set class
#   class(block.level) <- append("block", class(block.level))
#   return(block.level)
# }
#
#' #' Defines the element.list class
#' #'
#' #' The element.list class is used for lists of \code{\link{element}} objects.
#' #' This class is primarily used when producing plots and outputs from a
#' #' \code{\link{blockbuster}} object, that is the output from a
#' #' \code{\link{Blockbuster}} call.
#' #' @param element.level.list A list of \code{\link{element}} objects.
#' ElementLevelList <- function(element.level.list){
#'
#'   #if(!is.list(element.level.list)) stop("This is not a list.")
#'
#'   #if (!all(lapply(element.level.list, is.element))){
#'   #  stop("Every list element must be a blockbuster element object.")
#'   #}
#'
#'   class(element.level.list) <- append("element.list", class(element.level.list))
#'   return(element.level.list)
#' }

#' #' Defines the block.list class
#' #'
#' #' The block.list class is used for lists of \code{\link{block}} objects.
#' #' This class is primarily used when producing plots and outputs from a
#' #' \code{\link{blockbuster}} object, that is the output from a
#' #' \code{\link{Blockbuster}} call.
#' #' @param block.level.list A list of \code{\link{block}} objects.
#' BlockLevelList <- function(block.level.list){
#'
#'   #if(!is.list(block.level.list)) stop("This is not a list.")
#'
#'   #if (!all(lapply(block.level.list, is.block))){
#'   #  stop("Every list element must be a blockbuster block object.")
#'   #}
#'
#'   class(block.level.list) <- append("block.list", class(block.level.list))
#'   return(block.level.list)
#'
#' }
#'
#'
#' #' Defines the blockbuster class
#' #'
#' #' The \code{blockbuster} class is the output of the \code{\link{Blockbuster}}
#' #' function. It a list with one entry per timestep, and each list entry contains
#' #' an \code{\link{element}} object and \code{\link{block}} object providing a
#' #' snapshot of the estate condition.
#' #' @param blockbuster.output A list with each element containing an
#' #' \code{\link{element}} object and \code{\link{block}} object.
#' BlockbusterOutput <- function(blockbuster.output){
#'
#'   #if(!is.list(blockbuster.output)) stop("This is not a list.")
#'   #for (i in 1:length(blockbuster.output)){
#'   #  if(!is.element(blockbuster.output[[i]]$element) | !is.block(blockbuster.output[[i]]$block))
#'   #    stop("Each list element must contain an element object and a block object.")
#'   #}
#'
#'   class(blockbuster.output) <- append(class(blockbuster.output), "blockbuster")
#'   return(blockbuster.output)
#' }
#'
#' #' Defines the repair.backlog class
#' #'
#' #' The repair.backlog class is a long data format holding repair backlogs
#' #' suitable for passing to \code{ggplot2}.
#' #'
#' #' @param backlog A data frame containing columns titled \code{timestep},
#' #' \code{grade} and \code{backlog}. There can be other columns.
#' RepairBacklogClass <- function(backlog){
#'
#'   #if(!is.data.frame(backlog)){
#'   #  stop("repair.backlog class objects are data.frames.")
#'   #}
#'
#'   cols <- c("timestep", "grade", "backlog")
#'   missing.cols <- cols[!cols %in% names(backlog)]
#'
#'   if (length(missing.cols) > 0) {
#'     stop (paste("This object is missing the following columns:\n",
#'                 paste(missing.cols, collapse = ", ")))
#'   }
#'   class(backlog) <- append("repair.backlog", class(backlog))
#'   return(backlog)
#' }
#'
#' #' Defines the area class
#' #'
#' #' The area class is a long data format holding area condition summaries
#' #' suitable for passing to \code{ggplot2}.
#' #'
#' #' @param backlog A data frame containing columns titled \code{timestep},
#' #' \code{grade} and \code{area}. There can be other columns.
#' AreaClass <- function(area){
#'
#'   #if(!is.data.frame(area)){
#'   #  stop("area class objects are data.frames.")
#'   #}
#'
#'   cols <- c("timestep", "grade", "area")
#'   missing.cols <- cols[!cols %in% names(area)]
#'
#'   if (length(missing.cols) > 0) {
#'     stop (paste("This object is missing the following columns:\n",
#'                 paste(missing.cols, collapse = ", ")))
#'   }
#'   class(area) <- append("area", class(area))
#'   return(area)
#' }
#'
#' ElementBacklog <- function(element.backlog){
#'   #if(!is.data.frame(element.backlog)){
#'   #  stop("element.backlog class objects are data.frames.")
#'   #}
#'
#'   cols <- c("timestep", "backlog", "elementid", "buildingid")
#'   missing.cols <- cols[!cols %in% names(element.backlog)]
#'
#'   if (length(missing.cols) > 0) {
#'     stop (paste("This object is missing the following columns:\n",
#'                 paste(missing.cols, collapse = ", ")))
#'   }
#'   class(element.backlog) <- append("element.backlog", class(element.backlog))
#'   return(element.backlog)
#'   }
#'
#'
#' #------------------------------------------------------------------------------#
#' # Methods
#'
#' #' @rdname ElementLevel
#' is.element <- function(x){
#'   inherits(x, "element")
#' }
#'
#' #' @rdname BlockLevel
#' is.block <- function(x){
#'   inherits(x, "block")
#' }
#'
#' #' @rdname BlockLevelList
#' is.block.list <- function(x){
#'   inherits(x, "block.list")
#' }
#'
#' #' @rdname ElementLevelList
#' is.element.list <- function(x){
#'   inherits(x, "element.list")
#' }
#'
#' #' @rdname BlockbusterOutput
#' is.blockbuster <- function(x){
#'   inherits(x, "blockbuster")
#' }

# is.repair.backlog <- function(x){
#   inherits(x, "repair.backlog")
# }
#
# is.area <- function(x){
#   inherits(x, "area")
# }

# is.element.backlog <- function(x){
#   inherits(x, "element.backlog")
# }
#
# as.element.list <- function(x){
#   x <- lapply(x, ElementLevel)
#   x <- ElementLevelList(x)
# }
#'
#' filter.element <- function(...){
#'   dplyr:::filter.tbl_df(...) %>% ElementLevel
#' }
#'
#' slice.element <- function(...){
#'   dplyr:::slice.tbl_df(...) %>% ElementLevel
#' }
#'
#' select.element <- function(...){
#'   dplyr:::select.tbl_df(...) %>% ElementLevel
#' }
#'
#' `[.element` <- function(...){
#'   r <- NextMethod("[")
#'   t <- try(r %>% ElementLevel, silent = TRUE)
#'   if(is.element(t)){
#'     return(t)
#'   } else {
#'     return(r)
#'   }
#' }
#'
#'
#' filter.block <- function(...){
#'   dplyr:::filter.tbl_df(...) %>% BlockLevel
#' }
#'
#' slice.block <- function(...){
#'   dplyr:::slice.tbl_df(...) %>% BlockLevel
#' }
#'
#' select.block <- function(...){
#'   dplyr:::select.tbl_df(...) %>% BlockLevel
#' }
#'
#' `[.block` <- function(...){
#'   r <- NextMethod("[")
#'   t <- try(r %>% BlockLevel, silent = TRUE)
#'   if(is.block(t)){
#'     return(t)
#'   } else {
#'     return(r)
#'   }
#' }
#'
#' # plotting
#'
#' #' Plot method for repair.backlog objects
#' #'
#' #' If the input only covers one timestep then a barchart is plotted. Otherwise a
#' #' time series of the backlog at each grade is plotted.
#' #'
#' #' @param backlog A \code{\link{repair.backlog}} object.
#' #' @param total Logical. (optional) Default is \code{FALSE}. If \code{TRUE} then the
#' #' combined C,D backlog is plotted as a timeseries
#' plot.repair.backlog <- function(backlog, total = FALSE, grades = c("C", "D")){
#'   backlog <- backlog %>%
#'     filter(grade %in% grades)
#'
#'   if(total){
#'     backlog <- backlog %>%
#'       dplyr::group_by(timestep) %>%
#'       dplyr::summarise(backlog = sum(backlog))
#'     backlog$grade <- paste(grades, collapse = ", ")
#'   }
#'
#'   # Find nice form for y axis
#'   form <- CurrencyFormat(backlog)
#'
#'   # if backlog is for one timestep only then use barchart
#'   if(max(backlog$timestep) == min(backlog$timestep)){
#'     ggplot2::ggplot(backlog) +
#'       ggplot2::geom_bar(ggplot2::aes(x = grade, weight = backlog)) +
#'       ggplot2::scale_y_continuous(label = function(x) signif(x / (10^form$magnitude), 3)) +   # nicer y axis labels
#'       ggplot2::ylab(paste("Backlog in £", form$level, sep = "")) +
#'       ggplot2::xlab("Grade") +
#'       ggplot2::theme(legend.title = ggplot2::element_blank())
#'   } else {
#'     ggplot2::ggplot(backlog, ggplot2::aes(timestep, backlog)) +
#'       ggplot2::geom_line(ggplot2::aes(colour = grade)) +
#'       ggplot2::scale_y_continuous(label = function(x) signif(x / (10^form$magnitude), 3)) +   # nicer y axis labels
#'       ggplot2::ylab(paste("Backlog in £", form$level, sep = "")) +
#'       ggplot2::scale_x_discrete(limits = unique(backlog$timestep)) +
#'       ggplot2::xlab("Timestep") +
#'       ggplot2::theme(legend.title = ggplot2::element_blank())
#'   }
#' }
#'
#'
#' plot.blockbuster <- function(blockbuster, type = "backlog", ...){
#'   if(type == "backlog"){
#'     warning("IF you want to explore visualisations it is better to use
#'           LongRepairBacklog(blockbuster) and pass the result to plot since
#'             plotting blockbuster objects involves calling LongRepairBacklog anyway.")
#'     x <- LongRepairBacklog(blockbuster)
#'   }
#'   if(type == "area"){
#'     warning("IF you want to explore visualisations it is better to use
#'           LongArea(blockbuster) and pass the result to plot since
#'           plotting blockbuster objects involves calling LongArea anyway.")
#'     x <- LongArea(blockbuster)
#'   }
#'   plot(x, ...)
#' }
#'
#'
#' plot.block <- function(block, ...){
#'   warning("IF you want to explore visualisations it is better to use
#'           LongRepairBacklog(block) and pass the result to plot since
#'           plotting block objects involves calling LongRepairBacklog anyway.")
#'   x <- LongRepairBacklog(block)
#'   plot(x, ...)
#' }
#'
#' plot.area <- function(area, ...){
#'   # if input is for one timestep only then use barchart
#'   if(max(area$timestep) == 0){
#'     ggplot2::ggplot(area) +
#'       ggplot2::geom_bar(ggplot2::aes(x = grade, weight = area)) +
#'       ggplot2::ylab(expression("Area in m"^"2")) +
#'       ggplot2::xlab("Grade") +
#'       ggplot2::theme(legend.title = ggplot2::element_blank())
#'   } else{
#'     ggplot2::ggplot(area, aes(x = grade, y = area)) +
#'       ggplot2::geom_bar(stat = "identity") +
#'       ggplot2::facet_wrap(~timestep) +
#'       ggplot2::ylab(expression("Area in m"^"2")) +
#'       ggplot2::xlab("Grade") +
#'       ggplot2::theme(legend.title = ggplot2::element_blank())
#'   }
#' }
#'
#'
#' plot.element <- function(element, ...){
#'   warning("IF you want to explore visualisations it is better to use
#'           LongArea(element) and pass the result to plot since
#'           plotting element objects involves calling LongArea anyway.")
#'   x <- LongArea(element)
#'   plot(x, ...)
#' }
#'
#' plot.element.list <- function(element.list, ...){
#'   warning("IF you want to explore visualisations it is better to use
#'           LongArea(element.list) and pass the result to plot since
#'           plotting element.list objects involves calling LongArea anyway.")
#'   x <- LongArea(element.list)
#'   plot(x, ...)
#' }
#'
#' plot.element.backlog <- function(element.backlog, ...){
#'   ggplot2::ggplot(element.backlog, aes(x = grade, y = backlog)) +
#'     ggplot2::geom_boxplot(outlier.colour = "red") +
#'     ggplot2::facet_wrap(~timestep)
#' }
#' #' #------------------------------------------------------------------------------#
#' #'
#' #'#' Find the most appropriate format for the axis of a backlog plot
#' #'
#' #' @param backlog A \code{\link{backlog}} class object.
#' #'
#' #' @return A list containing the magnitude by which to divide the currency,
#' #' and the suffix for the plot axis.
#' CurrencyFormat <- function(backlog){
#'   magnitude <- backlog %>%
#'     .$backlog %>% # select values
#'     log10 %>% # find magnitude
#'     max(na.rm=TRUE) %>% # maximum magnitude
#'     `/`(3) %>% # name level
#'     floor #round it off
#'   levels <- c("", "K", "M", "B", "Tr")
#'   return(list(magnitude = magnitude * 3, level = levels[magnitude + 1]))
#' }
#'
#'
#' pounds <- function(money){
#'   mag <- (log10(money) / 3) %>% floor
#'   paste(round(money / 10 ^ (mag * 3), 1), c("", "K", "M", "B", "Tr")[mag + 1], sep = "")
#' }
#'
#' percent <- function(percent){
#'   paste(round(percent, 1), "%", sep = "")
#' }

