#' Component level data of similar size to the Property Data Survey with
#' randomized grades and unit_area.
#'
#' A test dataset of the same size as the Property Data Survey dataset, but with
#' randomized grades and unit_areas.
#'
#' @format An \code{\link{element}} class data frame with 2,679,527 rows and 23
#' variables:
#' \describe{
#' \item{buildingid}{Unique key identifying the building.}
#' \item{elementid}{Unique key identifying the type of component down to the
#' sub-element level.}
#' \item{unit_area}{The area of the component used for calculations of repair
#' costs.  This may not be equivalent to the actual size of the component for
#' certain types where area is not a suitable measure, e.g. Lifts.}
#' \item{A}{Proportion of area at grade A.}
#' \item{B}{Proportion of area at grade B.}
#' \item{C}{Proportion of area at grade C.}
#' \item{D}{Proportion of area at grade D.}
#' \item{ab}{Deterioration rate from grade A to grade B.}
#' \item{bc}{Deterioration rate from grade B to grade C.}
#' \item{cd}{Deterioration rate from grade C to grade D.}
#' \item{B.repair.cost}{Cost per unit area of repairing grade B.}
#' \item{C.repair.cost}{Cost per unit area of repairing grade C.}
#' \item{D.repair.cost}{Cost per unit area of repairing grade D.}
#' \item{B.repair.total}{Cost of repairing all grade B area.}
#' \item{C.repair.total}{Cost of repairing all grade C area.}
#' \item{D.repair.total}{Cost of repairing all grade D area.}
#' \item{gifa}{Gross Internal Floor Area.  This is used to determine the cost of
#' rebuilding each block.}
"test.element"



#' Building level data for the test component level dataset.
#'
#' A test dataset of the same size as the Property Data Survey dataset, but with
#' randomized grades and unit_areas. Note that this object
#' can be derived from \code{\link{test.element}} using
#' \code{\link{ConvertElementToBlock}}, that is
#' \code{ConvertElementToBlock(test.element)}
#'
#' @format A \code{\link{block}} class data frame with 67,009 rows and 7
#' variables:
#' \describe{
#' \item{buildingid}{Unique key identifying the building.}
#' \item{block.rebuild.cost}{The estimated cost to rebuild the block based on a
#' unit cost of £2507.50}.
#' \item{B.block.repair.cost}{Cost of repairing all grade B component area in a
#' block.}
#' \item{C.block.repair.cost}{Cost of repairing all grade C component area in a
#' block.}
#' \item{D.block.repair.cost}{Cost of repairing all grade D component area in a
#' block.}
#' \item{ratio}{The ratio between the total cost of repairing all grade C and D
#' component areas in a block and the cost to rebuild the block.  Used for
#' deciding what to rebuild.}
"test.block"


