#' Small toy element-level dataset
#'
#' A simulated element-level dataset compatible with \code{\link{Blockbuster}}.
#' It contains simulated data for 50 buildings with a total of 1944 elements
#' with deterioration rates, repair costs, grades and other information used by
#' the package.
#'
#' The dataset \code{\link{simulated_blocks}} summarises this dataset at block-
#' level.
#'
#' @format A data frame with 1944 rows and 21 variables:
#' \describe{
#'   \item{buildingid}{A key uniquely identifying the building the component is
#'     part of}
#'   \item{elementid}{A key uniquely identifying the component}
#'   \item{unit_area}{A quantification of the component size}
#'   \item{gifa}{The gross internal floor area of the building the component is
#'     part of}
#'   \item{B.repair.cost}{The cost to repair one unit area at grade B}
#'   \item{C.repair.cost}{The cost to repair one unit area at grade C}
#'   \item{D.repair.cost}{The cost to repair one unit area at grade D}
#'   \item{E.repair.cost}{The cost to repair one unit area at grade E}
#'   \item{ab}{The yearly deterioration probability from A to B}
#'   \item{bc}{The yearly deterioration probability from B to C}
#'   \item{cd}{The yearly deterioration probability from C to D}
#'   \item{de}{The yearly deterioration probability from D to E}
#'   \item{A}{The probability the component is at grade A}
#'   \item{B}{The probability the component is at grade B}
#'   \item{C}{The probability the component is at grade C}
#'   \item{D}{The probability the component is at grade D}
#'   \item{E}{The probability the component is at grade E}
#'   \item{B.repair.total}{The expected cost to repair the component at grade B}
#'   \item{C.repair.total}{The expected cost to repair the component at grade C}
#'   \item{D.repair.total}{The expected cost to repair the component at grade D}
#'   \item{E.repair.total}{The expected cost to repair the component at grade E}
#' }
"simulated_elements"

#' Small toy block-level dataset
#'
#' A simulated block-level dataset compatible with \code{\link{Blockbuster}}. It
#' contains simulated data for 50 buildings and is a summary of the data in
#' \code{\link{simulated_elements}}
#'
#' @format A dataframe with 50 rows and 7 variables:
#' \describe{
#'   \item{buildingid}{A key uniquely identifiying the block}
#'   \item{block.rebuild.cost}{The cost of rebuilding the block}
#'   \item{B.block.repair.cost}{The expected cost of repairing all components in
#'     the block at grade B}
#'   \item{C.block.repair.cost}{The expected cost of repairing all components in
#'     the block at grade C}
#'   \item{D.block.repair.cost}{The expected cost of repairing all components in
#'     the block at grade D}
#'   \item{E.block.repair.cost}{The expected cost of repairing all components in
#'     the block at grade E}
#'   \item{ratio}{The efficiency of rebuilding rather than repairing}
#' }
"simulated_blocks"
