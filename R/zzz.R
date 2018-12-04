.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to the Blockbuster deterioration model developed by Mat Gregory and Peter Curtis.")
}

globalVariables(c("elementid", "buildingid", "grade", "unit_area", "year",
                  "blockbuster_pds_repair_costs", "blockbuster_det_data",
                  "unit_area", ".", "gifa", "B.repair.total", "C.repair.total",
                  "D.repair.total", "E.repair.total", "B.block.repair.cost",
                  "C.block.repair.cost", "D.block.repair.cost",
                  "E.block.repair.cost", "ab", "bc", "cd", "de", "A", "B", "C",
                  "D", "E", "area", "backlog", "block.rebuild.cost", "strategy",
                  "ratio", "aes", "plot", "timestep", "B.repair.cost",
                  "C.repair.cost", "D.repair.cost", "E.repair.cost"))
