is_element_level <- function(element){
  vars <- c("A", "B", "C", "D", "E", "ab", "bc", "cd", "de", "elementid",
            "buildingid", "B.repair.cost", "C.repair.cost", "D.repair.cost",
            "E.repair.cost", "B.repair.total", "C.repair.total",
            "D.repair.total", "E.repair.total", "gifa", "unit_area")
  all(vars %in% names(element))
}

is_block_level <- function(block){
  vars <- c("buildingid", "block.rebuild.cost", "B.block.repair.cost",
            "C.block.repair.cost", "D.block.repair.cost", "E.block.repair.cost",
            "ratio")
  all(vars %in% names(block))
}

