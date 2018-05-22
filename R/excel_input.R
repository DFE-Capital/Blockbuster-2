load_forecast_horizon <- function(path = "./inst/Excel input.xlsx"){
  read_excel(path, range = "Inputs!G1", col_names = "forecast_horizon") %>% pull()
}

load_block_unit_rebuild <- function(path ="./inst/Excel input.xlsx"){
  read_excel(path, range = "Inputs!G2", col_names = "unit_rebuild_cost") %>% pull()
}

load_grade_order <- function(path ="./inst/Excel input.xlsx"){
  read_excel(path, range = "Inputs!G3", col_names = "grade_order") %>% pull() %>%
    strsplit("") %>% unlist()
}

load_location_factor <- function(path ="./inst/Excel input.xlsx"){
  read_excel(path, range = "Inputs!G4", col_names = "location_factor") %>% pull()
}

load_save_path <- function(path ="./inst/Excel input.xlsx"){
  read_excel(path, range = "Inputs!G6", col_names = "save_path") %>% pull()
}

load_file_label <- function(path ="./inst/Excel input.xlsx"){
  read_excel(path, range = "Inputs!G7", col_names = "file_label") %>% pull()
}

load_repair_budget <- function(path ="./inst/Excel input.xlsx", years){
  read_excel(path, range = paste0("Inputs!B2:B", years + 1), col_names = "repair_budget") %>% pull()
}

load_rebuild_budget <- function(path ="./inst/Excel input.xlsx", years){
  read_excel(path, range = paste0("Inputs!C2:C", years + 1), col_names = "rebuild_budget") %>% pull()
}

load_inflation <- function(path ="./inst/Excel input.xlsx", years){
  read_excel(path, range = paste0("Inputs!D2:D", years + 1), col_names = "inflation") %>% pull()
}

load_det_rates <- function(path ="./inst/Excel input.xlsx"){
  read_excel(path, range = "Deterioration rates!A1:E129") %>%
    rename(elementid = "Element ID")
}

load_repair_costs <- function(path ="./inst/Excel input.xlsx"){
  read_excel(path, range = "Repair costs!A1:E129") %>%
    rename(elementid = "Element ID")
}
