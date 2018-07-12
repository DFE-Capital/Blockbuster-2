#' Title
#'
#' @param path
#'
#' @return
#'
#' @examples
load_excel_inputs <- function(path){

  if(!file.exists(path)) {
    print(paste0("ERROR: Cannot access excel sheet at ", path))
    Sys.sleep(10)
  }
  message("Loading blockbuster inputs from excel sheet.")
  # load inputs
  inputs <- read.xlsx(path, sheetName = "Model", colIndex = 2, stringsAsFactors = FALSE, header = FALSE)
  forecast_horizon <- as.numeric(inputs[2, 1])
  unit_rebuild_cost <- as.numeric(inputs[3, 1])
  grade_order <- strsplit(inputs[4, 1], "")[[1]]
  location_factor <- as.logical(inputs[5, 1])
  save <- as.logical(inputs[6, 1])
  data_path <- inputs[1, 1]

  # load input budgets
  budgets <- read.xlsx(path, sheetName = "Inputs", colIndex = 2:4)
  repair_budget <- budgets$Repair.budget[1:forecast_horizon]
  rebuild_budget <- budgets$Rebuild.budget[1:forecast_horizon]
  inflation <- budgets$Inflation[1:forecast_horizon]

  # load deterioration rates
  det_rates <- read.xlsx(path, sheetName = "Deterioration rates",
                         colIndex = 1:8, stringsAsFactors = FALSE) %>%
    rename(elementid = "Element.ID")

  # load repair costs
  repair_costs <- read.xlsx(path, sheetName = "Repair costs",
                            colIndex = 1:8, stringsAsFactors = FALSE) %>%
    rename(elementid = "Element.ID")

  message("Inputs loaded")

  # return as list
  return(list(forecast_horizon = forecast_horizon,
              unit_rebuild_cost = unit_rebuild_cost,
              grade_order = grade_order,
              location_factor = location_factor,
              save = save,
              data_path = data_path,
              repair_budget = repair_budget,
              rebuild_budget = rebuild_budget,
              inflation = inflation,
              det_rates = det_rates,
              repair_costs = repair_costs))
}

#' Title
#'
#' @param path
#'
#' @return
#'
#' @examples
create_input_element_from_excel <- function(path = "./excel files/Excel input.xlsm"){

  inputs <- load_excel_inputs(path)

  if(!file.exists(inputs$data_path)) {
    message(paste0("ERROR: Cannot load estate data from ", inputs$data_path))
    Sys.sleep(10)
  }

  message(paste0("Loading data from ", inputs$data_path))

  data <- readRDS(file.path(inputs$data_path))

  # remove det rates and repair costs if already in the data
  if("B.repair.cost" %in% names(data$element)){
    data$element <- data$element %>%
      select(-B.repair.cost, -C.repair.cost, -D.repair.cost, -E.repair.cost)
  }

  if("ab" %in% names(data$element)){
    data$element <- data$element %>%
      select(-ab, -bc, -cd, -de)
  }

  # add the deterioration rates and repair costs to the data.
  inputs$element <- data$element %>%
    left_join(inputs$det_rates, by = "elementid", suffix= c("", "_IGNORE")) %>%
    left_join(inputs$repair_costs, by = "elementid", suffix = c("", "_IGNORE")) %>%
    # add in the building gifa from the data$building table as this is needed to
    # compute block rebuild costs
    left_join(data$building %>% select(buildingid, gifa), by = "buildingid") %>%
    # update the repair totals
    UpdateElementRepairs()

  return(inputs)
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
blockbuster_excel <- function(path){

  lib_path <- file.path(find.package("blockbuster2"))
  excel_path <- file.path(path, "Excel input.xlsm")

  message("pulling inputs from excel")
  # pull inputs from excel
  inputs <- create_input_element_from_excel(excel_path)
  time <- Sys.Date()

  # run Blockbuster
  message("Running Blockbuster model")
  results <- try(Blockbuster(element.data = inputs$element,
              forecast.horizon = inputs$forecast_horizon,
              rebuild.money = inputs$rebuild_budget,
              repair.money = inputs$repair_budget,
              block.rebuild.cost = inputs$unit_rebuild_cost,
              inflation = inputs$inflation,
              save = as.logical(inputs$save),
              path = file.path(path, "output"),
              grade.order = inputs$grade_order),
              outFile = file.path(path, "blockbusterlog.txt"))


  message("producing summary")

  results$"element summary" %>%
    filter(grade %in% c("C", "D", "E")) %>%
    group_by(year) %>%
    summarise(backlog = sum(backlog)) %>%
    as.data.frame() %>% # write.xlsx doesn't like tbl_df for some things
    write.xlsx(file = file.path(path, paste0("output", time, ".xlsx")),
               sheetName = "Summary",
               row.names = FALSE)

  results$"element summary" %>%
    group_by(year, grade) %>%
    summarise(area = sum(area), backlog = sum(backlog)) %>%
    as.data.frame() %>% # write.xlsx doesn't like tbl_df for some things
    write.xlsx(file = file.path(path, paste0("output", time, ".xlsx")),
               sheetName = "Totals",
               row.names = FALSE,
               append = TRUE)

  message("Creating element summary")

  results$"element summary" %>%
    group_by(year, elementid) %>%
    summarise(area = sum(area), backlog = sum(backlog)) %>%
    as.data.frame() %>% # write.xlsx doesn't like tbl_df for some things
    write.xlsx(file = file.path(path, paste0("output", time, ".xlsx")),
               sheetName = "Elements",
               row.names = FALSE,
               append = TRUE)

  message("Creating output charts")

  Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc") # This so the script looks for pandoc in the right place

  # TODO auto generate knitted document
  render(file.path(lib_path, "excel files/output_template.Rmd"),
                    encoding = "UTF-8",
                    output_file = file.path(path, paste0("output", time, ".docx")),
                    params = list(title = "Blockbuster Deterioration Model Output",
                                  subtitle = format(time, "%d %B %Y"),
                                  path = file.path(path, paste0("output", time, ".xlsx")),
                                  forecast_horizon = inputs$forecast_horizon,
                                  block_rebuild_cost = inputs$unit_rebuild_cost,
                                  repair_order = inputs$grade_order,
                                  inflation = ifelse(all(inputs$inflation == 1), "no", "yes"),
                                  repair_money = paste(inputs$repair_budget, collapse = ", "),
                                  rebuild_money =paste(inputs$rebuild_budget, collapse = ", "))
                    )

}
