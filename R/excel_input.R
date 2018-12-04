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
  start_year <- as.numeric(inputs[7, 1])

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
              repair_costs = repair_costs,
              start_year = start_year))
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
    # drop rows with no repair cost or det rate
    drop_na(ab, bc, cd, de, B.repair.cost, C.repair.cost, D.repair.cost, E.repair.cost) %>%
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
  excel_path <- file.path(Sys.getenv("TEMP"), "blockbuster_excel.xlsm")
  temp_path <- file.path(Sys.getenv("TEMP"))

  log_line("pulling inputs from excel")
  # pull inputs from excel
  inputs <- create_input_element_from_excel(excel_path)
  time <- Sys.Date()

  # run Blockbuster
  log_line("Running Blockbuster model")
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


  log_line("producing summary")

  results$"element summary" %>%
    filter(grade %in% c("C", "D", "E")) %>%
    group_by(year) %>%
    summarise(backlog = sum(backlog)) %>%
    as.data.frame() %>% # write.xlsx doesn't like tbl_df for some things
    write.xlsx(file = file.path(temp_path, paste0("output", time, ".xlsx")),
               sheetName = "Summary",
               row.names = FALSE)

  results$"element summary" %>%
    group_by(year, grade) %>%
    summarise(area = sum(area), backlog = sum(backlog)) %>%
    as.data.frame() %>% # write.xlsx doesn't like tbl_df for some things
    write.xlsx(file = file.path(temp_path, paste0("output", time, ".xlsx")),
               sheetName = "Totals",
               row.names = FALSE,
               append = TRUE)

  log_line("Creating element summary")

  results$"element summary" %>%
    group_by(year, elementid) %>%
    summarise(area = sum(area), backlog = sum(backlog)) %>%
    as.data.frame() %>% # write.xlsx doesn't like tbl_df for some things
    write.xlsx(file = file.path(temp_path, paste0("output", time, ".xlsx")),
               sheetName = "Elements",
               row.names = FALSE,
               append = TRUE)

  data.frame(Year = 0:inputs$forecast_horizon,
             "Rebuild budget" = c(NA, inputs$rebuild_budget),
             "Building failures" = results$"building failures",
             "Number of rebuilds" = results$"Number of rebuilds",
             "Number of buildings in need of rebuilding" = results$"Number of buildings in need of rebuilding",
             "Cost of rebuilding in need buildings" = results$"Cost of rebuilding in need buildings"
             ) %>%
    as.data.frame() %>% # write.xlsx doesn't like tbl_df for some things
    write.xlsx(file = file.path(temp_path, paste0("output", time, ".xlsx")),
               sheetName = "Rebuild data",
               row.names = FALSE,
               append = TRUE)

  data.frame(Year = 1:inputs$forecast_horizon,
             "Rebuild budget" = inputs$rebuild_budget,
             "Repair budget" = inputs$repair_budget) %>%
    write.xlsx(file = file.path(temp_path, paste0("output", time, ".xlsx")),
               sheetName = "Inputs",
               row.names = FALSE,
               append = TRUE)

  log_line("Creating output charts")

  Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc") # This so the script looks for pandoc in the right place

  # auto generate knitted document
  render(file.path(lib_path, "excel files/output_template.Rmd"),
                    encoding = "UTF-8",
                    output_file = file.path(temp_path, paste0("output", time, ".docx")),
                    params = list(title = "Blockbuster Deterioration Model Output",
                                  subtitle = format(time, "%d %B %Y"),
                                  path = file.path(temp_path, paste0("output", time, ".xlsx")),
                                  forecast_horizon = inputs$forecast_horizon,
                                  block_rebuild_cost = inputs$unit_rebuild_cost,
                                  repair_order = inputs$grade_order,
                                  inflation = ifelse(all(inputs$inflation == 1), "no", "yes"),
                                  repair_money = paste(inputs$repair_budget, collapse = ", "),
                                  rebuild_money =paste(inputs$rebuild_budget, collapse = ", "),
                                  start_year = inputs$start_year)
                    )
return(list(temp_excel = file.path(temp_path, paste0("output", time, ".xlsx")),
            temp_doc = file.path(temp_path, paste0("output", time, ".docx")),
            excel = file.path(path, paste0("output", time, ".xlsx")),
            doc = file.path(path, paste0("output", time, ".docx"))))
}


#' Render output word document from output excel
#'
#' @return Generates a word document
#' @export
#'
#' @examples
render_blockbuster <- function(input, output, file){

  time <- Sys.Date()
  inputs <- create_input_element_from_excel(input)

  render(file.path(find.package("blockbuster2"), "excel files/output_template.Rmd"),
         encoding = "UTF-8",
         output_file = file,
         params = list(
           title = "Blockbuster Deterioration Model Output",
           subtitle = format(time, "%d %B %Y"),
           path = output,
           forecast_horizon = inputs$forecast_horizon,
           block_rebuild_cost = inputs$unit_rebuild_cost,
           repair_order = inputs$grade_order,
           inflation = ifelse(all(inputs$inflation == 1), "no", "yes"),
           repair_money = paste(inputs$repair_budget, collapse = ", "),
           rebuild_money =paste(inputs$rebuild_budget, collapse = ", "),
           start_year = inputs$start_year)
         )
}

