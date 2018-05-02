library(tidyverse)
input <- readxl::read_excel("Excel input.xlsx", sheet = "Inputs")
repair_funds <- input %>% select(`Repair budget`) %>% as.numeric
rebuild_funds <- input %>% select(`Rebuild budget`) %>% as.numeric
inflation <- input %>% select(Inflation) %>% as.numeric
years <- names(input)[7] %>% as.numeric
block_rebuild_cost <- input[1, 7] %>% as.numeric
rebuild_order <- input[2, 7] %>% pull() %>% strsplit("") %>% unlist()
location_factor <- input[3, 7] %>% as.logical
save_path <- input[5, 7]
save_label <- input[6, 7]


