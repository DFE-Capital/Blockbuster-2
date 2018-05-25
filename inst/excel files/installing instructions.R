library(httr)
set_config(use_proxy(url = paste0("http://ad\\",
                                  winDialogString("Setting proxy: Please enter username", "username"), ":",
                                  winDialogString("Setting proxy: Please enter Windows password", "password"),
                                  "@192.168.2.40:8080"), port = 8080))

devtools::install_github("DFE-Capital/Blockbuster-2", ref = "excel")

# note that by loading the package, the excel sheet is automatically set to the correct folder locations
library(Blockbuster2)

