## ----warning=FALSE, message=FALSE, error=FALSE---------------------------
library(Blockbuster2)

## ------------------------------------------------------------------------
data("simulated_elements", package = "Blockbuster2")
dplyr::glimpse(simulated_elements)

## ------------------------------------------------------------------------
data("simulated_blocks", package = "Blockbuster2")
dplyr::glimpse(simulated_blocks)

## ---- warnings = FALSE, collapse = TRUE----------------------------------
#  output demonstration

# construct single block input
my_output <- Blockbuster(simulated_elements, save = FALSE)
dplyr::glimpse(my_output) # this function makes the output nicer to read

## ------------------------------------------------------------------------
library(tidyr)

## ---- collapse = TRUE----------------------------------------------------
my_block <- pull_Block_Data(my_output)
glimpse(my_block)

## ------------------------------------------------------------------------
my_block %>% filter(year == 1, grade == "D", backlog > 100000)

## ------------------------------------------------------------------------
my_block %>%
  filter(year == 1) %>%
  group_by(buildingid) %>%
  summarise(total = sum(backlog)) %>%
  arrange(desc(total))

## ------------------------------------------------------------------------
library(ggplot2)
my_block %>% group_by(year, grade) %>%
  summarise(backlog = sum(backlog)) %>%
  ggplot() +
  geom_line(aes(x = year, y = backlog, colour = grade))

## ------------------------------------------------------------------------
pull_Element_Data(my_output) %>% glimpse
pull_Element_Data(my_output, type = "area") %>% glimpse
pull_Element_Data(my_output, type = "backlog") %>% glimpse

