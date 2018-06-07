## ----warning=FALSE, message=FALSE, error=FALSE---------------------------
library(blockbuster2)

## ------------------------------------------------------------------------
data("simulated_elements", package = "blockbuster2")
dplyr::glimpse(simulated_elements)

## ------------------------------------------------------------------------
data("simulated_blocks", package = "blockbuster2")
dplyr::glimpse(simulated_blocks)

## ---- warnings = FALSE, collapse = TRUE----------------------------------
#  output demonstration

# construct single block input
my_output <- Blockbuster(simulated_elements, save = FALSE)
dplyr::glimpse(my_output) # this function makes the output nicer to read

## ------------------------------------------------------------------------
sum(my_output$"element summary"$backlog[my_output$"element summary"$year == 0])
sum(my_output$"element summary"$backlog[my_output$"element summary"$year == 1])

## ------------------------------------------------------------------------
my_output$"element summary" %>%
  group_by(year) %>%
  summarise(backlog = sum(backlog))

## ------------------------------------------------------------------------
library(ggplot2)
my_output$"element summary" %>%
  group_by(year, grade) %>%
  summarise(backlog = sum(backlog)) %>%
  ggplot() +
  geom_line(aes(x = year, y = backlog, colour = grade))

## ------------------------------------------------------------------------
formals(Blockbuster)

## ------------------------------------------------------------------------
simulated_elements[1:2, ] %>%
  select(elementid, ab, bc, cd, de, B.repair.cost, C.repair.cost, D.repair.cost, E.repair.cost)

