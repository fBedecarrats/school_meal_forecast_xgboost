library(shiny)
# library(tidyverse)
reactiveConsole(TRUE)

my_piv <- pivs()[48,]
selected_dates <- reactive({ my_piv[1,3:4] })
selected_cafet <- reactive({ "Tous" })
input <- list(select_cafet = "Tous",
              select_period = dplyr::pull(my_piv[1,2]),
              select_year = dplyr::pull(my_piv[1,1]))
