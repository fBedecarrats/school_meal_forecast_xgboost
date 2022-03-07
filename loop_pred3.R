library(tidyverse)
library(shiny)
shiny::reactiveConsole(enabled = TRUE)

all_freqs <- dt()$freqs





filt_freqs <- all_freqs %>%
  dplyr::filter(date < lubridate::ymd("2020-01-31") | date > lubridate::ymd("2020-02-14")) %>%
  dplyr::filter(date < lubridate::ymd("2020-03-23") | date > lubridate::ymd("2020-07-03")) %>%
  dplyr::filter(date < lubridate::ymd("2021-03-08") | date > lubridate::ymd("2021-04-23"))

filt_freqs %>%
  readr::write_csv(index$path[index$name == "freqs"])

mypreds2 <- tibble::tribble(
       ~annee,          ~periode,       ~debut,         ~fin,    ~colpred, ~training_type, ~confidence,
  "2021-2022", "Hiver-Printemps", "2022-02-20", "2022-04-08",      "reel", "xgb_interval",         0.9,
  "2021-2022",   "Ete-Printemps", "2021-08-31", "2022-04-08", "prevision", "xgb_interval",         0.7
  )



for (i in 1:nrow(mypreds2)) {
  print(paste(mypreds2$annee[i], mypreds2$periode[i], mypreds2$colpred[i]))
  run_verteego(begin_date = mypreds2$debut[i],
               column_to_predict = mypreds2$colpred[i],
               data_path = "data",
               confidence = mypreds2$confidence[i],
               end_date = mypreds2$fin[i],
               start_training_date='2016-09-01',
               training_type=mypreds2$training_type[i],
               weeks_latency=1)
}


