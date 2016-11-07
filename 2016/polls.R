library(pollstR)
library(dplyr)
library(tidyr)

slug <- "2016-general-election-trump-vs-clinton"
elec_2016_polls <- pollster_chart_data(slug)

elec_2016_polls_tidy <- elec_2016_polls %>%
    gather(choice, value, one_of("Clinton", "Trump", "Undecided", "Other")) %>%
    mutate(date = start_date +
               difftime(end_date, start_date, units = "days") / 2) %>%
    filter(!is.na(value))
