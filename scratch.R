library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)


# raw = stream_in(file(
#   paste0(
#     "https://www.doenet.org/api/getEventData.php?doenetId[]=_e4AbpmZyr2uiRRE7VPfZl"
#   )
# ))

events = raw$events[[1]]

dates <- pull_dates(events)
versions <- pull_versions(events)

cleaned <- clean_events(events, min(dates), max(dates))

cleaned <- cleaned_versions

summary_data <- summarize_events(cleaned)

times <-
  cleaned %>%
  select(userId, starts_with("X._")) %>%
  rename_all(list( ~ str_replace(., "X._", ""))) %>%
  type_convert() %>%
  group_by(userId) %>%
  dplyr::summarize(across(everything(), ~ sum(.x, na.rm = T))) %>%
  ungroup() %>%
  select(-userId) %>%
  dplyr::summarize(across(everything(), ~ sum(.x, na.rm = T)))
times

times <- get_times(cleaned)
times
