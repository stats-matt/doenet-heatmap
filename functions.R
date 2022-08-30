#====================pull_dates=================================================
pull_dates <- function(events) {
  out <- events %>% select(timestamp)
  out <- anytime(out$timestamp)
  return(out)
}
#==================pull_versions================================================
pull_versions <- function(events) {
  out <- events %>% distinct(activityCid) %>% nrow()
  return(out)
}


####################################################################################

clean_events <- function(events, min_date, max_date) {
  #extractting the visible verb and making it into a separate column
  events <-
    events %>%
    mutate(visible = (verb == "isVisible" | verb == "experienced"))
  #add timestamp, calculate the time visible started (new time)
  events <-
    events %>%
    group_by(userId) %>%
    mutate(timestamp = anytime(timestamp)) %>%
    mutate(newtime = case_when(visible ~ timestamp - 60, !visible ~ timestamp)) %>%
    mutate(time = newtime - min(newtime)) %>%
    ungroup()
  events <- events[order(events$newtime),]
  #events<- events %>% filter(between(timestamp, min_date,max_date))
  events <-
    events %>%
    mutate(new = map(object, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(new)
  events <-
    events %>%
    mutate(new = map(
      result,
      ~ fromJSON(.) %>% as.data.frame() %>% mutate_if(is.numeric, as.character)
    )) %>%
    unnest(new)
  # mutually adding json to context when it's empty
  events$context[events$visible] <- "{\"visible1\":\"true\"}"
  events <-
    events %>%
    mutate(new = map(context, ~ fromJSON(., flatten = TRUE) %>% as.data.frame())) %>%
    unnest(new)
  return(events)
}

get_times <- function(data) {
  times <- data %>% 
    select(userId, starts_with("X._")) %>%
    rename_all(list( ~ str_replace(., "X._", ""))) %>%
    type_convert() %>%
    group_by(userId) %>%
    dplyr::summarize(across(everything(), ~ sum(.x, na.rm = T))) %>%
    ungroup() %>%
    select(-userId) %>%
    dplyr::summarize(across(everything(), ~ sum(.x, na.rm = T)))
  return(times)
}





#############################################################
# cleaning - everything above this is definitely needed
#############################################################


#================version_filter=================================================
version_filter <- function(cleaned, input_version) {
  out <- cleaned %>% filter(cleaned$version_num == input_version)
  return(out)
}

library(plyr)

summarize_events <- function(data) {
  out <-
    data %>%
    dplyr::select(
      -doenetId,
      -activityCid,
      -pageCid,
      -pageVariantIndex,
      -object,-context,
      -result,
      -visible1,
      -answerAncestor,
      -responseText,
      -componentType,
      -componentName,-version,
      -activityVariantIndex
    ) %>%
    group_by(userId, pageNumber) %>%
    #filter(!(is.na(itemCreditAchieved))) %>%
    group_by(item) %>%
    mutate(avg = mean(itemCreditAchieved)) %>%
    ungroup() %>%
    group_by(response) %>%
    add_count(response) %>%
    ungroup()
  return(out)
}


#========================time spent on each question============================

#keep only itemCreditAchueved==1.0 / correct answers OR isvisible)
add_ts <- function(data) {
  drop <- subset(data, itemCreditAchieved == 1.0 |
                   verb == 'isVisible')
  return(drop)
}


#calculate the difference of time for each user.
time_spent <- function(data) {
  out <-
    data %>%
    group_by(userId) %>%
    mutate(time_spent = prepend(diff(time), time[1]))  %>%
    ungroup()
  
  return(out)
}

#view only time related columns for debugging
view_time_spent <- function(data) {
  result <- data %>%
    dplyr::select(userId,
                  visible,
                  time,
                  item,
                  itemCreditAchieved,
                  timestamp,
                  newtime,
                  time_spent)
  return(result)
}

#making the dataframe for plot
drop <- function(data) {
  out <-
    data %>%
    subset(itemCreditAchieved == 1.0)  %>%
    subset(userId != 'xnhzpBB4t93gJNO6Jyesr') %>%  #hard coded to get rid of the error on this userid
    dplyr::select(item, time_spent, userId)
}


#if all questions have been answered, ignore all visible?
# clean_time_spent <- function(data){
#   out <-
#     data %>%
#     group_by(userId) %>%
#
#     ungroup()
#
#   return(out)
#
# }

