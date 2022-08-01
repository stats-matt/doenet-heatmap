#Functions copied from analyizer 

library(plyr)

clean_events <- function(events,min_date,max_date) {
  
  #This block adds the timestamp column to the cleaned data set
  events <-
    events %>%
    group_by(userId) %>%
    mutate(timestamp = anytime(timestamp)) %>%
    mutate(time = timestamp - min(timestamp))
  #%>%
  # ungroup()
  
  
  #events<- events %>% filter(between(timestamp, min_date,max_date))
  
  events <-
    events %>%
    mutate(new = map(object, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(new)
  
  # result is causing the X._problem columns
  # from a single 
  events <-
    events %>%
    mutate(new = map(result, ~ fromJSON(.)%>% as.data.frame() %>% mutate_if(is.numeric, as.character))) %>%
    unnest(new)
  
  
  events <- 
    events %>%
    mutate (visible=(verb=="isVisible"))
  
  events$context[events$visible] <-"{\"visible1\":\"true\"}"
  
  # parse context when context not blank
  events <-
    events %>%
    mutate(new = map(context, ~ fromJSON(., flatten = TRUE) %>% as.data.frame())) %>% 
    unnest(new)
  
  return(events)
  
}


summarize_events <- function(data) {
  out <-
    data %>%
    dplyr::select(userId, verb, visible, item,itemCreditAchieved,pageCreditAchieved, time, timestamp, pageNumber, 
              response,creditAchieved) %>%
    group_by(userId, pageNumber) %>%
    #filter(!(is.na(itemCreditAchieved))) %>% 
    group_by(item) %>% 
    mutate(avg = mean(itemCreditAchieved))%>% 
    ungroup() %>%
    group_by(response) %>% 
    add_count(response) %>%
    ungroup()
  return(out)
}


#========================time spent on each question============================

#keep only itemCreditAchueved==1.0 / correct answers OR isvisible)
add_ts <- function(data){
  drop <- subset(data, itemCreditAchieved==1.0 | verb == 'isVisible')
  return(drop)
}

#calculate the difference of time for each user.
time_spent <-function(data){
  out <-
    data %>%
    group_by(userId) %>%
    mutate(time_spent = prepend(diff(time),time[1]))  %>%
    ungroup()
  
  return(out)
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


