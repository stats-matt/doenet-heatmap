#Functions copied from analyizer 

library(plyr)

clean_events <- function(events,min_date,max_date) {
  
  #extractting the visible verb and making it into a separate column
  events <- 
    events %>%
    mutate (visible=(verb=="isVisible"))
  
  
  #add timestamp, calculate the time visible started (new time)
  events <-
    events %>%
    group_by(userId) %>%
    mutate(timestamp = anytime(timestamp))%>%
    mutate(newtime = case_when(visible ~ timestamp-30,
                               !visible ~ timestamp))%>%
    mutate(time = newtime - min(newtime))%>%
ungroup()
  
  events <- events[order(events$newtime),]
  
  
  #events<- events %>% filter(between(timestamp, min_date,max_date))
  
  events <-
    events %>%
    mutate(new = map(object, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(new)
  
  events <-
    events %>%
    mutate(new = map(result, ~ fromJSON(.)%>% as.data.frame() %>% mutate_if(is.numeric, as.character))) %>%
    unnest(new)
    
  # mutually adding json to context when it's empty
  events$context[events$visible] <-"{\"visible1\":\"true\"}"
  
  events <-
    events %>%
    mutate(new = map(context, ~ fromJSON(., flatten = TRUE) %>% as.data.frame())) %>% 
    unnest(new)
  
  return(events)
  
}


summarize_events <- function(data) {
  out <-
    data %>%
    dplyr::select( -doenetId, -activityCid, -pageCid, -pageVariantIndex,-object,
                   -context, -result, -visible1, -answerAncestor, -responseText, -componentType, -componentName,
                   -version,-activityVariantIndex ) %>%
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

#view only time related columns for debugging 
view_time_spent <- function(data){
  result <- data %>% 
    dplyr :: select(userId, visible, time, timstamp, newtime, times_pent)
  return(result)
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


