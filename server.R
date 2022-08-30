library(shiny)
library(tidyverse)
library(jsonlite)
library(anytime)
library(dplyr)
library(magrittr)

shinyServer(function(input, output) {
  source("./functions.R")
  
  #==========PRE CLEANING INPUTS==================================================
  
  #maximum 5 ids to compare/ hard coded
  #So this code is going through and adding one extra Doenet ID entry box
  # for each one requested in input$numid. If (for example) you only request
  # two, then the remaining 3 output IDs will be set to NULL and not shown
  # It feels like we could accomplish this with a loop, but after multiple
  # attempts, none of us have been successful.
  output$rid <-
    renderUI({
      i = 1
      j = input$numid
      if (i <= j) {
        output$id1 <- renderUI({
          textInput("id1", "Doenet ID 1")
        })
        i = i + 1
      } else{
        output$id1 = NULL
      }
      if (i <= j) {
        output$id2 <- renderUI({
          textInput("id2", "Doenet ID 2")
        })
        i = i + 1
      } else{
        output$id2 = NULL
      }
      if (i <= j) {
        output$id3 <- renderUI({
          textInput("id3", "Doenet ID 3")
        })
        i = i + 1
      } else{
        output$id3 = NULL
      }
      if (i <= j) {
        output$id4 <- renderUI({
          textInput("id4", "Doenet ID 4")
        })
        i = i + 1
      } else{
        output$id4 = NULL
      }
      if (i <= j) {
        output$id5 <- renderUI({
          textInput("id5", "Doenet ID 5")
        })
        i = i + 1
      } else{
        output$id5 = NULL
      }
    })
  
  
  #Slider for time in the time plots
  output$time_slider_normal <-
    renderUI({
      sliderInput(
        "times_normal",
        min = 0,
        "Maximum time shown:",
        max = input$maxtime_set_normal,
        value =  c(500, 10000)
      )
      
    })
  
  output$time_slider_version <-
    renderUI({
      sliderInput(
        "times_version",
        min = 0,
        "Maximum time shown:",
        max = input$maxtime_set_version,
        value =  c(500, 10000)
      )
      
    })
  
  
  #These next two lines pull data directly from events (before it is cleaned)
  # that is crucial to determining the date and version selection on the sidebar.
  # As a rule we have typically tried to avoid working on events directly, but
  # because this determines how we clean we have made an exception.
  dates <- reactive(pull_dates(events()))
  versions <- reactive(pull_versions(events()))
  
  #This outputs the version selection and the date slider for the UI
  output$version_select = renderUI({
    selectInput("version_selected", "Version: ", c(1:versions()))
  })
  output$date_slider = renderUI({
    sliderInput(
      "date_range",
      "Data from: ",
      min = min(dates()),
      max = max(dates()),
      value = c(min(dates()),
                max(dates()))
    )
  })
  
  #==========================GETTING DATA=========================================
  # What this code is doing is pulling in the data
  # getQueryString() is a function that takes a query string and turns it into
  # a list, which allows us to find the "data" item of that list.
  # By default it pulls the query string from the app environment (?)
  # renderText turns it that list into a string and then checks if it is null
  # This is a check to make sure we are in fact looking for data that exists
  
  #Stream_in unpacks the json file we get from the URL into a 1 by 3 dataframe
  #First element is a boolean that tells if it was successful or not
  #Second element is a message (typically empty right now)
  #Third is the list that contains the event log
  #df contains this 1 by 3 frame at the end of this block
  df <- eventReactive(input$submit_extra | input$update, {
    if (input$submit_extra != 0) {
      end_of_link = paste0(
        "&doenetId[]=",
        input$id1,
        "&doenetId[]=",
        input$id2,
        "&doenetId[]=",
        input$id3,
        "&doenetId[]=",
        input$id4,
        "&doenetId[]=",
        input$id5
      )
      
    }
    else{
      end_of_link = ""
    }
    stream_in(file(
      paste0(
        "https://www.doenet.org/api/getEventData.php?doenetId[]=",
        #"_YImZRcgrUqyNBLHd0tbP2" # for debugging to have a set doenetid to use
        getQueryString()[["data"]],
        #"_e4AbpmZyr2uiRRE7VPfZl", # matts example id
        end_of_link
      )
    ))
  })
  
  #=================================PROCESSING DATA===============================
  
  events <- reactive({
    df()$events[[1]]
  })
  
  
  
  #Input from date slider determines which dates are included in the set.
  # was cleaned_version
  cleaned <- reactive({
    clean_events(events(), input$date_range[1], input$date_range[2])
  })
  
  cleaned_version <- cleaned
  
  summary_data_version <-
    reactive({
      summarize_events(cleaned_version())
    })
  
  #Filter takes in previously cleaned data and then the version we select
  #cleaned = reactive({version_filter(cleaned_version(), input$version_selected)})
  
  # summary_data = reactive({summarize_events(cleaned())})
  
  
  #=========================DOWNLOADING DATA======================================
  #This gives allows the user to download the data shown in a csv file for their
  #own purposes
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('events-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(events(), file)
    }
  )
  
  #=========================DATA TABLES===========================================
  # creates a table of cleaned data
  output$cleaned_data <- renderDataTable(cleaned())
  
  # creates a table of raw data
  output$raw <- renderDataTable(events())
  
  
  #=======================SUMMARY TEXT============================================
  #creates an output text detailing how many students in the data set
  output$num_students <-
    renderText(paste0(
      "There is/are ",
      n_distinct(events()$userId, na.rm = TRUE),
      " student(s)"
    ))
  #creates an output text detailing how many versions are present in the set
  output$num_versions <-
    renderText(paste0("There are ", versions()))
  
  #creates an output text detailing how many different doenet experiments
  #are represented in this set.
  output$num_doenetIds <-
    renderText(paste0(
      "There is/are ",
      n_distinct(events()$doenetId, na.rm = TRUE),
      " doenetId(s)"
    ))
  #creates an output text detailing how many pages are included in this dataset
  output$num_pages <-
    renderText(paste0(
      "There is/are ",
      n_distinct(summary_data_version()$pageNumber, na.rm = TRUE),
      " page(s)"
    ))
  
  #==========================================================================
  
  dropped <-  reactive({
    add_ts(summary_data_version())
  })
  time_s <-  reactive({
    time_spent(dropped())
  })
  short_r <-  reactive({
    view_time_spent(time_s())
  })
  drop_r <-  reactive(drop(short_r()))
  
  output$k_m <- renderDataTable(time_s())
  output$short <- renderDataTable(short_r())
  
  #plotting the bar graph of time on each question
  output$plot <-
    renderPlot(ggplot(drop_r(), aes(item, time_spent)) +
                 geom_bar(stat = "identity") +
                 facet_wrap( ~ userId))

  times <- reactive(get_times(cleaned()))
  
  output$time_table <- 
    renderDataTable(times())
  
  
  
  
  
  })