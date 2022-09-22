#enableBookmarking(store = "url")

shinyUI(fluidPage(
  titlePanel("Doenet Heatmap"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      actionButton("update", "Update Data", icon = icon("sync")),
      #bookmarkButton(),
      downloadButton('downloadData', 'Download Data'),
      #h1("Compare experiments:"),
      #textInput("extra_id", "Extra DoenetID"),
      #actionButton("submit_extra", "Submit"),
      numericInput(
        "numid",
        "Number of Doenet IDs",
        value = 1,
        min = 1,
        max = 5,
        step = 1
      ),
      #actionButton("gennum","next"),
      
      #hard-coded ui for doenet ids
      uiOutput("rid"),
      uiOutput("id1"),
      uiOutput("id2"),
      uiOutput("id3"),
      uiOutput("id4"),
      uiOutput("id5"),
      actionButton("submit_extra", "Submit"),
      uiOutput("date_slider"),
      uiOutput("version_select")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Summary Table", dataTableOutput("time_table")),
        tabPanel("Time on each question", dataTableOutput("k_m")),
        tabPanel("short table", dataTableOutput("short")),
        tabPanel("plot", plotOutput("plot"))
        #tabPanel("Kmeans", plotOutput("k_m"))
      )
    )
  )
))