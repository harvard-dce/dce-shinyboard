source("global.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (!is.null(query[['mpid']])) {
      updateTextInput(session, "mpid", value = query[['mpid']])
    }
  })

  dtv <- reactive({
    req(input$mpid)
    mpid_totalviews(input$mpid)
  })

  output$distPlot <- renderPlot({
    showGuide <- input$guide
    plot_mpid_totalviews(dtv(), guide = input$guide, vline = input$scrubber)
  })
}

# Define UI for application that draws
body <- dashboardBody(
  fluidPage(
    fluidRow(
      box(
        title="Student Viewing",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        plotOutput("distPlot")
      )
    )
  )
)

header <-  dashboardHeader(title = "Student Viewing")

sidebar <-  dashboardSidebar(
  verticalLayout(
    textInput("mpid", "Mediapackage ID:"),
    sliderInput("scrubber",NULL, min = 0, max = 3000, value = 0, step = 30, post="s", animate=TRUE),
    checkboxInput("guide", "Show Huid")
  )
)

# Run the application
ui <- dashboardPage(header, sidebar, body)
shinyApp(ui = ui, server = server)

