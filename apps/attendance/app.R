source("global.R")

data_file_path <- function(filename) {
  file.path(data.dir, filename)
}

server <- function(input, output, session) {
  attendance <- read.csv(data_file_path("attendance.csv"))
  rollcall <- read.csv(data_file_path("rollcall.csv"), stringsAsFactors = F)
  episodes <- reactiveValues(episodes = list())

  makeSparkline <- function() {
    df <- data.frame(x = floor(runif(14, min = 0, max = 100)))
    sparkline(df$x, type = "bar")
  }

  studentList <- function(series.id) {
    filter(rollcall, series == series.id & reg_level != "S")
  }

  lectureAttendance <- function(series.id, mp.id) {
    students <- studentList(series.id)
    attended <-
      filter(attendance, mpid == mp.id & huid %in% students$huid)
    paste(nrow(attended), "of", nrow(students))
  }

  lectureScores <- function(series.id, mp.id) {
    students <- studentList(series.id)
    select()
  }

  studentAttendance <- function(lectures, student.id) {
    attended <-
      filter(attendance, huid == student.id & mpid %in% lectures$mpid)
    paste(nrow(attended), "of", nrow(lectures))
  }

  observeEvent(input$term, {
    episodes$episodes <- episodesByTerm(input$term)
  })

  observe({
    courses <- dplyr::distinct(episodes$episodes, series, course)
    courses <- courses[order(courses$course), ]
    choices <-
      c(list("Select A Course" = ""), with(courses, split(courses$series, courses$course)))
    isolate({
      updateSelectInput(session, "course", choices = choices)
    })
  })

  observeEvent(input$course, {
    if (input$course != "") {
      # get the episodes for this course
      lectures <- dplyr::filter(
        episodes$episodes,
        series == input$course &
          is.na(available) == F &
          grepl("L", type)
      )
      # format duration, available date, and add attendance column
      lectures <- mutate(
        lectures,
        available = with_tz(ymd_hms(available, tz = default.tz)),
        duration = paste(as.integer((
          duration / 1000
        ) / 60), "m", sep = "")
      )

      #      sparkline.data <-
      #      sparkline.style <- "type: 'line'"
      #      sparkline.coldefs <- list(list(targets = 1, render = JS("function(data, type, full){ return '<span class=sparkSamples>' + data + '</span>' }")))
      #      sparkline.callback <- JS(paste0("function (oSettings, json) {\n  $('.sparkSamples:not(:has(canvas))').sparkline('html', { ", sparkline.style, " });\n}"), collapse = "")
      #
      output$lectureTable <- renderDataTable({
        # generate the attendance column
        browser()
        message(paste0(c("course input: ", input$course)))
        lectureTable <-
          lectures %>% rowwise() %>% mutate(attendance = lectureAttendance(input$course, mpid))
        # prune to columns we want
        lectureTable <-
          dplyr::select(lectureTable, one_of(lecture.fields))
        # rearrange the columns
        lectureTable <- lectureTable[lecture.fields]
        # order by title
        lectureTable <- lectureTable[order(lectureTable$available), ]
      }) #, rownames = F)

      # get the student list for this course
      students <-
        dplyr::filter(rollcall, series == input$course & reg_level != "S")

      output$studentTable <- renderDataTable({
        studentTable <-
          students %>% rowwise() %>% mutate(attendance = studentAttendance(lectures, huid))
        # create column contining full name
        studentTable <-
          dplyr::mutate(studentTable, name = paste(first_name, mi, last_name))

        # insert random stuff for demo
        studentTable <- studentTable[1:20, ]
        studentTable$name <- random.names
        studentTable$huid <-
          sample(10000000:99999999, 20, replace = FALSE)
        studentTable <- studentTable[order(studentTable$name), ]

        # order by last name
        #studentTable <- studentTable[order(studentTable$last_name),]
        # prune to the columns we want
        studentTable <- select(studentTable, one_of(student.fields))

      }) #, rownames = F)
    }
  },
  ignoreNULL = T,
  ignoreInit = T)
}

ui <- fluidPage(verticalLayout(
  # Application title
  titlePanel("Lecture Attendance Reports"),
  fluidRow(column(
    6,
    selectInput("term", "Term:", term.options, selected = default.term)
  ),
  column(
    6,
    selectInput("course", "Course:", list())
  )),
  column(12,
         tabsetPanel(
           tabPanel(
             "Lectures",
             tags$head(tags$style(HTML(
               ' .tab-content {margin-top: 20px;}'
             ))),
             dataTableOutput("lectureTable")
           ),
           tabPanel(
             "Students",
             tags$head(tags$style(HTML(
               ' .tab-content {margin-top: 20px;}'
             ))),
             dataTableOutput("studentTable")
           )
         ))
))

shinyApp(ui = ui, server = server)
