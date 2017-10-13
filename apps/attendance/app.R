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

  lectureAttendance <- function(students, mp.id) {
    nrow(filter(attendance, mpid == mp.id & huid %in% students$huid))
  }

  lectureScores <- function(series.id, mp.id) {
    students <- studentList(series.id)
    select()
  }

  studentAttendance <- function(lectures, student.id) {
    nrow(filter(attendance, huid == student.id & mpid %in% lectures$mpid))
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
        available = as.Date(available), # with_tz(ymd_hms(available, tz = default.tz)),
        duration = lubridate::seconds_to_period(duration / 1000)
      )

      message(paste0(c("course input: ", input$course)))

      students <- studentList(input$course)
      # generate the attended column
      lectures <- lectures %>% rowwise() %>% mutate(attended = lectureAttendance(students, mpid))

      output$lectureTable <- DT::renderDataTable({

        lectureTable <- transform(
          lectures,
          attendance = paste(attended, "of", nrow(students)),
          duration = sprintf('%02d:%02d:%02.0f', duration@hour, duration@minute, second(duration))
        )

        # prune to columns we want
        lectureTable <-
          dplyr::select(lectureTable, one_of(lecture.fields))
        # rearrange the columns
        lectureTable <- lectureTable[lecture.fields]
        # order by title
        lectureTable <- lectureTable[order(lectureTable$available), ]
      }) #, rownames = F)

      # get the student list for this course
      students <- dplyr::filter(rollcall, series == input$course & reg_level != "S")
      students <- students %>% rowwise() %>% mutate(attended = studentAttendance(lectures, huid))

      output$studentTable <- DT::renderDataTable({
        studentTable <- students %>% rowwise() %>% mutate(attendance = paste(attended, "of", nrow(lectures)))
        # create column contining full name
        studentTable <-
          dplyr::mutate(studentTable, name = paste(first_name, mi, last_name))

        studentTable <- studentTable[order(studentTable$name), ]

        # order by last name
        #studentTable <- studentTable[order(studentTable$last_name),]
        # prune to the columns we want
        studentTable <- select(studentTable, one_of(student.fields))

      }) #, rownames = F)

      output$totalLectures <- renderValueBox({
        valueBox(
          value = nrow(lectures),
          subtitle = "Total Lectures"
        )
      })

      output$totalStudents <- renderValueBox({
        valueBox(
          value = nrow(students),
          subtitle = "Total Students"
        )
      })

      output$totalDuration <- renderValueBox({
        valueBox(
          value = 50000,
          subtitle = "Total Duration"
        )
      })

      output$lecturePlot <- renderPlot({
        ggplot2::ggplot(lectures, aes(available, attended)) +
          geom_line() +
          scale_x_date(date_labels = "%b-%Y") +
          xlab("") +
          ylab("Attendence")
      })
    }
  },
  ignoreNULL = T,
  ignoreInit = T)
}

header <-  dashboardHeader(title = "Attendance Reports")

sidebar <-  dashboardSidebar(
  verticalLayout(
    selectInput("term", "Term:", term.options, selected = default.term),
    selectInput("course", "Course:", list())
  )
)

body <-  dashboardBody(
    fluidRow(
      valueBoxOutput("totalLectures"),
      valueBoxOutput("totalStudents"),
      valueBoxOutput("totalDuration")
    ),
    fluidRow(
      box(
        plotOutput("lecturePlot")
      )
    ),
    fluidRow(
      box(
        title="Lectures",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        dataTableOutput("lectureTable")
      )
    ),
    fluidRow(
      box(
        title="Students",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        dataTableOutput("studentTable")
      )
    )
)

ui <- dashboardPage(header, sidebar, body)
shinyApp(ui = ui, server = server)
