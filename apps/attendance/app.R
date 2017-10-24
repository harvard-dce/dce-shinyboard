source("global.R")

dataFilePath <- function(filename) {
  file.path(data.dir, filename)
}

getTermData <- function(data_product, term) {
  file_name <- paste(data_product, term, 'csv', sep = '.')
  read.csv(dataFilePath(file_name), stringsAsFactors = F)
}

attendance <- NULL
rollcall <- NULL

server <- function(input, output, session) {
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
    attendance <<- getTermData('attendance', input$term)
    rollcall <<- getTermData('rollcall', input$term)
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
    req(input$course)
    message(paste0(c("course input: ", input$course)))

    # get the episodes for this course
    lectures <- dplyr::filter(
      episodes$episodes,
      series == input$course &
        is.na(available) == F &
        grepl("L", type)
    )

    total_duration <- sum(lectures$duration)

    if (nrow(lectures) > 0) {

      lectures <- mutate(
        lectures,
        available = as.Date(available),
        duration = lubridate::seconds_to_period(duration / 1000)
      )

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

    }

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
      td_period = seconds_to_period(total_duration / 1000)
      valueBox(
        value = sprintf('%02d:%02d:%02.0f', td_period@hour, td_period@minute, second(td_period)),
        subtitle = "Total Duration"
      )
    })

    if (nrow(lectures) > 0) {
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
      column(width = 3,
        valueBoxOutput(width = NULL, "totalLectures"),
        valueBoxOutput(width = NULL, "totalStudents"),
        valueBoxOutput(width = NULL, "totalDuration")
      ),
      column(width = 9,
        box(
          title = "Lecture Attendance Over Time",
          width = NULL,
          plotOutput("lecturePlot")
        )
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
