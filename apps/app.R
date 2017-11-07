
library(shiny)
library(shinyBS)
library(shinyLP)
library(shinythemes)

# Define server logic
server <- function(input, output, session) { }

# Define UI for application
ui <- fluidPage(

    list(
        tags$head(
            HTML('<link rel="icon", href="logo.png", type="image/png" />')
        )
    ),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
            title="",
            windowTitle="DCE Learning Analytics Dashboard"
        )
    ),
    navbarPage(
        title=div(img(src="dce.gif", width="30", height="30"), "DCE Learning Analytics"),
        inverse = F, # for diff color view
        theme = shinytheme("united"),
        tabPanel("Home",
                 icon = icon("home"),
                 jumbotron("Welcome!", "Call attention to important application features or provide guidance",
                           buttonLabel = "Intro Video"),
                 fluidRow(
                     column(4,
                          thumbnail_label(image = 'Rlogo.png',
                              label = 'Attendance Reports',
                              content = 'Havana brown cornish rex bombay but bombay,
                                         but havana brown devonshire rex and devonshire rex.
                                         Tomcat egyptian mau. Cornish rex sphynx sphynx yet
                                         cougar and panther. Panther siberian. Lynx munchkin
                                         american shorthair. Norwegian forest. ',
                              button_link = '/attendance',
                              button_label = 'Click me')
                     ),
                     column(4,
                           thumbnail_label(image = 'Rlogo.png',
                              label = 'Lecture Views',
                              content = 'Havana brown cornish rex bombay but bombay,
                                         but havana brown devonshire rex and devonshire rex.
                                         Tomcat egyptian mau. Cornish rex sphynx sphynx yet
                                         cougar and panther. Panther siberian. Lynx munchkin
                                         american shorthair. Norwegian forest. ',
                              button_link = '/shiny2',
                              button_label = 'Click me')
                     ),
                     column(4,
                           thumbnail_label(image = 'Rlogo.png',
                              label = 'Live Viewing',
                              content = 'Havana brown cornish rex bombay but bombay,
                                         but havana brown devonshire rex and devonshire rex.
                                         Tomcat egyptian mau. Cornish rex sphynx sphynx yet
                                         cougar and panther. Panther siberian. Lynx munchkin
                                         american shorthair. Norwegian forest. ',
                              button_link = '/live',
                              button_label = 'Click me')
                     )
                  ),
                  fluidRow(
                      column(6, panel_div("info", panel_title = "Directions",
                             content = "How to use the app")),
                      column(6, panel_div("success", "Application Developers",
                             tags$ul(
                                  tags$li(tags$a(href="mailto:jay_luker@harvard.edu", "Jay Luker")),
                                  tags$li(tags$a(href="mailto:phoebemiller@g.harvard.edu", "Phoebe Miller"))
                                              )
                          ))
                  ),  # end of fluidRow
                  fluidRow(
                      column(6, panel_div("info", "App Status", "Include text with status, version and updates")),
                      column(6, panel_div("danger", "Security and License", "Copyright 2016")),

                      #### FAVICON TAGS SECTION ####
                      tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),

                      #### JAVASCRIPT TAGS SECTION #### - ENABLE WHEN READY
                      #tags$head(tags$script(src='pl.js')),

                      bsModal("modalExample", "Instructional Video", "tabBut", size = "large" ,
                          p("Additional text and widgets can be added in these modal boxes. Video plays in chrome browser"),
                          iframe(width = "560", height = "315", url_link = "https://www.youtube.com/embed/0fKg7e37bQE")
                      )
                  )
         ),
         tabPanel("About",
              wells(content = "Imporant Info can go up here before a
                    user starts exploring the application and its features",
              size = "default"),
              h1("Hello Visitors!", align = "center"),
              hr(),
              list_group(c(
                list_item("Application Updates", badge_value = 27),
                list_item("User Updates", badge_value = 24)
            ))
         )
    )
)


shinyApp(ui = ui, server = server)
