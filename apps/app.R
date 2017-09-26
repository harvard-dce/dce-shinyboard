
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
            windowTitle="Window Tab title"
        )
    ),

    navbarPage(
        title=div(img(src="Rlogo.png"), "Application NavBar Title"),
               inverse = F, # for diff color view
               theme = shinytheme("united"),

               tabPanel("Home Page 1", icon = icon("home"),

                        jumbotron("Hi ShinyLP!", "Call attention to important application features or provide guidance",
                                  buttonLabel = "Click Me"),
                         fluidRow(
                          column(6, panel_div(class_type = "primary", panel_title = "Directions",
                                              content = "How to use the app")),
                          column(6, panel_div("success", "Application Maintainers",
                                             "Email Me: <a href='mailto:jasmine.dumas@gmail.com?Subject=Shiny%20Help' target='_top'>Jasmine Dumas</a>"))
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

                        )),
               tabPanel("Home Page 2", icon = icon("cog"),
                        wells(content = "Imporant Info can go up here before a
                              user starts exploring the application and its features",
                              size = "default"),
                        h1("Hello Visitors!", align = "center"),
                        hr(),

                        list_group(c(list_item("Application Updates", badge_value = 27),
                                     list_item("User Updates", badge_value = 24)))


                        ),

               tabPanel("Home Page 3", icon = icon("calendar"),

                        jumbotron("Hello shinyLP!", "Dock Several Applications on a page as a portal",
                                  button = FALSE),
                        hr(),
                        fluidRow(
                          column(4, thumbnail_label(image = 'Rlogo.png', label = 'Application 1',
                                      content = 'Havana brown cornish rex bombay but bombay,
                                              but havana brown devonshire rex and devonshire rex.
                                              Tomcat egyptian mau. Cornish rex sphynx sphynx yet
                                              cougar and panther. Panther siberian. Lynx munchkin
                                              american shorthair. Norwegian forest. ',
                                      button_link = 'http://getbootstrap.com/', button_label = 'Click me')
                                 ),
                          column(4, thumbnail_label(image = 'Rlogo.png', label = 'Application 2',
                                                    content = 'Havana brown cornish rex bombay but bombay,
                                                    but havana brown devonshire rex and devonshire rex.
                                                    Tomcat egyptian mau. Cornish rex sphynx sphynx yet
                                                    cougar and panther. Panther siberian. Lynx munchkin
                                                    american shorthair. Norwegian forest. ',
                                                    button_link = 'http://getbootstrap.com/', button_label = 'Click me')),
                          column(4, thumbnail_label(image = 'Rlogo.png', label = 'Application 3',
                                                    content = 'Havana brown cornish rex bombay but bombay,
                                                    but havana brown devonshire rex and devonshire rex.
                                                    Tomcat egyptian mau. Cornish rex sphynx sphynx yet
                                                    cougar and panther. Panther siberian. Lynx munchkin
                                                    american shorthair. Norwegian forest. ',
                                                    button_link = 'http://getbootstrap.com/', button_label = 'Click me'))

                        )))

)

shinyApp(ui = ui, server = server)