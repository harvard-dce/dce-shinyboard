require(sparkline)
require(DT)
require(shiny)

# create data
spark_data1<- data.frame(id = c('spark1', 'spark2'),
                         spark = c("1,2,3", "3,2,1"))



ui <- fluidPage(
  sparklineOutput("test_spark"),
  DT::dataTableOutput("tbl")
)

server <- function(input, output) {

  line_string <- "type: 'line', lineColor: 'black', fillColor: '#ccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'"

  cd <- list(list(targets = 1, render = JS("function(data, type, full){ return '<span class=sparkSamples>' + data + '</span>' }")))

  cb = JS(paste0("function (oSettings, json) {\n  $('.sparkSamples:not(:has(canvas))').sparkline('html', { ",
                 line_string, " });\n}"), collapse = "")

  output$tbl <- DT::renderDataTable({
    dt <-  DT::datatable(as.data.frame(spark_data1),  rownames = FALSE, options = list(columnDefs = cd,fnDrawCallback = cb))
  })
}

shinyApp(ui = ui, server = server)
