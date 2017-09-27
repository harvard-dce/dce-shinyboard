#rm(list=ls())

library(elasticsearchr)
library(dplyr)
library(shiny)
library(stringr)
library(lubridate)

source("config.R")
source("es.R")

tryCatch({
  es <- elasticsearchr::elastic_version()
}, error = function(ex) {
  print("Unable to communicate with Elasticsearch");
  stopApp(1)
})

