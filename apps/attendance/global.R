rm(list=ls())

library(elastic)
library(elasticsearchr)
library(dplyr)
library(shiny)
library(stringr)
library(lubridate)
library(dotenv)
library(shinydashboard)
library(DT)
library(ggplot2)

source("config.R")
source("es.R")

