# BSD_2_clause

library(dplyr)
library(elastic)
# library(esadocs)
library(ggplot2)
library(ggthemes)
library(rio)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(stringdist)
library(stringr)
library(tokenizers)

system("touch restart.txt", intern = FALSE)

elastic::connect()

