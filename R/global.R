
# all packages needed were imported here
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(stringr)
library(readr)
library(rlist)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(plotly)


# options setting here
# maxium upload size
options(shiny.maxRequestSize=100*1024^2)

admin_password <- "delete"
