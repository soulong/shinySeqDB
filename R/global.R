
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

string_wrap <- function(stringlist, len=10) {
  re_s <- c()
  for(i in stringlist){
    re <- i %>%
            as.character() %>%
            str_extract_all(paste0(".{", len, "}|.{0,", len-1, "}$")) %>%
            `[[`(1) %>%
            str_c(collapse="\n")
    re_s <- c(re_s, re) }
  return(re_s)
}