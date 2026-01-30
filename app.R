library(shiny)
library(bslib)
library(thekidsbiostats)
library(readxl)
library(tools)

# Report rendering
library(quarto)
library(knitr)
library(scales)

source("R/helpers.R", local = TRUE)
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui, server)
