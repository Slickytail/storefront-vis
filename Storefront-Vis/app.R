library(shiny)
library(tidyverse)
library(anesrake)
library(weights)

source("components/load_data.R")
source("components/ui.R")
source("components/server.R")

shinyApp(ui, server)