library(shiny)

source("components/typeform.R", local=TRUE)
source("components/ui.R", local=TRUE)
source("components/server.R", local=TRUE)

shinyApp(ui, server)