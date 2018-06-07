library(tidyverse)
library(RColorBrewer)
library(shiny)
library(anesrake)
library(weights)
library(rmarkdown)
library(ggplot2)
library(scales)

server <- function(input, output) {
  # Raking Call Reactive() ----
  processRake <- reactive({
    req(input$rakevars)
    req(input$cap)
    tvars <- targets[names(targets) %in% input$rakevars]
    anesrake(tvars, survey, caseid=survey$caseID, cap=input$cap)$weightvec
  })
  
  # Filter Reactive() ----
  inSample <- reactive({
    req(input$filter.race)
    req(input$filter.gender)
    req(input$filter.own)
    req(input$filter.education)
    req(input$filter.party)
    req(input$filter.age)
    req(input$filter.orientation)
    req(input$filter.region)
    (survey$Ethnic %in% input$filter.race) &
      (survey$Gender %in% input$filter.gender) &
      (survey$Own_Rent %in% input$filter.own) &
      (survey$Educ %in% input$filter.education) &
      (survey$Party %in% input$filter.party) &
      (survey$Age_Bracket %in% input$filter.age) &
      (survey$Persuasion %in% input$filter.orientation) &
      (survey$Region %in% input$filter.region)
  })
  
  output$effect <- renderText({
    paste("General Design Effect:", round(generaldesigneffect(processRake()), digits=3))
  })
  
  output$wheaders <- renderText({
    ifelse(input$noweight, "Unweighted Distribution:", "Weighted Distribution:")
  })
  
  # Table Rendering ----
  output$weighted <- renderTable({dataTable()}, width='100%', height="auto")
  dataTable <- reactive({
    ww <- if (input$noweight) rep(1, sum(inSample())) else processRake()[inSample()]
    s <- filter(survey, inSample()) %>%
      mutate(w = ww) %>%
      select(v=input$variable, w, v2=if (input$variable2 == "NONE" | input$variable2 == input$variable) NULL else input$variable2)
    if (input$variable2 != "NONE" && input$variable2 != input$variable) {
      
      v2D <- wtd.table(s$v2, s$w)
      v2D <- setNames(v2D$sum.of.weights, v2D$x)
      v2C <- sum(v2D)
      
      vD <- wtd.table(s$v, s$w)
      vD <- setNames(vD$sum.of.weights, vD$x)
      vC <- sum(s$w)
      
      if (input$distrib.mode == "sample") {
        vD_F <- wpct(survey[[input$variable]], if (input$noweight) rep(1, nrow(survey)) else processRake())
        vC_F <- sum(if (input$noweight) rep(1, nrow(survey)) else processRake())
      }
      if (input$distrib.mode == "set") {
        vD_F <- wpct(survey[[input$variable]][inSample()], if (input$noweight) rep(1, sum(inSample())) else processRake()[inSample()])
        vC_F <- sum(if (input$noweight) rep(1, sum(inSample())) else processRake()[inSample()])
      }
      sc <- s %>%
        mutate(v = factor(v), v2=factor(v2)) %>%
        group_by(v, v2) %>%
        summarise(n = sum(w)) %>%
        mutate(n = n/map_dbl(v2, function(c, k) {k[[c]]}, v2D)) %>%
        mutate(n = switch(input$distrib.mode, "set"=, "sample"=n/map_dbl(v, function(c, k) {k[[c]]}, vD_F), n)) %>%
        mutate(n = paste0(round(100*n), "%")) %>%
        spread(v2, n) %>% ungroup() %>%
        mutate(n = paste0(round(vD))) %>%
        select("Option"=v, "N" = n, everything())
      
      sc$Option <- as.character(sc$Option)
      k <- as.data.frame(sc, stringsAsFactors = F)
      h <- list("Base", round(sum(vC)))
      length(h) <- 2+length(v2D)
      h[-1:-2] <- round(v2D)
      return(rbind(h, k))
    } 
    else {
      
      if (input$distrib.mode == "sample") {
        t <- wtd.table(survey[[input$variable]], weights=if (input$noweight) rep(1, nrow(survey)) else processRake())
        r <- setNames(t$sum.of.weights, t$x)
        S_r <- sum(processRake()) / sum(processRake()[inSample()])
      }
      sc <- s %>%
        group_by(v) %>%
        summarise(co = sum(w)) %>%
        mutate(nd = switch(input$distrib.mode, "sample"=S_r*co/map_dbl(v, function(c, k) {k[[c]]}, r), co/sum(processRake()[inSample()])))
      k <- data.frame("Option"=as.character(sc$v), "Amount"=paste0(round(100*sc$nd), "%"), stringsAsFactors=F)
      rbind(list("Base", round(sum(sc$co), digits=0)), k)
      
    }
  })
  
  # Plot Rendering ----
  output$weightedPlot <- renderPlot({barPlot()})
  barPlot <- reactive({
    ww <- if (input$noweight) rep(1, sum(inSample())) else processRake()[inSample()]
    s <- filter(survey, inSample()) %>%
      mutate(w = ww) %>%
      select(v=input$variable, w, v2=if (input$variable2 == "NONE" | input$variable2 == input$variable) NULL else input$variable2)
    if (input$variable2 != "NONE" && input$variable2 != input$variable) {
      r <- switch(input$distrib.mode, "set"=wtd.table(s$v2, ww),
                  "sample" = wtd.table(survey[[input$variable2]], if (input$noweight) rep(1, nrow(survey)) else processRake()))
      r <- setNames(r$sum.of.weights, r$x)
       
      sc <- s %>%
        mutate(v=factor(v), v2=factor(v2)) %>%
        group_by(v, v2) %>%
        summarise(co = sum(w)) %>%
        mutate(n_d = switch(input$distrib.mode, "real" = co, "sample"=, "set"=co/map_dbl(v2, function(c, k) {k[[c]]}, r)))

    ggplot(data = sc)  +
      geom_bar(aes(x=v, y=n_d, fill=v2), stat="identity", position = "fill") +
      scale_fill_brewer(palette= "Spectral") + 
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_text(angle = -20, hjust = 0),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color="gray", size=0.5),
            panel.grid.minor.y = element_line(color="lightgray", size=0.25, lineend = "round"),
            panel.background = element_blank()) +
      scale_y_continuous(labels=percent) + 
      labs(x = input$variable, fill=input$variable2, y=switch(input$distrib.mode, "real"="Percent of choices", "sample"=,"set"="Ratio of X choice to all X choice"))
    } 
    else {
      
      if (input$distrib.mode == "sample") {
        t <- wtd.table(survey[[input$variable]], weights=if (input$noweight) rep(1, nrow(survey)) else processRake())
        r <- setNames(t$sum.of.weights, t$x)
        S_r <- sum(processRake()) / sum(processRake()[inSample()])
      }
      sc <- s %>%
        group_by(v) %>%
        summarise(co = sum(w)) %>%
        mutate(co = switch(input$distrib.mode, "real"=co, "set"=co, "sample"=S_r*co/map_dbl(v, function(c, k) {k[[c]]}, r)))
      ggplot(data = sc)  +
        geom_bar(aes(x=v, y=co, fill=v), stat="identity") +
        scale_fill_brewer(palette= "Spectral") + 
        theme(axis.ticks.x=element_blank(),
              axis.text.x=element_text(angle = -20, hjust = 0),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color="gray", size=0.5),
              panel.grid.minor.y = element_line(color="lightgray", size=0.25, lineend = "round"),
              panel.background = element_blank()) +
        scale_y_continuous(labels=switch(input$distrib.mode, "sample"=percent, waiver())) + 
        labs(x = input$variable, fill=input$variable, y=switch(input$distrib.mode, "sample"="Ratio of choice distribution in subset to distribution across full sample", "Number of responses"))
    }
  })
  
  # Report Generation ----
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.rmd")
      file.copy("data/report.rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document

      params <- list(plot = barPlot(),
                     table = dataTable()
      )
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}