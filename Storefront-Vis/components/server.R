library(tidyverse)
library(RColorBrewer)
library(shiny)
library(anesrake)
library(weights)
library(rmarkdown)
library(ggplot2)
library(scales)
library(purrr)
library(ids)

server <- function(input, output, session) {
  # If the survey doesn't exist, exit the app
  if (!exists("getAllResponses")) {
    stopApp()
    stop("App was started incorrectly")
  }
  
  f <- reactiveValues(l = list(), s=data.frame())
  
  # UI Generation ----
  output$dropdowns <- renderUI({
    req(survey())
    vars <- colnames(survey())
    return(tagList(
      selectInput("variable", "X-Axis variable", as.list(vars)),
      selectInput("variable2", "Y-Axis variable", list(`Single-Variable`=c("None"="NONE"), `Survey Vars`=vars))
    )
  )})
  observeEvent(input$exttra, {
    if (input$exttra != "NONE") {
      id <- random_id()
      f$l <- append(f$l, id)
      isolate(updateSelectInput(session, "exttra", selected="NONE"))
      isolate(updateSelectInput(session, id, selected=input$exttra))
    }
  })
  
  observe({
    # Find "NONE" filters
    f$l <- f$l[!sapply(f$l, is.null)]
    j <- lapply(f$l, function(x, y) {y[[x]]}, input)
    f$l <- f$l[j != "NONE"]
    # Because we changed f$l, the filters will now render again, this time without the elements that are now NONE
    # Remove duplicates
    f$l <- f$l[!duplicated(j)]
  })


  output$filters <- renderUI({
    print("rendering")
    req(survey())
    req(f$l)
    o <- tagList()
    nexttra <- selectInput("exttra", label="Add new filter", list(`Disable`=c("None"="NONE"), `Survey Vars`=colnames(survey())), selected="NONE")
    # Update filters
    if (is.null(isolate(input$exttra)))
      return(nexttra)
    if (length(f$l)) {
      for (fi in 1:length(f$l)) {
        if (is.null(f$l[[fi]]) || f$l[[fi]]=="NONE") next
        var <- input[[f$l[[fi]]]]
        dropdown <- selectInput(f$l[[fi]], label=NULL, list(`Disable`=c("None"="NONE"), `Survey Vars`=colnames(survey())), selected=var)
        o <- tagList(o, dropdown)
        if (!is.null(var)) {
          options <- lapply(as.list(as.character(unique(survey()[[var]]))), function(x) {if (is.na(x)) "NA" else x})
          sel <- isolate(input[[str_c(f$l[[fi]], "box")]])
          if (is.null(sel) || !any(sel %in% options))
            sel <- options
          print(sel)
          print(options)
          boxes <- checkboxGroupInput(str_c(f$l[[fi]], "box"), label=NULL, choices=options, selected=sel)
          o <- tagList(o, boxes)
        }
      }
    }
    o <- tagList(o, nexttra)
    o
    
  })
  output$raking <- renderUI({
    
  })
  
  # Call typeform API ----
  observe({
    invalidateLater(1000*10*60)
    req(input$typeform.surveyCode)
    req(input$typeform.authtoken)
    typeform.request_url <- str_glue("http://api.typeform.com/forms/{input$typeform.surveyCode}/responses")
    typeform.authorization <- str_glue("bearer {input$typeform.authtoken}")
    form <- list("url"=typeform.request_url,
                 "auth"=typeform.authorization)
    
    if (nrow(isolate(f$s)) == 0) {
      d <- getAllResponses(form)
      if (is.null(d))
        return()
      if (nrow(d) == 0)
        return()
      print(paste("Got ", nrow(d), " responses"))
      f$s <- d
    }
    else {
      lastSubmitted <- max(isolate(f$s$submitted_at))
      # The typeforms api returns timestamps like the following: 2018-06-12T21:07:40Z
      # But in queries, it expects them in a different format:   2018-06-12T21:07:40
      d <- getAllResponses(form, since=str_remove(lastSubmitted, "Z"))
      if (is.null(d))
        return ()
      if (nrow(d)) {
        d <- filter(d, !(token %in% f$s$token))
        print(paste("Got ", nrow(d), " new responses"))
        f$s <- bind_rows(isolate(f$s), d)
      }
      
    }
  })
  
  survey <- reactive({
    j   <- .survey
    j[] <- lapply(j, factor)
    return(j)
    if (nrow(f$s)) {
      j   <- select(f$s, everything(), -landed_at, -submitted_at, -token)
      j[] <- lapply(j, factor)
      j
    }
  })
  
  # Raking Call Reactive() ----
  processRake <- reactive({
    req(survey())
    # # Make a list of targets
    # tvars <- targets[names(targets) %in% input$rakevars]
    # # Call anesrake and return the weight vector
    # anesrake(tvars, survey, caseid=survey$caseID, cap=input$cap)$weightvec
    return(rep(1, nrow(survey())))
    
  })
  
  # Filter Reactive() ----
  inSample <- reactive({
    req(survey())
    req(input$variable)
    req(input$variable2)
    k <- (!is.na(survey()[[input$variable]]))  # Filter out na values in var1
    # Filter out na values in var2
    if (input$variable2 != "NONE")
      k <- k & (!is.na(survey()[[input$variable2]]))
    
    for (fil in f$l) {
      if (is.null(fil)) return()
      varName <- input[[fil]]
      if (is.null(varName)) return()
      allowed <- input[[str_c(fil,"box")]]
      if (is.null(allowed)) return()
      if ("NA" %in% allowed) {
        t <- k & ( survey()[[varName]] %in% allowed | map_lgl(survey()[[varName]], is.na))
      }
      else
        t <- k & (survey()[[varName]] %in% allowed)
      if (!sum(t)) return()
      k <- t
    }
    k
  })
  
  output$effect <- renderText({
    req(survey())
    paste("General Design Effect:", round(generaldesigneffect(processRake()), digits=3))
  })
  
  output$wheaders <- renderText({
    ifelse(input$noweight, "Unweighted Distribution:", "Weighted Distribution:")  # Main header
  })
  
  # Table Rendering ----
  output$weighted <- renderTable({dataTable()}, width='100%', height="auto")
  # This is a reactive so that we can pass it to the report
  dataTable <- reactive({
    req(survey())
    req(inSample())
    req(input$variable)
    req(input$variable2)
    ww <- if (input$noweight) rep(1, sum(inSample())) else processRake()[inSample()] # Get weights
    # Filter to subset and give default variable names
    s <- filter(survey(), inSample()) %>%
      mutate(w = ww) %>%
      select(v=input$variable, w, v2=if (input$variable2 == "NONE" | input$variable2 == input$variable) NULL else input$variable2)
    # If we have a var2
    if (input$variable2 != "NONE" && input$variable2 != input$variable) {
      # In-set distribution of var2
      v2D <- wtd.table(s$v2, s$w)
      # Set names to easily calculate n/N
      v2D <- setNames(v2D$sum.of.weights, v2D$x)
      v2C <- sum(v2D)
      
      # In-set distribution of var1
      vD <- wtd.table(s$v, s$w)
      vD <- setNames(vD$sum.of.weights, vD$x)
      vC <- sum(s$w)
      
      if (input$distrib.mode == "sample") {  # Distribution of var1 for survey
        vD_F <- wpct(survey()[[input$variable]], if (input$noweight) rep(1, nrow(survey())) else processRake())
        vC_F <- sum(if (input$noweight) rep(1, nrow(survey())) else processRake())
      }
      if (input$distrib.mode == "set") {  # Distribution of var1 for subset
        vD_F <- wpct(survey()[[input$variable]][inSample()], if (input$noweight) rep(1, sum(inSample())) else processRake()[inSample()])
        vC_F <- sum(if (input$noweight) rep(1, sum(inSample())) else processRake()[inSample()])
      }
      sc <- s %>%  # Start DPLYR chain
        group_by(v, v2) %>%
        summarise(n = sum(w)) %>% # Weight sums for each combination of var1 and var2
        mutate(n = n/map_dbl(factor(v2), function(c, k) {k[[c]]}, v2D)) %>%  # Divide the counts of each v2 | v1 by the counts for v2
        # If mode=real, don't change n
        # Otherwise, divide (v2 | v1) / (v2) by (v1) 
        mutate(n = switch(input$distrib.mode, "set"=, "sample"=n/map_dbl(factor(v), function(c, k) {k[[c]]}, vD_F), n)) %>%
        mutate(n = paste0(round(100*n), "%")) %>%  # Formatting
        spread(v2, n) %>% ungroup() %>%  # We want v2 to be columns
        mutate(n = paste0(round(vD))) %>%  # More formatting
        select("Option"=v, "N" = n, everything())  # Reorder columns
      
      sc$Option <- as.character(sc$Option)  # Needs to be char not factor so we can add "Base"
      k <- as.data.frame(sc, stringsAsFactors = F)  # Convert to df from tibble
      h <- list("Base", round(sum(vC)))  # "Base" - Count
      length(h) <- 2+length(v2D)  # Expand top row
      h[-1:-2] <- round(v2D)  # Insert v2 counts and round 
      return(rbind(h, k))
    } 
    # If no var2
    else {
      # Do we need to correct the subset to the entire survey?
      if (input$distrib.mode == "sample") {
        # If we do need to calculate the relative distribution, we need to calculate the following:
        # ([Count for variable category within subset]/[Same count for whole survey]) * ([Number of survey rows]/[Number of subset rows])
        # = (n/N) * (S/s)
        # Counts across the entire survey
        t <- wtd.table(survey()[[input$variable]], weights=if (input$noweight) rep(1, nrow(survey())) else processRake()) # = N
        r <- setNames(t$sum.of.weights, t$x)  # Names are necessary for ease of comparing in-subset and entire-survey counts
        S_r <- sum(processRake()) / sum(processRake()[inSample()])  # = S/s
      }
      sc <- s %>%  # DPLYR chain
        group_by(v) %>%
        summarise(co = sum(w)) %>%  # Sum the weights (weighted count) within each var
        # Either calculate n/N * S/s or n/s
        mutate(nd = switch(input$distrib.mode, "sample"=S_r*co/map_dbl(factor(v), function(c, k) {k[[c]]}, r), co/sum(processRake()[inSample()])))
      k <- data.frame("Option"=as.character(sc$v), "Amount"=paste0(round(100*sc$nd), "%"), stringsAsFactors=F)  # Format and prettify the data
      rbind(list("Base", round(sum(sc$co), digits=0)), k)  # Add a counts row, and return the counts row and the data
      
    }
  })
  
  # Plot Rendering ----
  output$weightedPlot <- renderPlot({barPlot()})
  barPlot <- reactive({
    req(survey())
    req(inSample())
    req(input$variable)
    req(input$variable2)
    if (input$variable2 != "NONE" && input$variable2 != input$variable) {  # 2-var versiom
      ggplot(data = barPlotData())  +  # Check data
        geom_bar(aes(x=v, y=co, fill=v2), stat="identity", position = "fill") +  # Draw bars
        scale_fill_brewer(palette= "Spectral") +  # Set Palette
        theme(axis.ticks.x=element_blank(),  # No vertical ticks bc bar plot
              axis.text.x=element_text(angle = -20, hjust = 0),  # Angle labels so they don't clip
              panel.grid.major.x = element_blank(),  # No vertical lines bc bar plot
              panel.grid.major.y = element_line(color="gray", size=0.5),
              panel.grid.minor.y = element_line(color="lightgray", size=0.25, lineend = "round"),
              panel.background = element_blank()) +  # No grey background
        scale_y_continuous(labels=percent) + 
        labs(x = input$variable, fill=input$variable2, y=switch(input$distrib.mode, "real"="Percent of choices", "sample"=,"set"="Ratio of X choice to all X choice"))
    } else { # 1-var version
      ggplot(data = barPlotData())  +
        geom_bar(aes(x=v, y=co, fill=v), stat="identity") +  # Color of bar is just visual now
        scale_fill_brewer(palette= "Spectral") + 
        theme(axis.ticks.x=element_blank(),
              axis.text.x=element_text(angle = -20, hjust = 0),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color="gray", size=0.5),
              panel.grid.minor.y = element_line(color="lightgray", size=0.25, lineend = "round"),
              panel.background = element_blank()) +
        scale_y_continuous(labels=switch(input$distrib.mode, "sample"=percent, waiver())) +  # If we're in real mode we're showing counts not %s
        labs(x = input$variable, fill=input$variable, y=switch(input$distrib.mode, "sample"="Ratio of choice distribution in subset to distribution across full sample", "Number of responses"))
    }})
  # Plot is split into plot and data so that interactive plots can re-render without recomputing
  barPlotData <- reactive({
    ww <- if (input$noweight) rep(1, sum(inSample())) else processRake()[inSample()]  # Load weights
    s <- filter(survey(), inSample()) %>%  # DPLYR chain
      mutate(w = ww) %>%  # Add weights to frame
      select(v=input$variable, w, v2=if (input$variable2 == "NONE" | input$variable2 == input$variable) NULL else input$variable2)  # Generic names
    if (input$variable2 != "NONE" && input$variable2 != input$variable) {  # 2-var version
      r <- switch(input$distrib.mode, "set"=wtd.table(s$v2, ww),  # Relative Distribution table
                  "sample" = wtd.table(survey()[[input$variable2]], if (input$noweight) rep(1, nrow(survey())) else processRake()))
      r <- setNames(r$sum.of.weights, r$x)
       
      return (s %>%  # DPLYR chain
        mutate(v=factor(v), v2=factor(v2)) %>%  # Convert to factors to make levels correct
        group_by(v, v2) %>%  # Group by both vars
        summarise(co = sum(w)) %>%  # And add up weights ("counts")
        mutate(co = switch(input$distrib.mode, "real" = co, "sample"=, "set"=co/map_dbl(factor(v2), function(c, k) {k[[c]]}, r)))  # Divide by sample
        )
    } 
    else {
      
      if (input$distrib.mode == "sample") { # If we have to do relative distribution
        t <- wtd.table(survey()[[input$variable]], weights=if (input$noweight) rep(1, nrow(survey())) else processRake())
        r <- setNames(t$sum.of.weights, t$x)
        S_r <- sum(processRake()) / sum(processRake()[inSample()])
      }
      return (s %>%
        group_by(v) %>%
        summarise(co = sum(w)) %>%
        mutate(co = switch(input$distrib.mode, "real"=co, "set"=co, "sample"=S_r*co/map_dbl(factor(v), function(c, k) {k[[c]]}, r))))
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
      
      # Directly pass in the formatted table and rendered (but not rasterized) graph

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