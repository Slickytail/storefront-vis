library(tidyverse)
library(RColorBrewer)
library(shiny)
library(anesrake)
library(weights)
library(rmarkdown)
library(scales)
library(ids)

server <- function(input, output, session) {
  # If the survey doesn't exist, exit the app
  if (!exists("getAllResponses")) {
    stopApp()
    stop("App was started incorrectly")
  }
  
  f <- reactiveValues(filter_ids=list(), filter_vals=list(), s=data.frame())
  
  # UI Generation ----
  output$dropdowns <- renderUI({
    req(survey())
    vars <- colnames(survey())
    return(tagList(
      selectInput("variable", "X-Axis variable", as.list(vars)),
      selectInput("variable2", "Y-Axis variable", list(`Single-Variable`=c("None"="NONE"), `Survey Vars`=vars))
    )
  )})
  
  observeEvent(input$addfilter, {
    if (input$newvar %in% f$filter_vals) {
      cat(file=stderr(), "Canceled new dropdown because it would be a duplicate\n")
      return()
    }
    # After the user has clicked new
    id <- random_id()  # Generate a random ID for the new element
    f$filter_ids <- append(f$filter_ids, id)  # Add it to the ID list
    f$filter_vals <- append(f$filter_vals, input$newvar)  # Add what variable it stores to the list
    rmbutton <- actionButton(str_c(id, "rm"), label=NULL, icon=icon("minus"))  # Generate an action button
    
    observeEvent(input[[str_c(id, "rm")]], {  # Create an observer for this action button.
      # First we need to remove the filter from internal data
      ind <- which(f$filter_ids == id)
      f$filter_vals <- f$filter_vals[-ind]
      f$filter_ids <- f$filter_ids[-ind]
      # Next we need to remove the actual UI elements
      # Because we wrapped them in a div we can delete it easily
      removeUI(str_c("#", id))
    }, domain=session, once=TRUE)  # This observer should die after one run or after the page is closed.
    
    options <- lapply(as.character(unique(survey()[[input$newvar]])), function(x) {if (is.na(x)) "NA" else x})  # Get checkbox options
    checks <- checkboxGroupInput(str_c(id, "box"), label=NULL, choices=options, selected=options)  # Create the checkboxes 
    insertUI("#addfilterline", where="beforeBegin", ui=div(id=id, fluidRow(column(11, helpText(input$newvar)), column(1, rmbutton)), checks))  # Wrap the layout and insert it
  })
  
  

  output$raking <- renderUI({
    
  })
  
  # Call typeform API ----
  observe({
    invalidateLater(1000*60*30)  # = 1000 ms/s * 60 s/m * 30 m
    req(input$typeform.surveyCode)
    req(input$typeform.authtoken)
    typeform.request_url <- str_glue("http://api.typeform.com/forms/{input$typeform.surveyCode}/responses")
    typeform.authorization <- str_glue("bearer {input$typeform.authtoken}")
    form <- list("url"=typeform.request_url,
                 "auth"=typeform.authorization)
    
    if (nrow(isolate(f$s)) == 0) {  # Don't depend on f$s
      d <- getAllResponses(form)  # This is the first run. We get everything
      if (is.null(d))
        return()
      if (nrow(d) == 0)
        return()
      cat(file=stderr(), paste("Got ", nrow(d), " responses\n"))
      f$s <- d
      shinyjs::disable("typeform.surveyCode")  # Disable the shiny connection input to prevent surveys from being loaded on top of each other
      shinyjs::disable("typeform.authtoken")
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
        cat(file=stderr(), paste("Got ", nrow(d), " new responses\n"))
        f$s <- bind_rows(isolate(f$s), d)  # Join the survey to the file
      }
      
    }
  })
  
  survey <- reactive({
    if (nrow(f$s)) {
      j   <- select(f$s, everything(), -landed_at, -submitted_at, -token)
      j[] <- lapply(j, factor)
      isolate(updateSelectInput(session, "newvar", choices=colnames(j)))  # Whenever the survey is updated it's a good time to update the filter var options
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
    # Start by setting up a logical vector in the same length as the survey
    k <- (!is.na(survey()[[input$variable]]))  # Filter out na values in var1
    # Filter out na values in var2
    if (input$variable2 != "NONE")
      k <- k & (!is.na(survey()[[input$variable2]]))
    if (length(f$filter_ids)) {
      for (il in 1:length(f$filter_ids)) {  # For each filter
        varName <- f$filter_vals[[il]]
        fil <- f$filter_ids[[il]]
        allowed <- input[[str_c(fil,"box")]]
        if ("NA" %in% allowed) {
          t <- k & ( survey()[[varName]] %in% allowed | map_lgl(survey()[[varName]], is.na))
        }
        else
          t <- k & (survey()[[varName]] %in% allowed)
        if (!sum(t)) return()  # If nothing is selected, it's more obvious to the user to not display a graph or table than to just skip this filter
        k <- t
      }
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
  .barPlotTheme <- reactive({
    ggplot() +
    scale_fill_brewer(palette= "Spectral") +  # Set Palette
    theme(axis.ticks.x=element_blank(),  # No vertical ticks bc bar plot
          axis.text.x=element_text(angle = -20, hjust = 0),  # Angle labels so they don't clip
          panel.grid.major.x = element_blank(),  # No vertical lines bc bar plot
          panel.grid.major.y = element_line(color="gray", size=0.5),
          panel.grid.minor.y = element_line(color="lightgray", size=0.25, lineend = "round"),
          panel.background = element_blank())# No grey background
  })

  barPlot <- reactive({
    req(input$variable)
    req(input$variable2)
    if (input$variable2 != "NONE" && input$variable2 != input$variable) {  # 2-var versiom
      .barPlotTheme() + 
        geom_bar(data = barPlotData(), aes(x=v, y=co, fill=v2), stat="identity", position = "fill") +  # Draw bars
        scale_y_continuous(labels=percent) + 
        labs(x = input$variable, fill=input$variable2, y=switch(input$distrib.mode, "real"="Percent of choices", "sample"=,"set"="Ratio of X choice to all X choice"))
    } else { # 1-var version
      .barPlotTheme() + 
        geom_bar(data = barPlotData(), aes(x=v, y=co, fill=v), stat="identity") +  # Color of bar is just visual now
        scale_y_continuous(labels=switch(input$distrib.mode, "sample"=percent, function(x) {paste0(round(x))})) +  # If we're in real mode we're showing counts not %s
        labs(x = input$variable, fill=input$variable, y=switch(input$distrib.mode, "sample"="Ratio of choice distribution in subset to distribution across full sample", "Number of responses"))
    }})
  # Plot is split into plot and data so that interactive plots can re-render without recomputing
  barPlotData <- reactive({
    req(survey())
    req(inSample())
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