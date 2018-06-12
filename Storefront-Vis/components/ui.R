library(shiny)

# If the survey doesn't exist, let's return a blank UI with an error message
if (exists("getAllResponses")) {

surveyInfo <- tabPanel("Connect to typeform",
         h3("Typeform connect"),
         textInput("typeform.surveyCode", "ID of typeform survey"),
         textInput("typeform.authtoken", "Authorization token")
          )
# Output Options ----
varsPanel <- tabPanel("Variables",
         h3("Data Output"),
         # Dropdown to select primary variable
         uiOutput("dropdowns"),
         selectInput("distrib.mode", "Relative Distribution Mode",
                     # Display distribution as it appears in the sample
                     list("Real Distribution" = "real",
                          # Divide distributions of var2 | var1 by distribution of var2 in subset
                          "Relative to Subset" = "set",
                          # Divide subset distribution of var1 or (var2 | var1) by distribution of var1 or var2 in the whole survey
                          "Relative to entire sample" = "sample")),
         downloadButton("report", "Generate report")
)
# Filters ----
filterPanel <- tabPanel("Filters",
         # Creation of subsets to exclude certain choices
         h3("Sample Filtering"),
         uiOutput("filters")
         # fluidRow(
         #   # Two columns of checkboxes
         #   column(6,
         #          checkboxGroupInput("filter.race", "Subset Ethnicity", choices = levels(survey$Ethnic), selected=levels(survey$Ethnic)),
         #          checkboxGroupInput("filter.education", "Subset Education", choices = levels(survey$Educ), selected=levels(survey$Educ)),
         #          checkboxGroupInput("filter.gender", "Subset Gender", choices = levels(survey$Gender), selected=levels(survey$Gender)),
         #          checkboxGroupInput("filter.region", "Subset Region", choices = levels(survey$Region), selected=levels(survey$Region))
         #   ),
         # 
         #   column(6,
         #          checkboxGroupInput("filter.party", "Subset Party", choices = levels(survey$Party), selected=levels(survey$Party)),
         #          checkboxGroupInput("filter.age", "Subset Age Group", choices = levels(survey$Age_Bracket), selected=levels(survey$Age_Bracket)),
         #          checkboxGroupInput("filter.own", "Subset Home Owners", choices = levels(survey$Own_Rent), selected=levels(survey$Own_Rent)),
         #          checkboxGroupInput("filter.orientation", "Subset Sexual Orientation", choices = levels(survey$Persuasion), selected=levels(survey$Persuasion))
         #   )
         # 
         # )
)
# Raking/weighting options ----
weightPanel <- tabPanel("Weighting",
         # Raking options, user probably doesn't want to mess with these
         h3("Weighting"),
         checkboxInput("noweight", "Disable weighting?"),
         conditionalPanel(condition = "!input.noweight",
                          # checkboxGroupInput("rakevars", "Raking Variables",
                          #                    choices = list("Party" = "RakeParty",
                          #                                   "Gender" = "Gender",
                          #                                   "Age" = "Age_Bracket",
                          #                                   "Ethnicity" = "RakeEthnic",
                          #                                   "Birthplace" = "RakeBorn",
                          #                                   "Home Ownership" = "RakeOwn_Rent",
                          #                                   "Education" = "RakeEduc"),
                          #                    selected = c("RakeParty","Gender", "Age_Bracket", "RakeEthnic", "RakeBorn", "RakeOwn_Rent", "RakeEduc")),
                          uiOutput("raking"),
                          # Anesrake weight cap for any row
                          sliderInput("cap", "Weight cap", min = 4, max = 20, value = 7)
          )
)


# fluidPage call ----
ui <- fluidPage(
  titlePanel("CA Opinion Demographics"),
  
  sidebarLayout(
    sidebarPanel(
      # Panels we declared earlier
      tabsetPanel(
        varsPanel,
        filterPanel,
        weightPanel,
        surveyInfo
    )),
    # Main Panel ----
    mainPanel(
      # Either "Weighted distribution" or "Unweighted distribution"
      h3(textOutput("wheaders")),
      # The main plot output
      plotOutput("weightedPlot", width = "100%", height="50vh"),
      # Confusing table output
      tableOutput("weighted"),
      # If the survey is weighted, display the general design effect
      conditionalPanel(condition = "!input.noweight",
                       # This creates a tooltip on hover that explains what general design effect is
                       tags$div(title="Variance of weighted results. Lower is better. 1 is perfect.", textOutput("effect")))
      
    )
  )
  
)

} else {
  # We can try stopping the app before it loads, but that can fail
  stopApp()
  # Display an error.
  ui <- fluidPage(h4("Error"), p("survey wasn't loaded at time of UI generation. Is the app being loaded properly?"))
}

