library(shiny)

# If the survey doesn't exist, let's return a blank UI with an error message
if (exists("survey")) {

# Output Options ----
varsPanel <- tabPanel("Variables",
         h3("Data Output"),
         # Dropdown to select primary variable
         selectInput("variable", "X-axis variable",
                     list("Plans to Vote" = "GOTV",
                          "Political View" = "PoliticaView",
                          "View on Immigration" = "Immirgr",
                          "Most Important Issue" = "ImpIssue",
                          "Governor choice" = "FIrstBallot",
                          "Social Media Use" = "Social",
                          "Television Use" = "WatchTV",
                          # Passing named vectors allows us to create a section
                          `"Would you be likely to support a governor who supported this policy?"` = c(
                            "Universal preschool and Kindergarden" = "ChildEd",
                            "Improve state advocacy for dreamers" = "Dreamers",
                            "Create a housing trust fund" = "HousingTr",
                            "Protect Obamacare and expand Medicare" = "Healthcare",
                            "Make it easier to start a business" = "Business",
                            "Allow local governments to invest in affordable housing" = "InvestHous",
                            "Create a workforce training fund" = "WorkTrain"),
                          `Opinions of People` = c(
                            "Opinion of Donald Trump" = "TrumpFav",
                            "Opinion of Jerry Brown" = "BrownFav",
                            "Opinion of Dianne Feinstein" = "DFFav",
                            "Opinion of Antonio Villaraigosa" = "AVFav",
                            "Opinion of Gavin Newscom" = "GNFav",
                            "Opinion of John Chiang" = "JChFav",
                            "Opinion of John Cox" = "JCoFav",
                            "Opinion of Delaine Easton" = "DEFav",
                            "Opinion of Kamala Harris" = "KHFav"),
                          `Demographics` = c(
                            "Ethnicity" = "Ethnic",
                            "Gender" = "Gender",
                            "Home Ownership" = "Own_Rent",
                            "Education" = "Educ",
                            "Party" = "Party",
                            "Age" = "Age_Bracket",
                            "Sexual Orientation" = "Persuasion",
                            "Region" = "Region"
                            
                          )
                     )
         ),
         selectInput("variable2", "Second Variable",
                     list(`Only display one variable` = c("None" = "NONE"),  # Dummy section to indicate that this is a different option
                          `Fill with another variable` = c(
                            "Plans to Vote" = "GOTV",
                            "Political View" = "PoliticaView",
                            "View on Immigration" = "Immirgr",
                            "Most Important Issue" = "ImpIssue",
                            "Governor choice" = "FIrstBallot",
                            "Social Media Use" = "Social",
                            "Television Use" = "WatchTV"),
                          `"Would you be likely to support a governor who supported this policy?"` = c(
                            "Universal preschool and Kindergarden" = "ChildEd",
                            "Improve state advocacy for dreamers" = "Dreamers",
                            "Create a housing trust fund" = "HousingTr",
                            "Protect Obamacare and expand Medicare" = "Healthcare",
                            "Make it easier to start a business" = "Business",
                            "Allow local governments to invest in affordable housing" = "InvestHous",
                            "Create a workforce training fund" = "WorkTrain"),
                          `Opinions of People` = c(
                            "Opinion of Donald Trump" = "TrumpFav",
                            "Opinion of Jerry Brown" = "BrownFav",
                            "Opinion of Dianne Feinstein" = "DFFav",
                            "Opinion of Antonio Villaraigosa" = "AVFav",
                            "Opinion of Gavin Newscom" = "GNFav",
                            "Opinion of John Chiang" = "JChFav",
                            "Opinion of John Cox" = "JCoFav",
                            "Opinion of Delaine Easton" = "DEFav",
                            "Opinion of Kamala Harris" = "KHFav"),
                          `Demographics` = c(
                            "Ethnicity" = "Ethnic",
                            "Gender" = "Gender",
                            "Home Ownership" = "Own_Rent",
                            "Education" = "Educ",
                            "Party" = "Party",
                            "Age" = "Age_Bracket",
                            "Sexual Orientation" = "Persuasion",
                            "Region" = "Region"
                            
                          )
                     )
         ),
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
         fluidRow(
           # Two columns of checkboxes
           column(6, 
                  checkboxGroupInput("filter.race", "Subset Ethnicity", choices = levels(survey$Ethnic), selected=levels(survey$Ethnic)),
                  checkboxGroupInput("filter.education", "Subset Education", choices = levels(survey$Educ), selected=levels(survey$Educ)),
                  checkboxGroupInput("filter.gender", "Subset Gender", choices = levels(survey$Gender), selected=levels(survey$Gender)),
                  checkboxGroupInput("filter.region", "Subset Region", choices = levels(survey$Region), selected=levels(survey$Region))
           ),
           
           column(6, 
                  checkboxGroupInput("filter.party", "Subset Party", choices = levels(survey$Party), selected=levels(survey$Party)),
                  checkboxGroupInput("filter.age", "Subset Age Group", choices = levels(survey$Age_Bracket), selected=levels(survey$Age_Bracket)),
                  checkboxGroupInput("filter.own", "Subset Home Owners", choices = levels(survey$Own_Rent), selected=levels(survey$Own_Rent)),
                  checkboxGroupInput("filter.orientation", "Subset Sexual Orientation", choices = levels(survey$Persuasion), selected=levels(survey$Persuasion))
           )
           
         )
)
# Raking/weighting options ----
weightPanel <- tabPanel("Weighting",
         # Raking options, user probably doesn't want to mess with these
         h3("Weighting"),
         checkboxInput("noweight", "Disable weighting?"),
         conditionalPanel(condition = "!input.noweight",
                          checkboxGroupInput("rakevars", "Raking Variables", 
                                             choices = list("Party" = "RakeParty", 
                                                            "Gender" = "Gender", 
                                                            "Age" = "Age_Bracket",
                                                            "Ethnicity" = "RakeEthnic",
                                                            "Birthplace" = "RakeBorn",
                                                            "Home Ownership" = "RakeOwn_Rent",
                                                            "Education" = "RakeEduc"), 
                                             selected = c("RakeParty","Gender", "Age_Bracket", "RakeEthnic", "RakeBorn", "RakeOwn_Rent", "RakeEduc")),
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
        weightPanel
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
  # Display an error. The server will check if inputs exist and stop it.
  ui <- fluidPage(h4("Error"), p("survey wasn't loaded at time of UI generation. Is the app being loaded properly?"))
}

