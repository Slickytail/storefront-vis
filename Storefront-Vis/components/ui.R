library(shiny)

# If the survey doesn't exist, let's return a blank UI with an error message
if (exists("survey")) {

# Output Options ----
varsPanel <- tabPanel("Variables",
         h3("Data Output"),
         selectInput("variable", "Variable to display",
                     list("Plans to Vote" = "GOTV",
                          "Political View" = "PoliticaView",
                          "View on Immigration" = "Immirgr",
                          "Most Important Issue" = "ImpIssue",
                          "Governor choice" = "FIrstBallot",
                          "Social Media Use" = "Social",
                          "Television Use" = "WatchTV",
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
                     list(`Only display one variable` = c("None" = "NONE"),
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
                     list("Real Distribution" = "real",
                          "Relative to Subset" = "set",
                          "Relative to entire sample" = "sample")),
         downloadButton("report", "Generate report")
)
# Filters ----
filterPanel <- tabPanel("Filters",
         h3("Sample Filtering"),
         fluidRow(
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
                          sliderInput("cap", "Weight cap", min = 4, max = 20, value = 7)
          )
)


# fluidPage call ----
ui <- fluidPage(
  titlePanel("CA Opinion Demographics"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        varsPanel,
        filterPanel,
        weightPanel
    )),
    # Main Panel ----
    mainPanel(
      h3(textOutput("wheaders")),
      plotOutput("weightedPlot", width = "100%", height="50vh"),
      tableOutput("weighted"),
      conditionalPanel(condition = "!input.noweight",
                       tags$div(title="Error of weighted results. Lower is better. 1 is No Error.", textOutput("effect")))
      
    )
  )
  
)
} else {
  stopApp()
  ui <- fluidPage(h4("Error"), p("survey wasn't loaded at time of UI generation. Is the app being loaded properly?"))
}

