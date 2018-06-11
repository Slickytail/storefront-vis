library(tidyverse)

# Data Processing ----
survey.filename <- "data/Survey+Location.csv"#file.choose()
survey <- read_csv(survey.filename) %>%
  filter(!(
    is.na(Party) | is.na(Gender) | is.na(Age_Bracket) | 
      is.na(Ethnic) | is.na(Born) | is.na(Own_Rent) | is.na(Educ)
  )) %>% unclass %>% data.frame
levels(survey$Immirgr) <-
  c(
    "Not Sure",
    "People who come here illegally have broken the law.",
    "They are just here to work and to participate in the American Dream."
  )

party.target <- c(.48, .30, .22)
names(party.target) <- c("Democrat", "Republican", "Other")
survey$RakeParty <- survey$Party
levels(survey$RakeParty) <-
  c("Other", "Democrat", "Other", "Other", "Other", "Republican")

# Gender target ratios and name matching
gender.target <- c(.51, .49)
names(gender.target) <- c("Female", "Male")
# levels(survey$Gender) == "Female" "Male"

# Age bracket target ratios and name matching
age.target <- c(.19, .13, .17, .32, .19)
names(age.target) <- c("18-34", "35-45", "46-54", "55-71", "71+")
# levels(survey$Age_Bracket) == "18-34" "35-45" "46-54" "55-71" "71+"

# Ethnicity target bracket ratios and name matching
ethnicity.target <- c(.62, .22, .06, .10)
names(ethnicity.target) <-
  c("White", "Latino", "Asian", "Other") # Black has been combined with Other
survey$RakeEthnic <- survey$Ethnic
levels(survey$RakeEthnic) <-
  c("Asian", "Other", "Latino", "Other", "Other", "Other", "White")

# Nativity status target ratios and name matching
born.target <- c(.70, .30)
names(born.target) <- c("Native", "Foreign")
survey$RakeBorn <- survey$Born
levels(survey$RakeBorn) <- c("Native", "Native", "Foreign")

# Home ownership target ratios and name matching
owner.target <- c(.65, .31, .04)
names(owner.target) <- c("Own", "Rent", "Other")
survey$RakeOwn_Rent <- survey$Own_Rent
levels(survey$RakeOwn_Rent) <- c("Other", "Own", "Own", "Rent")

# Educational achievement target ratios and name matching
educ.target <- c(0.146, 0.364, 0.286, 0.204)
names(educ.target) <-
  c("High School", "Some College", "4YR Degree", "Postgrad")
survey$RakeEduc <- survey$Educ
levels(survey$RakeEduc) <-
  c(
    "4YR Degree",
    "Some College",
    "Some College",
    "High School",
    "Postgrad",
    "Postgrad",
    "Some College",
    "High School"
  )

# Case id is simply a number for each case in the survey that allows anesrake to work
survey$caseID <- 1:nrow(survey)
# Create a list of the different variable targets
targets = list(
  party.target,
  gender.target,
  age.target,
  ethnicity.target,
  born.target,
  owner.target,
  educ.target
)
# Name the target variables the same as the sample data variables
names(targets) <-
  c(
    "RakeParty",
    "Gender",
    "Age_Bracket",
    "RakeEthnic",
    "RakeBorn",
    "RakeOwn_Rent",
    "RakeEduc"
  )
