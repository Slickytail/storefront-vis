library(rjson)
library(httr)
library(stringr)

# typeform.formid <- "abcdefg"
# typeform.auth <- "1234567890abcxyzqwertyuiop"
# typeform.request_url <- str_glue("http://api.typeform.com/forms/{typeform.formid}/responses")
# typeform.authorization <- str_glue("bearer {typeform.auth}")

.GetResponses <- function(form, n=100, since=NULL, page=NULL) {
  resp <- GET(form$url, add_headers(authorization = form$auth),
              query = list(page_size=n, since=since, page=page))
  warn_for_status(resp, task="fetch survey responses")
  return(content(resp, "text"))
}
.FlattenTypeform <- function(x) {
  # Convert the text to an object
  k <- rjson::fromJSON(x)
  # Declare an empty dataframe
  df <- data.frame()
  # For each survey-taker
  for (each in k$items) {
    # Make a list starting with time information
    # Discard each$metadata although we could keep it.
    l <- list("landed_at"=each$landed_at, "submitted_at"=each$submitted_at)
    for (answer in each$answers) {
      if (answer$field$type == "short_text" || answer$field$type == "long_text")  # These types of answers aren't useful for our data, and are likely to cause errors.
        next
      if (answer$type == "choice")
        response <- answer$choice$label
      else if (answer$type == "choices")
        response <- paste(answer$choices$labels, collapse = ",")
      else
        response <- answer[[answer$type]]
      # Insert the answer into the list, using the ref for column name
      l[[answer$field$ref]] = response
    }
    # Add the user to the dataframe
    df <- bind_rows(df, l)
  }
  df
}

getAllResponses <- function(form, since=NULL) {
  j <- tryCatch({.GetResponses(form, n=1000, since=since, page=1)}, warning=function(w){
    warning(w)
    return("warning")})
  if (j == "warning")
    return(NULL)
  p <- rjson::fromJSON(j)
  df <- .FlattenTypeform(j)
  for (page in 2:p) {
    df <- bind_rows(df, .FlattenTypeform(form, .GetResponses(n=1000, since=since, page=page)))
  }
  df
}

.survey <- data.frame()
