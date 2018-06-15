# Storefront Political Survey Visualization App

### Prerequisites

* R
* R Packages:
	* shiny
	* tidyverse (dplyr, ggplot2, readr, purrr)
	* stringr
	* anesrake
	* weights
	* rmarkdown
	* scales
	* RColorBrewer
	* ids


### Usage

#### Setup
This app pulls responses from typeform. Upon launch, navigate to the typeform connection panel and enter your user authentication and the id of your survey.
After connecting to a typeform the app is ready to use. You may have to open each of the sidebar tabs to force them to render.<br/>
In order to get meaningful variable names, the survey questions must have custom refs. Text or short-text questions will be ignored by the API functions and not visible in the app.

#### Variable selection
The variables tab allows selecting the primary and secondary variables to graph. The bar graph will display a bar for each value of the primary variable and each bar will be filled according to the distribution of the second variable accross all rows matching that value of the first variable.<br/>
The distribution mode switch allows you to view the relative distribution. This is useful when the second variable has an uneven distribution of options. For example, there might be 3 options but the first option represents 75% of the sample. In this case, you can see in which categories certain options are overrepresented. Note that if relative distribution is selected, the numbers in the table will represent the ratio of the frequency of an option within some subset to the frequency of that option outside of the subset. For example, a "frequency" of 150% indicates that the option is 1.5x more popular locally.

#### Filtering
The filtering tab allows creating filters to filter the survey to only respondents who selected particular options.
To add a filter, change the "Add new filter" dropdown from None to whichever variable you'd like to filter. A set of checkboxes will appear. Unchecking one will filter out respondents who had given that answer. You can remove a filter by setting its dropdown back to None.<br/>
Filtering is performed functionally, meaning that the original survey data is not modified and filters can be added and removed without side effects. At the moment there is no support for conditional filter logic. It is forbidden to create two filters on the same variable. 

#### Raking
The raking tab allows providing real-world distributions of variables in order to correct for uneven survey sampling. Weights will be calculated for each individual in the survey and these weights will be used when generating counts and ratios in the graph and table. <br/>
Raking is **unimplemented** pending UX decisions.


### Known Bugs

* When changing a filter variable by changing its dropdown, if there are shared options between the old variable and the new one, the shared options will be the only ones selected rather than all options (as would occur if the new variable was added as a new filter).
