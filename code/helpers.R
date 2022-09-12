# Additional steps for building the website using R included here

# Link to github repository
library("usethis")

# Note - need to make at least one local commit before running this line
use_github()

# Save .qmd files as .R files for easy download and local code execution
knitr::purl("dung_beetles/dung_beetles_data_cleaning.qmd", 
            output = "code/dung_beetles_data_cleaning.R", documentation = 2)
knitr::purl("dung_beetles/dung_beetles_data_exploration.qmd", 
            output = "code/dung_beetles_data_exploration.R", documentation = 2)
knitr::purl("dung_beetles/dung_beetles_data_modelling.qmd", 
            output = "code/dung_beetles_data_modelling.R", documentation = 2)
