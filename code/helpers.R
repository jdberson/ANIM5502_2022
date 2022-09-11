# Additional steps for building the website using R included here

# Link to github repository
library("usethis")

# Note - need to make at least one local commit before running this line
use_github()

# Try using renv::snapshot() to overcome publishing issue of rmarkdown not being found
renv::snapshot() # writes a lock file

# Final shot