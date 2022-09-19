# Download R scripts ------------------------------------------------------

download.file("https://raw.githubusercontent.com/jdberson/ANIM5502_2022/main/code/dung_beetles_data_cleaning.R",
              "dung_beetles_data_cleaning.R")

download.file("https://raw.githubusercontent.com/jdberson/ANIM5502_2022/main/code/dung_beetles_data_exploration.R",
              "dung_beetles_data_exploration.R")

download.file("https://raw.githubusercontent.com/jdberson/ANIM5502_2022/main/code/dung_beetles_data_modelling.R",
              "dung_beetles_data_modelling.R")



# Download data -----------------------------------------------------------

# Create a data folder 
dir.create("data")

# Download Excel file
# The dung_beetles_data_cleaning.R script requires the Excel file to run.
# Download this from the SharePoint site and save in the "data" file of
# this project. However, it is not needed for the data exploration and 
# modelling parts so you can skip this step.

# Download the prepared .csv file
download.file("https://raw.githubusercontent.com/jdberson/ANIM5502_2022/main/data/db_data.csv",
              "data/db_data.csv")


# Install packages --------------------------------------------------------

pkgs <- c("tidyverse", "readxl", "janitor", "GGally", "broom", "broom.mixed", 
          "emmeans", "lme4", "car")
install.packages(pkgs)