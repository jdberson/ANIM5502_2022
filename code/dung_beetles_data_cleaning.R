#' ---
#' title: "Data cleaning"
#' ---
#' 
#' ## Setup
#' 
#' First we need to load the packages we well be using.
#' 
## ------------------------------------------------------------------------------------------------------
library("tidyverse")
library("readxl")
library("janitor")


#' 
#' 
#' ## Import and clean data
#' 
#' Now we are ready to import the data to R and clean it up!
#' 
## ------------------------------------------------------------------------------------------------------
db_data <- 
  
  # Use the readxl package to load the data directly from Excel
  read_xlsx("data/CSIRO quarantine project data.xlsx", 
            sheet = "Group data", na = c("", "N/A", "NA")) %>%
  
  # Use the janitor package to make the column headings more R friendly
  clean_names() %>%
  
  # Filter rows with all missing values
  filter(is.na(notes) | notes != "Missing") %>%
  
  # Change columns to numeric
  mutate(across(pronotum_length:head_horn, as.numeric)) %>%
  
  # Create a new group_id term for joining with vial info - note this is hacky!
  # Note the third line - this is necessary to add in the leading zeros where
  # they are missing
  mutate(id_new = id, .after = id) %>%
  separate(id_new, into = c("pop", "sex1", "individual")) %>%
  mutate(pop = sprintf("%02d", as.integer(pop))) %>%
  unite("group_id", pop, sex1, sep = "_") %>%
  select(-individual) %>%
  
  # Rename species and sex columns so that we can check these against vial info
  rename(sex_data = sex, species_data = species) %>%
  
  # Combine with the 'Vial info' worksheet
  left_join(
    
    # Load and clean 'Vial info' sheet
    read_xlsx("data/CSIRO quarantine project data.xlsx", 
              sheet = "Vial info") %>%
      clean_names()%>%
      drop_na(id) %>%
      select(-x8, -horn_priorities) %>%
      rename(sex_vial = sex, species_vial = species) %>%
      separate(id, into = c("pop", "sex1")) %>%
      mutate(pop = sprintf("%02d", as.integer(pop))) %>%
      unite("group_id", pop, sex1, sep = "_"),
    
    # The 'by' argument tells left_join() which variable to use for joining
    by = "group_id"
    
  ) %>%
  
  # Move the information about the individuals to the start of the tibble
  relocate(group_id, species_vial, species_data, shipment, generation, 
           treatment, sex_vial, sex_data, .after = id) %>%
  
  # Change generation to an integer - need to remove the 'f'
  mutate(generation = as.integer(str_remove(generation, "F"))) %>%
  
  # Species and sex information in group data sheet appear incorrect for some
  # records
  rename(species = species_vial, sex = sex_vial) %>%
  select(-c(species_data, sex_data)) %>%
  
  # Change the treatment assignment of generation 0 beetles to 'P' for parental.
  mutate(treatment = if_else(generation == 0, "P", treatment)) %>%
  
  # Include sphericity measure
  mutate(sphericity = (body_depth^2 / (body_length * elytra_width))^(1/3), 
         .after = head_horn) %>%
  
  # Some head horns have been reported in uM instead of mm - change those here
  mutate(head_horn = if_else(head_horn > 20, head_horn/1000, head_horn))





#' 
#' That took a lot of code just to import and clean the data!
#' 
#' This provides an important lesson - we could have saved ourselves some effort cleaning the data by setting more guidelines around data entry.
#' 
#' ## Save the cleaned data
#' 
#' Let's save the clean data in a new file.
#' 
## ------------------------------------------------------------------------------------------------------

write_csv(db_data, "data/db_data.csv")


#' 
#' 
