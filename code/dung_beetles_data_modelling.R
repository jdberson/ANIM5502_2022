#' ---
#' title: "Data modelling"
#' ---
#' 
#' ## Setup
#' 
#' Here we load the packages, set the ggplot theme and load the data.
#' 
## -------------------------------------------------------------------------------------------------------------
library("tidyverse")
library("broom")
library("emmeans")


#' 
#' Set the ggplot theme for all plots.
#' 
## -------------------------------------------------------------------------------------------------------------

theme_set(theme_minimal())
theme_update(strip.text = element_text(face = "bold", size=10, hjust=0), 
             strip.background = element_rect(fill = "grey80", colour = NA),
             axis.text = element_text(size=10, colour = "black"),
             axis.title = element_text(size=12, colour = "black"))



#' 
#' Import the data.
#' 
## -------------------------------------------------------------------------------------------------------------
# Read in the data
db_data <- 
  read_csv("data/db_data.csv") %>%
  
  # Some R packages require factors to be explicit
  mutate(across(.cols = c(id:shipment, treatment:sex), as.factor))


#' 
#' ## Hypothesis #1
#' 
#' First we want to know whether the treatment had an immediate effect on beetle morphology, and whether this effect differed for the two species.
#' 
#' Recall from our data exploration that two of the *Onthophagus vacca* shipments did not have both treatments. We will filter these out for our analysis, as well as generations other than the first generation.
#' 
## -------------------------------------------------------------------------------------------------------------

# Filter data
db_data_F1 <-
  db_data %>%
              filter(generation == 1 &
                     !shipment %in% c("5A", "5B")) %>%
  droplevels()

# Check sample sizes
db_data_F1 %>%
  drop_na(pronotum_width) %>%
  group_by(treatment, sex, shipment, species) %>%
  count()

# Fit the model
m1a_pw <- lm(pronotum_width ~ treatment*species + sex, data = db_data_F1)

# Check model conditions
par(mfrow = c(2,2)); plot(m1a_pw); par(mfrow = c(1,1))

# Can look at a histogram of the residuals
ggplot(data = augment(m1a_pw), aes(x = .resid)) +
  geom_histogram()

# Model results
tidy(m1a_pw)

#' 
#' There is no strong evidence for an interaction effect. I think it's reasonable to simplify the model and drop the interaction.
#' 
## -------------------------------------------------------------------------------------------------------------
# Fit the model
m1b_pw <- lm(pronotum_width ~ treatment + species + sex, data = db_data_F1)

# Check model conditions
par(mfrow = c(2,2)); plot(m1b_pw); par(mfrow = c(1,1))

# Can look at a histogram of the residuals
ggplot(data = augment(m1b_pw), aes(x = .resid)) +
  geom_histogram()

# Model results

# Using the broom package
tidy(m1b_pw)
glance(m1b_pw)

# Using summary
summary(m1b_pw)

#' 
#' We have evidence for a treatment effect and some coefficients to help us understand what the effect is. The emmeans package can help us out a bit more here.
#' 
## -------------------------------------------------------------------------------------------------------------
m1b_pw_emm <- emmeans(m1b_pw, "treatment")
m1b_pw_emm


#' 
#' Let's plot the results with our raw data - this is a great sanity check to make sure something has not gone horribly wrong!
#' 
## -------------------------------------------------------------------------------------------------------------

# Notice the use of tidy() from the broom package
ggplot(data = tidy(m1b_pw_emm, conf.int = TRUE), aes(x = treatment, y = estimate, colour = treatment, shape = treatment)) +
  geom_point(size = 4) +
 geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) + 
  geom_jitter(data = db_data_F1, aes(x = treatment, y = pronotum_width, colour = treatment), alpha = 0.2)
  


#' 
#' ## Hypothesis #2
#' 
#' Now let's investigate whether this effect persists across the generations.
#' 
#' Recall that our most complete data is for *Onthophagus vacca* shipments 2 and 4 up to generation 3. Let's use just this data for this hypothesis for now.
#' 
## -------------------------------------------------------------------------------------------------------------

# Filter data
db_data_F1_3 <-
  db_data %>%
              filter(generation %in% c(1, 2, 3) &
                     shipment %in% c("2", "4")) %>%
  droplevels()

# Check sample sizes
db_data_F1_3 %>%
  drop_na(pronotum_width) %>%
  group_by(treatment, generation, sex, shipment) %>%
  count() 

# Fit the model
m2a_pw <- lm(pronotum_width ~ treatment*generation*shipment, data = db_data_F1_3)

# Check model conditions
par(mfrow = c(2,2)); plot(m2a_pw); par(mfrow = c(1,1))

# Can look at a histogram of the residuals
ggplot(data = augment(m2a_pw), aes(x = .resid)) +
  geom_histogram()

# Model results
tidy(m2a_pw)
glance(m2a_pw)

summary(m2a_pw)



#' 
#' Let's plot the results with the raw data.
#' 
## -------------------------------------------------------------------------------------------------------------
ggplot(data = augment(m2a_pw, se_fit = TRUE), 
       aes(x = generation, y = .fitted, colour = treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin = .fitted - .se.fit, ymax = .fitted + .se.fit), 
              alpha = 0.2) +
  geom_jitter(aes(x = generation, y = pronotum_width, colour = treatment), alpha = 0.2) +
 facet_wrap(~shipment)

#' 
#' This does not look like a great fit, particularly for shipment 2.
#' 
#' Given the shipment 2 behaves differently to all the other shipments, I suggest we do some thorough checks of the data before pursuing more complex models.
#' 
#' I'll end it here for now - we can extend/update the analyses at our next meeting.