# Deleted code that I may come back to

# Plotting violin / boxplots and labelling outliers using ggstatsplot and purrr
library("ggstatsplot")

db_data_list_vacca <- 
  db_data %>%
  filter(species == "Onthophagus vacca") %>%
  pivot_longer(cols = pronotum_width:sphericity, names_to = "response") %>%
  filter(response != "sphericity") %>%
  split(f = .$response, drop = TRUE)

plot_list_vacca <- purrr::pmap(
  .l = list(
    data = db_data_list_vacca,
    x = "sex",
    y = "value",
    outlier.tagging = TRUE,
    outlier.label = "id",
    outlier.coef = 2.5,
    title = names(db_data_list_vacca)),
  .f = ggbetweenstats
)

plot_list_vacca
