# Data sources:
# https://www.metoffice.gov.uk/research/climate/maps-and-data/data/index
# https://www.gbif.org/
# https://ropensci.org/


packs <- c('tidyverse','sf','raster','ggthemes','rgbif','lubridate', 
           'caret', 'viridis', 'glmnet')

for (p in packs) {
  if (!require(p, character.only = T)) {
    install.packages(p)
    library(p, character.only = T)
  }
}

climate <- read_rds('climate_raster.rds')

# Convert to SpatialPixelDataFrame for plotting
climate_df <- mutate(
  .data = climate, 
  rasters = map(
    .x = rasters, 
    ~ as_tibble(as(.x, "SpatialPixelsDataFrame")))) %>%
  unnest(cols = c(rasters))

# Filter the data to plot
ggp_temperature <- climate_df %>%
  ggplot(aes(x = x, y = y)) + geom_tile(aes(fill = minimum.temperature)) +
  theme_map() + coord_equal() +
  facet_grid(~ decade) + scale_fill_distiller(palette = "Spectral") + 
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(title = "Minimum of Average Monthly Temperature (Celsius)", caption = 'Source: MetOffice UK')

# Display the map
ggp_temperature

#source("occ_search.R")
# Call the API to get the occurrence records of this species
gbif_response <- rgbif::occ_search(
  scientificName = "Loxia scotica", country = "GB",
  hasCoordinate = TRUE, hasGeospatialIssue = FALSE, limit = 2000)

# Inspect the class and names of gbif_response
class(gbif_response)
names(gbif_response)

# Print the first six lines of the data element in gbif_response
head(gbif_response$data,6)

birds_dated <- mutate(
  .data = gbif_response$data,
  # Create a new column specifying the decade of observation
  decade = ymd_hms(eventDate) %>% round_date("10y") %>% year())

birds_cleaned <- birds_dated %>%
  filter(
    #issues == "" &
      str_detect(license, "http://creativecommons.org/") &
      between(decade,1970,2010)
  ) %>%
  transmute(decade = decade, x = decimalLongitude, y = decimalLatitude) %>%
  arrange(decade)

# "Nest" the bird data
birds_nested <-  birds_cleaned %>% group_by(decade) %>% 
  nest(.key='presences')

head(birds_nested)

# Calculate the total number of records per decade
birds_counted <- birds_nested %>%
  mutate(n = map_dbl(.x=presences,.f=nrow))

head(birds_counted)

# Define geographical projections
proj_latlon <- st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj_ukgrid <- st_crs("+init=epsg:27700")

# Convert records to spatial points and project them
birds_presences <- mutate(birds_counted,
                          presences = map(presences, ~ .x %>%
                                            # Specify the current projection
                                            st_as_sf(coords = c("x", "y"), crs = proj_latlon) %>%
                                            # Transform to new projection
                                            st_transform(crs = proj_ukgrid)))

# Combine the bird data and the climate data in one data frame
birds_climate <- full_join(birds_presences, climate, by = 'decade')

presence_data <- map2_df(
  .x = birds_climate[["rasters"]],
  .y = birds_climate[["presences"]],
  # extract the raster values at presence locations
  ~ raster::extract(.x,.y) %>% 
    as_tibble() %>% 
    mutate(observation = "presence"))
# Define helper function for creating pseudo-absence data
create_pseudo_absences <- function(rasters, n, ...) {
  set.seed(12345)
  sampleRandom(rasters, size = n * 5, sp = TRUE) %>% 
    raster::extract(rasters, .) %>% as_tibble() %>%
    mutate(observation = "pseudo_absence")
}

# Create pseudo-absence proportional to the total number of records per decade
pseudo_absence_data <- pmap_df(.l = birds_climate, .f = create_pseudo_absences)

# Combine the two datasets
model_data <- full_join(presence_data, pseudo_absence_data) %>%
  mutate(observation = factor(observation)) %>% na.omit()

# Define helper function for creating pseudo-absence data
create_pseudo_absences <- function(rasters, n, ...) {
    set.seed(12345)
    sampleRandom(rasters, size = n * 5, sp = TRUE) %>% 
    raster::extract(rasters, .) %>% as_tibble() %>%
    mutate(observation = "pseudo_absence")
}

# Create pseudo-absence proportional to the total number of records per decade
pseudo_absence_data <- pmap_df(.l = birds_climate, .f = create_pseudo_absences)

# Combine the two datasets
model_data <- full_join(presence_data, pseudo_absence_data) %>%
  mutate(observation = factor(observation)) %>% na.omit()


# Define helper function for creating pseudo-absence data
create_pseudo_absences <- function(rasters, n, ...) {
  set.seed(12345)
  sampleRandom(rasters, size = n * 5, sp = TRUE) %>% 
    raster::extract(rasters, .) %>% as_tibble() %>%
    mutate(observation = "pseudo_absence")
}

# Create pseudo-absence proportional to the total number of records per decade
pseudo_absence_data <- pmap_df(.l = birds_climate, .f = create_pseudo_absences)

# Combine the two datasets
model_data <- full_join(presence_data, pseudo_absence_data) %>%
  mutate(observation = factor(observation)) %>% na.omit()

set.seed(12345)

# Create a tuning grid with sets of hyperparameters to try
tuneGrid <- expand.grid(alpha = c(0, 0.5, 1), lambda = c(.003, .01, .03, .06))

# Create settings for model training
trControl <- trainControl(method = 'repeatedcv', number = 5, repeats = 1,
                          classProbs = TRUE, verboseIter = FALSE, summaryFunction = twoClassSummary)

# Fit a statistical model to the data and plot
model_fit <- train(
  observation ~ ., data = model_data,
  method = "glmnet", family = "binomial", metric = "ROC",
  tuneGrid = tuneGrid, trControl = trControl)

plot(model_fit)

# Use our model to make a prediction
climate_df[["prediction"]] <- predict(
  object = model_fit,
  newdata = climate_df,
  type = "prob")[["presence"]]

head(climate_df)

# Create the plot
ggp_changemap <- climate_df %>% ggplot(aes(x = x, y = y)) +
  geom_tile(aes(fill = prediction)) +
  # Style the plot with the appropriate settings for a map
  theme_map() + coord_equal() +
  scale_fill_viridis(option = "A") + theme(legend.position = "bottom") +
  # Add faceting by decade
  facet_grid(~ decade) +
  labs(title = 'Habitat Suitability', subtitle = 'by decade',
       caption = 'Source:\nGBIF data and\nMetOffice UK climate data',
       fill = 'Habitat Suitability [0 low - high 1]')

# Display the plot
ggp_changemap
