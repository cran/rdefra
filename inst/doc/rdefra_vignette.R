## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  eval = FALSE
)

## ----installation_cran--------------------------------------------------------
#  install.packages("rdefra")

## ----installation_github------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("ropensci/rdefra")

## ----load_library, eval = TRUE------------------------------------------------
library("rdefra")

## ----catalogue_full, eval = TRUE----------------------------------------------
# Get full catalogue
stations <- ukair_catalogue()
stations

## ----catalogue_filter, eval = TRUE--------------------------------------------
stations_EnglandOzone <- ukair_catalogue(pollutant = 1, country_id = 1)
stations_EnglandOzone

## ----get_coords, eval = TRUE--------------------------------------------------
# How many stations have missing coordinates?
length(which(is.na(stations$Latitude) | is.na(stations$Longitude)))

# Scrape DEFRA website to get Easting/Northing (if available)
stations <- ukair_get_coordinates(stations)

# How many stations still have missing coordinates?
length(which(is.na(stations$Latitude) | is.na(stations$Longitude)))

## ----get_site_id, eval = FALSE------------------------------------------------
#  stations$SiteID <- ukair_get_site_id(stations$UK.AIR.ID)

## ----load_dataset_stations----------------------------------------------------
#  data("stations")

## ----get_hourly_data, eval = TRUE, fig.width = 7, fig.height = 5, fig.cap = "\\label{fig:hdata}Hourly ozone data from London Marylebone Road monitoring station in 2015"----
# Get 1 year of hourly ozone data from London Marylebone Road monitoring station
df <- ukair_get_hourly_data("MY1", years = 2015)

# Aggregate to daily means and plot
# please note we use the zoo package here because time series could be irregular
library("zoo")
my1 <- zoo(x = df$Ozone, order.by = as.POSIXlt(df$datetime))

daily_means <- aggregate(my1, as.Date(as.POSIXlt(df$datetime)), mean)

plot(daily_means, main = "", xlab = "",
     ylab = expression(paste("Ozone concentration [", mu, "g/", m^3, "]")))

## ----ozone_data, eval = FALSE-------------------------------------------------
#  # Get 15 years of hourly ozone data from the same monitoring station
#  library("ggplot2")
#  library("dplyr")
#  library("lubridate")
#  
#  df <- ukair_get_hourly_data("MY1", years = 2000:2015)
#  df <- mutate(df,
#               year = year(datetime),
#               month = month(datetime),
#               year_month = strftime(datetime, "%Y-%m"))
#  
#  df %>%
#    group_by(month, year_month) %>%
#    summarize(ozone = mean(Ozone, na.rm=TRUE)) %>%
#    ggplot() +
#    geom_boxplot(aes(x = as.factor(month), y = ozone, group = month),
#                 outlier.shape = NA) +
#    xlab("Month of the year") +
#    ylab(expression(paste("Ozone concentration (", mu, "g/",m^3,")"))) +
#    ggtitle("15 years of hourly ozone data from London Marylebone Road monitoring station")

## ----map_data, eval = FALSE---------------------------------------------------
#  # Keep only station with coordinates
#  stations_with_coords <- stations[complete.cases(stations[, c("Longitude",
#                                                               "Latitude")]), ]
#  # Keep only station with known SiteID
#  stations_with_SiteID <- which(!is.na(stations_with_coords$SiteID))
#  
#  # An interactive map
#  library("leaflet")
#  leaflet(data = stations_with_coords) %>% addTiles() %>%
#    addCircleMarkers(lng = ~Longitude,
#                     lat = ~Latitude,
#                     popup = ~SiteID,
#                     radius = 1, color="blue", fill = FALSE) %>%
#    addCircleMarkers(lng = ~Longitude[stations_with_SiteID],
#                     lat = ~Latitude[stations_with_SiteID],
#                     radius = 0.5, color="red",
#                     popup = ~SiteID[stations_with_SiteID])

## ----dotchart1, eval = TRUE, fig.width = 7, fig.height = 10, fig.cap = "\\label{fig:dotchart1}Spatial distribution of the monitoring stations across zones."----
# Zone
dotchart(as.matrix(table(stations$Zone))[,1])

## ----dotchart2, eval = TRUE, fig.width = 7, fig.height = 5, fig.cap = "\\label{fig:dotchart2}Spatial distribution of the monitoring stations across environment types."----
# Environment.Type
dotchart(as.matrix(table(stations$Environment.Type[stations$Environment.Type != "Unknown Unknown"]))[,1])

## ----parallel_example, eval = FALSE-------------------------------------------
#  library("parallel")
#  
#  # Use detectCores() to find out many cores are available on your machine
#  cl <- makeCluster(getOption("cl.cores", detectCores()))
#  
#  system.time(myList <- parLapply(cl, stations$SiteID[stations_with_SiteID],
#                                  ukair_get_hourly_data, years=1999:2016))
#  
#  stopCluster(cl)
#  
#  df <- bind_rows(myList)

