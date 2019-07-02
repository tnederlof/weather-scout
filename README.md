# Weather Scout

Every ten years NOAA creates an incredible climate data set based on the past 30 years of historical data. Despite containing valuable information in the dataset, the current access tools are incredibly challenging to use, which prevents exploration. This shiny based web application combines an interactive map with a powerful location search tool, the NOAA Climate Normals dataset and some prediction techniques to create a rich exploration of what temperatures might be at any location in the United States at different points during the year.

See the application in action at: [https://historical-weather.shinyapps.io/weather-scout/]

Note: This information is based on historical weather data and is NOT a prediction of future weather patterns, please check a short-term forcast at weather.gov before heading out on a trip).    

## Getting Started

These instructions will get you a copy of the project which can be run locally, deployed on shinyapps.io (managed shiny hosting) or shiny server (shiny self hosted).

Currently this application requires a PostgreSQL database to store the bulk of the NOAA data. To populate this database with the correct data follow the steps below.

1) Rename "example-config.yml" to "config.yml" and fill in all the PostgreSQL server details
2) Run "data_parsing/1-populate_database_daily.R" to populate daily weather station data
3) Run "data_parsing/2-populate_database_hourly.R" to populate hourly weather station data
4) Run "data_parsing/3-populate_database_weather_stations.R" to populate the overall weather station information data

### Prerequisites

R packages that must be installed to run the application

```
install.packages(leaflet)
install.packages(elevatr)
install.packages(rgdal)
install.packages(sp)
install.packages(ggmap)
install.packages(geosphere)
install.packages(shinycssloaders)
install.packages(tidyverse)
install.packages(openxlsx)
install.packages(readxl)
install.packages(stringr)
install.packages(lubridate)
install.packages(DBI)
install.packages(scales)
install.packages(RCurl)
install.packages(RPostgres)
install.packages(plotly)
install.packages(promises)
install.packages(future)
install.packages(pool)
install.packages(shiny)
install.packages(shinydashboard)
install.packages(config)
```

## Author

* **Trevor Nederlof** - [tnederlof](https://github.com/tnederlof)


## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details