FROM openanalytics/r-base

LABEL maintainer "Trevor Nederlof <tnederlof@weather-scout.com>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    libjq-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the euler app
RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('googleway', repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('rstudio/pool', force = TRUE)"
RUN R -e "devtools::install_github('andrewsali/shinycssloaders', force = TRUE)"
RUN R -e "install.packages('leaflet', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rgdal', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('sp', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('ggmap', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('geosphere', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('tidyverse', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('openxlsx', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('readxl', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('stringr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('lubridate', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('DBI', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('scales', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('RCurl', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('RPostgres', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('plotly', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('promises', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('future', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinydashboard', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('config', repos='https://cloud.r-project.org/')"


# copy the app to the image
RUN mkdir /root/weather-scout
COPY weather-scout /root/weather-scout

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/weather-scout')"]