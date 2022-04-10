# Set base image
FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

#ENV R_BASE_VERSION 4.1.2

# Install ubuntu packages
RUN apt-get update && \
    apt-get install -y \
    git \
    wget \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    libsasl2-dev \
    lsb-release \
    r-base-dev && \
    apt-get clean && \
    apt-get purge && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Install R packages #### May 2, 2021 7:00 PM CDT ####
RUN R -e "install.packages(pkgs=c(\
    'shiny', \
    'tidyverse', \
    'reactable', \
    'reactablefmtr', \
    'bslib', \
    'readxl', \
    'here', \
    'mongolite', \
    'jsonlite', \
    'httr', \
    'glue', \
    'shinyWidgets', \
    'shinyFiles', \
    'shinyjs', \
    'DT'), \
    repos='https://packagemanager.rstudio.com/all/2696074')" 

# Install Shiny Server
RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.16.958-amd64.deb && \
dpkg -i shiny-server-1.5.16.958-amd64.deb



# Expose port for Shiny
EXPOSE 3838

# Run Shiny app
COPY --chown=shiny:shiny . /srv/shiny-server/trial-match/
RUN chmod a+r /srv/shiny-server/trial-match/data/tempus/tempus_current.xlsx
RUN chown shiny:shiny /var/lib/shiny-server

# Set user to non-root
USER shiny

CMD ["/usr/bin/shiny-server"]