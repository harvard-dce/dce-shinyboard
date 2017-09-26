# dce-shinyboard

FROM rocker/shiny
LABEL version="0.1.0"
LABEL description="a shiny server container for dce apps and dashboards"

RUN apt-get update && apt-get install -y \
    libxml2-dev libssl-dev libssh2-1-dev

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('rstudio/packrat')"

COPY ./apps /srv/shiny-server/apps

RUN R -e "install.packages(packrat:::dirDependencies('/srv/shiny-server'))"

COPY ./assets/shiny-server.conf /etc/shiny-server/shiny-server.conf



