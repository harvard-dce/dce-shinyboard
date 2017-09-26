#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

exec shiny-server --pidfile=/var/run/shiny-server.pid >> /var/log/shiny-server/server.log 2>&1
