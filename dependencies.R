# LIST OF REQUIRED PACKAGES -----------------------------------------------
# 
#  required_packages <- c(
#    "checkpoint"
#  )
#  
#  # install missing packages
# 
# new.packages - required_packages[!(required_packages %in% installed.packages()[,"Package"])]
#  
# if (length(new.packages)) {
#    install.packages(new.packages)
# }
# 
#  rm(new.packages)

#library(checkpoint)
#checkpoint(snapshotDate = "2021-04-20", checkpointLocation = ".checkpoint/",)
library(htmltools)
library(shinythemes)
library(neurobase)
library(plotly)
library(stats)
library(shiny)
library(shinyBS)
library(shinyalert)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(survminer)
library(tidyverse)
library(viridis)
library(visNetwork)
library(igraph)
library(rsconnect)
library(rintrojs)
library(DT)
library(classInt)
library(e1071)
library(furniture)
library(gamlss)
library(ggridges)
library(ggseg3d)
library(proxy)
library(data.table)
library(qicharts2)
library(openssl)
library(tribe)
library(wk)
library(stringr)
