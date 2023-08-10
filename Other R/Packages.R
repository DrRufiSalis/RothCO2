#### 1) Install and Load packages and Set Working Directory ####
#Install Packages
Packages_Pack <- c("SoilR", "tidyverse", "writexl", "ggplot2", "ggforce", "plotly", "readxl", "scales")
install.packages(Packages_Pack)

install.packages("pacman")
pacman::p_load(SoilR, tidyverse, writexl, ggplot2, ggforce, plotly, readxl, scales) 

# Load Packages
library(SoilR) #RothC function
library(tidyverse) #Basic data processing package
library(writexl) #Export Excel files
library(ggplot2) #Plots
library(ggforce) #Facet_zoom function
library(plotly) #Create interactive plots
library(readxl) #Import Excel files
library(scales) #Modification of plots' axis