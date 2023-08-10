install.packages("devtools")
install.packages("roxygen2")

library(devtools)
library(roxygen2)

remove.packages(rlang)
install.packages("rlang")

setwd("C:/Users/marti/2.-0 LCA team Dropbox/2.-0 LCA consultants/20LCA Adm/RothC - R Package") #replace "marti" with your username

devtools::create("SoilR2.0")

devtools::document()
