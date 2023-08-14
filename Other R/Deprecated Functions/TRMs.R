#### Set Tillage Rate Modifiers ####

#TRMs are extracted from https://doi.org/10.1016/j.still.2022.105428
Set_TRM <- function(CTRM = 1.00, RTRM = 0.93, NTRM = 0.95){
  CTRM <<- CTRM #Conventional Tillage Rate Modifier
  RTRM <<- RTRM #Reduced Tillage Rate Modifier
  NTRM <<- NTRM #No Tillage Rate Modifier
}