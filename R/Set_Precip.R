#' Set Monthly Precipitation Vector
#'
#' The Set_Precip function creates a column with 12 rows and one column. The first
#' column indicates the Month (1-12). The second column indicates the mean Precipitation,
#' either filled by the user or autofilled by default values for Spain 1990-2020.
#'
#' @return A mvector with monthly precipitation values
#' @export


#### Climatic Variables - Precipitation ####
#Default values are for Spain Mean 1990-2020
Set_Precip <- function(PJan = 60.59, #January
                       PFeb = 49.02, #February
                       PMar = 59.08, #March
                       PApr = 58.46, #April
                       PMay = 56.81, #May
                       PJun = 32.81, #June
                       PJul = 18.48, #July
                       PAug = 23.88, #August
                       PSep = 45.09, #September
                       POct = 79.58, #October
                       PNov = 73.53, #November
                       PDec = 70.59) #December
{
  Precip=data.frame(Month=1:12, Precip=c(#Precipitation
    PJan, #January
    PFeb, #February
    PMar, #March
    PApr, #April
    PMay, #May
    PJun, #June
    PJul, #July
    PAug, #August
    PSep, #September
    POct, #October
    PNov, #November
    PDec)) #December

  Precip <<- Precip
}
