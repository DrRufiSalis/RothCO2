#' Set Monthly Temperature Vector
#'
#' The Set_Temp function create a column with 12 rows and one column. The first
#' column indicates the Month (1-12). The second column indicates the mean Temperature,
#' either filled by the user or autofilled by default values for Spain 1990-2020.
#'
#' @return A vector with monthly temperature values
#' @export


#### Climatic Variables - Temperature ####
#Default values are for Spain Mean 1990-2020
Set_Temp <- function(TJan = 6.26, #January
                     TFeb = 7.17, #February
                     TMar = 9.79, #March
                     TApr = 11.82, #April
                     TMay = 15.54, #May
                     TJun = 19.95, #June
                     TJul = 23.06, #July
                     TAug = 23.09, #August
                     TSep = 19.46, #September
                     TOct = 14.83, #October
                     TNov = 9.74, #November
                     TDec = 6.95) #December
{
  Temp=data.frame(Month=1:12, Temp=c(#Temperature
    TJan, #January
    TFeb, #February
    TMar, #March
    TApr, #April
    TMay, #May
    TJun, #June
    TJul, #July
    TAug, #August
    TSep, #September
    TOct, #October
    TNov, #November
    TDec)) #December

  Temp <<- Temp
}
