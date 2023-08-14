#' Set Monthly Mean Temperatures
#'
#' The Set_Temp function creates a dataframe with two columns and 12 rows. The first column indicates the Month (1-12). The second column indicates the monthly mean temperatures (celsius degrees), either filled by the user or autofilled by default values for Spain 1990-2020.
#'
#' @param TJan Mean Temperature (celsius degrees) in January. Default uses values from Spain 1990-2020
#' @param TFeb Mean Temperature (celsius degrees) in February. Default uses values from Spain 1990-2020
#' @param TMar Mean Temperature (celsius degrees) in March. Default uses values from Spain 1990-2020
#' @param TApr Mean Temperature (celsius degrees) in April. Default uses values from Spain 1990-2020
#' @param TMay Mean Temperature (celsius degrees) in May. Default uses values from Spain 1990-2020
#' @param TJun Mean Temperature (celsius degrees) in June. Default uses values from Spain 1990-2020
#' @param TJul Mean Temperature (celsius degrees) in July. Default uses values from Spain 1990-2020
#' @param TAug Mean Temperature (celsius degrees) in August. Default uses values from Spain 1990-2020
#' @param TSep Mean Temperature (celsius degrees) in September. Default uses values from Spain 1990-2020
#' @param TOct Mean Temperature (celsius degrees) in October. Default uses values from Spain 1990-2020
#' @param TNov Mean Temperature (celsius degrees) in November. Default uses values from Spain 1990-2020
#' @param TDec Mean Temperature (celsius degrees) in December. Default uses values from Spain 1990-2020
#'
#' @return A dataframe with monthly mean temperatures (celsius degrees).
#' @export


#### Climatic Variables - Temperature (celsius degrees) ####
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
  Temp=data.frame(Month=1:12, Temp=c(#Temperature (celsius)
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
