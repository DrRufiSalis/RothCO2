#' Set Monthly Mean Precipitations
#'
#' The Set_Precip function creates a column with 12 rows and one column. The first column indicates the Month (1-12). The second column indicates the motnhly mean precipitations (mm), either filled by the user or autofilled by default values for Spain 1990-2020.
#'
#' @param PJan Mean Potential Precipitation (mm) in January. Default uses values from Spain 1990-2020
#' @param PFeb Mean Potential Precipitation (mm) in February. Default uses values from Spain 1990-2020
#' @param PMar Mean Potential Precipitation (mm) in March. Default uses values from Spain 1990-2020
#' @param PApr Mean Potential Precipitation (mm) in April. Default uses values from Spain 1990-2020
#' @param PMay Mean Potential Precipitation (mm) in May. Default uses values from Spain 1990-2020
#' @param PJun Mean Potential Precipitation (mm) in June. Default uses values from Spain 1990-2020
#' @param PJul Mean Potential Precipitation (mm) in July. Default uses values from Spain 1990-2020
#' @param PAug Mean Potential Precipitation (mm) in August. Default uses values from Spain 1990-2020
#' @param PSep Mean Potential Precipitation (mm) in September. Default uses values from Spain 1990-2020
#' @param POct Mean Potential Precipitation (mm) in October. Default uses values from Spain 1990-2020
#' @param PNov Mean Potential Precipitation (mm) in November. Default uses values from Spain 1990-2020
#' @param PDec Mean Potential Precipitation (mm) in December. Default uses values from Spain 1990-2020
#'
#' @return A dataframe with monthly mean precipitation (mm).
#' @export


#### Climatic Variables - Precipitation (mm) ####
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
  Precip=data.frame(Month=1:12, Precip=c(#Precipitation (mm)
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
