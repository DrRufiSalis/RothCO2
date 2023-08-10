#' Set Monthly Evapotranspiration Vector
#'
#' The Set_Evp function create a column with 12 rows and one column. The first
#' column indicates the Month (1-12). The second column indicates the mean monthly evapotranspiration,
#' either filled by the user or autofilled by default values calculated using the
#' Thornthwaite equiation using Temperature data from Set_Temp
#' @return A mvector with monthly evapotranspiration values
#' @export

#### Climatic Variables - Evapotranspiration ####
#Default values are for Spain Mean 1990-2020 considering Thornthwaite equation
Set_Evp <- function(EvpJan = EvpJan_Default, #January
                    EvpFeb = EvpFeb_Default, #February
                    EvpMar = EvpMar_Default, #March
                    EvpApr = EvpApr_Default, #April
                    EvpMay = EvpMay_Default, #May
                    EvpJun = EvpJun_Default, #June
                    EvpJul = EvpJul_Default, #July
                    EvpAug = EvpAug_Default, #August
                    EvpSep = EvpSep_Default, #September
                    EvpOct = EvpOct_Default, #October
                    EvpNov = EvpNov_Default, #November
                    EvpDec = EvpDec_Default) #December

{
  #If there is no Evp data, we calculate Evp based on Thornthwaite (Evp=16(10t/I)^a) (see https://www.vitoria-gasteiz.org/docs/wb021/contenidosEstaticos/adjuntos/es/78/98/37898.pdf)
  #t=average monthly temperature
  #I = sum of i's. Annual heat index
  #a = 0,492+0,0179*I-0,0000771*I^2 + 0,000000675*I^3

  iJan = sign(Temp[1,2])*(abs(Temp[1,2])/5)^1.514 #January
  iFeb = sign(Temp[2,2])*(abs(Temp[2,2])/5)^1.514 #February
  iMar = sign(Temp[3,2])*(abs(Temp[3,2])/5)^1.514 #March
  iApr = sign(Temp[4,2])*(abs(Temp[4,2])/5)^1.514 #April
  iMay = sign(Temp[5,2])*(abs(Temp[5,2])/5)^1.514 #May
  iJun = sign(Temp[6,2])*(abs(Temp[6,2])/5)^1.514 #June
  iJul = sign(Temp[7,2])*(abs(Temp[7,2])/5)^1.514 #July
  iAug = sign(Temp[8,2])*(abs(Temp[8,2])/5)^1.514 #August
  iSep = sign(Temp[9,2])*(abs(Temp[9,2])/5)^1.514 #September
  iOct = sign(Temp[10,2])*(abs(Temp[10,2])/5)^1.514 #October
  iNov = sign(Temp[11,2])*(abs(Temp[11,2])/5)^1.514 #November
  iDec = sign(Temp[12,2])*(abs(Temp[12,2])/5)^1.514 #December

  I=iJan+iFeb+iMar+iApr+iMay+iJun+iJul+iAug+iSep+iOct+iNov+iDec
  a=0.492+(0.0179*I)-(0.0000771*(I^2)) + (0.000000675*(I^3))

  EvpJan_Default = sign(Temp[1,2])*16*(((10*abs(Temp[1,2]))/I)^a) #January
  EvpFeb_Default = sign(Temp[2,2])*16*(((10*abs(Temp[2,2]))/I)^a) #February
  EvpMar_Default = sign(Temp[3,2])*16*(((10*abs(Temp[3,2]))/I)^a) #March
  EvpApr_Default = sign(Temp[4,2])*16*(((10*abs(Temp[4,2]))/I)^a) #April
  EvpMay_Default = sign(Temp[5,2])*16*(((10*abs(Temp[5,2]))/I)^a) #May
  EvpJun_Default = sign(Temp[6,2])*16*(((10*abs(Temp[6,2]))/I)^a) #June
  EvpJul_Default = sign(Temp[7,2])*16*(((10*abs(Temp[7,2]))/I)^a) #July
  EvpAug_Default = sign(Temp[8,2])*16*(((10*abs(Temp[8,2]))/I)^a) #August
  EvpSep_Default = sign(Temp[9,2])*16*(((10*abs(Temp[9,2]))/I)^a) #September
  EvpOct_Default = sign(Temp[10,2])*16*(((10*abs(Temp[10,2]))/I)^a) #October
  EvpNov_Default = sign(Temp[11,2])*16*(((10*abs(Temp[11,2]))/I)^a) #November
  EvpDec_Default = sign(Temp[12,2])*16*(((10*abs(Temp[12,2]))/I)^a) #December

  Evp=data.frame(Month=1:12, Evp=c(#Evapotranspiration
    EvpJan, #January
    EvpFeb, #February
    EvpMar, #March
    EvpApr, #April
    EvpMay, #May
    EvpJun, #June
    EvpJul, #July
    EvpAug, #August
    EvpSep, #September
    EvpOct, #October
    EvpNov, #November
    EvpDec)) #December

  Evp <<- Evp
}
