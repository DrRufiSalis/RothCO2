#' Export Merged Models - CSV
#'
#' The Exports_CSVMM function exports all merged models created.
#'
#' @return CSV files with the models
#' @export

Export_CSVMM <- function(){
  dir.create(file.path("MergedModels"))
  write.csv(MergedModels_VC,"MergedModels\\MergedModels_VC.csv") #VXC Models
  write.csv(MergedModels_VP,"MergedModels\\MergedModels_VP.csv") #VXP Models
  write.csv(MergedModels_VP_YCO2,"MergedModels\\MergedModels_VPYCO2.csv") #Yearly CO2 emissions (no delay)
  write.csv(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VPYCO2D.csv") #Yearly CO2 emissions (delay)
  write.csv(MergedModels_VP_YC,"MergedModels\\MergedModels_VPYC.csv") #Yearly C emissions
  write.csv(MergedModels_VP_YCT,"MergedModels\\MergedModels_VPYCT.csv") #Yearly C emissions
}
