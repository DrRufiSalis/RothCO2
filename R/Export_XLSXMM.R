#' Export Merged Models - XLSX
#'
#' The Export_XLSXMM function exports all merged models created.
#'
#' @import writexl
#' @return Excel files with the models
#' @export

Export_XLSXMM <- function(){
dir.create(file.path("MergedModels"))
write_xlsx(MergedModels_VC,"MergedModels\\MergedModels_VC.xlsx") #VXC Models
write_xlsx(MergedModels_VP,"MergedModels\\MergedModels_VP.xlsx") #VXP Models
write_xlsx(MergedModels_VP_YCO2,"MergedModels\\MergedModels_VPYCO2.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VPYCO2D.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(MergedModels_VP_YC,"MergedModels\\MergedModels_VPYC.xlsx") #Yearly C emissions
write_xlsx(MergedModels_VP_YCT,"MergedModels\\MergedModels_VPYCT.xlsx") #Yearly C emissions
}
