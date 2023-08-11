#' Merge YCO2ND Models
#'
#' The Merge_YCO2ND function merge all YCO2ND models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr readxl writexl
#' @return A dataframe with all YCO2ND models merged together
#' @export

Merge_YCO2ND <- function(Export = "Both"){
  #Put all data frames into list
  path_YCO2ND <- "CO2Emissions_P_NoDelay"

  Model_list_VP_YCO2ND <- list.files(path = path_YCO2ND, pattern = "\\.xlsx$", full.names = TRUE)

  data_frames_YCO2ND <- list()

  for (file in Model_list_VP_YCO2ND) {
    df_YCO2ND <- read_excel(file)
    data_frames_YCO2ND[[file]] <- df_YCO2ND
  }

  #merge all data frames together
  MergedModels_VP_YCO2ND <- data_frames_YCO2ND %>% reduce(left_join, by='Year')
  MergedModels_VP_YCO2ND <- MergedModels_VP_YCO2ND %>% select(-contains("Month")) #Delete Columns containing the word "Month"
  MergedModels_VP_YCO2ND <<- MergedModels_VP_YCO2ND

  if(Export = "XLSX"){
    dir.create(file.path("MergedModels"))
    write_xlsx(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.xlsx") #Yearly C emissions
  }
  else if(Export = "CSV"){
    dir.create(file.path("MergedModels"))
    write.csv(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.csv") #Yearly C emissions
  }
  else{
    dir.create(file.path("MergedModels"))
    write_xlsx(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.xlsx") #Yearly C emissions
    write.csv(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.csv") #Yearly C emissions
  }
}
