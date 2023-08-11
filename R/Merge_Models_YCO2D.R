#' Merge YCO2D Models
#'
#' The Merge_YCO2D function merges all YCO2D models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr readxl writexl
#' @return A dataframe with all YCO2D models merged together
#' @export

Merge_YCO2D <- function(Export = "Both"){
  #Put all data frames into list
  path_YCO2D <- "CO2Emissions_P_Delay"

  Model_list_VP_YCO2D <- list.files(path = path_YCO2D, pattern = "\\.xlsx$", full.names = TRUE)

  data_frames_YCO2D <- list()

  for (file in Model_list_VP_YCO2D) {
    df_YCO2D <- read_excel(file)
    data_frames_YCO2D[[file]] <- df_YCO2D
  }

  #merge all data frames together
  MergedModels_VP_YCO2D <- data_frames_YCO2D %>% reduce(left_join, by='Year')
  MergedModels_VP_YCO2D <- MergedModels_VP_YCO2D %>% select(-contains("Month")) #Delete Columns containing the word "Month"
  MergedModels_VP_YCO2D <<- MergedModels_VP_YCO2D

  if(Export = "XLSX"){
    dir.create(file.path("MergedModels"))
    write_xlsx(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VP_YCO2D.xlsx") #Yearly C emissions
  }
  else if(Export = "CSV"){
    dir.create(file.path("MergedModels"))
    write.csv(MergedModels_VP_YC,"MergedModels\\MergedModels_VP_YCO2D.csv") #Yearly C emissions
  }
  else{
    dir.create(file.path("MergedModels"))
    write_xlsx(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VP_YCO2D.xlsx") #Yearly C emissions
    write.csv(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VP_YCO2D.csv") #Yearly C emissions
  }
}

