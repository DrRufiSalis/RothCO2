#' Merge VXP Models
#'
#' The Merge_VXP function merge all VXP models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr readxl writexl
#' @return A dataframe with all VXP models merged together
#' @export

Merge_VXP <- function(Export = "Both"){
  #Put all data frames into list
  path_VXP <- "VXP_Models"

  Model_list_VP_VXP <- list.files(path = path_VXP, pattern = "\\.xlsx$", full.names = TRUE)

  data_frames_VXP <- list()

  for (file in Model_list_VP_VXP) {
    df_VXP <- read_excel(file)
    data_frames_VXP[[file]] <- df_VXP
  }

  #merge all data frames together
  MergedModels_VXP <- data_frames_VXP %>% reduce(left_join, by='MNumber')
  MergedModels_VXP <<- MergedModels_VXP

  if(Export == "XLSX"){
    dir.create(file.path("MergedModels"))
    write_xlsx(MergedModels_VXP,"MergedModels\\MergedModels_VXP.xlsx") #Yearly C emissions
  }
  else if(Export == "CSV"){
    dir.create(file.path("MergedModels"))
    write.csv(MergedModels_VXP,"MergedModels\\MergedModels_VXP.csv") #Yearly C emissions
  }
  else{
    dir.create(file.path("MergedModels"))
    write_xlsx(MergedModels_VXP,"MergedModels\\MergedModels_VXP.xlsx") #Yearly C emissions
    write.csv(MergedModels_VXP,"MergedModels\\MergedModels_VXP.csv") #Yearly C emissions
  }
}
Merge_VXP()
