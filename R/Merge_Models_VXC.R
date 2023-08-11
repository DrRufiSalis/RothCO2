#' Merge VXC Models
#'
#' The Merge_VXC function merge all VXC models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr readxl writexl
#' @return A dataframe with all VXC models merged together
#' @export

Merge_VXC <- function(Export = "Both"){
  #Put all data frames into list
  path_VXC <- "VXC_Models"

  Model_list_VP_VXC <- list.files(path = path_VXC, pattern = "\\.xlsx$", full.names = TRUE)

  data_frames_VXC <- list()

  for (file in Model_list_VP_VXC) {
    df_VXC <- read_excel(file)
    data_frames_VXC[[file]] <- df_VXC
  }

  #merge all data frames together
  MergedModels_VXC <- data_frames_VXC %>% reduce(left_join, by='MNumber')
  MergedModels_VXC <<- MergedModels_VXC

  if(Export = "XLSX"){
    dir.create(file.path("MergedModels"))
    write_xlsx(MergedModels_VXC,"MergedModels\\MergedModels_VXC.xlsx") #Yearly C emissions
  }
  else if(Export = "CSV"){
    dir.create(file.path("MergedModels"))
    write.csv(MergedModels_VXC,"MergedModels\\MergedModels_VXC.csv") #Yearly C emissions
  }
  else{
    dir.create(file.path("MergedModels"))
    write_xlsx(MergedModels_VXC,"MergedModels\\MergedModels_VXC.xlsx") #Yearly C emissions
    write.csv(MergedModels_VXC,"MergedModels\\MergedModels_VXC.csv") #Yearly C emissions
  }
}
