#' Merge YCT Models
#'
#' The Merge_YCT function merge all YCT models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr readxl writexl
#' @return A dataframe with all YCT models merged together
#' @export

Merge_YCT <- function(Export = "Both"){
  #Put all data frames into list
  path_YCT <- "CTails_P"

  Model_list_VP_YCT <- list.files(path = path_YCT, pattern = "\\.xlsx$", full.names = TRUE)

  data_frames_YCT <- list()

  for (file in Model_list_VP_YCT) {
    df_YCT <- read_excel(file)
    data_frames_YCT[[file]] <- df_YCT
  }

  #merge all data frames together
  MergedModels_VP_YCT <- data_frames_YCT %>% reduce(left_join, by='Year')
  MergedModels_VP_YCT <- MergedModels_VP_YCT %>% select(-contains("Month")) #Delete Columns containing the word "Month"
  MergedModels_VP_YCT <<- MergedModels_VP_YCT

  if(Export = "XLSX"){
    dir.create(file.path("MergedModels"))
    write_xlsx(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.xlsx") #Yearly C emissions
  }
  else if(Export = "CSV"){
    dir.create(file.path("MergedModels"))
    write.csv(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.csv") #Yearly C emissions
  }
  else{
    dir.create(file.path("MergedModels"))
    write_xlsx(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.xlsx") #Yearly C emissions
    write.csv(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.csv") #Yearly C emissions
  }
}

