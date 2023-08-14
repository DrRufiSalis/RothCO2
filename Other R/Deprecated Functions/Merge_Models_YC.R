#' Merge YC Models
#'
#' The Merge_YC function merge all YC models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr readxl writexl
#' @return A dataframe with all YC models merged together
#' @export

Merge_YC <- function(Export = "Both"){
#Put all data frames into list
  path_YC <- "CEmissions_P"

  Model_list_VP_YC <- list.files(path = path_YC, pattern = "\\.xlsx$", full.names = TRUE)

  data_frames_YC <- list()

  for (file in Model_list_VP_YC) {
    df_YC <- read_excel(file)
    data_frames_YC[[file]] <- df_YC
  }

#merge all data frames together
MergedModels_VP_YC <- data_frames_YC %>% reduce(left_join, by='Year')
MergedModels_VP_YC <- MergedModels_VP_YC %>% select(-contains("Month")) #Delete Columns containing the word "Month"
MergedModels_VP_YC <<- MergedModels_VP_YC

if(Export == "XLSX"){
  dir.create(file.path("MergedModels"))
  write_xlsx(MergedModels_VP_YC,"MergedModels\\MergedModels_VPYC.xlsx") #Yearly C emissions
}
else if(Export == "CSV"){
  dir.create(file.path("MergedModels"))
  write.csv(MergedModels_VP_YC,"MergedModels\\MergedModels_VPYC.csv") #Yearly C emissions
}
else{
  dir.create(file.path("MergedModels"))
  write_xlsx(MergedModels_VP_YC,"MergedModels\\MergedModels_VPYC.xlsx") #Yearly C emissions
  write.csv(MergedModels_VP_YC,"MergedModels\\MergedModels_VPYC.csv") #Yearly C emissions
}
}
