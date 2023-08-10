#' Merge YCO2ND Models
#'
#' The Merge_YCO2D function merge all YCO2ND models created by the functions Run_RothC_[].
#' @import dplyr purrr
#' @return A dataframe with all YCO2ND models merged together
#' @export

Merge_YCO2ND <- function(){
#Put all data frames into list
Model_list_VP_YCO2 <- list(
  #VXC Models; Litter, Conventional Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLC0P_YCO2,
  ModelDFSL_VLC1P_YCO2,
  ModelDFSL_VLC2P_YCO2,
  ModelDFSL_VLC3P_YCO2,
  ModelDFSL_VLC4P_YCO2,
  ModelDFSL_VLC5P_YCO2,
  ModelDFSL_VLC6P_YCO2,
  ModelDFSL_VLC7P_YCO2,
  ModelDFSL_VLC8P_YCO2,
  ModelDFSL_VLC9P_YCO2,
  ModelDFSL_VLC10P_YCO2,
  #VXC Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLR0P_YCO2,
  ModelDFSL_VLR1P_YCO2,
  ModelDFSL_VLR2P_YCO2,
  ModelDFSL_VLR3P_YCO2,
  ModelDFSL_VLR4P_YCO2,
  ModelDFSL_VLR5P_YCO2,
  ModelDFSL_VLR6P_YCO2,
  ModelDFSL_VLR7P_YCO2,
  ModelDFSL_VLR8P_YCO2,
  ModelDFSL_VLR9P_YCO2,
  ModelDFSL_VLR10P_YCO2,
  #VXC Models; Litter, No Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLN0P_YCO2,
  ModelDFSL_VLN1P_YCO2,
  ModelDFSL_VLN2P_YCO2,
  ModelDFSL_VLN3P_YCO2,
  ModelDFSL_VLN4P_YCO2,
  ModelDFSL_VLN5P_YCO2,
  ModelDFSL_VLN6P_YCO2,
  ModelDFSL_VLN7P_YCO2,
  ModelDFSL_VLN8P_YCO2,
  ModelDFSL_VLN9P_YCO2,
  ModelDFSL_VLN10P_YCO2,
  #VXC Models; Litter, Conventional Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMC0P_YCO2,
  ModelDFSL_VMC1P_YCO2,
  ModelDFSL_VMC2P_YCO2,
  ModelDFSL_VMC3P_YCO2,
  ModelDFSL_VMC4P_YCO2,
  ModelDFSL_VMC5P_YCO2,
  ModelDFSL_VMC6P_YCO2,
  ModelDFSL_VMC7P_YCO2,
  ModelDFSL_VMC8P_YCO2,
  ModelDFSL_VMC9P_YCO2,
  ModelDFSL_VMC10P_YCO2,
  #VXC Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMR0P_YCO2,
  ModelDFSL_VMR1P_YCO2,
  ModelDFSL_VMR2P_YCO2,
  ModelDFSL_VMR3P_YCO2,
  ModelDFSL_VMR4P_YCO2,
  ModelDFSL_VMR5P_YCO2,
  ModelDFSL_VMR6P_YCO2,
  ModelDFSL_VMR7P_YCO2,
  ModelDFSL_VMR8P_YCO2,
  ModelDFSL_VMR9P_YCO2,
  ModelDFSL_VMR10P_YCO2,
  #VXC Models; Litter, No Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMN0P_YCO2,
  ModelDFSL_VMN1P_YCO2,
  ModelDFSL_VMN2P_YCO2,
  ModelDFSL_VMN3P_YCO2,
  ModelDFSL_VMN4P_YCO2,
  ModelDFSL_VMN5P_YCO2,
  ModelDFSL_VMN6P_YCO2,
  ModelDFSL_VMN7P_YCO2,
  ModelDFSL_VMN8P_YCO2,
  ModelDFSL_VMN9P_YCO2,
  ModelDFSL_VMN10P_YCO2
)

# Filter the list to include only data frames from the global environment
Model_list_VP_YCO2 <- purrr::keep(Model_list_VP_YCO2, ~ exists(.x, envir = .GlobalEnv) && is.data.frame(get(.x, envir = .GlobalEnv)))

#merge all data frames together
MergedModels_VP_YCO2 <- Model_list_VP_YCO2 %>% reduce(full_join, by='Year')
MergedModels_VP_YCO2 <- MergedModels_VP_YCO2 %>% select(-contains("Month")) #Delete Columns containing the word "Month"
}

