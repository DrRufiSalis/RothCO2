#' Merge YCO2D Models
#'
#' The Merge_YCO2D function merges all YCO2D models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr
#' @return A dataframe with YCO2 Models merged together
#' @export

Merge_YCO2D <- function(){
#Put all data frames into list
Model_list_VP_YCO2D <- list(
  #VXC Models; Litter, Conventional Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLC0P_YCO2D,
  ModelDFSL_VLC1P_YCO2D,
  ModelDFSL_VLC2P_YCO2D,
  ModelDFSL_VLC3P_YCO2D,
  ModelDFSL_VLC4P_YCO2D,
  ModelDFSL_VLC5P_YCO2D,
  ModelDFSL_VLC6P_YCO2D,
  ModelDFSL_VLC7P_YCO2D,
  ModelDFSL_VLC8P_YCO2D,
  ModelDFSL_VLC9P_YCO2D,
  ModelDFSL_VLC10P_YCO2D,
  #VXC Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLR0P_YCO2D,
  ModelDFSL_VLR1P_YCO2D,
  ModelDFSL_VLR2P_YCO2D,
  ModelDFSL_VLR3P_YCO2D,
  ModelDFSL_VLR4P_YCO2D,
  ModelDFSL_VLR5P_YCO2D,
  ModelDFSL_VLR6P_YCO2D,
  ModelDFSL_VLR7P_YCO2D,
  ModelDFSL_VLR8P_YCO2D,
  ModelDFSL_VLR9P_YCO2D,
  ModelDFSL_VLR10P_YCO2D,
  #VXC Models; Litter, No Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLN0P_YCO2D,
  ModelDFSL_VLN1P_YCO2D,
  ModelDFSL_VLN2P_YCO2D,
  ModelDFSL_VLN3P_YCO2D,
  ModelDFSL_VLN4P_YCO2D,
  ModelDFSL_VLN5P_YCO2D,
  ModelDFSL_VLN6P_YCO2D,
  ModelDFSL_VLN7P_YCO2D,
  ModelDFSL_VLN8P_YCO2D,
  ModelDFSL_VLN9P_YCO2D,
  ModelDFSL_VLN10P_YCO2D,
  #VXC Models; Litter, Conventional Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMC0P_YCO2D,
  ModelDFSL_VMC1P_YCO2D,
  ModelDFSL_VMC2P_YCO2D,
  ModelDFSL_VMC3P_YCO2D,
  ModelDFSL_VMC4P_YCO2D,
  ModelDFSL_VMC5P_YCO2D,
  ModelDFSL_VMC6P_YCO2D,
  ModelDFSL_VMC7P_YCO2D,
  ModelDFSL_VMC8P_YCO2D,
  ModelDFSL_VMC9P_YCO2D,
  ModelDFSL_VMC10P_YCO2D,
  #VXC Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMR0P_YCO2D,
  ModelDFSL_VMR1P_YCO2D,
  ModelDFSL_VMR2P_YCO2D,
  ModelDFSL_VMR3P_YCO2D,
  ModelDFSL_VMR4P_YCO2D,
  ModelDFSL_VMR5P_YCO2D,
  ModelDFSL_VMR6P_YCO2D,
  ModelDFSL_VMR7P_YCO2D,
  ModelDFSL_VMR8P_YCO2D,
  ModelDFSL_VMR9P_YCO2D,
  ModelDFSL_VMR10P_YCO2D,
  #VXC Models; Litter, No Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMN0P_YCO2D,
  ModelDFSL_VMN1P_YCO2D,
  ModelDFSL_VMN2P_YCO2D,
  ModelDFSL_VMN3P_YCO2D,
  ModelDFSL_VMN4P_YCO2D,
  ModelDFSL_VMN5P_YCO2D,
  ModelDFSL_VMN6P_YCO2D,
  ModelDFSL_VMN7P_YCO2D,
  ModelDFSL_VMN8P_YCO2D,
  ModelDFSL_VMN9P_YCO2D,
  ModelDFSL_VMN10P_YCO2D
)

# Filter the list to include only data frames from the global environment
Model_list_VP_YCO2D <- purrr::keep(Model_list_VP_YCO2D, ~ exists(.x, envir = .GlobalEnv) && is.data.frame(get(.x, envir = .GlobalEnv)))

#merge all data frames together
MergedModels_VP_YCO2D <- Model_list_VP_YCO2D %>% reduce(full_join, by='Year')
MergedModels_VP_YCO2D <- MergedModels_VP_YCO2D %>% select(-contains("Month")) #Delete Columns containing the word "Month"
MergedModels_VP_YCO2D <- MergedModels_VP_YCO2D %>% select(-contains("GWP100")) #Delete Columns containing the word "GWP100"
}


