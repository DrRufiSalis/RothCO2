#' Merge YCT Models
#'
#' The Merge_YCT function merge all YCT models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr
#' @return A dataframe with all YCT models merged together
#' @export

Merge_YCT <- function(){
#Put all data frames into list
Model_list_VP_YCT <- list(
  ModelDFSL_VLC0P_YCT,
  ModelDFSL_VLC1P_YCT,
  ModelDFSL_VLC2P_YCT,
  ModelDFSL_VLC3P_YCT,
  ModelDFSL_VLC4P_YCT,
  ModelDFSL_VLC5P_YCT,
  ModelDFSL_VLC6P_YCT,
  ModelDFSL_VLC7P_YCT,
  ModelDFSL_VLC8P_YCT,
  ModelDFSL_VLC9P_YCT,
  ModelDFSL_VLC10P_YCT,
  #VXC Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLR0P_YCT,
  ModelDFSL_VLR1P_YCT,
  ModelDFSL_VLR2P_YCT,
  ModelDFSL_VLR3P_YCT,
  ModelDFSL_VLR4P_YCT,
  ModelDFSL_VLR5P_YCT,
  ModelDFSL_VLR6P_YCT,
  ModelDFSL_VLR7P_YCT,
  ModelDFSL_VLR8P_YCT,
  ModelDFSL_VLR9P_YCT,
  ModelDFSL_VLR10P_YCT,
  #VXC Models; Litter, No Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLN0P_YCT,
  ModelDFSL_VLN1P_YCT,
  ModelDFSL_VLN2P_YCT,
  ModelDFSL_VLN3P_YCT,
  ModelDFSL_VLN4P_YCT,
  ModelDFSL_VLN5P_YCT,
  ModelDFSL_VLN6P_YCT,
  ModelDFSL_VLN7P_YCT,
  ModelDFSL_VLN8P_YCT,
  ModelDFSL_VLN9P_YCT,
  ModelDFSL_VLN10P_YCT,
  #VXC Models; Litter, Conventional Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMC0P_YCT,
  ModelDFSL_VMC1P_YCT,
  ModelDFSL_VMC2P_YCT,
  ModelDFSL_VMC3P_YCT,
  ModelDFSL_VMC4P_YCT,
  ModelDFSL_VMC5P_YCT,
  ModelDFSL_VMC6P_YCT,
  ModelDFSL_VMC7P_YCT,
  ModelDFSL_VMC8P_YCT,
  ModelDFSL_VMC9P_YCT,
  ModelDFSL_VMC10P_YCT,
  #VXC Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMR0P_YCT,
  ModelDFSL_VMR1P_YCT,
  ModelDFSL_VMR2P_YCT,
  ModelDFSL_VMR3P_YCT,
  ModelDFSL_VMR4P_YCT,
  ModelDFSL_VMR5P_YCT,
  ModelDFSL_VMR6P_YCT,
  ModelDFSL_VMR7P_YCT,
  ModelDFSL_VMR8P_YCT,
  ModelDFSL_VMR9P_YCT,
  ModelDFSL_VMR10P_YCT,
  #VXC Models; Litter, No Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMN0P_YCT,
  ModelDFSL_VMN1P_YCT,
  ModelDFSL_VMN2P_YCT,
  ModelDFSL_VMN3P_YCT,
  ModelDFSL_VMN4P_YCT,
  ModelDFSL_VMN5P_YCT,
  ModelDFSL_VMN6P_YCT,
  ModelDFSL_VMN7P_YCT,
  ModelDFSL_VMN8P_YCT,
  ModelDFSL_VMN9P_YCT,
  ModelDFSL_VMN10P_YCT
)

# Filter the list to include only data frames from the global environment
Model_list_VP_YCT <- purrr::keep(Model_list_VP_YCT, ~ exists(.x, envir = .GlobalEnv) && is.data.frame(get(.x, envir = .GlobalEnv)))

#merge all data frames together
MergedModels_VP_YCT <- Model_list_VP_YCT %>% reduce(full_join, by='Year')
MergedModels_VP_YCT <- MergedModels_VP_YCT %>% select(-contains("Month")) #Delete Columns containing the word "Month"
}

