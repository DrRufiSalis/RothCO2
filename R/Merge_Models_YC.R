#' Merge YC Models
#'
#' The Merge_YC function merge all YC models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr
#' @return A dataframe with all YC models merged together
#' @export

Merge_YC <- function(){
#Put all data frames into list
Model_list_VP_YC <- list(
  ModelDFSL_VLC0P_YC,
  ModelDFSL_VLC1P_YC,
  ModelDFSL_VLC2P_YC,
  ModelDFSL_VLC3P_YC,
  ModelDFSL_VLC4P_YC,
  ModelDFSL_VLC5P_YC,
  ModelDFSL_VLC6P_YC,
  ModelDFSL_VLC7P_YC,
  ModelDFSL_VLC8P_YC,
  ModelDFSL_VLC9P_YC,
  ModelDFSL_VLC10P_YC,
  #VXC Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLR0P_YC,
  ModelDFSL_VLR1P_YC,
  ModelDFSL_VLR2P_YC,
  ModelDFSL_VLR3P_YC,
  ModelDFSL_VLR4P_YC,
  ModelDFSL_VLR5P_YC,
  ModelDFSL_VLR6P_YC,
  ModelDFSL_VLR7P_YC,
  ModelDFSL_VLR8P_YC,
  ModelDFSL_VLR9P_YC,
  ModelDFSL_VLR10P_YC,
  #VXC Models; Litter, No Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLN0P_YC,
  ModelDFSL_VLN1P_YC,
  ModelDFSL_VLN2P_YC,
  ModelDFSL_VLN3P_YC,
  ModelDFSL_VLN4P_YC,
  ModelDFSL_VLN5P_YC,
  ModelDFSL_VLN6P_YC,
  ModelDFSL_VLN7P_YC,
  ModelDFSL_VLN8P_YC,
  ModelDFSL_VLN9P_YC,
  ModelDFSL_VLN10P_YC,
  #VXC Models; Litter, Conventional Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMC0P_YC,
  ModelDFSL_VMC1P_YC,
  ModelDFSL_VMC2P_YC,
  ModelDFSL_VMC3P_YC,
  ModelDFSL_VMC4P_YC,
  ModelDFSL_VMC5P_YC,
  ModelDFSL_VMC6P_YC,
  ModelDFSL_VMC7P_YC,
  ModelDFSL_VMC8P_YC,
  ModelDFSL_VMC9P_YC,
  ModelDFSL_VMC10P_YC,
  #VXC Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMR0P_YC,
  ModelDFSL_VMR1P_YC,
  ModelDFSL_VMR2P_YC,
  ModelDFSL_VMR3P_YC,
  ModelDFSL_VMR4P_YC,
  ModelDFSL_VMR5P_YC,
  ModelDFSL_VMR6P_YC,
  ModelDFSL_VMR7P_YC,
  ModelDFSL_VMR8P_YC,
  ModelDFSL_VMR9P_YC,
  ModelDFSL_VMR10P_YC,
  #VXC Models; Litter, No Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMN0P_YC,
  ModelDFSL_VMN1P_YC,
  ModelDFSL_VMN2P_YC,
  ModelDFSL_VMN3P_YC,
  ModelDFSL_VMN4P_YC,
  ModelDFSL_VMN5P_YC,
  ModelDFSL_VMN6P_YC,
  ModelDFSL_VMN7P_YC,
  ModelDFSL_VMN8P_YC,
  ModelDFSL_VMN9P_YC,
  ModelDFSL_VMN10P_YC
)

# Filter the list to include only data frames from the global environment
Model_list_VP_YC <- purrr::keep(Model_list_VP_YC, ~ exists(.x, envir = .GlobalEnv) && is.data.frame(get(.x, envir = .GlobalEnv)))

#merge all data frames together
MergedModels_VP_YC <- Model_list_VP_YC %>% reduce(full_join, by='Year')
MergedModels_VP_YC <- MergedModels_VP_YC %>% select(-contains("Month")) #Delete Columns containing the word "Month"
}
