#' Merge VXP Models
#'
#' The Merge_VXP function merge all VXP models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr
#' @return A mvector with monthly precipitation values
#' @export

Merge_VXP <- function(){
#Put all data frames into list
Model_list_VP <- list(
  #VXC Models; Litter, Conventional Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLC0P,
  ModelDFSL_VLC1P,
  ModelDFSL_VLC2P,
  ModelDFSL_VLC3P,
  ModelDFSL_VLC4P,
  ModelDFSL_VLC5P,
  ModelDFSL_VLC6P,
  ModelDFSL_VLC7P,
  ModelDFSL_VLC8P,
  ModelDFSL_VLC9P,
  ModelDFSL_VLC10P,
  #VXC Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLR0P,
  ModelDFSL_VLR1P,
  ModelDFSL_VLR2P,
  ModelDFSL_VLR3P,
  ModelDFSL_VLR4P,
  ModelDFSL_VLR5P,
  ModelDFSL_VLR6P,
  ModelDFSL_VLR7P,
  ModelDFSL_VLR8P,
  ModelDFSL_VLR9P,
  ModelDFSL_VLR10P,
  #VXC Models; Litter, No Tillage, 0-100% (+10%) Clay
  ModelDFSL_VLN0P,
  ModelDFSL_VLN1P,
  ModelDFSL_VLN2P,
  ModelDFSL_VLN3P,
  ModelDFSL_VLN4P,
  ModelDFSL_VLN5P,
  ModelDFSL_VLN6P,
  ModelDFSL_VLN7P,
  ModelDFSL_VLN8P,
  ModelDFSL_VLN9P,
  ModelDFSL_VLN10P,
  #VXC Models; Litter, Conventional Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMC0P,
  ModelDFSL_VMC1P,
  ModelDFSL_VMC2P,
  ModelDFSL_VMC3P,
  ModelDFSL_VMC4P,
  ModelDFSL_VMC5P,
  ModelDFSL_VMC6P,
  ModelDFSL_VMC7P,
  ModelDFSL_VMC8P,
  ModelDFSL_VMC9P,
  ModelDFSL_VMC10P,
  #VXC Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMR0P,
  ModelDFSL_VMR1P,
  ModelDFSL_VMR2P,
  ModelDFSL_VMR3P,
  ModelDFSL_VMR4P,
  ModelDFSL_VMR5P,
  ModelDFSL_VMR6P,
  ModelDFSL_VMR7P,
  ModelDFSL_VMR8P,
  ModelDFSL_VMR9P,
  ModelDFSL_VMR10P,
  #VXC Models; Litter, No Tillage, 0-100% (+10%) Clay
  ModelDFSL_VMN0P,
  ModelDFSL_VMN1P,
  ModelDFSL_VMN2P,
  ModelDFSL_VMN3P,
  ModelDFSL_VMN4P,
  ModelDFSL_VMN5P,
  ModelDFSL_VMN6P,
  ModelDFSL_VMN7P,
  ModelDFSL_VMN8P,
  ModelDFSL_VMN9P,
  ModelDFSL_VMN10P
)

# Filter the list to include only data frames from the global environment
Model_list_VP <- purrr::keep(Model_list_VP, ~ exists(.x, envir = .GlobalEnv) && is.data.frame(get(.x, envir = .GlobalEnv)))

#merge all data frames together
MergedModels_VP <- Model_list_VP %>% reduce(full_join, by='MNumber')
}
