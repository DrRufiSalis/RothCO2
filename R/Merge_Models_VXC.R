#' Merge VXC Models
#'
#' The Merge_VXC function merge all VXC models created by the functions Run_RothC_[].
#'
#' @import dplyr purrr
#' @return A dataframe with all VXC models merged together
#' @export

Merge_VXC <- function(){
#Put all data frames into list

  Model_list_VC <- list(
  #VXC Models; Litter, Conventional Tillage, 0-100% (+10%) Clay
  "ModelDFSL_VLC0C",
  "ModelDFSL_VLC1C",
  "ModelDFSL_VLC2C",
  "ModelDFSL_VLC3C",
  "ModelDFSL_VLC4C",
  "ModelDFSL_VLC5C",
  "ModelDFSL_VLC6C",
  "ModelDFSL_VLC7C",
  "ModelDFSL_VLC8C",
  "ModelDFSL_VLC9C",
  "ModelDFSL_VLC10C",
  #VXC "Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  "ModelDFSL_VLR0C",
  "ModelDFSL_VLR1C",
  "ModelDFSL_VLR2C",
  "ModelDFSL_VLR3C",
  "ModelDFSL_VLR4C",
  "ModelDFSL_VLR5C",
  "ModelDFSL_VLR6C",
  "ModelDFSL_VLR7C",
  "ModelDFSL_VLR8C",
  "ModelDFSL_VLR9C",
  "ModelDFSL_VLR10C",
  #VXC "Models; Litter, No Tillage, 0-100% (+10%) Clay
  "ModelDFSL_VLN0C",
  "ModelDFSL_VLN1C",
  "ModelDFSL_VLN2C",
  "ModelDFSL_VLN3C",
  "ModelDFSL_VLN4C",
  "ModelDFSL_VLN5C",
  "ModelDFSL_VLN6C",
  "ModelDFSL_VLN7C",
  "ModelDFSL_VLN8C",
  "ModelDFSL_VLN9C",
  "ModelDFSL_VLN10C",
  #VXC "Models; Litter, Conventional Tillage, 0-100% (+10%) Clay
  "ModelDFSL_VMC0C",
  "ModelDFSL_VMC1C",
  "ModelDFSL_VMC2C",
  "ModelDFSL_VMC3C",
  "ModelDFSL_VMC4C",
  "ModelDFSL_VMC5C",
  "ModelDFSL_VMC6C",
  "ModelDFSL_VMC7C",
  "ModelDFSL_VMC8C",
  "ModelDFSL_VMC9C",
  "ModelDFSL_VMC10C",
  #VXC "Models; Litter, Reduced Tillage, 0-100% (+10%) Clay
  "ModelDFSL_VMR0C",
  "ModelDFSL_VMR1C",
  "ModelDFSL_VMR2C",
  "ModelDFSL_VMR3C",
  "ModelDFSL_VMR4C",
  "ModelDFSL_VMR5C",
  "ModelDFSL_VMR6C",
  "ModelDFSL_VMR7C",
  "ModelDFSL_VMR8C",
  "ModelDFSL_VMR9C",
  "ModelDFSL_VMR10C",
  #VXC "Models; Litter, No Tillage, 0-100% (+10%) Clay
  "ModelDFSL_VMN0C",
  "ModelDFSL_VMN1C",
  "ModelDFSL_VMN2C",
  "ModelDFSL_VMN3C",
  "ModelDFSL_VMN4C",
  "ModelDFSL_VMN5C",
  "ModelDFSL_VMN6C",
  "ModelDFSL_VMN7C",
  "ModelDFSL_VMN8C",
  "ModelDFSL_VMN9C",
  "ModelDFSL_VMN10C"
)

# Filter the list to include only data frames from the global environment
Model_list_VC <- purrr::keep(Model_list_VC, ~ exists(.x, envir = .GlobalEnv) && is.data.frame(get(.x, envir = .GlobalEnv)))

#merge all data frames together
MergedModels_VC <- Model_list_VC %>% reduce(full_join, by='MNumber')
}


