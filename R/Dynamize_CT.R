#' Dynamize Carbon Tails for Dynamic Soil C Balances
#'
#' The Dynamic_CT function creates a dataframe of C Tails for a given amount of years to serve the basis for Dynamic Soil C Modelling based on Run_RothC_[].
#'
#' Model Combinations - Litter, Conventional Tillage, 0-100% (10%) Clay
#' VLC0) Litter; 0%clay; Conventional tillage
#' VLC1) Litter; 10%clay; Conventional tillage
#' VLC2) Litter; 20%clay; Conventional tillage
#' VLC3) Litter; 30%clay; Conventional tillage
#' VLC4) Litter; 40%clay; Conventional tillage
#' VLC5) Litter; 50%clay; Conventional tillage
#' VLC6) Litter; 60%clay; Conventional tillage
#' VLC7) Litter; 70%clay; Conventional tillage
#' VLC8) Litter; 80%clay; Conventional tillage
#' VLC9) Litter; 90%clay; Conventional tillage
#' VLC10) Litter; 100%clay; Conventional tillage

#' Model Combinations - Litter; Reduced Tillage, 0-100% (+10%) Clay
#' VLR0) Litter; 0%clay; Reduced tillage
#' VLR1) Litter; 10%clay; Reduced tillage
#' VLR2) Litter; 20%clay; Reduced tillage
#' VLR3) Litter; 30%clay; Reduced tillage
#' VLR4) Litter; 40%clay; Reduced tillage
#' VLR5) Litter; 50%clay; Reduced tillage
#' VLR6) Litter; 60%clay; Reduced tillage
#' VLR7) Litter; 70%clay; Reduced tillage
#' VLR8) Litter; 80%clay; Reduced tillage
#' VLR9) Litter; 90%clay; Reduced tillage
#' VLR10) Litter; 100%clay; Reduced tillage

#' Model Combinations - Litter; No Tillage, 0-100% (+10%) Clay
#' VLN0) Litter; 0%clay; No tillage
#' VLN1) Litter; 10%clay; No Tillage
#' VLN2) Litter; 20%clay; No Tillage
#' VLN3) Litter; 30%clay; No Tillage
#' VLN4) Litter; 40%clay; No Tillage
#' VLN5) Litter; 50%clay; No Tillage
#' VLN6) Litter; 60%clay; No Tillage
#' VLN7) Litter; 70%clay; No Tillage
#' VLN8) Litter; 80%clay; No Tillage
#' VLN9) Litter; 90%clay; No Tillage
#' VLN10) Litter; 100%clay; No Tillage

#' Model Combinations - Manure; Conventional Tillage, 0-100% (+10%) Clay
#' VMC0) Manure; 0%clay; Conventional tillage
#' VMC1) Manure; 10%clay; Conventional tillage
#' VMC2) Manure; 20%clay; Conventional tillage
#' VMC3) Manure; 30%clay; Conventional tillage
#' VMC4) Manure; 40%clay; Conventional tillage
#' VMC5) Manure; 50%clay; Conventional tillage
#' VMC6) Manure; 60%clay; Conventional tillage
#' VMC7) Manure; 70%clay; Conventional tillage
#' VMC8) Manure; 80%clay; Conventional tillage
#' VMC9) Manure; 90%clay; Conventional tillage
#' VMC10) Manure; 100%clay; Conventional tillage
#' @param Mod_Comb Model combination, to choose from the list in the Function Details (e.g. VLC3 for Litter + Conventional Tillage + 30% clay, or VMR2 for Manure + Reduced Tillage + 20% clay). The model has to have been created previously using the Run_RothCO2() function.
#' @param tonCy Vector with Tons of C added each year
#' @import dplyr purrr readxl writexl qpcR
#' @export

Dynamic_CT <- function(Mod_Comb = "VLC3", #Model Combination
                       tonCy, #Vector with Tons of C added each year
                       Import = "XLSX") #Import format: XLSX or CSV

  {

if(Import == "XLSX"){
  Merged_File <- read_excel("MergedModels/MergedModels_VP_YCT.xlsx")
}

else if(Import == "CSV"){
  Merged_File <- read.csv("MergedModels/MergedModels_VP_YCT.csv")
  Merged_File <- Merged_File[,-1]
}

#Keep only the Model Comb name in the columns
foo <- function(x) gsub("^[^_]*_", "", x)
Merged_File <- Merged_File %>% rename_all(foo)

#Select only the column that coincides with the Model Combination specified by the user
Merged_File <- Merged_File %>% dplyr::select(Mod_Comb)
colnames(Merged_File) <- "Year_0"

#Extract the number of rows to obtain the simulation length
SL_years <- as.integer(nrow(Merged_File))

#Loop Lag to Dynamize Tails
for (i in 1:SL_years) {
  Merged_File[nrow(Merged_File) + 1, ] <- NA
  Merged_File[paste0("Year_", i)] <- dplyr::lag(Merged_File$Year_0, n = i)
}

#NAs to 0s
Merged_File[is.na(Merged_File)] <- 0

Merged_File <<- Merged_File

tonCy <- as.data.frame(tonCy_V)
# Final_File <- cbind(tonCy, Merged_File)

# creating the column id for dataframe 1
Merged_File = cbind("id"=rownames(Merged_File),Merged_File)

# creating column id for dataframe 2
tonCy = cbind("id"=rownames(tonCy),tonCy)

Final_File <- merge(tonCy,Merged_File,by="id",all=T)
Final_File$id <- as.numeric(as.character(Final_File$id))
Final_File <- Final_File[order(Final_File$id),]
Final_File[is.na(Final_File)] <- 0


Final_File <<- Final_File

}

tonCy_V <- c(1,3,6,8,10)
Dynamic_CT("VLC3", tonCy_V, "XLSX")
