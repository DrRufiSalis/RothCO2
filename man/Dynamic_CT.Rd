% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dynamize_CT.R
\name{Dynamic_CT}
\alias{Dynamic_CT}
\title{Dynamize Carbon Tails for Dynamic Soil C Balances}
\usage{
Dynamic_CT(Mod_Comb = "VLC3", tonCy, Import = "XLSX")
}
\arguments{
\item{Mod_Comb}{Model combination, to choose from the list in the Function Details (e.g. VLC3 for Litter + Conventional Tillage + 30\% clay, or VMR2 for Manure + Reduced Tillage + 20\% clay). The model has to have been created previously using the Run_RothCO2() function.}

\item{tonCy}{Vector with Tons of C added each year}
}
\description{
The Dynamic_CT function creates a dataframe of C Tails for a given amount of years to serve the basis for Dynamic Soil C Modelling based on Run_RothC_[].
}
\details{
Model Combinations - Litter, Conventional Tillage, 0-100\% (10\%) Clay
VLC0) Litter; 0\%clay; Conventional tillage
VLC1) Litter; 10\%clay; Conventional tillage
VLC2) Litter; 20\%clay; Conventional tillage
VLC3) Litter; 30\%clay; Conventional tillage
VLC4) Litter; 40\%clay; Conventional tillage
VLC5) Litter; 50\%clay; Conventional tillage
VLC6) Litter; 60\%clay; Conventional tillage
VLC7) Litter; 70\%clay; Conventional tillage
VLC8) Litter; 80\%clay; Conventional tillage
VLC9) Litter; 90\%clay; Conventional tillage
VLC10) Litter; 100\%clay; Conventional tillage
Model Combinations - Litter; Reduced Tillage, 0-100\% (+10\%) Clay
VLR0) Litter; 0\%clay; Reduced tillage
VLR1) Litter; 10\%clay; Reduced tillage
VLR2) Litter; 20\%clay; Reduced tillage
VLR3) Litter; 30\%clay; Reduced tillage
VLR4) Litter; 40\%clay; Reduced tillage
VLR5) Litter; 50\%clay; Reduced tillage
VLR6) Litter; 60\%clay; Reduced tillage
VLR7) Litter; 70\%clay; Reduced tillage
VLR8) Litter; 80\%clay; Reduced tillage
VLR9) Litter; 90\%clay; Reduced tillage
VLR10) Litter; 100\%clay; Reduced tillage
Model Combinations - Litter; No Tillage, 0-100\% (+10\%) Clay
VLN0) Litter; 0\%clay; No tillage
VLN1) Litter; 10\%clay; No Tillage
VLN2) Litter; 20\%clay; No Tillage
VLN3) Litter; 30\%clay; No Tillage
VLN4) Litter; 40\%clay; No Tillage
VLN5) Litter; 50\%clay; No Tillage
VLN6) Litter; 60\%clay; No Tillage
VLN7) Litter; 70\%clay; No Tillage
VLN8) Litter; 80\%clay; No Tillage
VLN9) Litter; 90\%clay; No Tillage
VLN10) Litter; 100\%clay; No Tillage
Model Combinations - Manure; Conventional Tillage, 0-100\% (+10\%) Clay
VMC0) Manure; 0\%clay; Conventional tillage
VMC1) Manure; 10\%clay; Conventional tillage
VMC2) Manure; 20\%clay; Conventional tillage
VMC3) Manure; 30\%clay; Conventional tillage
VMC4) Manure; 40\%clay; Conventional tillage
VMC5) Manure; 50\%clay; Conventional tillage
VMC6) Manure; 60\%clay; Conventional tillage
VMC7) Manure; 70\%clay; Conventional tillage
VMC8) Manure; 80\%clay; Conventional tillage
VMC9) Manure; 90\%clay; Conventional tillage
VMC10) Manure; 100\%clay; Conventional tillage
}
