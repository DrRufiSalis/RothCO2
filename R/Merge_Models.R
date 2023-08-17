#' Merge Models created by Run_RothCO2
#'
#' The Merge_Models function merge all chosen models by "Models" parameter created by the functions Run_RothC_[].
#' @param Models From the various models (folders) created in the function Run_RothCO2, choose the ones to merge. Defaults to "All", but can also be: "VXC", "VXP", "YC", "YCO2D", "YCO2ND", "YCT". "VXC" considers a continuous input of C each year (1tC/year). "VXP": considers a one-off input of C in the first year (1tC/fist-year). "YC": yearly C emissions considering a GWP100 with delay (considering a one-off input of 1tC in the first year). "YCO2D": yearly CO2 emissions considering a GWP100 with delay (considering a one-off input of 1tC in the first year). "YCO2ND": yearly CO2 emissions (considering a one-off input of 1tC in the first year). "YCT": yearly C tails (considering a one-off input of 1tC in the first year) - i.e. what remains on the soil.
#' @param Export Export format (.csv or .xlsx). Defaults to "Both", but can also be "XLSX" (.xlsx; microsoft excel file format) or "CSV" (.csv; comma separated values format)
#' @import dplyr purrr readxl writexl
#' @return merged models exported in respective folders and merged models saved in the global environment.
#' @export

Merge_Models <- function(Models = "All",
                         Export = "Both"){



  if(Models == "All"){
 #VXC in All
    #Put all data frames into list
    path_VXC <- "VXC_Models"

    Model_list_VP_VXC <- list.files(path = path_VXC, pattern = "\\.xlsx$", full.names = TRUE)

    data_frames_VXC <- list()

    for (file in Model_list_VP_VXC) {
      df_VXC <- read_excel(file)
      data_frames_VXC[[file]] <- df_VXC
    }

    #merge all data frames together
    MergedModels_VXC <- data_frames_VXC %>% reduce(left_join, by='MNumber')
    MergedModels_VXC <<- MergedModels_VXC

    if(Export == "XLSX"){
      dir.create(file.path("MergedModels"))
      write_xlsx(MergedModels_VXC,"MergedModels\\MergedModels_VXC.xlsx") #Yearly C emissions
    }
    else if(Export == "CSV"){
      dir.create(file.path("MergedModels"))
      write.csv(MergedModels_VXC,"MergedModels\\MergedModels_VXC.csv") #Yearly C emissions
    }
    else{
      dir.create(file.path("MergedModels"))
      write_xlsx(MergedModels_VXC,"MergedModels\\MergedModels_VXC.xlsx") #Yearly C emissions
      write.csv(MergedModels_VXC,"MergedModels\\MergedModels_VXC.csv") #Yearly C emissions
    }

#VXP in All
    #Put all data frames into list
    path_VXP <- "VXP_Models"

    Model_list_VP_VXP <- list.files(path = path_VXP, pattern = "\\.xlsx$", full.names = TRUE)

    data_frames_VXP <- list()

    for (file in Model_list_VP_VXP) {
      df_VXP <- read_excel(file)
      data_frames_VXP[[file]] <- df_VXP
    }

    #merge all data frames together
    MergedModels_VXP <- data_frames_VXP %>% reduce(left_join, by='MNumber')
    MergedModels_VXP <<- MergedModels_VXP

    if(Export == "XLSX"){
      dir.create(file.path("MergedModels"))
      write_xlsx(MergedModels_VXP,"MergedModels\\MergedModels_VXP.xlsx") #Yearly C emissions
    }
    else if(Export == "CSV"){
      dir.create(file.path("MergedModels"))
      write.csv(MergedModels_VXP,"MergedModels\\MergedModels_VXP.csv") #Yearly C emissions
    }
    else{
      dir.create(file.path("MergedModels"))
      write_xlsx(MergedModels_VXP,"MergedModels\\MergedModels_VXP.xlsx") #Yearly C emissions
      write.csv(MergedModels_VXP,"MergedModels\\MergedModels_VXP.csv") #Yearly C emissions
    }

#YC in All
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

#YCO2D in All

    #Put all data frames into list
    path_YCO2D <- "CO2Emissions_P_Delay"

    Model_list_VP_YCO2D <- list.files(path = path_YCO2D, pattern = "\\.xlsx$", full.names = TRUE)

    data_frames_YCO2D <- list()

    for (file in Model_list_VP_YCO2D) {
      df_YCO2D <- read_excel(file)
      data_frames_YCO2D[[file]] <- df_YCO2D
    }

    #merge all data frames together
    MergedModels_VP_YCO2D <- data_frames_YCO2D %>% reduce(left_join, by='Year')
    MergedModels_VP_YCO2D <- MergedModels_VP_YCO2D %>% select(-contains("Month")) #Delete Columns containing the word "Month"
    MergedModels_VP_YCO2D <- MergedModels_VP_YCO2D %>% select(-contains("GWP"))
    MergedModels_VP_YCO2D <- MergedModels_VP_YCO2D %>% select(-contains("AnnualCO2_"))
    MergedModels_VP_YCO2D <<- MergedModels_VP_YCO2D

    if(Export == "XLSX"){
      dir.create(file.path("MergedModels"))
      write_xlsx(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VP_YCO2D.xlsx") #Yearly C emissions
    }
    else if(Export == "CSV"){
      dir.create(file.path("MergedModels"))
      write.csv(MergedModels_VP_YC,"MergedModels\\MergedModels_VP_YCO2D.csv") #Yearly C emissions
    }
    else{
      dir.create(file.path("MergedModels"))
      write_xlsx(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VP_YCO2D.xlsx") #Yearly C emissions
      write.csv(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VP_YCO2D.csv") #Yearly C emissions
    }

#YCO2ND in All
    #Put all data frames into list
    path_YCO2ND <- "CO2Emissions_P_NoDelay"

    Model_list_VP_YCO2ND <- list.files(path = path_YCO2ND, pattern = "\\.xlsx$", full.names = TRUE)

    data_frames_YCO2ND <- list()

    for (file in Model_list_VP_YCO2ND) {
      df_YCO2ND <- read_excel(file)
      data_frames_YCO2ND[[file]] <- df_YCO2ND
    }

    #merge all data frames together
    MergedModels_VP_YCO2ND <- data_frames_YCO2ND %>% reduce(left_join, by='Year')
    MergedModels_VP_YCO2ND <- MergedModels_VP_YCO2ND %>% select(-contains("Month")) #Delete Columns containing the word "Month"
    MergedModels_VP_YCO2ND <<- MergedModels_VP_YCO2ND

    if(Export == "XLSX"){
      dir.create(file.path("MergedModels"))
      write_xlsx(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.xlsx") #Yearly C emissions
    }
    else if(Export == "CSV"){
      dir.create(file.path("MergedModels"))
      write.csv(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.csv") #Yearly C emissions
    }
    else{
      dir.create(file.path("MergedModels"))
      write_xlsx(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.xlsx") #Yearly C emissions
      write.csv(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.csv") #Yearly C emissions
    }

#YCT in All
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

    if(Export == "XLSX"){
      dir.create(file.path("MergedModels"))
      write_xlsx(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.xlsx") #Yearly C emissions
    }
    else if(Export == "CSV"){
      dir.create(file.path("MergedModels"))
      write.csv(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.csv") #Yearly C emissions
    }
    else{
      dir.create(file.path("MergedModels"))
      write_xlsx(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.xlsx") #Yearly C emissions
      write.csv(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.csv") #Yearly C emissions
    }


  }


  else if(Models == "VXC"){
          #Put all data frames into list
          path_VXC <- "VXC_Models"

          Model_list_VP_VXC <- list.files(path = path_VXC, pattern = "\\.xlsx$", full.names = TRUE)

          data_frames_VXC <- list()

          for (file in Model_list_VP_VXC) {
            df_VXC <- read_excel(file)
            data_frames_VXC[[file]] <- df_VXC
          }

          #merge all data frames together
          MergedModels_VXC <- data_frames_VXC %>% reduce(left_join, by='MNumber')
          MergedModels_VXC <<- MergedModels_VXC

          if(Export == "XLSX"){
            dir.create(file.path("MergedModels"))
            write_xlsx(MergedModels_VXC,"MergedModels\\MergedModels_VXC.xlsx") #Yearly C emissions
          }
          else if(Export == "CSV"){
            dir.create(file.path("MergedModels"))
            write.csv(MergedModels_VXC,"MergedModels\\MergedModels_VXC.csv") #Yearly C emissions
          }
          else{
            dir.create(file.path("MergedModels"))
            write_xlsx(MergedModels_VXC,"MergedModels\\MergedModels_VXC.xlsx") #Yearly C emissions
            write.csv(MergedModels_VXC,"MergedModels\\MergedModels_VXC.csv") #Yearly C emissions
          }
  }


  else if(Models == "VXP"){
          #Put all data frames into list
          path_VXP <- "VXP_Models"

          Model_list_VP_VXP <- list.files(path = path_VXP, pattern = "\\.xlsx$", full.names = TRUE)

          data_frames_VXP <- list()

          for (file in Model_list_VP_VXP) {
            df_VXP <- read_excel(file)
            data_frames_VXP[[file]] <- df_VXP
          }

          #merge all data frames together
          MergedModels_VXP <- data_frames_VXP %>% reduce(left_join, by='MNumber')
          MergedModels_VXP <<- MergedModels_VXP

          if(Export == "XLSX"){
            dir.create(file.path("MergedModels"))
            write_xlsx(MergedModels_VXP,"MergedModels\\MergedModels_VXP.xlsx") #Yearly C emissions
          }
          else if(Export == "CSV"){
            dir.create(file.path("MergedModels"))
            write.csv(MergedModels_VXP,"MergedModels\\MergedModels_VXP.csv") #Yearly C emissions
          }
          else{
            dir.create(file.path("MergedModels"))
            write_xlsx(MergedModels_VXP,"MergedModels\\MergedModels_VXP.xlsx") #Yearly C emissions
            write.csv(MergedModels_VXP,"MergedModels\\MergedModels_VXP.csv") #Yearly C emissions
          }
        }


  else if(Models == "YC"){
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


  else if(Models == "YCO2D"){
          #Put all data frames into list
          path_YCO2D <- "CO2Emissions_P_Delay"

          Model_list_VP_YCO2D <- list.files(path = path_YCO2D, pattern = "\\.xlsx$", full.names = TRUE)

          data_frames_YCO2D <- list()

          for (file in Model_list_VP_YCO2D) {
            df_YCO2D <- read_excel(file)
            data_frames_YCO2D[[file]] <- df_YCO2D
          }

          #merge all data frames together
          MergedModels_VP_YCO2D <- data_frames_YCO2D %>% reduce(left_join, by='Year')
          MergedModels_VP_YCO2D <- MergedModels_VP_YCO2D %>% select(-contains("Month")) #Delete Columns containing the word "Month"
          MergedModels_VP_YCO2D <- MergedModels_VP_YCO2D %>% select(-contains("GWP")) #Delete Columns containing the word "GWP"
          MergedModels_VP_YCO2D <- MergedModels_VP_YCO2D %>% select(-contains("AnnualCO2_")) #Delete Columns containing the word "AnnualCO_", this keeps only the delayed CO2 and deleted the non-delayed
          MergedModels_VP_YCO2D <<- MergedModels_VP_YCO2D

          if(Export == "XLSX"){
            dir.create(file.path("MergedModels"))
            write_xlsx(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VP_YCO2D.xlsx") #Yearly C emissions
          }
          else if(Export == "CSV"){
            dir.create(file.path("MergedModels"))
            write.csv(MergedModels_VP_YC,"MergedModels\\MergedModels_VP_YCO2D.csv") #Yearly C emissions
          }
          else{
            dir.create(file.path("MergedModels"))
            write_xlsx(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VP_YCO2D.xlsx") #Yearly C emissions
            write.csv(MergedModels_VP_YCO2D,"MergedModels\\MergedModels_VP_YCO2D.csv") #Yearly C emissions
          }
        }


  else if(Models == "YCO2ND"){
          #Put all data frames into list
          path_YCO2ND <- "CO2Emissions_P_NoDelay"

          Model_list_VP_YCO2ND <- list.files(path = path_YCO2ND, pattern = "\\.xlsx$", full.names = TRUE)

          data_frames_YCO2ND <- list()

          for (file in Model_list_VP_YCO2ND) {
            df_YCO2ND <- read_excel(file)
            data_frames_YCO2ND[[file]] <- df_YCO2ND
          }

          #merge all data frames together
          MergedModels_VP_YCO2ND <- data_frames_YCO2ND %>% reduce(left_join, by='Year')
          MergedModels_VP_YCO2ND <- MergedModels_VP_YCO2ND %>% select(-contains("Month")) #Delete Columns containing the word "Month"
          MergedModels_VP_YCO2ND <<- MergedModels_VP_YCO2ND

          if(Export == "XLSX"){
            dir.create(file.path("MergedModels"))
            write_xlsx(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.xlsx") #Yearly C emissions
          }
          else if(Export == "CSV"){
            dir.create(file.path("MergedModels"))
            write.csv(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.csv") #Yearly C emissions
          }
          else{
            dir.create(file.path("MergedModels"))
            write_xlsx(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.xlsx") #Yearly C emissions
            write.csv(MergedModels_VP_YCO2ND,"MergedModels\\MergedModels_VP_YCO2ND.csv") #Yearly C emissions
          }
        }


  else if(Models == "YCT"){

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

          if(Export == "XLSX"){
            dir.create(file.path("MergedModels"))
            write_xlsx(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.xlsx") #Yearly C emissions
          }
          else if(Export == "CSV"){
            dir.create(file.path("MergedModels"))
            write.csv(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.csv") #Yearly C emissions
          }
          else{
            dir.create(file.path("MergedModels"))
            write_xlsx(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.xlsx") #Yearly C emissions
            write.csv(MergedModels_VP_YCT,"MergedModels\\MergedModels_VP_YCT.csv") #Yearly C emissions
          }
        }

  }
