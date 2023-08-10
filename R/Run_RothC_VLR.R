#' Run RothC_VLR
#'
#' The Merge_VXC function merge all VXC models created by the functions Run_RothC_[].
#'
#' @return A dataframe with all VXC models merged together
#' @import SoilR ggplot2 stringr
#' @importFrom plotly ggplotly
#' @export

#Function to Run and Create the multiple RothC Combinations

Run_RothC_VLR <- function(SL_years = 100,
                          soil.thick = 23,
                          Tem = Temp,
                          Pre = Precip,
                          Eva = Evp,
                          RTRM = 0.93)

{
  # Create Folders to Save Models
  dir.create(file.path("VXC_Models"))
  dir.create(file.path("VXP_Models"))
  dir.create(file.path("CO2Emissions_P_NoDelay"))
  dir.create(file.path("CO2Emissions_P_Delay"))
  dir.create(file.path("CEmissions_P"))
  dir.create(file.path("CTails_P"))
  #### Pre-requisites ####
  #Define Clay Range (Fixed)
  clay0=0        #0% - Percentage of clay
  clay100=100    #100% - Percentage of clay

  #Define C-Input Simulation Parameters
  Scalar_LitterCinputs=1 #Litter ANNUAL C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  M_Scalar_LitterCinputs=Scalar_LitterCinputs/12 #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  Scalar_ManureCinputs=1 #Manure ANNUAL C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  M_Scalar_ManureCinputs=Scalar_ManureCinputs/12 #Manure MONHTLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams

  #Wrangle Simulation Length
  SimulationLength_months = SL_years*12
  years = seq(1/12, SL_years, by=1/12) #Writing this instead of just "SimulationLengthYear" increases the resolution of the model

  #### 8) Model Combinations - Litter; Reduced Tillage, 0-100% (+10%) Clay ####
  #### 8.1 - VLR0) Litter; 0%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR0=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR0=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay0,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR0=data.frame(years,rep(fT_VLR0*fW_VLR0,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR0=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay0, #Percent clay in mineral soil
    xi=xi.frame_VLR0) #Loads the model

  Ct_VLR0=getC(Model_VLR0) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR0, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR0 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR0 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR0 <- as.data.frame(VEC_Lit_VLR0) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR0$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR0 <- as.data.frame(VEC_Man_VLR0) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR0$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR0, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR0, class) #Check that class is numeric
  LitterCinputs_VLR0=VEC_LitDF_VLR0   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR0=VEC_ManDF_VLR0 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR0 <- merge(LitterCinputs_VLR0, ManureCinputs_VLR0, by = "MNumber")
  MCinputs_VLR0$MInput_VLR0 <- MCinputs_VLR0$VEC_Lit_VLR0 + MCinputs_VLR0$VEC_Man_VLR0

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR0)[which(names(MCinputs_VLR0) == "VEC_Lit_VLR0")] <- "LitterC_VLR0"
  colnames(MCinputs_VLR0)[which(names(MCinputs_VLR0) == "VEC_Man_VLR0")] <- "ManureC_VLR0"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR0 <- as.data.frame(Ct_VLR0)
  colnames(ModelDF_VLR0) <- c('DPM_VLR0','RPM_VLR0','BIO_VLR0', 'HUM_VLR0', 'IOM_VLR0')
  ModelDFS_VLR0 <- ModelDF_VLR0[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR0$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR0$AllPools_VLR0 <- ModelDFS_VLR0$DPM_VLR0 + ModelDFS_VLR0$RPM_VLR0 + ModelDFS_VLR0$BIO_VLR0 + ModelDFS_VLR0$HUM_VLR0 + ModelDFS_VLR0$IOM_VLR0

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR0$AllPools_noIOM_VLR0 <- ModelDFS_VLR0$DPM_VLR0 + ModelDFS_VLR0$RPM_VLR0 + ModelDFS_VLR0$BIO_VLR0 + ModelDFS_VLR0$HUM_VLR0

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR0 <- merge(ModelDFS_VLR0, MCinputs_VLR0, by = "MNumber")

  ModelDFSL_VLR0$MInput_VLR0 <- ModelDFSL_VLR0$LitterC_VLR0 + ModelDFSL_VLR0$ManureC_VLR0
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR0$CTails_VLR0 <- ModelDFSL_VLR0$AllPools_noIOM_VLR0 + ModelDFSL_VLR0$MInput_VLR0

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR0$AccumInput_VLR0 = ModelDFSL_VLR0$AccumInput_VLR0=cumsum(ModelDFSL_VLR0$MInput_VLR0)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR0$MGrowth_DPM_VLR0 <- ave(ModelDFSL_VLR0$DPM_VLR0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR0$MGrowth_RPM_VLR0 <- ave(ModelDFSL_VLR0$RPM_VLR0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR0$MGrowth_BIO_VLR0 <- ave(ModelDFSL_VLR0$BIO_VLR0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR0$MGrowth_HUM_VLR0 <- ave(ModelDFSL_VLR0$HUM_VLR0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR0$MGrowth_IOM_VLR0 <- ave(ModelDFSL_VLR0$IOM_VLR0, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR0$M_CCO2_VLR0 <- ModelDFSL_VLR0$MInput_VLR0 - ModelDFSL_VLR0$MGrowth_DPM_VLR0 - ModelDFSL_VLR0$MGrowth_RPM_VLR0 - ModelDFSL_VLR0$MGrowth_BIO_VLR0 - ModelDFSL_VLR0$MGrowth_HUM_VLR0
  ModelDFSL_VLR0$Accum_CCO2_VLR0 <- ModelDFSL_VLR0$AccumInput_VLR0 - ModelDFSL_VLR0$AllPools_noIOM_VLR0

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR0$M_CCO2_VLR0[1] <- 0
  ModelDFSL_VLR0$Accum_CCO2_VLR0[1] <- 0

  #Balance validation
  ModelDFSL_VLR0$Balance_VLR0 <- ModelDFSL_VLR0$AccumInput_VLR0 - ModelDFSL_VLR0$Accum_CCO2_VLR0 - (ModelDFSL_VLR0$DPM_VLR0 + ModelDFSL_VLR0$RPM_VLR0 + ModelDFSL_VLR0$BIO_VLR0 + ModelDFSL_VLR0$HUM_VLR0)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR0$M_CO2_VLR0 <- ModelDFSL_VLR0$M_CCO2_VLR0 * 44/12
  ModelDFSL_VLR0$Accum_CO2_VLR0 <- ModelDFSL_VLR0$Accum_CCO2_VLR0 * 44/12

  #This model will be called VLR0C because implies a continuous input of C
  ModelDFSL_VLR0C <- ModelDFSL_VLR0

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR0C,"VXC_Models\\ModelDFSL_R_VLR0C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR0 <- ModelDFSL_VLR0 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR0.1 <- rbind(c(0:0), ModelDFSLt0_VLR0)
  ModelDFSLt1_VLR0.2 <- rbind(c(0:0), ModelDFSLt1_VLR0.1)
  ModelDFSLt1_VLR0.3 <- rbind(c(0:0), ModelDFSLt1_VLR0.2)
  ModelDFSLt1_VLR0.4 <- rbind(c(0:0), ModelDFSLt1_VLR0.3)
  ModelDFSLt1_VLR0.5 <- rbind(c(0:0), ModelDFSLt1_VLR0.4)
  ModelDFSLt1_VLR0.6 <- rbind(c(0:0), ModelDFSLt1_VLR0.5)
  ModelDFSLt1_VLR0.7 <- rbind(c(0:0), ModelDFSLt1_VLR0.6)
  ModelDFSLt1_VLR0.8 <- rbind(c(0:0), ModelDFSLt1_VLR0.7)
  ModelDFSLt1_VLR0.9 <- rbind(c(0:0), ModelDFSLt1_VLR0.8)
  ModelDFSLt1_VLR0.10 <- rbind(c(0:0), ModelDFSLt1_VLR0.9)
  ModelDFSLt1_VLR0.11 <- rbind(c(0:0), ModelDFSLt1_VLR0.10)
  ModelDFSLt1_VLR0.12 <- rbind(c(0:0), ModelDFSLt1_VLR0.11)
  ModelDFSLt1_VLR0.13 <- ModelDFSLt1_VLR0.12[-nrow(ModelDFSLt1_VLR0.12),]
  ModelDFSLt1_VLR0.14 <- ModelDFSLt1_VLR0.13[-nrow(ModelDFSLt1_VLR0.13),]
  ModelDFSLt1_VLR0.15 <- ModelDFSLt1_VLR0.14[-nrow(ModelDFSLt1_VLR0.14),]
  ModelDFSLt1_VLR0.16 <- ModelDFSLt1_VLR0.15[-nrow(ModelDFSLt1_VLR0.15),]
  ModelDFSLt1_VLR0.17 <- ModelDFSLt1_VLR0.16[-nrow(ModelDFSLt1_VLR0.16),]
  ModelDFSLt1_VLR0.18 <- ModelDFSLt1_VLR0.17[-nrow(ModelDFSLt1_VLR0.17),]
  ModelDFSLt1_VLR0.19 <- ModelDFSLt1_VLR0.18[-nrow(ModelDFSLt1_VLR0.18),]
  ModelDFSLt1_VLR0.20 <- ModelDFSLt1_VLR0.19[-nrow(ModelDFSLt1_VLR0.19),]
  ModelDFSLt1_VLR0.21 <- ModelDFSLt1_VLR0.20[-nrow(ModelDFSLt1_VLR0.20),]
  ModelDFSLt1_VLR0.22 <- ModelDFSLt1_VLR0.21[-nrow(ModelDFSLt1_VLR0.21),]
  ModelDFSLt1_VLR0.23 <- ModelDFSLt1_VLR0.22[-nrow(ModelDFSLt1_VLR0.22),]
  ModelDFSLt1_VLR0.24 <- ModelDFSLt1_VLR0.23[-nrow(ModelDFSLt1_VLR0.23),]

  ModelDFSLt1_VLR0 <- ModelDFSLt1_VLR0.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR0 <- ModelDFSLt0_VLR0 - ModelDFSLt1_VLR0

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR0$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR0P because implies a one-off input of C
  ModelDFSL_VLR0P <- ModelDFSL1y_VLR0

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR0P,"VXP_Models\\ModelDFSL_R_VLR0P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR0 <- ggplot(ModelDFSL_VLR0P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR0, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR0, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR0, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR0, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR0, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR0, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR0, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR0
  ggplotly(P_CFluxI1y_VLR0)

  #Plot 3.1: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR0P_YCO2 <- ModelDFSL_VLR0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR0P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR0")

  PA_CO21y_VLR0 <- ggplot(ModelDFSL_VLR0P_YCO2, aes(x = Year, y = AnnualCO2_VLR0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR0
  ggplotly(PA_CO21y_VLR0)

  #Plot 3.2: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR0P_YCO2D <- merge(ModelDFSL_VLR0P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR0P_YCO2D$AnnualCO2D_VLR0 <- ModelDFSL_VLR0P_YCO2D$AnnualCO2_VLR0 * ModelDFSL_VLR0P_YCO2D$GWP100

  PA_CO21yD_VLR0 <- ggplot(ModelDFSL_VLR0P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR0
  ggplotly(PA_CO21yD_VLR0)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR0P_YC <- ModelDFSL_VLR0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR0P_YC) <- c("Year", "Months", "AnnualCTail_VLR0")

  PA_C1y_VLR0 <- ggplot(ModelDFSL_VLR0P_YC, aes(x = Year, y = AnnualCTail_VLR0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR0
  ggplotly(PA_C1y_VLR0)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR0P_YCT <- ModelDFSL_VLR0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR0)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR0P_YCT) <- c("Year", "Months", "AnnualCTail_VLR0")

  PA_CT1y_VLR0 <- ggplot(ModelDFSL_VLR0P_YCT, aes(x = Year, y = AnnualCTail_VLR0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR0
  ggplotly(PA_CT1y_VLR0)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR0P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR0P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR0P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR0P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR0P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR0P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR0P_YCT,"CTails_P\\ModelDFSL_R_C_VLR0P.xlsx") #Yearly C emissions


  #### 8.2 - VLR1) Litter; 10%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR1=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR1=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.1,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR1=data.frame(years,rep(fT_VLR1*fW_VLR1,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR1=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.1, #Percent clay in mineral soil
    xi=xi.frame_VLR1) #Loads the model

  Ct_VLR1=getC(Model_VLR1) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR1, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR1 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR1 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR1 <- as.data.frame(VEC_Lit_VLR1) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR1$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR1 <- as.data.frame(VEC_Man_VLR1) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR1$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR1, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR1, class) #Check that class is numeric
  LitterCinputs_VLR1=VEC_LitDF_VLR1   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR1=VEC_ManDF_VLR1 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR1 <- merge(LitterCinputs_VLR1, ManureCinputs_VLR1, by = "MNumber")
  MCinputs_VLR1$MInput_VLR1 <- MCinputs_VLR1$VEC_Lit_VLR1 + MCinputs_VLR1$VEC_Man_VLR1

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR1)[which(names(MCinputs_VLR1) == "VEC_Lit_VLR1")] <- "LitterC_VLR1"
  colnames(MCinputs_VLR1)[which(names(MCinputs_VLR1) == "VEC_Man_VLR1")] <- "ManureC_VLR1"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR1 <- as.data.frame(Ct_VLR1)
  colnames(ModelDF_VLR1) <- c('DPM_VLR1','RPM_VLR1','BIO_VLR1', 'HUM_VLR1', 'IOM_VLR1')
  ModelDFS_VLR1 <- ModelDF_VLR1[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR1$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR1$AllPools_VLR1 <- ModelDFS_VLR1$DPM_VLR1 + ModelDFS_VLR1$RPM_VLR1 + ModelDFS_VLR1$BIO_VLR1 + ModelDFS_VLR1$HUM_VLR1 + ModelDFS_VLR1$IOM_VLR1

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR1$AllPools_noIOM_VLR1 <- ModelDFS_VLR1$DPM_VLR1 + ModelDFS_VLR1$RPM_VLR1 + ModelDFS_VLR1$BIO_VLR1 + ModelDFS_VLR1$HUM_VLR1

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR1 <- merge(ModelDFS_VLR1, MCinputs_VLR1, by = "MNumber")

  ModelDFSL_VLR1$MInput_VLR1 <- ModelDFSL_VLR1$LitterC_VLR1 + ModelDFSL_VLR1$ManureC_VLR1
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR1$CTails_VLR1 <- ModelDFSL_VLR1$AllPools_noIOM_VLR1 + ModelDFSL_VLR1$MInput_VLR1

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR1$AccumInput_VLR1 = ModelDFSL_VLR1$AccumInput_VLR1=cumsum(ModelDFSL_VLR1$MInput_VLR1)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR1$MGrowth_DPM_VLR1 <- ave(ModelDFSL_VLR1$DPM_VLR1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR1$MGrowth_RPM_VLR1 <- ave(ModelDFSL_VLR1$RPM_VLR1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR1$MGrowth_BIO_VLR1 <- ave(ModelDFSL_VLR1$BIO_VLR1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR1$MGrowth_HUM_VLR1 <- ave(ModelDFSL_VLR1$HUM_VLR1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR1$MGrowth_IOM_VLR1 <- ave(ModelDFSL_VLR1$IOM_VLR1, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR1$M_CCO2_VLR1 <- ModelDFSL_VLR1$MInput_VLR1 - ModelDFSL_VLR1$MGrowth_DPM_VLR1 - ModelDFSL_VLR1$MGrowth_RPM_VLR1 - ModelDFSL_VLR1$MGrowth_BIO_VLR1 - ModelDFSL_VLR1$MGrowth_HUM_VLR1
  ModelDFSL_VLR1$Accum_CCO2_VLR1 <- ModelDFSL_VLR1$AccumInput_VLR1 - ModelDFSL_VLR1$AllPools_noIOM_VLR1

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR1$M_CCO2_VLR1[1] <- 0
  ModelDFSL_VLR1$Accum_CCO2_VLR1[1] <- 0

  #Balance validation
  ModelDFSL_VLR1$Balance_VLR1 <- ModelDFSL_VLR1$AccumInput_VLR1 - ModelDFSL_VLR1$Accum_CCO2_VLR1 - (ModelDFSL_VLR1$DPM_VLR1 + ModelDFSL_VLR1$RPM_VLR1 + ModelDFSL_VLR1$BIO_VLR1 + ModelDFSL_VLR1$HUM_VLR1)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR1$M_CO2_VLR1 <- ModelDFSL_VLR1$M_CCO2_VLR1 * 44/12
  ModelDFSL_VLR1$Accum_CO2_VLR1 <- ModelDFSL_VLR1$Accum_CCO2_VLR1 * 44/12

  #This model will be called VLR1C because implies a continuous input of C
  ModelDFSL_VLR1C <- ModelDFSL_VLR1

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR1C,"VXC_Models\\ModelDFSL_R_VLR1C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR1 <- ModelDFSL_VLR1 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR1.1 <- rbind(c(0:0), ModelDFSLt0_VLR1)
  ModelDFSLt1_VLR1.2 <- rbind(c(0:0), ModelDFSLt1_VLR1.1)
  ModelDFSLt1_VLR1.3 <- rbind(c(0:0), ModelDFSLt1_VLR1.2)
  ModelDFSLt1_VLR1.4 <- rbind(c(0:0), ModelDFSLt1_VLR1.3)
  ModelDFSLt1_VLR1.5 <- rbind(c(0:0), ModelDFSLt1_VLR1.4)
  ModelDFSLt1_VLR1.6 <- rbind(c(0:0), ModelDFSLt1_VLR1.5)
  ModelDFSLt1_VLR1.7 <- rbind(c(0:0), ModelDFSLt1_VLR1.6)
  ModelDFSLt1_VLR1.8 <- rbind(c(0:0), ModelDFSLt1_VLR1.7)
  ModelDFSLt1_VLR1.9 <- rbind(c(0:0), ModelDFSLt1_VLR1.8)
  ModelDFSLt1_VLR1.10 <- rbind(c(0:0), ModelDFSLt1_VLR1.9)
  ModelDFSLt1_VLR1.11 <- rbind(c(0:0), ModelDFSLt1_VLR1.10)
  ModelDFSLt1_VLR1.12 <- rbind(c(0:0), ModelDFSLt1_VLR1.11)
  ModelDFSLt1_VLR1.13 <- ModelDFSLt1_VLR1.12[-nrow(ModelDFSLt1_VLR1.12),]
  ModelDFSLt1_VLR1.14 <- ModelDFSLt1_VLR1.13[-nrow(ModelDFSLt1_VLR1.13),]
  ModelDFSLt1_VLR1.15 <- ModelDFSLt1_VLR1.14[-nrow(ModelDFSLt1_VLR1.14),]
  ModelDFSLt1_VLR1.16 <- ModelDFSLt1_VLR1.15[-nrow(ModelDFSLt1_VLR1.15),]
  ModelDFSLt1_VLR1.17 <- ModelDFSLt1_VLR1.16[-nrow(ModelDFSLt1_VLR1.16),]
  ModelDFSLt1_VLR1.18 <- ModelDFSLt1_VLR1.17[-nrow(ModelDFSLt1_VLR1.17),]
  ModelDFSLt1_VLR1.19 <- ModelDFSLt1_VLR1.18[-nrow(ModelDFSLt1_VLR1.18),]
  ModelDFSLt1_VLR1.20 <- ModelDFSLt1_VLR1.19[-nrow(ModelDFSLt1_VLR1.19),]
  ModelDFSLt1_VLR1.21 <- ModelDFSLt1_VLR1.20[-nrow(ModelDFSLt1_VLR1.20),]
  ModelDFSLt1_VLR1.22 <- ModelDFSLt1_VLR1.21[-nrow(ModelDFSLt1_VLR1.21),]
  ModelDFSLt1_VLR1.23 <- ModelDFSLt1_VLR1.22[-nrow(ModelDFSLt1_VLR1.22),]
  ModelDFSLt1_VLR1.24 <- ModelDFSLt1_VLR1.23[-nrow(ModelDFSLt1_VLR1.23),]

  ModelDFSLt1_VLR1 <- ModelDFSLt1_VLR1.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR1 <- ModelDFSLt0_VLR1 - ModelDFSLt1_VLR1

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR1$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR1P because implies a one-off input of C
  ModelDFSL_VLR1P <- ModelDFSL1y_VLR1

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR1P,"VXP_Models\\ModelDFSL_R_VLR1P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR1 <- ggplot(ModelDFSL_VLR1P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR1, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR1, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR1, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR1, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR1, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR1, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR1, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR1
  ggplotly(P_CFluxI1y_VLR1)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR1P_YCO2 <- ModelDFSL_VLR1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR1P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR1")

  PA_CO21y_VLR1 <- ggplot(ModelDFSL_VLR1P_YCO2, aes(x = Year, y = AnnualCO2_VLR1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR1
  ggplotly(PA_CO21y_VLR1)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR1P_YCO2D <- merge(ModelDFSL_VLR1P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR1P_YCO2D$AnnualCO2D_VLR1 <- ModelDFSL_VLR1P_YCO2D$AnnualCO2_VLR1 * ModelDFSL_VLR1P_YCO2D$GWP100

  PA_CO21yD_VLR1 <- ggplot(ModelDFSL_VLR1P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR1
  ggplotly(PA_CO21yD_VLR1)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR1P_YC <- ModelDFSL_VLR1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR1P_YC) <- c("Year", "Months", "AnnualCTail_VLR1")

  PA_C1y_VLR1 <- ggplot(ModelDFSL_VLR1P_YC, aes(x = Year, y = AnnualCTail_VLR1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR1
  ggplotly(PA_C1y_VLR1)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR1P_YCT <- ModelDFSL_VLR1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR1)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR1P_YCT) <- c("Year", "Months", "AnnualCTail_VLR1")

  PA_CT1y_VLR1 <- ggplot(ModelDFSL_VLR1P_YCT, aes(x = Year, y = AnnualCTail_VLR1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR1
  ggplotly(PA_CT1y_VLR1)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR1P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR1P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR1P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR1P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR1P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR1P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR1P_YCT,"CTails_P\\ModelDFSL_R_C_VLR1P.xlsx") #Yearly C emissions



  #### 8.3 - VLR2) Litter; 20%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR2=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR2=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.2,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR2=data.frame(years,rep(fT_VLR2*fW_VLR2,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR2=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.2, #Percent clay in mineral soil
    xi=xi.frame_VLR2) #Loads the model

  Ct_VLR2=getC(Model_VLR2) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR2, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR2 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR2 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR2 <- as.data.frame(VEC_Lit_VLR2) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR2$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR2 <- as.data.frame(VEC_Man_VLR2) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR2$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR2, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR2, class) #Check that class is numeric
  LitterCinputs_VLR2=VEC_LitDF_VLR2   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR2=VEC_ManDF_VLR2 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR2 <- merge(LitterCinputs_VLR2, ManureCinputs_VLR2, by = "MNumber")
  MCinputs_VLR2$MInput_VLR2 <- MCinputs_VLR2$VEC_Lit_VLR2 + MCinputs_VLR2$VEC_Man_VLR2

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR2)[which(names(MCinputs_VLR2) == "VEC_Lit_VLR2")] <- "LitterC_VLR2"
  colnames(MCinputs_VLR2)[which(names(MCinputs_VLR2) == "VEC_Man_VLR2")] <- "ManureC_VLR2"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR2 <- as.data.frame(Ct_VLR2)
  colnames(ModelDF_VLR2) <- c('DPM_VLR2','RPM_VLR2','BIO_VLR2', 'HUM_VLR2', 'IOM_VLR2')
  ModelDFS_VLR2 <- ModelDF_VLR2[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR2$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR2$AllPools_VLR2 <- ModelDFS_VLR2$DPM_VLR2 + ModelDFS_VLR2$RPM_VLR2 + ModelDFS_VLR2$BIO_VLR2 + ModelDFS_VLR2$HUM_VLR2 + ModelDFS_VLR2$IOM_VLR2

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR2$AllPools_noIOM_VLR2 <- ModelDFS_VLR2$DPM_VLR2 + ModelDFS_VLR2$RPM_VLR2 + ModelDFS_VLR2$BIO_VLR2 + ModelDFS_VLR2$HUM_VLR2

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR2 <- merge(ModelDFS_VLR2, MCinputs_VLR2, by = "MNumber")

  ModelDFSL_VLR2$MInput_VLR2 <- ModelDFSL_VLR2$LitterC_VLR2 + ModelDFSL_VLR2$ManureC_VLR2
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR2$CTails_VLR2 <- ModelDFSL_VLR2$AllPools_noIOM_VLR2 + ModelDFSL_VLR2$MInput_VLR2

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR2$AccumInput_VLR2 = ModelDFSL_VLR2$AccumInput_VLR2=cumsum(ModelDFSL_VLR2$MInput_VLR2)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR2$MGrowth_DPM_VLR2 <- ave(ModelDFSL_VLR2$DPM_VLR2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR2$MGrowth_RPM_VLR2 <- ave(ModelDFSL_VLR2$RPM_VLR2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR2$MGrowth_BIO_VLR2 <- ave(ModelDFSL_VLR2$BIO_VLR2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR2$MGrowth_HUM_VLR2 <- ave(ModelDFSL_VLR2$HUM_VLR2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR2$MGrowth_IOM_VLR2 <- ave(ModelDFSL_VLR2$IOM_VLR2, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR2$M_CCO2_VLR2 <- ModelDFSL_VLR2$MInput_VLR2 - ModelDFSL_VLR2$MGrowth_DPM_VLR2 - ModelDFSL_VLR2$MGrowth_RPM_VLR2 - ModelDFSL_VLR2$MGrowth_BIO_VLR2 - ModelDFSL_VLR2$MGrowth_HUM_VLR2
  ModelDFSL_VLR2$Accum_CCO2_VLR2 <- ModelDFSL_VLR2$AccumInput_VLR2 - ModelDFSL_VLR2$AllPools_noIOM_VLR2

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR2$M_CCO2_VLR2[1] <- 0
  ModelDFSL_VLR2$Accum_CCO2_VLR2[1] <- 0

  #Balance validation
  ModelDFSL_VLR2$Balance_VLR2 <- ModelDFSL_VLR2$AccumInput_VLR2 - ModelDFSL_VLR2$Accum_CCO2_VLR2 - (ModelDFSL_VLR2$DPM_VLR2 + ModelDFSL_VLR2$RPM_VLR2 + ModelDFSL_VLR2$BIO_VLR2 + ModelDFSL_VLR2$HUM_VLR2)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR2$M_CO2_VLR2 <- ModelDFSL_VLR2$M_CCO2_VLR2 * 44/12
  ModelDFSL_VLR2$Accum_CO2_VLR2 <- ModelDFSL_VLR2$Accum_CCO2_VLR2 * 44/12

  #This model will be called VLR2C because implies a continuous input of C
  ModelDFSL_VLR2C <- ModelDFSL_VLR2

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR2C,"VXC_Models\\ModelDFSL_R_VLR2C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR2 <- ModelDFSL_VLR2 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR2.1 <- rbind(c(0:0), ModelDFSLt0_VLR2)
  ModelDFSLt1_VLR2.2 <- rbind(c(0:0), ModelDFSLt1_VLR2.1)
  ModelDFSLt1_VLR2.3 <- rbind(c(0:0), ModelDFSLt1_VLR2.2)
  ModelDFSLt1_VLR2.4 <- rbind(c(0:0), ModelDFSLt1_VLR2.3)
  ModelDFSLt1_VLR2.5 <- rbind(c(0:0), ModelDFSLt1_VLR2.4)
  ModelDFSLt1_VLR2.6 <- rbind(c(0:0), ModelDFSLt1_VLR2.5)
  ModelDFSLt1_VLR2.7 <- rbind(c(0:0), ModelDFSLt1_VLR2.6)
  ModelDFSLt1_VLR2.8 <- rbind(c(0:0), ModelDFSLt1_VLR2.7)
  ModelDFSLt1_VLR2.9 <- rbind(c(0:0), ModelDFSLt1_VLR2.8)
  ModelDFSLt1_VLR2.10 <- rbind(c(0:0), ModelDFSLt1_VLR2.9)
  ModelDFSLt1_VLR2.11 <- rbind(c(0:0), ModelDFSLt1_VLR2.10)
  ModelDFSLt1_VLR2.12 <- rbind(c(0:0), ModelDFSLt1_VLR2.11)
  ModelDFSLt1_VLR2.13 <- ModelDFSLt1_VLR2.12[-nrow(ModelDFSLt1_VLR2.12),]
  ModelDFSLt1_VLR2.14 <- ModelDFSLt1_VLR2.13[-nrow(ModelDFSLt1_VLR2.13),]
  ModelDFSLt1_VLR2.15 <- ModelDFSLt1_VLR2.14[-nrow(ModelDFSLt1_VLR2.14),]
  ModelDFSLt1_VLR2.16 <- ModelDFSLt1_VLR2.15[-nrow(ModelDFSLt1_VLR2.15),]
  ModelDFSLt1_VLR2.17 <- ModelDFSLt1_VLR2.16[-nrow(ModelDFSLt1_VLR2.16),]
  ModelDFSLt1_VLR2.18 <- ModelDFSLt1_VLR2.17[-nrow(ModelDFSLt1_VLR2.17),]
  ModelDFSLt1_VLR2.19 <- ModelDFSLt1_VLR2.18[-nrow(ModelDFSLt1_VLR2.18),]
  ModelDFSLt1_VLR2.20 <- ModelDFSLt1_VLR2.19[-nrow(ModelDFSLt1_VLR2.19),]
  ModelDFSLt1_VLR2.21 <- ModelDFSLt1_VLR2.20[-nrow(ModelDFSLt1_VLR2.20),]
  ModelDFSLt1_VLR2.22 <- ModelDFSLt1_VLR2.21[-nrow(ModelDFSLt1_VLR2.21),]
  ModelDFSLt1_VLR2.23 <- ModelDFSLt1_VLR2.22[-nrow(ModelDFSLt1_VLR2.22),]
  ModelDFSLt1_VLR2.24 <- ModelDFSLt1_VLR2.23[-nrow(ModelDFSLt1_VLR2.23),]

  ModelDFSLt1_VLR2 <- ModelDFSLt1_VLR2.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR2 <- ModelDFSLt0_VLR2 - ModelDFSLt1_VLR2

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR2$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR2P because implies a one-off input of C
  ModelDFSL_VLR2P <- ModelDFSL1y_VLR2

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR2P,"VXP_Models\\ModelDFSL_R_VLR2P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR2 <- ggplot(ModelDFSL_VLR2P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR2, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR2, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR2, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR2, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR2, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR2, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR2, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR2
  ggplotly(P_CFluxI1y_VLR2)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR2P_YCO2 <- ModelDFSL_VLR2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR2P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR2")

  PA_CO21y_VLR2 <- ggplot(ModelDFSL_VLR2P_YCO2, aes(x = Year, y = AnnualCO2_VLR2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR2
  ggplotly(PA_CO21y_VLR2)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR2P_YCO2D <- merge(ModelDFSL_VLR2P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR2P_YCO2D$AnnualCO2D_VLR2 <- ModelDFSL_VLR2P_YCO2D$AnnualCO2_VLR2 * ModelDFSL_VLR2P_YCO2D$GWP100

  PA_CO21yD_VLR2 <- ggplot(ModelDFSL_VLR2P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR2
  ggplotly(PA_CO21yD_VLR2)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR2P_YC <- ModelDFSL_VLR2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR2P_YC) <- c("Year", "Months", "AnnualCTail_VLR2")

  PA_C1y_VLR2 <- ggplot(ModelDFSL_VLR2P_YC, aes(x = Year, y = AnnualCTail_VLR2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR2
  ggplotly(PA_C1y_VLR2)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR2P_YCT <- ModelDFSL_VLR2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR2)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR2P_YCT) <- c("Year", "Months", "AnnualCTail_VLR2")

  PA_CT1y_VLR2 <- ggplot(ModelDFSL_VLR2P_YCT, aes(x = Year, y = AnnualCTail_VLR2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR2
  ggplotly(PA_CT1y_VLR2)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR2P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR2P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR2P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR2P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR2P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR2P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR2P_YCT,"CTails_P\\ModelDFSL_R_C_VLR2P.xlsx") #Yearly C emissions



  #### 8.4 - VLR3) Litter; 30%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR3=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR3=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.3,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR3=data.frame(years,rep(fT_VLR3*fW_VLR3,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR3=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.3, #Percent clay in mineral soil
    xi=xi.frame_VLR3) #Loads the model

  Ct_VLR3=getC(Model_VLR3) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR3, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR3 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR3 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR3 <- as.data.frame(VEC_Lit_VLR3) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR3$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR3 <- as.data.frame(VEC_Man_VLR3) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR3$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR3, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR3, class) #Check that class is numeric
  LitterCinputs_VLR3=VEC_LitDF_VLR3   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR3=VEC_ManDF_VLR3 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR3 <- merge(LitterCinputs_VLR3, ManureCinputs_VLR3, by = "MNumber")
  MCinputs_VLR3$MInput_VLR3 <- MCinputs_VLR3$VEC_Lit_VLR3 + MCinputs_VLR3$VEC_Man_VLR3

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR3)[which(names(MCinputs_VLR3) == "VEC_Lit_VLR3")] <- "LitterC_VLR3"
  colnames(MCinputs_VLR3)[which(names(MCinputs_VLR3) == "VEC_Man_VLR3")] <- "ManureC_VLR3"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR3 <- as.data.frame(Ct_VLR3)
  colnames(ModelDF_VLR3) <- c('DPM_VLR3','RPM_VLR3','BIO_VLR3', 'HUM_VLR3', 'IOM_VLR3')
  ModelDFS_VLR3 <- ModelDF_VLR3[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR3$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR3$AllPools_VLR3 <- ModelDFS_VLR3$DPM_VLR3 + ModelDFS_VLR3$RPM_VLR3 + ModelDFS_VLR3$BIO_VLR3 + ModelDFS_VLR3$HUM_VLR3 + ModelDFS_VLR3$IOM_VLR3

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR3$AllPools_noIOM_VLR3 <- ModelDFS_VLR3$DPM_VLR3 + ModelDFS_VLR3$RPM_VLR3 + ModelDFS_VLR3$BIO_VLR3 + ModelDFS_VLR3$HUM_VLR3

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR3 <- merge(ModelDFS_VLR3, MCinputs_VLR3, by = "MNumber")

  ModelDFSL_VLR3$MInput_VLR3 <- ModelDFSL_VLR3$LitterC_VLR3 + ModelDFSL_VLR3$ManureC_VLR3
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR3$CTails_VLR3 <- ModelDFSL_VLR3$AllPools_noIOM_VLR3 + ModelDFSL_VLR3$MInput_VLR3

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR3$AccumInput_VLR3 = ModelDFSL_VLR3$AccumInput_VLR3=cumsum(ModelDFSL_VLR3$MInput_VLR3)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR3$MGrowth_DPM_VLR3 <- ave(ModelDFSL_VLR3$DPM_VLR3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR3$MGrowth_RPM_VLR3 <- ave(ModelDFSL_VLR3$RPM_VLR3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR3$MGrowth_BIO_VLR3 <- ave(ModelDFSL_VLR3$BIO_VLR3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR3$MGrowth_HUM_VLR3 <- ave(ModelDFSL_VLR3$HUM_VLR3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR3$MGrowth_IOM_VLR3 <- ave(ModelDFSL_VLR3$IOM_VLR3, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR3$M_CCO2_VLR3 <- ModelDFSL_VLR3$MInput_VLR3 - ModelDFSL_VLR3$MGrowth_DPM_VLR3 - ModelDFSL_VLR3$MGrowth_RPM_VLR3 - ModelDFSL_VLR3$MGrowth_BIO_VLR3 - ModelDFSL_VLR3$MGrowth_HUM_VLR3
  ModelDFSL_VLR3$Accum_CCO2_VLR3 <- ModelDFSL_VLR3$AccumInput_VLR3 - ModelDFSL_VLR3$AllPools_noIOM_VLR3

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR3$M_CCO2_VLR3[1] <- 0
  ModelDFSL_VLR3$Accum_CCO2_VLR3[1] <- 0

  #Balance validation
  ModelDFSL_VLR3$Balance_VLR3 <- ModelDFSL_VLR3$AccumInput_VLR3 - ModelDFSL_VLR3$Accum_CCO2_VLR3 - (ModelDFSL_VLR3$DPM_VLR3 + ModelDFSL_VLR3$RPM_VLR3 + ModelDFSL_VLR3$BIO_VLR3 + ModelDFSL_VLR3$HUM_VLR3)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR3$M_CO2_VLR3 <- ModelDFSL_VLR3$M_CCO2_VLR3 * 44/12
  ModelDFSL_VLR3$Accum_CO2_VLR3 <- ModelDFSL_VLR3$Accum_CCO2_VLR3 * 44/12

  #This model will be called VLR3C because implies a continuous input of C
  ModelDFSL_VLR3C <- ModelDFSL_VLR3

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR3C,"VXC_Models\\ModelDFSL_R_VLR3C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR3 <- ModelDFSL_VLR3 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR3.1 <- rbind(c(0:0), ModelDFSLt0_VLR3)
  ModelDFSLt1_VLR3.2 <- rbind(c(0:0), ModelDFSLt1_VLR3.1)
  ModelDFSLt1_VLR3.3 <- rbind(c(0:0), ModelDFSLt1_VLR3.2)
  ModelDFSLt1_VLR3.4 <- rbind(c(0:0), ModelDFSLt1_VLR3.3)
  ModelDFSLt1_VLR3.5 <- rbind(c(0:0), ModelDFSLt1_VLR3.4)
  ModelDFSLt1_VLR3.6 <- rbind(c(0:0), ModelDFSLt1_VLR3.5)
  ModelDFSLt1_VLR3.7 <- rbind(c(0:0), ModelDFSLt1_VLR3.6)
  ModelDFSLt1_VLR3.8 <- rbind(c(0:0), ModelDFSLt1_VLR3.7)
  ModelDFSLt1_VLR3.9 <- rbind(c(0:0), ModelDFSLt1_VLR3.8)
  ModelDFSLt1_VLR3.10 <- rbind(c(0:0), ModelDFSLt1_VLR3.9)
  ModelDFSLt1_VLR3.11 <- rbind(c(0:0), ModelDFSLt1_VLR3.10)
  ModelDFSLt1_VLR3.12 <- rbind(c(0:0), ModelDFSLt1_VLR3.11)
  ModelDFSLt1_VLR3.13 <- ModelDFSLt1_VLR3.12[-nrow(ModelDFSLt1_VLR3.12),]
  ModelDFSLt1_VLR3.14 <- ModelDFSLt1_VLR3.13[-nrow(ModelDFSLt1_VLR3.13),]
  ModelDFSLt1_VLR3.15 <- ModelDFSLt1_VLR3.14[-nrow(ModelDFSLt1_VLR3.14),]
  ModelDFSLt1_VLR3.16 <- ModelDFSLt1_VLR3.15[-nrow(ModelDFSLt1_VLR3.15),]
  ModelDFSLt1_VLR3.17 <- ModelDFSLt1_VLR3.16[-nrow(ModelDFSLt1_VLR3.16),]
  ModelDFSLt1_VLR3.18 <- ModelDFSLt1_VLR3.17[-nrow(ModelDFSLt1_VLR3.17),]
  ModelDFSLt1_VLR3.19 <- ModelDFSLt1_VLR3.18[-nrow(ModelDFSLt1_VLR3.18),]
  ModelDFSLt1_VLR3.20 <- ModelDFSLt1_VLR3.19[-nrow(ModelDFSLt1_VLR3.19),]
  ModelDFSLt1_VLR3.21 <- ModelDFSLt1_VLR3.20[-nrow(ModelDFSLt1_VLR3.20),]
  ModelDFSLt1_VLR3.22 <- ModelDFSLt1_VLR3.21[-nrow(ModelDFSLt1_VLR3.21),]
  ModelDFSLt1_VLR3.23 <- ModelDFSLt1_VLR3.22[-nrow(ModelDFSLt1_VLR3.22),]
  ModelDFSLt1_VLR3.24 <- ModelDFSLt1_VLR3.23[-nrow(ModelDFSLt1_VLR3.23),]

  ModelDFSLt1_VLR3 <- ModelDFSLt1_VLR3.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR3 <- ModelDFSLt0_VLR3 - ModelDFSLt1_VLR3

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR3$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR3P because implies a one-off input of C
  ModelDFSL_VLR3P <- ModelDFSL1y_VLR3

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR3P,"VXP_Models\\ModelDFSL_R_VLR3P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR3 <- ggplot(ModelDFSL_VLR3P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR3, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR3, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR3, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR3, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR3, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR3, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR3, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR3
  ggplotly(P_CFluxI1y_VLR3)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR3P_YCO2 <- ModelDFSL_VLR3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR3P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR3")

  PA_CO21y_VLR3 <- ggplot(ModelDFSL_VLR3P_YCO2, aes(x = Year, y = AnnualCO2_VLR3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR3
  ggplotly(PA_CO21y_VLR3)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR3P_YCO2D <- merge(ModelDFSL_VLR3P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR3P_YCO2D$AnnualCO2D_VLR3 <- ModelDFSL_VLR3P_YCO2D$AnnualCO2_VLR3 * ModelDFSL_VLR3P_YCO2D$GWP100

  PA_CO21yD_VLR3 <- ggplot(ModelDFSL_VLR3P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR3
  ggplotly(PA_CO21yD_VLR3)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR3P_YC <- ModelDFSL_VLR3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR3P_YC) <- c("Year", "Months", "AnnualCTail_VLR3")

  PA_C1y_VLR3 <- ggplot(ModelDFSL_VLR3P_YC, aes(x = Year, y = AnnualCTail_VLR3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR3
  ggplotly(PA_C1y_VLR3)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR3P_YCT <- ModelDFSL_VLR3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR3)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR3P_YCT) <- c("Year", "Months", "AnnualCTail_VLR3")

  PA_CT1y_VLR3 <- ggplot(ModelDFSL_VLR3P_YCT, aes(x = Year, y = AnnualCTail_VLR3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR3
  ggplotly(PA_CT1y_VLR3)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR3P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR3P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR3P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR3P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR3P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR3P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR3P_YCT,"CTails_P\\ModelDFSL_R_C_VLR3P.xlsx") #Yearly C emissions



  #### 8.5 - VLR4) Litter; 40%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR4=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR4=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.4,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR4=data.frame(years,rep(fT_VLR4*fW_VLR4,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR4=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.4, #Percent clay in mineral soil
    xi=xi.frame_VLR4) #Loads the model

  Ct_VLR4=getC(Model_VLR4) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR4, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR4 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR4 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR4 <- as.data.frame(VEC_Lit_VLR4) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR4$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR4 <- as.data.frame(VEC_Man_VLR4) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR4$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR4, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR4, class) #Check that class is numeric
  LitterCinputs_VLR4=VEC_LitDF_VLR4   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR4=VEC_ManDF_VLR4 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR4 <- merge(LitterCinputs_VLR4, ManureCinputs_VLR4, by = "MNumber")
  MCinputs_VLR4$MInput_VLR4 <- MCinputs_VLR4$VEC_Lit_VLR4 + MCinputs_VLR4$VEC_Man_VLR4

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR4)[which(names(MCinputs_VLR4) == "VEC_Lit_VLR4")] <- "LitterC_VLR4"
  colnames(MCinputs_VLR4)[which(names(MCinputs_VLR4) == "VEC_Man_VLR4")] <- "ManureC_VLR4"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR4 <- as.data.frame(Ct_VLR4)
  colnames(ModelDF_VLR4) <- c('DPM_VLR4','RPM_VLR4','BIO_VLR4', 'HUM_VLR4', 'IOM_VLR4')
  ModelDFS_VLR4 <- ModelDF_VLR4[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR4$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR4$AllPools_VLR4 <- ModelDFS_VLR4$DPM_VLR4 + ModelDFS_VLR4$RPM_VLR4 + ModelDFS_VLR4$BIO_VLR4 + ModelDFS_VLR4$HUM_VLR4 + ModelDFS_VLR4$IOM_VLR4

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR4$AllPools_noIOM_VLR4 <- ModelDFS_VLR4$DPM_VLR4 + ModelDFS_VLR4$RPM_VLR4 + ModelDFS_VLR4$BIO_VLR4 + ModelDFS_VLR4$HUM_VLR4

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR4 <- merge(ModelDFS_VLR4, MCinputs_VLR4, by = "MNumber")

  ModelDFSL_VLR4$MInput_VLR4 <- ModelDFSL_VLR4$LitterC_VLR4 + ModelDFSL_VLR4$ManureC_VLR4
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR4$CTails_VLR4 <- ModelDFSL_VLR4$AllPools_noIOM_VLR4 + ModelDFSL_VLR4$MInput_VLR4

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR4$AccumInput_VLR4 = ModelDFSL_VLR4$AccumInput_VLR4=cumsum(ModelDFSL_VLR4$MInput_VLR4)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR4$MGrowth_DPM_VLR4 <- ave(ModelDFSL_VLR4$DPM_VLR4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR4$MGrowth_RPM_VLR4 <- ave(ModelDFSL_VLR4$RPM_VLR4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR4$MGrowth_BIO_VLR4 <- ave(ModelDFSL_VLR4$BIO_VLR4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR4$MGrowth_HUM_VLR4 <- ave(ModelDFSL_VLR4$HUM_VLR4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR4$MGrowth_IOM_VLR4 <- ave(ModelDFSL_VLR4$IOM_VLR4, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR4$M_CCO2_VLR4 <- ModelDFSL_VLR4$MInput_VLR4 - ModelDFSL_VLR4$MGrowth_DPM_VLR4 - ModelDFSL_VLR4$MGrowth_RPM_VLR4 - ModelDFSL_VLR4$MGrowth_BIO_VLR4 - ModelDFSL_VLR4$MGrowth_HUM_VLR4
  ModelDFSL_VLR4$Accum_CCO2_VLR4 <- ModelDFSL_VLR4$AccumInput_VLR4 - ModelDFSL_VLR4$AllPools_noIOM_VLR4

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR4$M_CCO2_VLR4[1] <- 0
  ModelDFSL_VLR4$Accum_CCO2_VLR4[1] <- 0

  #Balance validation
  ModelDFSL_VLR4$Balance_VLR4 <- ModelDFSL_VLR4$AccumInput_VLR4 - ModelDFSL_VLR4$Accum_CCO2_VLR4 - (ModelDFSL_VLR4$DPM_VLR4 + ModelDFSL_VLR4$RPM_VLR4 + ModelDFSL_VLR4$BIO_VLR4 + ModelDFSL_VLR4$HUM_VLR4)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR4$M_CO2_VLR4 <- ModelDFSL_VLR4$M_CCO2_VLR4 * 44/12
  ModelDFSL_VLR4$Accum_CO2_VLR4 <- ModelDFSL_VLR4$Accum_CCO2_VLR4 * 44/12

  #This model will be called VLR4C because implies a continuous input of C
  ModelDFSL_VLR4C <- ModelDFSL_VLR4

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR4C,"VXC_Models\\ModelDFSL_R_VLR4C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR4 <- ModelDFSL_VLR4 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR4.1 <- rbind(c(0:0), ModelDFSLt0_VLR4)
  ModelDFSLt1_VLR4.2 <- rbind(c(0:0), ModelDFSLt1_VLR4.1)
  ModelDFSLt1_VLR4.3 <- rbind(c(0:0), ModelDFSLt1_VLR4.2)
  ModelDFSLt1_VLR4.4 <- rbind(c(0:0), ModelDFSLt1_VLR4.3)
  ModelDFSLt1_VLR4.5 <- rbind(c(0:0), ModelDFSLt1_VLR4.4)
  ModelDFSLt1_VLR4.6 <- rbind(c(0:0), ModelDFSLt1_VLR4.5)
  ModelDFSLt1_VLR4.7 <- rbind(c(0:0), ModelDFSLt1_VLR4.6)
  ModelDFSLt1_VLR4.8 <- rbind(c(0:0), ModelDFSLt1_VLR4.7)
  ModelDFSLt1_VLR4.9 <- rbind(c(0:0), ModelDFSLt1_VLR4.8)
  ModelDFSLt1_VLR4.10 <- rbind(c(0:0), ModelDFSLt1_VLR4.9)
  ModelDFSLt1_VLR4.11 <- rbind(c(0:0), ModelDFSLt1_VLR4.10)
  ModelDFSLt1_VLR4.12 <- rbind(c(0:0), ModelDFSLt1_VLR4.11)
  ModelDFSLt1_VLR4.13 <- ModelDFSLt1_VLR4.12[-nrow(ModelDFSLt1_VLR4.12),]
  ModelDFSLt1_VLR4.14 <- ModelDFSLt1_VLR4.13[-nrow(ModelDFSLt1_VLR4.13),]
  ModelDFSLt1_VLR4.15 <- ModelDFSLt1_VLR4.14[-nrow(ModelDFSLt1_VLR4.14),]
  ModelDFSLt1_VLR4.16 <- ModelDFSLt1_VLR4.15[-nrow(ModelDFSLt1_VLR4.15),]
  ModelDFSLt1_VLR4.17 <- ModelDFSLt1_VLR4.16[-nrow(ModelDFSLt1_VLR4.16),]
  ModelDFSLt1_VLR4.18 <- ModelDFSLt1_VLR4.17[-nrow(ModelDFSLt1_VLR4.17),]
  ModelDFSLt1_VLR4.19 <- ModelDFSLt1_VLR4.18[-nrow(ModelDFSLt1_VLR4.18),]
  ModelDFSLt1_VLR4.20 <- ModelDFSLt1_VLR4.19[-nrow(ModelDFSLt1_VLR4.19),]
  ModelDFSLt1_VLR4.21 <- ModelDFSLt1_VLR4.20[-nrow(ModelDFSLt1_VLR4.20),]
  ModelDFSLt1_VLR4.22 <- ModelDFSLt1_VLR4.21[-nrow(ModelDFSLt1_VLR4.21),]
  ModelDFSLt1_VLR4.23 <- ModelDFSLt1_VLR4.22[-nrow(ModelDFSLt1_VLR4.22),]
  ModelDFSLt1_VLR4.24 <- ModelDFSLt1_VLR4.23[-nrow(ModelDFSLt1_VLR4.23),]

  ModelDFSLt1_VLR4 <- ModelDFSLt1_VLR4.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR4 <- ModelDFSLt0_VLR4 - ModelDFSLt1_VLR4

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR4$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR4P because implies a one-off input of C
  ModelDFSL_VLR4P <- ModelDFSL1y_VLR4

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR4P,"VXP_Models\\ModelDFSL_R_VLR4P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR4 <- ggplot(ModelDFSL_VLR4P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR4, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR4, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR4, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR4, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR4, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR4, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR4, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR4
  ggplotly(P_CFluxI1y_VLR4)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR4P_YCO2 <- ModelDFSL_VLR4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR4P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR4")

  PA_CO21y_VLR4 <- ggplot(ModelDFSL_VLR4P_YCO2, aes(x = Year, y = AnnualCO2_VLR4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR4
  ggplotly(PA_CO21y_VLR4)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR4P_YCO2D <- merge(ModelDFSL_VLR4P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR4P_YCO2D$AnnualCO2D_VLR4 <- ModelDFSL_VLR4P_YCO2D$AnnualCO2_VLR4 * ModelDFSL_VLR4P_YCO2D$GWP100

  PA_CO21yD_VLR4 <- ggplot(ModelDFSL_VLR4P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR4
  ggplotly(PA_CO21yD_VLR4)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR4P_YC <- ModelDFSL_VLR4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR4P_YC) <- c("Year", "Months", "AnnualCTail_VLR4")

  PA_C1y_VLR4 <- ggplot(ModelDFSL_VLR4P_YC, aes(x = Year, y = AnnualCTail_VLR4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR4
  ggplotly(PA_C1y_VLR4)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR4P_YCT <- ModelDFSL_VLR4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR4)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR4P_YCT) <- c("Year", "Months", "AnnualCTail_VLR4")

  PA_CT1y_VLR4 <- ggplot(ModelDFSL_VLR4P_YCT, aes(x = Year, y = AnnualCTail_VLR4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR4
  ggplotly(PA_CT1y_VLR4)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR4P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR4P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR4P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR4P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR4P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR4P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR4P_YCT,"CTails_P\\ModelDFSL_R_C_VLR4P.xlsx") #Yearly C emissions



  #### 8.6 - VLR5) Litter; 50%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR5=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR5=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.5,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR5=data.frame(years,rep(fT_VLR5*fW_VLR5,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR5=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.5, #Percent clay in mineral soil
    xi=xi.frame_VLR5) #Loads the model

  Ct_VLR5=getC(Model_VLR5) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR5, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR5 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR5 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR5 <- as.data.frame(VEC_Lit_VLR5) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR5$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR5 <- as.data.frame(VEC_Man_VLR5) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR5$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR5, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR5, class) #Check that class is numeric
  LitterCinputs_VLR5=VEC_LitDF_VLR5   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR5=VEC_ManDF_VLR5 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR5 <- merge(LitterCinputs_VLR5, ManureCinputs_VLR5, by = "MNumber")
  MCinputs_VLR5$MInput_VLR5 <- MCinputs_VLR5$VEC_Lit_VLR5 + MCinputs_VLR5$VEC_Man_VLR5

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR5)[which(names(MCinputs_VLR5) == "VEC_Lit_VLR5")] <- "LitterC_VLR5"
  colnames(MCinputs_VLR5)[which(names(MCinputs_VLR5) == "VEC_Man_VLR5")] <- "ManureC_VLR5"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR5 <- as.data.frame(Ct_VLR5)
  colnames(ModelDF_VLR5) <- c('DPM_VLR5','RPM_VLR5','BIO_VLR5', 'HUM_VLR5', 'IOM_VLR5')
  ModelDFS_VLR5 <- ModelDF_VLR5[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR5$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR5$AllPools_VLR5 <- ModelDFS_VLR5$DPM_VLR5 + ModelDFS_VLR5$RPM_VLR5 + ModelDFS_VLR5$BIO_VLR5 + ModelDFS_VLR5$HUM_VLR5 + ModelDFS_VLR5$IOM_VLR5

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR5$AllPools_noIOM_VLR5 <- ModelDFS_VLR5$DPM_VLR5 + ModelDFS_VLR5$RPM_VLR5 + ModelDFS_VLR5$BIO_VLR5 + ModelDFS_VLR5$HUM_VLR5

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR5 <- merge(ModelDFS_VLR5, MCinputs_VLR5, by = "MNumber")

  ModelDFSL_VLR5$MInput_VLR5 <- ModelDFSL_VLR5$LitterC_VLR5 + ModelDFSL_VLR5$ManureC_VLR5
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR5$CTails_VLR5 <- ModelDFSL_VLR5$AllPools_noIOM_VLR5 + ModelDFSL_VLR5$MInput_VLR5

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR5$AccumInput_VLR5 = ModelDFSL_VLR5$AccumInput_VLR5=cumsum(ModelDFSL_VLR5$MInput_VLR5)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR5$MGrowth_DPM_VLR5 <- ave(ModelDFSL_VLR5$DPM_VLR5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR5$MGrowth_RPM_VLR5 <- ave(ModelDFSL_VLR5$RPM_VLR5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR5$MGrowth_BIO_VLR5 <- ave(ModelDFSL_VLR5$BIO_VLR5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR5$MGrowth_HUM_VLR5 <- ave(ModelDFSL_VLR5$HUM_VLR5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR5$MGrowth_IOM_VLR5 <- ave(ModelDFSL_VLR5$IOM_VLR5, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR5$M_CCO2_VLR5 <- ModelDFSL_VLR5$MInput_VLR5 - ModelDFSL_VLR5$MGrowth_DPM_VLR5 - ModelDFSL_VLR5$MGrowth_RPM_VLR5 - ModelDFSL_VLR5$MGrowth_BIO_VLR5 - ModelDFSL_VLR5$MGrowth_HUM_VLR5
  ModelDFSL_VLR5$Accum_CCO2_VLR5 <- ModelDFSL_VLR5$AccumInput_VLR5 - ModelDFSL_VLR5$AllPools_noIOM_VLR5

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR5$M_CCO2_VLR5[1] <- 0
  ModelDFSL_VLR5$Accum_CCO2_VLR5[1] <- 0

  #Balance validation
  ModelDFSL_VLR5$Balance_VLR5 <- ModelDFSL_VLR5$AccumInput_VLR5 - ModelDFSL_VLR5$Accum_CCO2_VLR5 - (ModelDFSL_VLR5$DPM_VLR5 + ModelDFSL_VLR5$RPM_VLR5 + ModelDFSL_VLR5$BIO_VLR5 + ModelDFSL_VLR5$HUM_VLR5)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR5$M_CO2_VLR5 <- ModelDFSL_VLR5$M_CCO2_VLR5 * 44/12
  ModelDFSL_VLR5$Accum_CO2_VLR5 <- ModelDFSL_VLR5$Accum_CCO2_VLR5 * 44/12

  #This model will be called VLR5C because implies a continuous input of C
  ModelDFSL_VLR5C <- ModelDFSL_VLR5

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR5C,"VXC_Models\\ModelDFSL_R_VLR5C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR5 <- ModelDFSL_VLR5 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR5.1 <- rbind(c(0:0), ModelDFSLt0_VLR5)
  ModelDFSLt1_VLR5.2 <- rbind(c(0:0), ModelDFSLt1_VLR5.1)
  ModelDFSLt1_VLR5.3 <- rbind(c(0:0), ModelDFSLt1_VLR5.2)
  ModelDFSLt1_VLR5.4 <- rbind(c(0:0), ModelDFSLt1_VLR5.3)
  ModelDFSLt1_VLR5.5 <- rbind(c(0:0), ModelDFSLt1_VLR5.4)
  ModelDFSLt1_VLR5.6 <- rbind(c(0:0), ModelDFSLt1_VLR5.5)
  ModelDFSLt1_VLR5.7 <- rbind(c(0:0), ModelDFSLt1_VLR5.6)
  ModelDFSLt1_VLR5.8 <- rbind(c(0:0), ModelDFSLt1_VLR5.7)
  ModelDFSLt1_VLR5.9 <- rbind(c(0:0), ModelDFSLt1_VLR5.8)
  ModelDFSLt1_VLR5.10 <- rbind(c(0:0), ModelDFSLt1_VLR5.9)
  ModelDFSLt1_VLR5.11 <- rbind(c(0:0), ModelDFSLt1_VLR5.10)
  ModelDFSLt1_VLR5.12 <- rbind(c(0:0), ModelDFSLt1_VLR5.11)
  ModelDFSLt1_VLR5.13 <- ModelDFSLt1_VLR5.12[-nrow(ModelDFSLt1_VLR5.12),]
  ModelDFSLt1_VLR5.14 <- ModelDFSLt1_VLR5.13[-nrow(ModelDFSLt1_VLR5.13),]
  ModelDFSLt1_VLR5.15 <- ModelDFSLt1_VLR5.14[-nrow(ModelDFSLt1_VLR5.14),]
  ModelDFSLt1_VLR5.16 <- ModelDFSLt1_VLR5.15[-nrow(ModelDFSLt1_VLR5.15),]
  ModelDFSLt1_VLR5.17 <- ModelDFSLt1_VLR5.16[-nrow(ModelDFSLt1_VLR5.16),]
  ModelDFSLt1_VLR5.18 <- ModelDFSLt1_VLR5.17[-nrow(ModelDFSLt1_VLR5.17),]
  ModelDFSLt1_VLR5.19 <- ModelDFSLt1_VLR5.18[-nrow(ModelDFSLt1_VLR5.18),]
  ModelDFSLt1_VLR5.20 <- ModelDFSLt1_VLR5.19[-nrow(ModelDFSLt1_VLR5.19),]
  ModelDFSLt1_VLR5.21 <- ModelDFSLt1_VLR5.20[-nrow(ModelDFSLt1_VLR5.20),]
  ModelDFSLt1_VLR5.22 <- ModelDFSLt1_VLR5.21[-nrow(ModelDFSLt1_VLR5.21),]
  ModelDFSLt1_VLR5.23 <- ModelDFSLt1_VLR5.22[-nrow(ModelDFSLt1_VLR5.22),]
  ModelDFSLt1_VLR5.24 <- ModelDFSLt1_VLR5.23[-nrow(ModelDFSLt1_VLR5.23),]

  ModelDFSLt1_VLR5 <- ModelDFSLt1_VLR5.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR5 <- ModelDFSLt0_VLR5 - ModelDFSLt1_VLR5

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR5$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR5P because implies a one-off input of C
  ModelDFSL_VLR5P <- ModelDFSL1y_VLR5

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR5P,"VXP_Models\\ModelDFSL_R_VLR5P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR5 <- ggplot(ModelDFSL_VLR5P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR5, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR5, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR5, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR5, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR5, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR5, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR5, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR5
  ggplotly(P_CFluxI1y_VLR5)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR5P_YCO2 <- ModelDFSL_VLR5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR5P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR5")

  PA_CO21y_VLR5 <- ggplot(ModelDFSL_VLR5P_YCO2, aes(x = Year, y = AnnualCO2_VLR5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR5
  ggplotly(PA_CO21y_VLR5)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR5P_YCO2D <- merge(ModelDFSL_VLR5P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR5P_YCO2D$AnnualCO2D_VLR5 <- ModelDFSL_VLR5P_YCO2D$AnnualCO2_VLR5 * ModelDFSL_VLR5P_YCO2D$GWP100

  PA_CO21yD_VLR5 <- ggplot(ModelDFSL_VLR5P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR5
  ggplotly(PA_CO21yD_VLR5)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR5P_YC <- ModelDFSL_VLR5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR5P_YC) <- c("Year", "Months", "AnnualCTail_VLR5")

  PA_C1y_VLR5 <- ggplot(ModelDFSL_VLR5P_YC, aes(x = Year, y = AnnualCTail_VLR5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR5
  ggplotly(PA_C1y_VLR5)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR5P_YCT <- ModelDFSL_VLR5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR5)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR5P_YCT) <- c("Year", "Months", "AnnualCTail_VLR5")

  PA_CT1y_VLR5 <- ggplot(ModelDFSL_VLR5P_YCT, aes(x = Year, y = AnnualCTail_VLR5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR5
  ggplotly(PA_CT1y_VLR5)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR5P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR5P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR5P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR5P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR5P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR5P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR5P_YCT,"CTails_P\\ModelDFSL_R_C_VLR5P.xlsx") #Yearly C emissions



  #### 8.7 - VLR6) Litter; 60%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR6=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR6=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.6,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR6=data.frame(years,rep(fT_VLR6*fW_VLR6,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR6=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.6, #Percent clay in mineral soil
    xi=xi.frame_VLR6) #Loads the model

  Ct_VLR6=getC(Model_VLR6) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR6, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR6 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR6 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR6 <- as.data.frame(VEC_Lit_VLR6) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR6$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR6 <- as.data.frame(VEC_Man_VLR6) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR6$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR6, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR6, class) #Check that class is numeric
  LitterCinputs_VLR6=VEC_LitDF_VLR6   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR6=VEC_ManDF_VLR6 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR6 <- merge(LitterCinputs_VLR6, ManureCinputs_VLR6, by = "MNumber")
  MCinputs_VLR6$MInput_VLR6 <- MCinputs_VLR6$VEC_Lit_VLR6 + MCinputs_VLR6$VEC_Man_VLR6

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR6)[which(names(MCinputs_VLR6) == "VEC_Lit_VLR6")] <- "LitterC_VLR6"
  colnames(MCinputs_VLR6)[which(names(MCinputs_VLR6) == "VEC_Man_VLR6")] <- "ManureC_VLR6"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR6 <- as.data.frame(Ct_VLR6)
  colnames(ModelDF_VLR6) <- c('DPM_VLR6','RPM_VLR6','BIO_VLR6', 'HUM_VLR6', 'IOM_VLR6')
  ModelDFS_VLR6 <- ModelDF_VLR6[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR6$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR6$AllPools_VLR6 <- ModelDFS_VLR6$DPM_VLR6 + ModelDFS_VLR6$RPM_VLR6 + ModelDFS_VLR6$BIO_VLR6 + ModelDFS_VLR6$HUM_VLR6 + ModelDFS_VLR6$IOM_VLR6

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR6$AllPools_noIOM_VLR6 <- ModelDFS_VLR6$DPM_VLR6 + ModelDFS_VLR6$RPM_VLR6 + ModelDFS_VLR6$BIO_VLR6 + ModelDFS_VLR6$HUM_VLR6

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR6 <- merge(ModelDFS_VLR6, MCinputs_VLR6, by = "MNumber")

  ModelDFSL_VLR6$MInput_VLR6 <- ModelDFSL_VLR6$LitterC_VLR6 + ModelDFSL_VLR6$ManureC_VLR6
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR6$CTails_VLR6 <- ModelDFSL_VLR6$AllPools_noIOM_VLR6 + ModelDFSL_VLR6$MInput_VLR6

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR6$AccumInput_VLR6 = ModelDFSL_VLR6$AccumInput_VLR6=cumsum(ModelDFSL_VLR6$MInput_VLR6)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR6$MGrowth_DPM_VLR6 <- ave(ModelDFSL_VLR6$DPM_VLR6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR6$MGrowth_RPM_VLR6 <- ave(ModelDFSL_VLR6$RPM_VLR6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR6$MGrowth_BIO_VLR6 <- ave(ModelDFSL_VLR6$BIO_VLR6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR6$MGrowth_HUM_VLR6 <- ave(ModelDFSL_VLR6$HUM_VLR6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR6$MGrowth_IOM_VLR6 <- ave(ModelDFSL_VLR6$IOM_VLR6, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR6$M_CCO2_VLR6 <- ModelDFSL_VLR6$MInput_VLR6 - ModelDFSL_VLR6$MGrowth_DPM_VLR6 - ModelDFSL_VLR6$MGrowth_RPM_VLR6 - ModelDFSL_VLR6$MGrowth_BIO_VLR6 - ModelDFSL_VLR6$MGrowth_HUM_VLR6
  ModelDFSL_VLR6$Accum_CCO2_VLR6 <- ModelDFSL_VLR6$AccumInput_VLR6 - ModelDFSL_VLR6$AllPools_noIOM_VLR6

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR6$M_CCO2_VLR6[1] <- 0
  ModelDFSL_VLR6$Accum_CCO2_VLR6[1] <- 0

  #Balance validation
  ModelDFSL_VLR6$Balance_VLR6 <- ModelDFSL_VLR6$AccumInput_VLR6 - ModelDFSL_VLR6$Accum_CCO2_VLR6 - (ModelDFSL_VLR6$DPM_VLR6 + ModelDFSL_VLR6$RPM_VLR6 + ModelDFSL_VLR6$BIO_VLR6 + ModelDFSL_VLR6$HUM_VLR6)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR6$M_CO2_VLR6 <- ModelDFSL_VLR6$M_CCO2_VLR6 * 44/12
  ModelDFSL_VLR6$Accum_CO2_VLR6 <- ModelDFSL_VLR6$Accum_CCO2_VLR6 * 44/12

  #This model will be called VLR6C because implies a continuous input of C
  ModelDFSL_VLR6C <- ModelDFSL_VLR6

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR6C,"VXC_Models\\ModelDFSL_R_VLR6C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR6 <- ModelDFSL_VLR6 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR6.1 <- rbind(c(0:0), ModelDFSLt0_VLR6)
  ModelDFSLt1_VLR6.2 <- rbind(c(0:0), ModelDFSLt1_VLR6.1)
  ModelDFSLt1_VLR6.3 <- rbind(c(0:0), ModelDFSLt1_VLR6.2)
  ModelDFSLt1_VLR6.4 <- rbind(c(0:0), ModelDFSLt1_VLR6.3)
  ModelDFSLt1_VLR6.5 <- rbind(c(0:0), ModelDFSLt1_VLR6.4)
  ModelDFSLt1_VLR6.6 <- rbind(c(0:0), ModelDFSLt1_VLR6.5)
  ModelDFSLt1_VLR6.7 <- rbind(c(0:0), ModelDFSLt1_VLR6.6)
  ModelDFSLt1_VLR6.8 <- rbind(c(0:0), ModelDFSLt1_VLR6.7)
  ModelDFSLt1_VLR6.9 <- rbind(c(0:0), ModelDFSLt1_VLR6.8)
  ModelDFSLt1_VLR6.10 <- rbind(c(0:0), ModelDFSLt1_VLR6.9)
  ModelDFSLt1_VLR6.11 <- rbind(c(0:0), ModelDFSLt1_VLR6.10)
  ModelDFSLt1_VLR6.12 <- rbind(c(0:0), ModelDFSLt1_VLR6.11)
  ModelDFSLt1_VLR6.13 <- ModelDFSLt1_VLR6.12[-nrow(ModelDFSLt1_VLR6.12),]
  ModelDFSLt1_VLR6.14 <- ModelDFSLt1_VLR6.13[-nrow(ModelDFSLt1_VLR6.13),]
  ModelDFSLt1_VLR6.15 <- ModelDFSLt1_VLR6.14[-nrow(ModelDFSLt1_VLR6.14),]
  ModelDFSLt1_VLR6.16 <- ModelDFSLt1_VLR6.15[-nrow(ModelDFSLt1_VLR6.15),]
  ModelDFSLt1_VLR6.17 <- ModelDFSLt1_VLR6.16[-nrow(ModelDFSLt1_VLR6.16),]
  ModelDFSLt1_VLR6.18 <- ModelDFSLt1_VLR6.17[-nrow(ModelDFSLt1_VLR6.17),]
  ModelDFSLt1_VLR6.19 <- ModelDFSLt1_VLR6.18[-nrow(ModelDFSLt1_VLR6.18),]
  ModelDFSLt1_VLR6.20 <- ModelDFSLt1_VLR6.19[-nrow(ModelDFSLt1_VLR6.19),]
  ModelDFSLt1_VLR6.21 <- ModelDFSLt1_VLR6.20[-nrow(ModelDFSLt1_VLR6.20),]
  ModelDFSLt1_VLR6.22 <- ModelDFSLt1_VLR6.21[-nrow(ModelDFSLt1_VLR6.21),]
  ModelDFSLt1_VLR6.23 <- ModelDFSLt1_VLR6.22[-nrow(ModelDFSLt1_VLR6.22),]
  ModelDFSLt1_VLR6.24 <- ModelDFSLt1_VLR6.23[-nrow(ModelDFSLt1_VLR6.23),]

  ModelDFSLt1_VLR6 <- ModelDFSLt1_VLR6.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR6 <- ModelDFSLt0_VLR6 - ModelDFSLt1_VLR6

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR6$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR6P because implies a one-off input of C
  ModelDFSL_VLR6P <- ModelDFSL1y_VLR6

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR6P,"VXP_Models\\ModelDFSL_R_VLR6P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR6 <- ggplot(ModelDFSL_VLR6P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR6, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR6, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR6, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR6, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR6, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR6, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR6, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR6
  ggplotly(P_CFluxI1y_VLR6)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR6P_YCO2 <- ModelDFSL_VLR6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR6P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR6")

  PA_CO21y_VLR6 <- ggplot(ModelDFSL_VLR6P_YCO2, aes(x = Year, y = AnnualCO2_VLR6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR6
  ggplotly(PA_CO21y_VLR6)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR6P_YCO2D <- merge(ModelDFSL_VLR6P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR6P_YCO2D$AnnualCO2D_VLR6 <- ModelDFSL_VLR6P_YCO2D$AnnualCO2_VLR6 * ModelDFSL_VLR6P_YCO2D$GWP100

  PA_CO21yD_VLR6 <- ggplot(ModelDFSL_VLR6P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR6
  ggplotly(PA_CO21yD_VLR6)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR6P_YC <- ModelDFSL_VLR6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR6P_YC) <- c("Year", "Months", "AnnualCTail_VLR6")

  PA_C1y_VLR6 <- ggplot(ModelDFSL_VLR6P_YC, aes(x = Year, y = AnnualCTail_VLR6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR6
  ggplotly(PA_C1y_VLR6)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR6P_YCT <- ModelDFSL_VLR6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR6)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR6P_YCT) <- c("Year", "Months", "AnnualCTail_VLR6")

  PA_CT1y_VLR6 <- ggplot(ModelDFSL_VLR6P_YCT, aes(x = Year, y = AnnualCTail_VLR6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR6
  ggplotly(PA_CT1y_VLR6)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR6P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR6P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR6P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR6P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR6P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR6P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR6P_YCT,"CTails_P\\ModelDFSL_R_C_VLR6P.xlsx") #Yearly C emissions



  #### 8.8 - VLR7) Litter; 70%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR7=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR7=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.7,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR7=data.frame(years,rep(fT_VLR7*fW_VLR7,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR7=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.7, #Percent clay in mineral soil
    xi=xi.frame_VLR7) #Loads the model

  Ct_VLR7=getC(Model_VLR7) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR7, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR7 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR7 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR7 <- as.data.frame(VEC_Lit_VLR7) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR7$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR7 <- as.data.frame(VEC_Man_VLR7) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR7$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR7, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR7, class) #Check that class is numeric
  LitterCinputs_VLR7=VEC_LitDF_VLR7   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR7=VEC_ManDF_VLR7 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR7 <- merge(LitterCinputs_VLR7, ManureCinputs_VLR7, by = "MNumber")
  MCinputs_VLR7$MInput_VLR7 <- MCinputs_VLR7$VEC_Lit_VLR7 + MCinputs_VLR7$VEC_Man_VLR7

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR7)[which(names(MCinputs_VLR7) == "VEC_Lit_VLR7")] <- "LitterC_VLR7"
  colnames(MCinputs_VLR7)[which(names(MCinputs_VLR7) == "VEC_Man_VLR7")] <- "ManureC_VLR7"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR7 <- as.data.frame(Ct_VLR7)
  colnames(ModelDF_VLR7) <- c('DPM_VLR7','RPM_VLR7','BIO_VLR7', 'HUM_VLR7', 'IOM_VLR7')
  ModelDFS_VLR7 <- ModelDF_VLR7[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR7$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR7$AllPools_VLR7 <- ModelDFS_VLR7$DPM_VLR7 + ModelDFS_VLR7$RPM_VLR7 + ModelDFS_VLR7$BIO_VLR7 + ModelDFS_VLR7$HUM_VLR7 + ModelDFS_VLR7$IOM_VLR7

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR7$AllPools_noIOM_VLR7 <- ModelDFS_VLR7$DPM_VLR7 + ModelDFS_VLR7$RPM_VLR7 + ModelDFS_VLR7$BIO_VLR7 + ModelDFS_VLR7$HUM_VLR7

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR7 <- merge(ModelDFS_VLR7, MCinputs_VLR7, by = "MNumber")

  ModelDFSL_VLR7$MInput_VLR7 <- ModelDFSL_VLR7$LitterC_VLR7 + ModelDFSL_VLR7$ManureC_VLR7
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR7$CTails_VLR7 <- ModelDFSL_VLR7$AllPools_noIOM_VLR7 + ModelDFSL_VLR7$MInput_VLR7

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR7$AccumInput_VLR7 = ModelDFSL_VLR7$AccumInput_VLR7=cumsum(ModelDFSL_VLR7$MInput_VLR7)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR7$MGrowth_DPM_VLR7 <- ave(ModelDFSL_VLR7$DPM_VLR7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR7$MGrowth_RPM_VLR7 <- ave(ModelDFSL_VLR7$RPM_VLR7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR7$MGrowth_BIO_VLR7 <- ave(ModelDFSL_VLR7$BIO_VLR7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR7$MGrowth_HUM_VLR7 <- ave(ModelDFSL_VLR7$HUM_VLR7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR7$MGrowth_IOM_VLR7 <- ave(ModelDFSL_VLR7$IOM_VLR7, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR7$M_CCO2_VLR7 <- ModelDFSL_VLR7$MInput_VLR7 - ModelDFSL_VLR7$MGrowth_DPM_VLR7 - ModelDFSL_VLR7$MGrowth_RPM_VLR7 - ModelDFSL_VLR7$MGrowth_BIO_VLR7 - ModelDFSL_VLR7$MGrowth_HUM_VLR7
  ModelDFSL_VLR7$Accum_CCO2_VLR7 <- ModelDFSL_VLR7$AccumInput_VLR7 - ModelDFSL_VLR7$AllPools_noIOM_VLR7

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR7$M_CCO2_VLR7[1] <- 0
  ModelDFSL_VLR7$Accum_CCO2_VLR7[1] <- 0

  #Balance validation
  ModelDFSL_VLR7$Balance_VLR7 <- ModelDFSL_VLR7$AccumInput_VLR7 - ModelDFSL_VLR7$Accum_CCO2_VLR7 - (ModelDFSL_VLR7$DPM_VLR7 + ModelDFSL_VLR7$RPM_VLR7 + ModelDFSL_VLR7$BIO_VLR7 + ModelDFSL_VLR7$HUM_VLR7)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR7$M_CO2_VLR7 <- ModelDFSL_VLR7$M_CCO2_VLR7 * 44/12
  ModelDFSL_VLR7$Accum_CO2_VLR7 <- ModelDFSL_VLR7$Accum_CCO2_VLR7 * 44/12

  #This model will be called VLR7C because implies a continuous input of C
  ModelDFSL_VLR7C <- ModelDFSL_VLR7

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR7C,"VXC_Models\\ModelDFSL_R_VLR7C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR7 <- ModelDFSL_VLR7 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR7.1 <- rbind(c(0:0), ModelDFSLt0_VLR7)
  ModelDFSLt1_VLR7.2 <- rbind(c(0:0), ModelDFSLt1_VLR7.1)
  ModelDFSLt1_VLR7.3 <- rbind(c(0:0), ModelDFSLt1_VLR7.2)
  ModelDFSLt1_VLR7.4 <- rbind(c(0:0), ModelDFSLt1_VLR7.3)
  ModelDFSLt1_VLR7.5 <- rbind(c(0:0), ModelDFSLt1_VLR7.4)
  ModelDFSLt1_VLR7.6 <- rbind(c(0:0), ModelDFSLt1_VLR7.5)
  ModelDFSLt1_VLR7.7 <- rbind(c(0:0), ModelDFSLt1_VLR7.6)
  ModelDFSLt1_VLR7.8 <- rbind(c(0:0), ModelDFSLt1_VLR7.7)
  ModelDFSLt1_VLR7.9 <- rbind(c(0:0), ModelDFSLt1_VLR7.8)
  ModelDFSLt1_VLR7.10 <- rbind(c(0:0), ModelDFSLt1_VLR7.9)
  ModelDFSLt1_VLR7.11 <- rbind(c(0:0), ModelDFSLt1_VLR7.10)
  ModelDFSLt1_VLR7.12 <- rbind(c(0:0), ModelDFSLt1_VLR7.11)
  ModelDFSLt1_VLR7.13 <- ModelDFSLt1_VLR7.12[-nrow(ModelDFSLt1_VLR7.12),]
  ModelDFSLt1_VLR7.14 <- ModelDFSLt1_VLR7.13[-nrow(ModelDFSLt1_VLR7.13),]
  ModelDFSLt1_VLR7.15 <- ModelDFSLt1_VLR7.14[-nrow(ModelDFSLt1_VLR7.14),]
  ModelDFSLt1_VLR7.16 <- ModelDFSLt1_VLR7.15[-nrow(ModelDFSLt1_VLR7.15),]
  ModelDFSLt1_VLR7.17 <- ModelDFSLt1_VLR7.16[-nrow(ModelDFSLt1_VLR7.16),]
  ModelDFSLt1_VLR7.18 <- ModelDFSLt1_VLR7.17[-nrow(ModelDFSLt1_VLR7.17),]
  ModelDFSLt1_VLR7.19 <- ModelDFSLt1_VLR7.18[-nrow(ModelDFSLt1_VLR7.18),]
  ModelDFSLt1_VLR7.20 <- ModelDFSLt1_VLR7.19[-nrow(ModelDFSLt1_VLR7.19),]
  ModelDFSLt1_VLR7.21 <- ModelDFSLt1_VLR7.20[-nrow(ModelDFSLt1_VLR7.20),]
  ModelDFSLt1_VLR7.22 <- ModelDFSLt1_VLR7.21[-nrow(ModelDFSLt1_VLR7.21),]
  ModelDFSLt1_VLR7.23 <- ModelDFSLt1_VLR7.22[-nrow(ModelDFSLt1_VLR7.22),]
  ModelDFSLt1_VLR7.24 <- ModelDFSLt1_VLR7.23[-nrow(ModelDFSLt1_VLR7.23),]

  ModelDFSLt1_VLR7 <- ModelDFSLt1_VLR7.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR7 <- ModelDFSLt0_VLR7 - ModelDFSLt1_VLR7

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR7$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR7P because implies a one-off input of C
  ModelDFSL_VLR7P <- ModelDFSL1y_VLR7

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR7P,"VXP_Models\\ModelDFSL_R_VLR7P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR7 <- ggplot(ModelDFSL_VLR7P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR7, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR7, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR7, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR7, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR7, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR7, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR7, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR7
  ggplotly(P_CFluxI1y_VLR7)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR7P_YCO2 <- ModelDFSL_VLR7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR7P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR7")

  PA_CO21y_VLR7 <- ggplot(ModelDFSL_VLR7P_YCO2, aes(x = Year, y = AnnualCO2_VLR7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR7
  ggplotly(PA_CO21y_VLR7)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR7P_YCO2D <- merge(ModelDFSL_VLR7P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR7P_YCO2D$AnnualCO2D_VLR7 <- ModelDFSL_VLR7P_YCO2D$AnnualCO2_VLR7 * ModelDFSL_VLR7P_YCO2D$GWP100

  PA_CO21yD_VLR7 <- ggplot(ModelDFSL_VLR7P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR7
  ggplotly(PA_CO21yD_VLR7)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR7P_YC <- ModelDFSL_VLR7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR7P_YC) <- c("Year", "Months", "AnnualCTail_VLR7")

  PA_C1y_VLR7 <- ggplot(ModelDFSL_VLR7P_YC, aes(x = Year, y = AnnualCTail_VLR7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR7
  ggplotly(PA_C1y_VLR7)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR7P_YCT <- ModelDFSL_VLR7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR7)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR7P_YCT) <- c("Year", "Months", "AnnualCTail_VLR7")

  PA_CT1y_VLR7 <- ggplot(ModelDFSL_VLR7P_YCT, aes(x = Year, y = AnnualCTail_VLR7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR7
  ggplotly(PA_CT1y_VLR7)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR7P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR7P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR7P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR7P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR7P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR7P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR7P_YCT,"CTails_P\\ModelDFSL_R_C_VLR7P.xlsx") #Yearly C emissions



  #### 8.9 - VLR8) Litter; 80%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR8=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR8=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.8,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR8=data.frame(years,rep(fT_VLR8*fW_VLR8,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR8=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.8, #Percent clay in mineral soil
    xi=xi.frame_VLR8) #Loads the model

  Ct_VLR8=getC(Model_VLR8) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR8, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR8 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR8 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR8 <- as.data.frame(VEC_Lit_VLR8) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR8$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR8 <- as.data.frame(VEC_Man_VLR8) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR8$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR8, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR8, class) #Check that class is numeric
  LitterCinputs_VLR8=VEC_LitDF_VLR8   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR8=VEC_ManDF_VLR8 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR8 <- merge(LitterCinputs_VLR8, ManureCinputs_VLR8, by = "MNumber")
  MCinputs_VLR8$MInput_VLR8 <- MCinputs_VLR8$VEC_Lit_VLR8 + MCinputs_VLR8$VEC_Man_VLR8

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR8)[which(names(MCinputs_VLR8) == "VEC_Lit_VLR8")] <- "LitterC_VLR8"
  colnames(MCinputs_VLR8)[which(names(MCinputs_VLR8) == "VEC_Man_VLR8")] <- "ManureC_VLR8"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR8 <- as.data.frame(Ct_VLR8)
  colnames(ModelDF_VLR8) <- c('DPM_VLR8','RPM_VLR8','BIO_VLR8', 'HUM_VLR8', 'IOM_VLR8')
  ModelDFS_VLR8 <- ModelDF_VLR8[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR8$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR8$AllPools_VLR8 <- ModelDFS_VLR8$DPM_VLR8 + ModelDFS_VLR8$RPM_VLR8 + ModelDFS_VLR8$BIO_VLR8 + ModelDFS_VLR8$HUM_VLR8 + ModelDFS_VLR8$IOM_VLR8

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR8$AllPools_noIOM_VLR8 <- ModelDFS_VLR8$DPM_VLR8 + ModelDFS_VLR8$RPM_VLR8 + ModelDFS_VLR8$BIO_VLR8 + ModelDFS_VLR8$HUM_VLR8

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR8 <- merge(ModelDFS_VLR8, MCinputs_VLR8, by = "MNumber")

  ModelDFSL_VLR8$MInput_VLR8 <- ModelDFSL_VLR8$LitterC_VLR8 + ModelDFSL_VLR8$ManureC_VLR8
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR8$CTails_VLR8 <- ModelDFSL_VLR8$AllPools_noIOM_VLR8 + ModelDFSL_VLR8$MInput_VLR8

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR8$AccumInput_VLR8 = ModelDFSL_VLR8$AccumInput_VLR8=cumsum(ModelDFSL_VLR8$MInput_VLR8)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR8$MGrowth_DPM_VLR8 <- ave(ModelDFSL_VLR8$DPM_VLR8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR8$MGrowth_RPM_VLR8 <- ave(ModelDFSL_VLR8$RPM_VLR8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR8$MGrowth_BIO_VLR8 <- ave(ModelDFSL_VLR8$BIO_VLR8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR8$MGrowth_HUM_VLR8 <- ave(ModelDFSL_VLR8$HUM_VLR8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR8$MGrowth_IOM_VLR8 <- ave(ModelDFSL_VLR8$IOM_VLR8, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR8$M_CCO2_VLR8 <- ModelDFSL_VLR8$MInput_VLR8 - ModelDFSL_VLR8$MGrowth_DPM_VLR8 - ModelDFSL_VLR8$MGrowth_RPM_VLR8 - ModelDFSL_VLR8$MGrowth_BIO_VLR8 - ModelDFSL_VLR8$MGrowth_HUM_VLR8
  ModelDFSL_VLR8$Accum_CCO2_VLR8 <- ModelDFSL_VLR8$AccumInput_VLR8 - ModelDFSL_VLR8$AllPools_noIOM_VLR8

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR8$M_CCO2_VLR8[1] <- 0
  ModelDFSL_VLR8$Accum_CCO2_VLR8[1] <- 0

  #Balance validation
  ModelDFSL_VLR8$Balance_VLR8 <- ModelDFSL_VLR8$AccumInput_VLR8 - ModelDFSL_VLR8$Accum_CCO2_VLR8 - (ModelDFSL_VLR8$DPM_VLR8 + ModelDFSL_VLR8$RPM_VLR8 + ModelDFSL_VLR8$BIO_VLR8 + ModelDFSL_VLR8$HUM_VLR8)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR8$M_CO2_VLR8 <- ModelDFSL_VLR8$M_CCO2_VLR8 * 44/12
  ModelDFSL_VLR8$Accum_CO2_VLR8 <- ModelDFSL_VLR8$Accum_CCO2_VLR8 * 44/12

  #This model will be called VLR8C because implies a continuous input of C
  ModelDFSL_VLR8C <- ModelDFSL_VLR8

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR8C,"VXC_Models\\ModelDFSL_R_VLR8C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR8 <- ModelDFSL_VLR8 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR8.1 <- rbind(c(0:0), ModelDFSLt0_VLR8)
  ModelDFSLt1_VLR8.2 <- rbind(c(0:0), ModelDFSLt1_VLR8.1)
  ModelDFSLt1_VLR8.3 <- rbind(c(0:0), ModelDFSLt1_VLR8.2)
  ModelDFSLt1_VLR8.4 <- rbind(c(0:0), ModelDFSLt1_VLR8.3)
  ModelDFSLt1_VLR8.5 <- rbind(c(0:0), ModelDFSLt1_VLR8.4)
  ModelDFSLt1_VLR8.6 <- rbind(c(0:0), ModelDFSLt1_VLR8.5)
  ModelDFSLt1_VLR8.7 <- rbind(c(0:0), ModelDFSLt1_VLR8.6)
  ModelDFSLt1_VLR8.8 <- rbind(c(0:0), ModelDFSLt1_VLR8.7)
  ModelDFSLt1_VLR8.9 <- rbind(c(0:0), ModelDFSLt1_VLR8.8)
  ModelDFSLt1_VLR8.10 <- rbind(c(0:0), ModelDFSLt1_VLR8.9)
  ModelDFSLt1_VLR8.11 <- rbind(c(0:0), ModelDFSLt1_VLR8.10)
  ModelDFSLt1_VLR8.12 <- rbind(c(0:0), ModelDFSLt1_VLR8.11)
  ModelDFSLt1_VLR8.13 <- ModelDFSLt1_VLR8.12[-nrow(ModelDFSLt1_VLR8.12),]
  ModelDFSLt1_VLR8.14 <- ModelDFSLt1_VLR8.13[-nrow(ModelDFSLt1_VLR8.13),]
  ModelDFSLt1_VLR8.15 <- ModelDFSLt1_VLR8.14[-nrow(ModelDFSLt1_VLR8.14),]
  ModelDFSLt1_VLR8.16 <- ModelDFSLt1_VLR8.15[-nrow(ModelDFSLt1_VLR8.15),]
  ModelDFSLt1_VLR8.17 <- ModelDFSLt1_VLR8.16[-nrow(ModelDFSLt1_VLR8.16),]
  ModelDFSLt1_VLR8.18 <- ModelDFSLt1_VLR8.17[-nrow(ModelDFSLt1_VLR8.17),]
  ModelDFSLt1_VLR8.19 <- ModelDFSLt1_VLR8.18[-nrow(ModelDFSLt1_VLR8.18),]
  ModelDFSLt1_VLR8.20 <- ModelDFSLt1_VLR8.19[-nrow(ModelDFSLt1_VLR8.19),]
  ModelDFSLt1_VLR8.21 <- ModelDFSLt1_VLR8.20[-nrow(ModelDFSLt1_VLR8.20),]
  ModelDFSLt1_VLR8.22 <- ModelDFSLt1_VLR8.21[-nrow(ModelDFSLt1_VLR8.21),]
  ModelDFSLt1_VLR8.23 <- ModelDFSLt1_VLR8.22[-nrow(ModelDFSLt1_VLR8.22),]
  ModelDFSLt1_VLR8.24 <- ModelDFSLt1_VLR8.23[-nrow(ModelDFSLt1_VLR8.23),]

  ModelDFSLt1_VLR8 <- ModelDFSLt1_VLR8.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR8 <- ModelDFSLt0_VLR8 - ModelDFSLt1_VLR8

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR8$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR8P because implies a one-off input of C
  ModelDFSL_VLR8P <- ModelDFSL1y_VLR8

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR8P,"VXP_Models\\ModelDFSL_R_VLR8P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR8 <- ggplot(ModelDFSL_VLR8P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR8, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR8, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR8, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR8, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR8, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR8, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR8, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR8
  ggplotly(P_CFluxI1y_VLR8)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR8P_YCO2 <- ModelDFSL_VLR8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR8P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR8")

  PA_CO21y_VLR8 <- ggplot(ModelDFSL_VLR8P_YCO2, aes(x = Year, y = AnnualCO2_VLR8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR8
  ggplotly(PA_CO21y_VLR8)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR8P_YCO2D <- merge(ModelDFSL_VLR8P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR8P_YCO2D$AnnualCO2D_VLR8 <- ModelDFSL_VLR8P_YCO2D$AnnualCO2_VLR8 * ModelDFSL_VLR8P_YCO2D$GWP100

  PA_CO21yD_VLR8 <- ggplot(ModelDFSL_VLR8P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR8
  ggplotly(PA_CO21yD_VLR8)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR8P_YC <- ModelDFSL_VLR8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR8P_YC) <- c("Year", "Months", "AnnualCTail_VLR8")

  PA_C1y_VLR8 <- ggplot(ModelDFSL_VLR8P_YC, aes(x = Year, y = AnnualCTail_VLR8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR8
  ggplotly(PA_C1y_VLR8)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR8P_YCT <- ModelDFSL_VLR8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR8)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR8P_YCT) <- c("Year", "Months", "AnnualCTail_VLR8")

  PA_CT1y_VLR8 <- ggplot(ModelDFSL_VLR8P_YCT, aes(x = Year, y = AnnualCTail_VLR8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR8
  ggplotly(PA_CT1y_VLR8)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR8P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR8P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR8P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR8P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR8P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR8P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR8P_YCT,"CTails_P\\ModelDFSL_R_C_VLR8P.xlsx") #Yearly C emissions



  #### 8.10 - VLR9) Litter; 90%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR9=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR9=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.9,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR9=data.frame(years,rep(fT_VLR9*fW_VLR9,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR9=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.9, #Percent clay in mineral soil
    xi=xi.frame_VLR9) #Loads the model

  Ct_VLR9=getC(Model_VLR9) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR9, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR9 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR9 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR9 <- as.data.frame(VEC_Lit_VLR9) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR9$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR9 <- as.data.frame(VEC_Man_VLR9) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR9$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR9, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR9, class) #Check that class is numeric
  LitterCinputs_VLR9=VEC_LitDF_VLR9   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR9=VEC_ManDF_VLR9 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR9 <- merge(LitterCinputs_VLR9, ManureCinputs_VLR9, by = "MNumber")
  MCinputs_VLR9$MInput_VLR9 <- MCinputs_VLR9$VEC_Lit_VLR9 + MCinputs_VLR9$VEC_Man_VLR9

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR9)[which(names(MCinputs_VLR9) == "VEC_Lit_VLR9")] <- "LitterC_VLR9"
  colnames(MCinputs_VLR9)[which(names(MCinputs_VLR9) == "VEC_Man_VLR9")] <- "ManureC_VLR9"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR9 <- as.data.frame(Ct_VLR9)
  colnames(ModelDF_VLR9) <- c('DPM_VLR9','RPM_VLR9','BIO_VLR9', 'HUM_VLR9', 'IOM_VLR9')
  ModelDFS_VLR9 <- ModelDF_VLR9[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR9$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR9$AllPools_VLR9 <- ModelDFS_VLR9$DPM_VLR9 + ModelDFS_VLR9$RPM_VLR9 + ModelDFS_VLR9$BIO_VLR9 + ModelDFS_VLR9$HUM_VLR9 + ModelDFS_VLR9$IOM_VLR9

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR9$AllPools_noIOM_VLR9 <- ModelDFS_VLR9$DPM_VLR9 + ModelDFS_VLR9$RPM_VLR9 + ModelDFS_VLR9$BIO_VLR9 + ModelDFS_VLR9$HUM_VLR9

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR9 <- merge(ModelDFS_VLR9, MCinputs_VLR9, by = "MNumber")

  ModelDFSL_VLR9$MInput_VLR9 <- ModelDFSL_VLR9$LitterC_VLR9 + ModelDFSL_VLR9$ManureC_VLR9
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR9$CTails_VLR9 <- ModelDFSL_VLR9$AllPools_noIOM_VLR9 + ModelDFSL_VLR9$MInput_VLR9

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR9$AccumInput_VLR9 = ModelDFSL_VLR9$AccumInput_VLR9=cumsum(ModelDFSL_VLR9$MInput_VLR9)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR9$MGrowth_DPM_VLR9 <- ave(ModelDFSL_VLR9$DPM_VLR9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR9$MGrowth_RPM_VLR9 <- ave(ModelDFSL_VLR9$RPM_VLR9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR9$MGrowth_BIO_VLR9 <- ave(ModelDFSL_VLR9$BIO_VLR9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR9$MGrowth_HUM_VLR9 <- ave(ModelDFSL_VLR9$HUM_VLR9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR9$MGrowth_IOM_VLR9 <- ave(ModelDFSL_VLR9$IOM_VLR9, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR9$M_CCO2_VLR9 <- ModelDFSL_VLR9$MInput_VLR9 - ModelDFSL_VLR9$MGrowth_DPM_VLR9 - ModelDFSL_VLR9$MGrowth_RPM_VLR9 - ModelDFSL_VLR9$MGrowth_BIO_VLR9 - ModelDFSL_VLR9$MGrowth_HUM_VLR9
  ModelDFSL_VLR9$Accum_CCO2_VLR9 <- ModelDFSL_VLR9$AccumInput_VLR9 - ModelDFSL_VLR9$AllPools_noIOM_VLR9

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR9$M_CCO2_VLR9[1] <- 0
  ModelDFSL_VLR9$Accum_CCO2_VLR9[1] <- 0

  #Balance validation
  ModelDFSL_VLR9$Balance_VLR9 <- ModelDFSL_VLR9$AccumInput_VLR9 - ModelDFSL_VLR9$Accum_CCO2_VLR9 - (ModelDFSL_VLR9$DPM_VLR9 + ModelDFSL_VLR9$RPM_VLR9 + ModelDFSL_VLR9$BIO_VLR9 + ModelDFSL_VLR9$HUM_VLR9)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR9$M_CO2_VLR9 <- ModelDFSL_VLR9$M_CCO2_VLR9 * 44/12
  ModelDFSL_VLR9$Accum_CO2_VLR9 <- ModelDFSL_VLR9$Accum_CCO2_VLR9 * 44/12

  #This model will be called VLR9C because implies a continuous input of C
  ModelDFSL_VLR9C <- ModelDFSL_VLR9

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR9C,"VXC_Models\\ModelDFSL_R_VLR9C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR9 <- ModelDFSL_VLR9 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR9.1 <- rbind(c(0:0), ModelDFSLt0_VLR9)
  ModelDFSLt1_VLR9.2 <- rbind(c(0:0), ModelDFSLt1_VLR9.1)
  ModelDFSLt1_VLR9.3 <- rbind(c(0:0), ModelDFSLt1_VLR9.2)
  ModelDFSLt1_VLR9.4 <- rbind(c(0:0), ModelDFSLt1_VLR9.3)
  ModelDFSLt1_VLR9.5 <- rbind(c(0:0), ModelDFSLt1_VLR9.4)
  ModelDFSLt1_VLR9.6 <- rbind(c(0:0), ModelDFSLt1_VLR9.5)
  ModelDFSLt1_VLR9.7 <- rbind(c(0:0), ModelDFSLt1_VLR9.6)
  ModelDFSLt1_VLR9.8 <- rbind(c(0:0), ModelDFSLt1_VLR9.7)
  ModelDFSLt1_VLR9.9 <- rbind(c(0:0), ModelDFSLt1_VLR9.8)
  ModelDFSLt1_VLR9.10 <- rbind(c(0:0), ModelDFSLt1_VLR9.9)
  ModelDFSLt1_VLR9.11 <- rbind(c(0:0), ModelDFSLt1_VLR9.10)
  ModelDFSLt1_VLR9.12 <- rbind(c(0:0), ModelDFSLt1_VLR9.11)
  ModelDFSLt1_VLR9.13 <- ModelDFSLt1_VLR9.12[-nrow(ModelDFSLt1_VLR9.12),]
  ModelDFSLt1_VLR9.14 <- ModelDFSLt1_VLR9.13[-nrow(ModelDFSLt1_VLR9.13),]
  ModelDFSLt1_VLR9.15 <- ModelDFSLt1_VLR9.14[-nrow(ModelDFSLt1_VLR9.14),]
  ModelDFSLt1_VLR9.16 <- ModelDFSLt1_VLR9.15[-nrow(ModelDFSLt1_VLR9.15),]
  ModelDFSLt1_VLR9.17 <- ModelDFSLt1_VLR9.16[-nrow(ModelDFSLt1_VLR9.16),]
  ModelDFSLt1_VLR9.18 <- ModelDFSLt1_VLR9.17[-nrow(ModelDFSLt1_VLR9.17),]
  ModelDFSLt1_VLR9.19 <- ModelDFSLt1_VLR9.18[-nrow(ModelDFSLt1_VLR9.18),]
  ModelDFSLt1_VLR9.20 <- ModelDFSLt1_VLR9.19[-nrow(ModelDFSLt1_VLR9.19),]
  ModelDFSLt1_VLR9.21 <- ModelDFSLt1_VLR9.20[-nrow(ModelDFSLt1_VLR9.20),]
  ModelDFSLt1_VLR9.22 <- ModelDFSLt1_VLR9.21[-nrow(ModelDFSLt1_VLR9.21),]
  ModelDFSLt1_VLR9.23 <- ModelDFSLt1_VLR9.22[-nrow(ModelDFSLt1_VLR9.22),]
  ModelDFSLt1_VLR9.24 <- ModelDFSLt1_VLR9.23[-nrow(ModelDFSLt1_VLR9.23),]

  ModelDFSLt1_VLR9 <- ModelDFSLt1_VLR9.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR9 <- ModelDFSLt0_VLR9 - ModelDFSLt1_VLR9

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR9$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR9P because implies a one-off input of C
  ModelDFSL_VLR9P <- ModelDFSL1y_VLR9

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR9P,"VXP_Models\\ModelDFSL_R_VLR9P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR9 <- ggplot(ModelDFSL_VLR9P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR9, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR9, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR9, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR9, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR9, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR9, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR9, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR9
  ggplotly(P_CFluxI1y_VLR9)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR9P_YCO2 <- ModelDFSL_VLR9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR9P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR9")

  PA_CO21y_VLR9 <- ggplot(ModelDFSL_VLR9P_YCO2, aes(x = Year, y = AnnualCO2_VLR9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR9
  ggplotly(PA_CO21y_VLR9)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR9P_YCO2D <- merge(ModelDFSL_VLR9P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR9P_YCO2D$AnnualCO2D_VLR9 <- ModelDFSL_VLR9P_YCO2D$AnnualCO2_VLR9 * ModelDFSL_VLR9P_YCO2D$GWP100

  PA_CO21yD_VLR9 <- ggplot(ModelDFSL_VLR9P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR9
  ggplotly(PA_CO21yD_VLR9)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR9P_YC <- ModelDFSL_VLR9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR9P_YC) <- c("Year", "Months", "AnnualCTail_VLR9")

  PA_C1y_VLR9 <- ggplot(ModelDFSL_VLR9P_YC, aes(x = Year, y = AnnualCTail_VLR9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR9
  ggplotly(PA_C1y_VLR9)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR9P_YCT <- ModelDFSL_VLR9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR9)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR9P_YCT) <- c("Year", "Months", "AnnualCTail_VLR9")

  PA_CT1y_VLR9 <- ggplot(ModelDFSL_VLR9P_YCT, aes(x = Year, y = AnnualCTail_VLR9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR9
  ggplotly(PA_CT1y_VLR9)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR9P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR9P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR9P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR9P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR9P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR9P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR9P_YCT,"CTails_P\\ModelDFSL_R_C_VLR9P.xlsx") #Yearly C emissions



  #### 8.11 - VLR10) Litter; 100%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLR10=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLR10=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                           S.Thick = soil.thick, pClay = clay100,
                           pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLR10=data.frame(years,rep(fT_VLR10*fW_VLR10,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLR10=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100, #Percent clay in mineral soil
    xi=xi.frame_VLR10) #Loads the model

  Ct_VLR10=getC(Model_VLR10) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLR10, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLR10 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLR10 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLR10 <- as.data.frame(VEC_Lit_VLR10) #Converting the Litter vector to a data frame
  VEC_LitDF_VLR10$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLR10 <- as.data.frame(VEC_Man_VLR10) #Converting the Manure vector to a data frame
  VEC_ManDF_VLR10$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLR10, class) #Check that class is numeric
  sapply(VEC_ManDF_VLR10, class) #Check that class is numeric
  LitterCinputs_VLR10=VEC_LitDF_VLR10   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLR10=VEC_ManDF_VLR10 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLR10 <- merge(LitterCinputs_VLR10, ManureCinputs_VLR10, by = "MNumber")
  MCinputs_VLR10$MInput_VLR10 <- MCinputs_VLR10$VEC_Lit_VLR10 + MCinputs_VLR10$VEC_Man_VLR10

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLR10)[which(names(MCinputs_VLR10) == "VEC_Lit_VLR10")] <- "LitterC_VLR10"
  colnames(MCinputs_VLR10)[which(names(MCinputs_VLR10) == "VEC_Man_VLR10")] <- "ManureC_VLR10"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLR10 <- as.data.frame(Ct_VLR10)
  colnames(ModelDF_VLR10) <- c('DPM_VLR10','RPM_VLR10','BIO_VLR10', 'HUM_VLR10', 'IOM_VLR10')
  ModelDFS_VLR10 <- ModelDF_VLR10[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLR10$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLR10$AllPools_VLR10 <- ModelDFS_VLR10$DPM_VLR10 + ModelDFS_VLR10$RPM_VLR10 + ModelDFS_VLR10$BIO_VLR10 + ModelDFS_VLR10$HUM_VLR10 + ModelDFS_VLR10$IOM_VLR10

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLR10$AllPools_noIOM_VLR10 <- ModelDFS_VLR10$DPM_VLR10 + ModelDFS_VLR10$RPM_VLR10 + ModelDFS_VLR10$BIO_VLR10 + ModelDFS_VLR10$HUM_VLR10

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLR10 <- merge(ModelDFS_VLR10, MCinputs_VLR10, by = "MNumber")

  ModelDFSL_VLR10$MInput_VLR10 <- ModelDFSL_VLR10$LitterC_VLR10 + ModelDFSL_VLR10$ManureC_VLR10
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLR10$CTails_VLR10 <- ModelDFSL_VLR10$AllPools_noIOM_VLR10 + ModelDFSL_VLR10$MInput_VLR10

  #Create Monthly Accumulated input of C
  ModelDFSL_VLR10$AccumInput_VLR10 = ModelDFSL_VLR10$AccumInput_VLR10=cumsum(ModelDFSL_VLR10$MInput_VLR10)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLR10$MGrowth_DPM_VLR10 <- ave(ModelDFSL_VLR10$DPM_VLR10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR10$MGrowth_RPM_VLR10 <- ave(ModelDFSL_VLR10$RPM_VLR10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR10$MGrowth_BIO_VLR10 <- ave(ModelDFSL_VLR10$BIO_VLR10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR10$MGrowth_HUM_VLR10 <- ave(ModelDFSL_VLR10$HUM_VLR10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLR10$MGrowth_IOM_VLR10 <- ave(ModelDFSL_VLR10$IOM_VLR10, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLR10$M_CCO2_VLR10 <- ModelDFSL_VLR10$MInput_VLR10 - ModelDFSL_VLR10$MGrowth_DPM_VLR10 - ModelDFSL_VLR10$MGrowth_RPM_VLR10 - ModelDFSL_VLR10$MGrowth_BIO_VLR10 - ModelDFSL_VLR10$MGrowth_HUM_VLR10
  ModelDFSL_VLR10$Accum_CCO2_VLR10 <- ModelDFSL_VLR10$AccumInput_VLR10 - ModelDFSL_VLR10$AllPools_noIOM_VLR10

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLR10$M_CCO2_VLR10[1] <- 0
  ModelDFSL_VLR10$Accum_CCO2_VLR10[1] <- 0

  #Balance validation
  ModelDFSL_VLR10$Balance_VLR10 <- ModelDFSL_VLR10$AccumInput_VLR10 - ModelDFSL_VLR10$Accum_CCO2_VLR10 - (ModelDFSL_VLR10$DPM_VLR10 + ModelDFSL_VLR10$RPM_VLR10 + ModelDFSL_VLR10$BIO_VLR10 + ModelDFSL_VLR10$HUM_VLR10)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLR10$M_CO2_VLR10 <- ModelDFSL_VLR10$M_CCO2_VLR10 * 44/12
  ModelDFSL_VLR10$Accum_CO2_VLR10 <- ModelDFSL_VLR10$Accum_CCO2_VLR10 * 44/12

  #This model will be called VLR10C because implies a continuous input of C
  ModelDFSL_VLR10C <- ModelDFSL_VLR10

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR10C,"VXC_Models\\ModelDFSL_R_VLR10C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLR10 <- ModelDFSL_VLR10 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLR10.1 <- rbind(c(0:0), ModelDFSLt0_VLR10)
  ModelDFSLt1_VLR10.2 <- rbind(c(0:0), ModelDFSLt1_VLR10.1)
  ModelDFSLt1_VLR10.3 <- rbind(c(0:0), ModelDFSLt1_VLR10.2)
  ModelDFSLt1_VLR10.4 <- rbind(c(0:0), ModelDFSLt1_VLR10.3)
  ModelDFSLt1_VLR10.5 <- rbind(c(0:0), ModelDFSLt1_VLR10.4)
  ModelDFSLt1_VLR10.6 <- rbind(c(0:0), ModelDFSLt1_VLR10.5)
  ModelDFSLt1_VLR10.7 <- rbind(c(0:0), ModelDFSLt1_VLR10.6)
  ModelDFSLt1_VLR10.8 <- rbind(c(0:0), ModelDFSLt1_VLR10.7)
  ModelDFSLt1_VLR10.9 <- rbind(c(0:0), ModelDFSLt1_VLR10.8)
  ModelDFSLt1_VLR10.10 <- rbind(c(0:0), ModelDFSLt1_VLR10.9)
  ModelDFSLt1_VLR10.11 <- rbind(c(0:0), ModelDFSLt1_VLR10.10)
  ModelDFSLt1_VLR10.12 <- rbind(c(0:0), ModelDFSLt1_VLR10.11)
  ModelDFSLt1_VLR10.13 <- ModelDFSLt1_VLR10.12[-nrow(ModelDFSLt1_VLR10.12),]
  ModelDFSLt1_VLR10.14 <- ModelDFSLt1_VLR10.13[-nrow(ModelDFSLt1_VLR10.13),]
  ModelDFSLt1_VLR10.15 <- ModelDFSLt1_VLR10.14[-nrow(ModelDFSLt1_VLR10.14),]
  ModelDFSLt1_VLR10.16 <- ModelDFSLt1_VLR10.15[-nrow(ModelDFSLt1_VLR10.15),]
  ModelDFSLt1_VLR10.17 <- ModelDFSLt1_VLR10.16[-nrow(ModelDFSLt1_VLR10.16),]
  ModelDFSLt1_VLR10.18 <- ModelDFSLt1_VLR10.17[-nrow(ModelDFSLt1_VLR10.17),]
  ModelDFSLt1_VLR10.19 <- ModelDFSLt1_VLR10.18[-nrow(ModelDFSLt1_VLR10.18),]
  ModelDFSLt1_VLR10.20 <- ModelDFSLt1_VLR10.19[-nrow(ModelDFSLt1_VLR10.19),]
  ModelDFSLt1_VLR10.21 <- ModelDFSLt1_VLR10.20[-nrow(ModelDFSLt1_VLR10.20),]
  ModelDFSLt1_VLR10.22 <- ModelDFSLt1_VLR10.21[-nrow(ModelDFSLt1_VLR10.21),]
  ModelDFSLt1_VLR10.23 <- ModelDFSLt1_VLR10.22[-nrow(ModelDFSLt1_VLR10.22),]
  ModelDFSLt1_VLR10.24 <- ModelDFSLt1_VLR10.23[-nrow(ModelDFSLt1_VLR10.23),]

  ModelDFSLt1_VLR10 <- ModelDFSLt1_VLR10.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLR10 <- ModelDFSLt0_VLR10 - ModelDFSLt1_VLR10

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLR10$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLR10P because implies a one-off input of C
  ModelDFSL_VLR10P <- ModelDFSL1y_VLR10

  #Export the dataframe
  write_xlsx(ModelDFSL_VLR10P,"VXP_Models\\ModelDFSL_R_VLR10P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLR10 <- ggplot(ModelDFSL_VLR10P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLR10, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLR10, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLR10, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLR10, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLR10, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLR10, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLR10, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLR10
  ggplotly(P_CFluxI1y_VLR10)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLR10P_YCO2 <- ModelDFSL_VLR10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLR10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR10P_YCO2) <- c("Year", "Months", "AnnualCO2_VLR10")

  PA_CO21y_VLR10 <- ggplot(ModelDFSL_VLR10P_YCO2, aes(x = Year, y = AnnualCO2_VLR10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLR10
  ggplotly(PA_CO21y_VLR10)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLR10P_YCO2D <- merge(ModelDFSL_VLR10P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLR10P_YCO2D$AnnualCO2D_VLR10 <- ModelDFSL_VLR10P_YCO2D$AnnualCO2_VLR10 * ModelDFSL_VLR10P_YCO2D$GWP100

  PA_CO21yD_VLR10 <- ggplot(ModelDFSL_VLR10P_YCO2D, aes(x = Year, y = AnnualCO2D_VLR10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLR10
  ggplotly(PA_CO21yD_VLR10)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLR10P_YC <- ModelDFSL_VLR10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLR10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR10P_YC) <- c("Year", "Months", "AnnualCTail_VLR10")

  PA_C1y_VLR10 <- ggplot(ModelDFSL_VLR10P_YC, aes(x = Year, y = AnnualCTail_VLR10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLR10
  ggplotly(PA_C1y_VLR10)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLR10P_YCT <- ModelDFSL_VLR10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLR10)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLR10P_YCT) <- c("Year", "Months", "AnnualCTail_VLR10")

  PA_CT1y_VLR10 <- ggplot(ModelDFSL_VLR10P_YCT, aes(x = Year, y = AnnualCTail_VLR10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLR10
  ggplotly(PA_CT1y_VLR10)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLR10P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLR10P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLR10P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLR10P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLR10P_YC,"CEmissions_P\\ModelDFSL_R_C_VLR10P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLR10P_YCT,"CTails_P\\ModelDFSL_R_C_VLR10P.xlsx") #Yearly C emissions











}

