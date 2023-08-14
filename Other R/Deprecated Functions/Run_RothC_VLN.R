#' Run RothC_VLN
#'
#' The Merge_VXC function merge all VXC models created by the functions Run_RothC_[].
#'
#' @return A dataframe with all VXC models merged together
#' @import SoilR ggplot2 stringr
#' @importFrom plotly ggplotly
#' @export

#Function to Run and Create the multiple RothC Combinations

Run_RothC_VLN <- function(SL_years = 100,
                          soil.thick = 23,
                          Tem = Temp,
                          Pre = Precip,
                          Eva = Evp,
                          NTRM = 0.95)

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

  #### 9) Model Combinations - Litter; No Tillage, 0-100% (+10%) Clay ####
  #### 9.1 - VLN0) Litter; 0%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN0=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN0=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay0,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN0=data.frame(years,rep(fT_VLN0*fW_VLN0,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN0=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay0, #Percent clay in mineral soil
    xi=xi.frame_VLN0) #Loads the model

  Ct_VLN0=getC(Model_VLN0) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN0, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN0 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN0 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN0 <- as.data.frame(VEC_Lit_VLN0) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN0$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN0 <- as.data.frame(VEC_Man_VLN0) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN0$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN0, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN0, class) #Check that class is numeric
  LitterCinputs_VLN0=VEC_LitDF_VLN0   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN0=VEC_ManDF_VLN0 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN0 <- merge(LitterCinputs_VLN0, ManureCinputs_VLN0, by = "MNumber")
  MCinputs_VLN0$MInput_VLN0 <- MCinputs_VLN0$VEC_Lit_VLN0 + MCinputs_VLN0$VEC_Man_VLN0

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN0)[which(names(MCinputs_VLN0) == "VEC_Lit_VLN0")] <- "LitterC_VLN0"
  colnames(MCinputs_VLN0)[which(names(MCinputs_VLN0) == "VEC_Man_VLN0")] <- "ManureC_VLN0"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN0 <- as.data.frame(Ct_VLN0)
  colnames(ModelDF_VLN0) <- c('DPM_VLN0','RPM_VLN0','BIO_VLN0', 'HUM_VLN0', 'IOM_VLN0')
  ModelDFS_VLN0 <- ModelDF_VLN0[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN0$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN0$AllPools_VLN0 <- ModelDFS_VLN0$DPM_VLN0 + ModelDFS_VLN0$RPM_VLN0 + ModelDFS_VLN0$BIO_VLN0 + ModelDFS_VLN0$HUM_VLN0 + ModelDFS_VLN0$IOM_VLN0

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN0$AllPools_noIOM_VLN0 <- ModelDFS_VLN0$DPM_VLN0 + ModelDFS_VLN0$RPM_VLN0 + ModelDFS_VLN0$BIO_VLN0 + ModelDFS_VLN0$HUM_VLN0

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN0 <- merge(ModelDFS_VLN0, MCinputs_VLN0, by = "MNumber")

  ModelDFSL_VLN0$MInput_VLN0 <- ModelDFSL_VLN0$LitterC_VLN0 + ModelDFSL_VLN0$ManureC_VLN0
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN0$CTails_VLN0 <- ModelDFSL_VLN0$AllPools_noIOM_VLN0 + ModelDFSL_VLN0$MInput_VLN0

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN0$AccumInput_VLN0 = ModelDFSL_VLN0$AccumInput_VLN0=cumsum(ModelDFSL_VLN0$MInput_VLN0)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN0$MGrowth_DPM_VLN0 <- ave(ModelDFSL_VLN0$DPM_VLN0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN0$MGrowth_RPM_VLN0 <- ave(ModelDFSL_VLN0$RPM_VLN0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN0$MGrowth_BIO_VLN0 <- ave(ModelDFSL_VLN0$BIO_VLN0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN0$MGrowth_HUM_VLN0 <- ave(ModelDFSL_VLN0$HUM_VLN0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN0$MGrowth_IOM_VLN0 <- ave(ModelDFSL_VLN0$IOM_VLN0, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN0$M_CCO2_VLN0 <- ModelDFSL_VLN0$MInput_VLN0 - ModelDFSL_VLN0$MGrowth_DPM_VLN0 - ModelDFSL_VLN0$MGrowth_RPM_VLN0 - ModelDFSL_VLN0$MGrowth_BIO_VLN0 - ModelDFSL_VLN0$MGrowth_HUM_VLN0
  ModelDFSL_VLN0$Accum_CCO2_VLN0 <- ModelDFSL_VLN0$AccumInput_VLN0 - ModelDFSL_VLN0$AllPools_noIOM_VLN0

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN0$M_CCO2_VLN0[1] <- 0
  ModelDFSL_VLN0$Accum_CCO2_VLN0[1] <- 0

  #Balance validation
  ModelDFSL_VLN0$Balance_VLN0 <- ModelDFSL_VLN0$AccumInput_VLN0 - ModelDFSL_VLN0$Accum_CCO2_VLN0 - (ModelDFSL_VLN0$DPM_VLN0 + ModelDFSL_VLN0$RPM_VLN0 + ModelDFSL_VLN0$BIO_VLN0 + ModelDFSL_VLN0$HUM_VLN0)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN0$M_CO2_VLN0 <- ModelDFSL_VLN0$M_CCO2_VLN0 * 44/12
  ModelDFSL_VLN0$Accum_CO2_VLN0 <- ModelDFSL_VLN0$Accum_CCO2_VLN0 * 44/12

  #This model will be called VLN0C because implies a continuous input of C
  ModelDFSL_VLN0C <- ModelDFSL_VLN0

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN0C,"VXC_Models\\ModelDFSL_R_VLN0C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN0 <- ModelDFSL_VLN0 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN0.1 <- rbind(c(0:0), ModelDFSLt0_VLN0)
  ModelDFSLt1_VLN0.2 <- rbind(c(0:0), ModelDFSLt1_VLN0.1)
  ModelDFSLt1_VLN0.3 <- rbind(c(0:0), ModelDFSLt1_VLN0.2)
  ModelDFSLt1_VLN0.4 <- rbind(c(0:0), ModelDFSLt1_VLN0.3)
  ModelDFSLt1_VLN0.5 <- rbind(c(0:0), ModelDFSLt1_VLN0.4)
  ModelDFSLt1_VLN0.6 <- rbind(c(0:0), ModelDFSLt1_VLN0.5)
  ModelDFSLt1_VLN0.7 <- rbind(c(0:0), ModelDFSLt1_VLN0.6)
  ModelDFSLt1_VLN0.8 <- rbind(c(0:0), ModelDFSLt1_VLN0.7)
  ModelDFSLt1_VLN0.9 <- rbind(c(0:0), ModelDFSLt1_VLN0.8)
  ModelDFSLt1_VLN0.10 <- rbind(c(0:0), ModelDFSLt1_VLN0.9)
  ModelDFSLt1_VLN0.11 <- rbind(c(0:0), ModelDFSLt1_VLN0.10)
  ModelDFSLt1_VLN0.12 <- rbind(c(0:0), ModelDFSLt1_VLN0.11)
  ModelDFSLt1_VLN0.13 <- ModelDFSLt1_VLN0.12[-nrow(ModelDFSLt1_VLN0.12),]
  ModelDFSLt1_VLN0.14 <- ModelDFSLt1_VLN0.13[-nrow(ModelDFSLt1_VLN0.13),]
  ModelDFSLt1_VLN0.15 <- ModelDFSLt1_VLN0.14[-nrow(ModelDFSLt1_VLN0.14),]
  ModelDFSLt1_VLN0.16 <- ModelDFSLt1_VLN0.15[-nrow(ModelDFSLt1_VLN0.15),]
  ModelDFSLt1_VLN0.17 <- ModelDFSLt1_VLN0.16[-nrow(ModelDFSLt1_VLN0.16),]
  ModelDFSLt1_VLN0.18 <- ModelDFSLt1_VLN0.17[-nrow(ModelDFSLt1_VLN0.17),]
  ModelDFSLt1_VLN0.19 <- ModelDFSLt1_VLN0.18[-nrow(ModelDFSLt1_VLN0.18),]
  ModelDFSLt1_VLN0.20 <- ModelDFSLt1_VLN0.19[-nrow(ModelDFSLt1_VLN0.19),]
  ModelDFSLt1_VLN0.21 <- ModelDFSLt1_VLN0.20[-nrow(ModelDFSLt1_VLN0.20),]
  ModelDFSLt1_VLN0.22 <- ModelDFSLt1_VLN0.21[-nrow(ModelDFSLt1_VLN0.21),]
  ModelDFSLt1_VLN0.23 <- ModelDFSLt1_VLN0.22[-nrow(ModelDFSLt1_VLN0.22),]
  ModelDFSLt1_VLN0.24 <- ModelDFSLt1_VLN0.23[-nrow(ModelDFSLt1_VLN0.23),]

  ModelDFSLt1_VLN0 <- ModelDFSLt1_VLN0.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN0 <- ModelDFSLt0_VLN0 - ModelDFSLt1_VLN0

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN0$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN0P because implies a one-off input of C
  ModelDFSL_VLN0P <- ModelDFSL1y_VLN0

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN0P,"VXP_Models\\ModelDFSL_R_VLN0P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN0 <- ggplot(ModelDFSL_VLN0P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN0, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN0, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN0, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN0, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN0, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN0, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN0, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN0
  ggplotly(P_CFluxI1y_VLN0)

  #Plot 3.1: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN0P_YCO2 <- ModelDFSL_VLN0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN0P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN0")

  PA_CO21y_VLN0 <- ggplot(ModelDFSL_VLN0P_YCO2, aes(x = Year, y = AnnualCO2_VLN0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN0
  ggplotly(PA_CO21y_VLN0)

  #Plot 3.2: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN0P_YCO2D <- merge(ModelDFSL_VLN0P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN0P_YCO2D$AnnualCO2D_VLN0 <- ModelDFSL_VLN0P_YCO2D$AnnualCO2_VLN0 * ModelDFSL_VLN0P_YCO2D$GWP100

  PA_CO21yD_VLN0 <- ggplot(ModelDFSL_VLN0P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN0
  ggplotly(PA_CO21yD_VLN0)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN0P_YC <- ModelDFSL_VLN0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN0P_YC) <- c("Year", "Months", "AnnualCTail_VLN0")

  PA_C1y_VLN0 <- ggplot(ModelDFSL_VLN0P_YC, aes(x = Year, y = AnnualCTail_VLN0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN0
  ggplotly(PA_C1y_VLN0)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN0P_YCT <- ModelDFSL_VLN0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN0)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN0P_YCT) <- c("Year", "Months", "AnnualCTail_VLN0")

  PA_CT1y_VLN0 <- ggplot(ModelDFSL_VLN0P_YCT, aes(x = Year, y = AnnualCTail_VLN0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN0
  ggplotly(PA_CT1y_VLN0)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN0P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN0P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN0P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN0P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN0P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN0P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN0P_YCT,"CTails_P\\ModelDFSL_R_C_VLN0P.xlsx") #Yearly C emissions


  #### 9.2 - VLN1) Litter; 10%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN1=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN1=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.1,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN1=data.frame(years,rep(fT_VLN1*fW_VLN1,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN1=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.1, #Percent clay in mineral soil
    xi=xi.frame_VLN1) #Loads the model

  Ct_VLN1=getC(Model_VLN1) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN1, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN1 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN1 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN1 <- as.data.frame(VEC_Lit_VLN1) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN1$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN1 <- as.data.frame(VEC_Man_VLN1) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN1$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN1, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN1, class) #Check that class is numeric
  LitterCinputs_VLN1=VEC_LitDF_VLN1   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN1=VEC_ManDF_VLN1 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN1 <- merge(LitterCinputs_VLN1, ManureCinputs_VLN1, by = "MNumber")
  MCinputs_VLN1$MInput_VLN1 <- MCinputs_VLN1$VEC_Lit_VLN1 + MCinputs_VLN1$VEC_Man_VLN1

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN1)[which(names(MCinputs_VLN1) == "VEC_Lit_VLN1")] <- "LitterC_VLN1"
  colnames(MCinputs_VLN1)[which(names(MCinputs_VLN1) == "VEC_Man_VLN1")] <- "ManureC_VLN1"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN1 <- as.data.frame(Ct_VLN1)
  colnames(ModelDF_VLN1) <- c('DPM_VLN1','RPM_VLN1','BIO_VLN1', 'HUM_VLN1', 'IOM_VLN1')
  ModelDFS_VLN1 <- ModelDF_VLN1[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN1$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN1$AllPools_VLN1 <- ModelDFS_VLN1$DPM_VLN1 + ModelDFS_VLN1$RPM_VLN1 + ModelDFS_VLN1$BIO_VLN1 + ModelDFS_VLN1$HUM_VLN1 + ModelDFS_VLN1$IOM_VLN1

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN1$AllPools_noIOM_VLN1 <- ModelDFS_VLN1$DPM_VLN1 + ModelDFS_VLN1$RPM_VLN1 + ModelDFS_VLN1$BIO_VLN1 + ModelDFS_VLN1$HUM_VLN1

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN1 <- merge(ModelDFS_VLN1, MCinputs_VLN1, by = "MNumber")

  ModelDFSL_VLN1$MInput_VLN1 <- ModelDFSL_VLN1$LitterC_VLN1 + ModelDFSL_VLN1$ManureC_VLN1
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN1$CTails_VLN1 <- ModelDFSL_VLN1$AllPools_noIOM_VLN1 + ModelDFSL_VLN1$MInput_VLN1

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN1$AccumInput_VLN1 = ModelDFSL_VLN1$AccumInput_VLN1=cumsum(ModelDFSL_VLN1$MInput_VLN1)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN1$MGrowth_DPM_VLN1 <- ave(ModelDFSL_VLN1$DPM_VLN1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN1$MGrowth_RPM_VLN1 <- ave(ModelDFSL_VLN1$RPM_VLN1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN1$MGrowth_BIO_VLN1 <- ave(ModelDFSL_VLN1$BIO_VLN1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN1$MGrowth_HUM_VLN1 <- ave(ModelDFSL_VLN1$HUM_VLN1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN1$MGrowth_IOM_VLN1 <- ave(ModelDFSL_VLN1$IOM_VLN1, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN1$M_CCO2_VLN1 <- ModelDFSL_VLN1$MInput_VLN1 - ModelDFSL_VLN1$MGrowth_DPM_VLN1 - ModelDFSL_VLN1$MGrowth_RPM_VLN1 - ModelDFSL_VLN1$MGrowth_BIO_VLN1 - ModelDFSL_VLN1$MGrowth_HUM_VLN1
  ModelDFSL_VLN1$Accum_CCO2_VLN1 <- ModelDFSL_VLN1$AccumInput_VLN1 - ModelDFSL_VLN1$AllPools_noIOM_VLN1

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN1$M_CCO2_VLN1[1] <- 0
  ModelDFSL_VLN1$Accum_CCO2_VLN1[1] <- 0

  #Balance validation
  ModelDFSL_VLN1$Balance_VLN1 <- ModelDFSL_VLN1$AccumInput_VLN1 - ModelDFSL_VLN1$Accum_CCO2_VLN1 - (ModelDFSL_VLN1$DPM_VLN1 + ModelDFSL_VLN1$RPM_VLN1 + ModelDFSL_VLN1$BIO_VLN1 + ModelDFSL_VLN1$HUM_VLN1)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN1$M_CO2_VLN1 <- ModelDFSL_VLN1$M_CCO2_VLN1 * 44/12
  ModelDFSL_VLN1$Accum_CO2_VLN1 <- ModelDFSL_VLN1$Accum_CCO2_VLN1 * 44/12

  #This model will be called VLN1C because implies a continuous input of C
  ModelDFSL_VLN1C <- ModelDFSL_VLN1

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN1C,"VXC_Models\\ModelDFSL_R_VLN1C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN1 <- ModelDFSL_VLN1 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN1.1 <- rbind(c(0:0), ModelDFSLt0_VLN1)
  ModelDFSLt1_VLN1.2 <- rbind(c(0:0), ModelDFSLt1_VLN1.1)
  ModelDFSLt1_VLN1.3 <- rbind(c(0:0), ModelDFSLt1_VLN1.2)
  ModelDFSLt1_VLN1.4 <- rbind(c(0:0), ModelDFSLt1_VLN1.3)
  ModelDFSLt1_VLN1.5 <- rbind(c(0:0), ModelDFSLt1_VLN1.4)
  ModelDFSLt1_VLN1.6 <- rbind(c(0:0), ModelDFSLt1_VLN1.5)
  ModelDFSLt1_VLN1.7 <- rbind(c(0:0), ModelDFSLt1_VLN1.6)
  ModelDFSLt1_VLN1.8 <- rbind(c(0:0), ModelDFSLt1_VLN1.7)
  ModelDFSLt1_VLN1.9 <- rbind(c(0:0), ModelDFSLt1_VLN1.8)
  ModelDFSLt1_VLN1.10 <- rbind(c(0:0), ModelDFSLt1_VLN1.9)
  ModelDFSLt1_VLN1.11 <- rbind(c(0:0), ModelDFSLt1_VLN1.10)
  ModelDFSLt1_VLN1.12 <- rbind(c(0:0), ModelDFSLt1_VLN1.11)
  ModelDFSLt1_VLN1.13 <- ModelDFSLt1_VLN1.12[-nrow(ModelDFSLt1_VLN1.12),]
  ModelDFSLt1_VLN1.14 <- ModelDFSLt1_VLN1.13[-nrow(ModelDFSLt1_VLN1.13),]
  ModelDFSLt1_VLN1.15 <- ModelDFSLt1_VLN1.14[-nrow(ModelDFSLt1_VLN1.14),]
  ModelDFSLt1_VLN1.16 <- ModelDFSLt1_VLN1.15[-nrow(ModelDFSLt1_VLN1.15),]
  ModelDFSLt1_VLN1.17 <- ModelDFSLt1_VLN1.16[-nrow(ModelDFSLt1_VLN1.16),]
  ModelDFSLt1_VLN1.18 <- ModelDFSLt1_VLN1.17[-nrow(ModelDFSLt1_VLN1.17),]
  ModelDFSLt1_VLN1.19 <- ModelDFSLt1_VLN1.18[-nrow(ModelDFSLt1_VLN1.18),]
  ModelDFSLt1_VLN1.20 <- ModelDFSLt1_VLN1.19[-nrow(ModelDFSLt1_VLN1.19),]
  ModelDFSLt1_VLN1.21 <- ModelDFSLt1_VLN1.20[-nrow(ModelDFSLt1_VLN1.20),]
  ModelDFSLt1_VLN1.22 <- ModelDFSLt1_VLN1.21[-nrow(ModelDFSLt1_VLN1.21),]
  ModelDFSLt1_VLN1.23 <- ModelDFSLt1_VLN1.22[-nrow(ModelDFSLt1_VLN1.22),]
  ModelDFSLt1_VLN1.24 <- ModelDFSLt1_VLN1.23[-nrow(ModelDFSLt1_VLN1.23),]

  ModelDFSLt1_VLN1 <- ModelDFSLt1_VLN1.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN1 <- ModelDFSLt0_VLN1 - ModelDFSLt1_VLN1

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN1$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN1P because implies a one-off input of C
  ModelDFSL_VLN1P <- ModelDFSL1y_VLN1

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN1P,"VXP_Models\\ModelDFSL_R_VLN1P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN1 <- ggplot(ModelDFSL_VLN1P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN1, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN1, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN1, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN1, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN1, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN1, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN1, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN1
  ggplotly(P_CFluxI1y_VLN1)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN1P_YCO2 <- ModelDFSL_VLN1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN1P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN1")

  PA_CO21y_VLN1 <- ggplot(ModelDFSL_VLN1P_YCO2, aes(x = Year, y = AnnualCO2_VLN1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN1
  ggplotly(PA_CO21y_VLN1)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN1P_YCO2D <- merge(ModelDFSL_VLN1P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN1P_YCO2D$AnnualCO2D_VLN1 <- ModelDFSL_VLN1P_YCO2D$AnnualCO2_VLN1 * ModelDFSL_VLN1P_YCO2D$GWP100

  PA_CO21yD_VLN1 <- ggplot(ModelDFSL_VLN1P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN1
  ggplotly(PA_CO21yD_VLN1)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN1P_YC <- ModelDFSL_VLN1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN1P_YC) <- c("Year", "Months", "AnnualCTail_VLN1")

  PA_C1y_VLN1 <- ggplot(ModelDFSL_VLN1P_YC, aes(x = Year, y = AnnualCTail_VLN1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN1
  ggplotly(PA_C1y_VLN1)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN1P_YCT <- ModelDFSL_VLN1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN1)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN1P_YCT) <- c("Year", "Months", "AnnualCTail_VLN1")

  PA_CT1y_VLN1 <- ggplot(ModelDFSL_VLN1P_YCT, aes(x = Year, y = AnnualCTail_VLN1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN1
  ggplotly(PA_CT1y_VLN1)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN1P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN1P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN1P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN1P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN1P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN1P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN1P_YCT,"CTails_P\\ModelDFSL_R_C_VLN1P.xlsx") #Yearly C emissions



  #### 9.3 - VLN2) Litter; 20%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN2=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN2=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.2,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN2=data.frame(years,rep(fT_VLN2*fW_VLN2,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN2=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.2, #Percent clay in mineral soil
    xi=xi.frame_VLN2) #Loads the model

  Ct_VLN2=getC(Model_VLN2) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN2, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN2 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN2 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN2 <- as.data.frame(VEC_Lit_VLN2) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN2$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN2 <- as.data.frame(VEC_Man_VLN2) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN2$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN2, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN2, class) #Check that class is numeric
  LitterCinputs_VLN2=VEC_LitDF_VLN2   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN2=VEC_ManDF_VLN2 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN2 <- merge(LitterCinputs_VLN2, ManureCinputs_VLN2, by = "MNumber")
  MCinputs_VLN2$MInput_VLN2 <- MCinputs_VLN2$VEC_Lit_VLN2 + MCinputs_VLN2$VEC_Man_VLN2

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN2)[which(names(MCinputs_VLN2) == "VEC_Lit_VLN2")] <- "LitterC_VLN2"
  colnames(MCinputs_VLN2)[which(names(MCinputs_VLN2) == "VEC_Man_VLN2")] <- "ManureC_VLN2"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN2 <- as.data.frame(Ct_VLN2)
  colnames(ModelDF_VLN2) <- c('DPM_VLN2','RPM_VLN2','BIO_VLN2', 'HUM_VLN2', 'IOM_VLN2')
  ModelDFS_VLN2 <- ModelDF_VLN2[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN2$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN2$AllPools_VLN2 <- ModelDFS_VLN2$DPM_VLN2 + ModelDFS_VLN2$RPM_VLN2 + ModelDFS_VLN2$BIO_VLN2 + ModelDFS_VLN2$HUM_VLN2 + ModelDFS_VLN2$IOM_VLN2

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN2$AllPools_noIOM_VLN2 <- ModelDFS_VLN2$DPM_VLN2 + ModelDFS_VLN2$RPM_VLN2 + ModelDFS_VLN2$BIO_VLN2 + ModelDFS_VLN2$HUM_VLN2

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN2 <- merge(ModelDFS_VLN2, MCinputs_VLN2, by = "MNumber")

  ModelDFSL_VLN2$MInput_VLN2 <- ModelDFSL_VLN2$LitterC_VLN2 + ModelDFSL_VLN2$ManureC_VLN2
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN2$CTails_VLN2 <- ModelDFSL_VLN2$AllPools_noIOM_VLN2 + ModelDFSL_VLN2$MInput_VLN2

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN2$AccumInput_VLN2 = ModelDFSL_VLN2$AccumInput_VLN2=cumsum(ModelDFSL_VLN2$MInput_VLN2)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN2$MGrowth_DPM_VLN2 <- ave(ModelDFSL_VLN2$DPM_VLN2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN2$MGrowth_RPM_VLN2 <- ave(ModelDFSL_VLN2$RPM_VLN2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN2$MGrowth_BIO_VLN2 <- ave(ModelDFSL_VLN2$BIO_VLN2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN2$MGrowth_HUM_VLN2 <- ave(ModelDFSL_VLN2$HUM_VLN2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN2$MGrowth_IOM_VLN2 <- ave(ModelDFSL_VLN2$IOM_VLN2, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN2$M_CCO2_VLN2 <- ModelDFSL_VLN2$MInput_VLN2 - ModelDFSL_VLN2$MGrowth_DPM_VLN2 - ModelDFSL_VLN2$MGrowth_RPM_VLN2 - ModelDFSL_VLN2$MGrowth_BIO_VLN2 - ModelDFSL_VLN2$MGrowth_HUM_VLN2
  ModelDFSL_VLN2$Accum_CCO2_VLN2 <- ModelDFSL_VLN2$AccumInput_VLN2 - ModelDFSL_VLN2$AllPools_noIOM_VLN2

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN2$M_CCO2_VLN2[1] <- 0
  ModelDFSL_VLN2$Accum_CCO2_VLN2[1] <- 0

  #Balance validation
  ModelDFSL_VLN2$Balance_VLN2 <- ModelDFSL_VLN2$AccumInput_VLN2 - ModelDFSL_VLN2$Accum_CCO2_VLN2 - (ModelDFSL_VLN2$DPM_VLN2 + ModelDFSL_VLN2$RPM_VLN2 + ModelDFSL_VLN2$BIO_VLN2 + ModelDFSL_VLN2$HUM_VLN2)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN2$M_CO2_VLN2 <- ModelDFSL_VLN2$M_CCO2_VLN2 * 44/12
  ModelDFSL_VLN2$Accum_CO2_VLN2 <- ModelDFSL_VLN2$Accum_CCO2_VLN2 * 44/12

  #This model will be called VLN2C because implies a continuous input of C
  ModelDFSL_VLN2C <- ModelDFSL_VLN2

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN2C,"VXC_Models\\ModelDFSL_R_VLN2C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN2 <- ModelDFSL_VLN2 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN2.1 <- rbind(c(0:0), ModelDFSLt0_VLN2)
  ModelDFSLt1_VLN2.2 <- rbind(c(0:0), ModelDFSLt1_VLN2.1)
  ModelDFSLt1_VLN2.3 <- rbind(c(0:0), ModelDFSLt1_VLN2.2)
  ModelDFSLt1_VLN2.4 <- rbind(c(0:0), ModelDFSLt1_VLN2.3)
  ModelDFSLt1_VLN2.5 <- rbind(c(0:0), ModelDFSLt1_VLN2.4)
  ModelDFSLt1_VLN2.6 <- rbind(c(0:0), ModelDFSLt1_VLN2.5)
  ModelDFSLt1_VLN2.7 <- rbind(c(0:0), ModelDFSLt1_VLN2.6)
  ModelDFSLt1_VLN2.8 <- rbind(c(0:0), ModelDFSLt1_VLN2.7)
  ModelDFSLt1_VLN2.9 <- rbind(c(0:0), ModelDFSLt1_VLN2.8)
  ModelDFSLt1_VLN2.10 <- rbind(c(0:0), ModelDFSLt1_VLN2.9)
  ModelDFSLt1_VLN2.11 <- rbind(c(0:0), ModelDFSLt1_VLN2.10)
  ModelDFSLt1_VLN2.12 <- rbind(c(0:0), ModelDFSLt1_VLN2.11)
  ModelDFSLt1_VLN2.13 <- ModelDFSLt1_VLN2.12[-nrow(ModelDFSLt1_VLN2.12),]
  ModelDFSLt1_VLN2.14 <- ModelDFSLt1_VLN2.13[-nrow(ModelDFSLt1_VLN2.13),]
  ModelDFSLt1_VLN2.15 <- ModelDFSLt1_VLN2.14[-nrow(ModelDFSLt1_VLN2.14),]
  ModelDFSLt1_VLN2.16 <- ModelDFSLt1_VLN2.15[-nrow(ModelDFSLt1_VLN2.15),]
  ModelDFSLt1_VLN2.17 <- ModelDFSLt1_VLN2.16[-nrow(ModelDFSLt1_VLN2.16),]
  ModelDFSLt1_VLN2.18 <- ModelDFSLt1_VLN2.17[-nrow(ModelDFSLt1_VLN2.17),]
  ModelDFSLt1_VLN2.19 <- ModelDFSLt1_VLN2.18[-nrow(ModelDFSLt1_VLN2.18),]
  ModelDFSLt1_VLN2.20 <- ModelDFSLt1_VLN2.19[-nrow(ModelDFSLt1_VLN2.19),]
  ModelDFSLt1_VLN2.21 <- ModelDFSLt1_VLN2.20[-nrow(ModelDFSLt1_VLN2.20),]
  ModelDFSLt1_VLN2.22 <- ModelDFSLt1_VLN2.21[-nrow(ModelDFSLt1_VLN2.21),]
  ModelDFSLt1_VLN2.23 <- ModelDFSLt1_VLN2.22[-nrow(ModelDFSLt1_VLN2.22),]
  ModelDFSLt1_VLN2.24 <- ModelDFSLt1_VLN2.23[-nrow(ModelDFSLt1_VLN2.23),]

  ModelDFSLt1_VLN2 <- ModelDFSLt1_VLN2.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN2 <- ModelDFSLt0_VLN2 - ModelDFSLt1_VLN2

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN2$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN2P because implies a one-off input of C
  ModelDFSL_VLN2P <- ModelDFSL1y_VLN2

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN2P,"VXP_Models\\ModelDFSL_R_VLN2P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN2 <- ggplot(ModelDFSL_VLN2P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN2, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN2, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN2, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN2, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN2, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN2, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN2, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN2
  ggplotly(P_CFluxI1y_VLN2)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN2P_YCO2 <- ModelDFSL_VLN2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN2P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN2")

  PA_CO21y_VLN2 <- ggplot(ModelDFSL_VLN2P_YCO2, aes(x = Year, y = AnnualCO2_VLN2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN2
  ggplotly(PA_CO21y_VLN2)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN2P_YCO2D <- merge(ModelDFSL_VLN2P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN2P_YCO2D$AnnualCO2D_VLN2 <- ModelDFSL_VLN2P_YCO2D$AnnualCO2_VLN2 * ModelDFSL_VLN2P_YCO2D$GWP100

  PA_CO21yD_VLN2 <- ggplot(ModelDFSL_VLN2P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN2
  ggplotly(PA_CO21yD_VLN2)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN2P_YC <- ModelDFSL_VLN2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN2P_YC) <- c("Year", "Months", "AnnualCTail_VLN2")

  PA_C1y_VLN2 <- ggplot(ModelDFSL_VLN2P_YC, aes(x = Year, y = AnnualCTail_VLN2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN2
  ggplotly(PA_C1y_VLN2)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN2P_YCT <- ModelDFSL_VLN2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN2)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN2P_YCT) <- c("Year", "Months", "AnnualCTail_VLN2")

  PA_CT1y_VLN2 <- ggplot(ModelDFSL_VLN2P_YCT, aes(x = Year, y = AnnualCTail_VLN2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN2
  ggplotly(PA_CT1y_VLN2)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN2P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN2P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN2P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN2P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN2P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN2P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN2P_YCT,"CTails_P\\ModelDFSL_R_C_VLN2P.xlsx") #Yearly C emissions



  #### 9.4 - VLN3) Litter; 30%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN3=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN3=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.3,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN3=data.frame(years,rep(fT_VLN3*fW_VLN3,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN3=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.3, #Percent clay in mineral soil
    xi=xi.frame_VLN3) #Loads the model

  Ct_VLN3=getC(Model_VLN3) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN3, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN3 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN3 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN3 <- as.data.frame(VEC_Lit_VLN3) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN3$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN3 <- as.data.frame(VEC_Man_VLN3) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN3$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN3, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN3, class) #Check that class is numeric
  LitterCinputs_VLN3=VEC_LitDF_VLN3   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN3=VEC_ManDF_VLN3 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN3 <- merge(LitterCinputs_VLN3, ManureCinputs_VLN3, by = "MNumber")
  MCinputs_VLN3$MInput_VLN3 <- MCinputs_VLN3$VEC_Lit_VLN3 + MCinputs_VLN3$VEC_Man_VLN3

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN3)[which(names(MCinputs_VLN3) == "VEC_Lit_VLN3")] <- "LitterC_VLN3"
  colnames(MCinputs_VLN3)[which(names(MCinputs_VLN3) == "VEC_Man_VLN3")] <- "ManureC_VLN3"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN3 <- as.data.frame(Ct_VLN3)
  colnames(ModelDF_VLN3) <- c('DPM_VLN3','RPM_VLN3','BIO_VLN3', 'HUM_VLN3', 'IOM_VLN3')
  ModelDFS_VLN3 <- ModelDF_VLN3[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN3$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN3$AllPools_VLN3 <- ModelDFS_VLN3$DPM_VLN3 + ModelDFS_VLN3$RPM_VLN3 + ModelDFS_VLN3$BIO_VLN3 + ModelDFS_VLN3$HUM_VLN3 + ModelDFS_VLN3$IOM_VLN3

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN3$AllPools_noIOM_VLN3 <- ModelDFS_VLN3$DPM_VLN3 + ModelDFS_VLN3$RPM_VLN3 + ModelDFS_VLN3$BIO_VLN3 + ModelDFS_VLN3$HUM_VLN3

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN3 <- merge(ModelDFS_VLN3, MCinputs_VLN3, by = "MNumber")

  ModelDFSL_VLN3$MInput_VLN3 <- ModelDFSL_VLN3$LitterC_VLN3 + ModelDFSL_VLN3$ManureC_VLN3
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN3$CTails_VLN3 <- ModelDFSL_VLN3$AllPools_noIOM_VLN3 + ModelDFSL_VLN3$MInput_VLN3

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN3$AccumInput_VLN3 = ModelDFSL_VLN3$AccumInput_VLN3=cumsum(ModelDFSL_VLN3$MInput_VLN3)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN3$MGrowth_DPM_VLN3 <- ave(ModelDFSL_VLN3$DPM_VLN3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN3$MGrowth_RPM_VLN3 <- ave(ModelDFSL_VLN3$RPM_VLN3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN3$MGrowth_BIO_VLN3 <- ave(ModelDFSL_VLN3$BIO_VLN3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN3$MGrowth_HUM_VLN3 <- ave(ModelDFSL_VLN3$HUM_VLN3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN3$MGrowth_IOM_VLN3 <- ave(ModelDFSL_VLN3$IOM_VLN3, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN3$M_CCO2_VLN3 <- ModelDFSL_VLN3$MInput_VLN3 - ModelDFSL_VLN3$MGrowth_DPM_VLN3 - ModelDFSL_VLN3$MGrowth_RPM_VLN3 - ModelDFSL_VLN3$MGrowth_BIO_VLN3 - ModelDFSL_VLN3$MGrowth_HUM_VLN3
  ModelDFSL_VLN3$Accum_CCO2_VLN3 <- ModelDFSL_VLN3$AccumInput_VLN3 - ModelDFSL_VLN3$AllPools_noIOM_VLN3

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN3$M_CCO2_VLN3[1] <- 0
  ModelDFSL_VLN3$Accum_CCO2_VLN3[1] <- 0

  #Balance validation
  ModelDFSL_VLN3$Balance_VLN3 <- ModelDFSL_VLN3$AccumInput_VLN3 - ModelDFSL_VLN3$Accum_CCO2_VLN3 - (ModelDFSL_VLN3$DPM_VLN3 + ModelDFSL_VLN3$RPM_VLN3 + ModelDFSL_VLN3$BIO_VLN3 + ModelDFSL_VLN3$HUM_VLN3)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN3$M_CO2_VLN3 <- ModelDFSL_VLN3$M_CCO2_VLN3 * 44/12
  ModelDFSL_VLN3$Accum_CO2_VLN3 <- ModelDFSL_VLN3$Accum_CCO2_VLN3 * 44/12

  #This model will be called VLN3C because implies a continuous input of C
  ModelDFSL_VLN3C <- ModelDFSL_VLN3

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN3C,"VXC_Models\\ModelDFSL_R_VLN3C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN3 <- ModelDFSL_VLN3 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN3.1 <- rbind(c(0:0), ModelDFSLt0_VLN3)
  ModelDFSLt1_VLN3.2 <- rbind(c(0:0), ModelDFSLt1_VLN3.1)
  ModelDFSLt1_VLN3.3 <- rbind(c(0:0), ModelDFSLt1_VLN3.2)
  ModelDFSLt1_VLN3.4 <- rbind(c(0:0), ModelDFSLt1_VLN3.3)
  ModelDFSLt1_VLN3.5 <- rbind(c(0:0), ModelDFSLt1_VLN3.4)
  ModelDFSLt1_VLN3.6 <- rbind(c(0:0), ModelDFSLt1_VLN3.5)
  ModelDFSLt1_VLN3.7 <- rbind(c(0:0), ModelDFSLt1_VLN3.6)
  ModelDFSLt1_VLN3.8 <- rbind(c(0:0), ModelDFSLt1_VLN3.7)
  ModelDFSLt1_VLN3.9 <- rbind(c(0:0), ModelDFSLt1_VLN3.8)
  ModelDFSLt1_VLN3.10 <- rbind(c(0:0), ModelDFSLt1_VLN3.9)
  ModelDFSLt1_VLN3.11 <- rbind(c(0:0), ModelDFSLt1_VLN3.10)
  ModelDFSLt1_VLN3.12 <- rbind(c(0:0), ModelDFSLt1_VLN3.11)
  ModelDFSLt1_VLN3.13 <- ModelDFSLt1_VLN3.12[-nrow(ModelDFSLt1_VLN3.12),]
  ModelDFSLt1_VLN3.14 <- ModelDFSLt1_VLN3.13[-nrow(ModelDFSLt1_VLN3.13),]
  ModelDFSLt1_VLN3.15 <- ModelDFSLt1_VLN3.14[-nrow(ModelDFSLt1_VLN3.14),]
  ModelDFSLt1_VLN3.16 <- ModelDFSLt1_VLN3.15[-nrow(ModelDFSLt1_VLN3.15),]
  ModelDFSLt1_VLN3.17 <- ModelDFSLt1_VLN3.16[-nrow(ModelDFSLt1_VLN3.16),]
  ModelDFSLt1_VLN3.18 <- ModelDFSLt1_VLN3.17[-nrow(ModelDFSLt1_VLN3.17),]
  ModelDFSLt1_VLN3.19 <- ModelDFSLt1_VLN3.18[-nrow(ModelDFSLt1_VLN3.18),]
  ModelDFSLt1_VLN3.20 <- ModelDFSLt1_VLN3.19[-nrow(ModelDFSLt1_VLN3.19),]
  ModelDFSLt1_VLN3.21 <- ModelDFSLt1_VLN3.20[-nrow(ModelDFSLt1_VLN3.20),]
  ModelDFSLt1_VLN3.22 <- ModelDFSLt1_VLN3.21[-nrow(ModelDFSLt1_VLN3.21),]
  ModelDFSLt1_VLN3.23 <- ModelDFSLt1_VLN3.22[-nrow(ModelDFSLt1_VLN3.22),]
  ModelDFSLt1_VLN3.24 <- ModelDFSLt1_VLN3.23[-nrow(ModelDFSLt1_VLN3.23),]

  ModelDFSLt1_VLN3 <- ModelDFSLt1_VLN3.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN3 <- ModelDFSLt0_VLN3 - ModelDFSLt1_VLN3

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN3$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN3P because implies a one-off input of C
  ModelDFSL_VLN3P <- ModelDFSL1y_VLN3

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN3P,"VXP_Models\\ModelDFSL_R_VLN3P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN3 <- ggplot(ModelDFSL_VLN3P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN3, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN3, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN3, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN3, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN3, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN3, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN3, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN3
  ggplotly(P_CFluxI1y_VLN3)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN3P_YCO2 <- ModelDFSL_VLN3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN3P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN3")

  PA_CO21y_VLN3 <- ggplot(ModelDFSL_VLN3P_YCO2, aes(x = Year, y = AnnualCO2_VLN3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN3
  ggplotly(PA_CO21y_VLN3)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN3P_YCO2D <- merge(ModelDFSL_VLN3P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN3P_YCO2D$AnnualCO2D_VLN3 <- ModelDFSL_VLN3P_YCO2D$AnnualCO2_VLN3 * ModelDFSL_VLN3P_YCO2D$GWP100

  PA_CO21yD_VLN3 <- ggplot(ModelDFSL_VLN3P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN3
  ggplotly(PA_CO21yD_VLN3)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN3P_YC <- ModelDFSL_VLN3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN3P_YC) <- c("Year", "Months", "AnnualCTail_VLN3")

  PA_C1y_VLN3 <- ggplot(ModelDFSL_VLN3P_YC, aes(x = Year, y = AnnualCTail_VLN3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN3
  ggplotly(PA_C1y_VLN3)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN3P_YCT <- ModelDFSL_VLN3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN3)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN3P_YCT) <- c("Year", "Months", "AnnualCTail_VLN3")

  PA_CT1y_VLN3 <- ggplot(ModelDFSL_VLN3P_YCT, aes(x = Year, y = AnnualCTail_VLN3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN3
  ggplotly(PA_CT1y_VLN3)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN3P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN3P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN3P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN3P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN3P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN3P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN3P_YCT,"CTails_P\\ModelDFSL_R_C_VLN3P.xlsx") #Yearly C emissions



  #### 9.5 - VLN4) Litter; 40%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN4=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN4=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.4,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN4=data.frame(years,rep(fT_VLN4*fW_VLN4,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN4=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.4, #Percent clay in mineral soil
    xi=xi.frame_VLN4) #Loads the model

  Ct_VLN4=getC(Model_VLN4) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN4, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN4 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN4 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN4 <- as.data.frame(VEC_Lit_VLN4) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN4$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN4 <- as.data.frame(VEC_Man_VLN4) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN4$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN4, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN4, class) #Check that class is numeric
  LitterCinputs_VLN4=VEC_LitDF_VLN4   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN4=VEC_ManDF_VLN4 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN4 <- merge(LitterCinputs_VLN4, ManureCinputs_VLN4, by = "MNumber")
  MCinputs_VLN4$MInput_VLN4 <- MCinputs_VLN4$VEC_Lit_VLN4 + MCinputs_VLN4$VEC_Man_VLN4

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN4)[which(names(MCinputs_VLN4) == "VEC_Lit_VLN4")] <- "LitterC_VLN4"
  colnames(MCinputs_VLN4)[which(names(MCinputs_VLN4) == "VEC_Man_VLN4")] <- "ManureC_VLN4"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN4 <- as.data.frame(Ct_VLN4)
  colnames(ModelDF_VLN4) <- c('DPM_VLN4','RPM_VLN4','BIO_VLN4', 'HUM_VLN4', 'IOM_VLN4')
  ModelDFS_VLN4 <- ModelDF_VLN4[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN4$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN4$AllPools_VLN4 <- ModelDFS_VLN4$DPM_VLN4 + ModelDFS_VLN4$RPM_VLN4 + ModelDFS_VLN4$BIO_VLN4 + ModelDFS_VLN4$HUM_VLN4 + ModelDFS_VLN4$IOM_VLN4

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN4$AllPools_noIOM_VLN4 <- ModelDFS_VLN4$DPM_VLN4 + ModelDFS_VLN4$RPM_VLN4 + ModelDFS_VLN4$BIO_VLN4 + ModelDFS_VLN4$HUM_VLN4

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN4 <- merge(ModelDFS_VLN4, MCinputs_VLN4, by = "MNumber")

  ModelDFSL_VLN4$MInput_VLN4 <- ModelDFSL_VLN4$LitterC_VLN4 + ModelDFSL_VLN4$ManureC_VLN4
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN4$CTails_VLN4 <- ModelDFSL_VLN4$AllPools_noIOM_VLN4 + ModelDFSL_VLN4$MInput_VLN4

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN4$AccumInput_VLN4 = ModelDFSL_VLN4$AccumInput_VLN4=cumsum(ModelDFSL_VLN4$MInput_VLN4)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN4$MGrowth_DPM_VLN4 <- ave(ModelDFSL_VLN4$DPM_VLN4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN4$MGrowth_RPM_VLN4 <- ave(ModelDFSL_VLN4$RPM_VLN4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN4$MGrowth_BIO_VLN4 <- ave(ModelDFSL_VLN4$BIO_VLN4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN4$MGrowth_HUM_VLN4 <- ave(ModelDFSL_VLN4$HUM_VLN4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN4$MGrowth_IOM_VLN4 <- ave(ModelDFSL_VLN4$IOM_VLN4, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN4$M_CCO2_VLN4 <- ModelDFSL_VLN4$MInput_VLN4 - ModelDFSL_VLN4$MGrowth_DPM_VLN4 - ModelDFSL_VLN4$MGrowth_RPM_VLN4 - ModelDFSL_VLN4$MGrowth_BIO_VLN4 - ModelDFSL_VLN4$MGrowth_HUM_VLN4
  ModelDFSL_VLN4$Accum_CCO2_VLN4 <- ModelDFSL_VLN4$AccumInput_VLN4 - ModelDFSL_VLN4$AllPools_noIOM_VLN4

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN4$M_CCO2_VLN4[1] <- 0
  ModelDFSL_VLN4$Accum_CCO2_VLN4[1] <- 0

  #Balance validation
  ModelDFSL_VLN4$Balance_VLN4 <- ModelDFSL_VLN4$AccumInput_VLN4 - ModelDFSL_VLN4$Accum_CCO2_VLN4 - (ModelDFSL_VLN4$DPM_VLN4 + ModelDFSL_VLN4$RPM_VLN4 + ModelDFSL_VLN4$BIO_VLN4 + ModelDFSL_VLN4$HUM_VLN4)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN4$M_CO2_VLN4 <- ModelDFSL_VLN4$M_CCO2_VLN4 * 44/12
  ModelDFSL_VLN4$Accum_CO2_VLN4 <- ModelDFSL_VLN4$Accum_CCO2_VLN4 * 44/12

  #This model will be called VLN4C because implies a continuous input of C
  ModelDFSL_VLN4C <- ModelDFSL_VLN4

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN4C,"VXC_Models\\ModelDFSL_R_VLN4C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN4 <- ModelDFSL_VLN4 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN4.1 <- rbind(c(0:0), ModelDFSLt0_VLN4)
  ModelDFSLt1_VLN4.2 <- rbind(c(0:0), ModelDFSLt1_VLN4.1)
  ModelDFSLt1_VLN4.3 <- rbind(c(0:0), ModelDFSLt1_VLN4.2)
  ModelDFSLt1_VLN4.4 <- rbind(c(0:0), ModelDFSLt1_VLN4.3)
  ModelDFSLt1_VLN4.5 <- rbind(c(0:0), ModelDFSLt1_VLN4.4)
  ModelDFSLt1_VLN4.6 <- rbind(c(0:0), ModelDFSLt1_VLN4.5)
  ModelDFSLt1_VLN4.7 <- rbind(c(0:0), ModelDFSLt1_VLN4.6)
  ModelDFSLt1_VLN4.8 <- rbind(c(0:0), ModelDFSLt1_VLN4.7)
  ModelDFSLt1_VLN4.9 <- rbind(c(0:0), ModelDFSLt1_VLN4.8)
  ModelDFSLt1_VLN4.10 <- rbind(c(0:0), ModelDFSLt1_VLN4.9)
  ModelDFSLt1_VLN4.11 <- rbind(c(0:0), ModelDFSLt1_VLN4.10)
  ModelDFSLt1_VLN4.12 <- rbind(c(0:0), ModelDFSLt1_VLN4.11)
  ModelDFSLt1_VLN4.13 <- ModelDFSLt1_VLN4.12[-nrow(ModelDFSLt1_VLN4.12),]
  ModelDFSLt1_VLN4.14 <- ModelDFSLt1_VLN4.13[-nrow(ModelDFSLt1_VLN4.13),]
  ModelDFSLt1_VLN4.15 <- ModelDFSLt1_VLN4.14[-nrow(ModelDFSLt1_VLN4.14),]
  ModelDFSLt1_VLN4.16 <- ModelDFSLt1_VLN4.15[-nrow(ModelDFSLt1_VLN4.15),]
  ModelDFSLt1_VLN4.17 <- ModelDFSLt1_VLN4.16[-nrow(ModelDFSLt1_VLN4.16),]
  ModelDFSLt1_VLN4.18 <- ModelDFSLt1_VLN4.17[-nrow(ModelDFSLt1_VLN4.17),]
  ModelDFSLt1_VLN4.19 <- ModelDFSLt1_VLN4.18[-nrow(ModelDFSLt1_VLN4.18),]
  ModelDFSLt1_VLN4.20 <- ModelDFSLt1_VLN4.19[-nrow(ModelDFSLt1_VLN4.19),]
  ModelDFSLt1_VLN4.21 <- ModelDFSLt1_VLN4.20[-nrow(ModelDFSLt1_VLN4.20),]
  ModelDFSLt1_VLN4.22 <- ModelDFSLt1_VLN4.21[-nrow(ModelDFSLt1_VLN4.21),]
  ModelDFSLt1_VLN4.23 <- ModelDFSLt1_VLN4.22[-nrow(ModelDFSLt1_VLN4.22),]
  ModelDFSLt1_VLN4.24 <- ModelDFSLt1_VLN4.23[-nrow(ModelDFSLt1_VLN4.23),]

  ModelDFSLt1_VLN4 <- ModelDFSLt1_VLN4.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN4 <- ModelDFSLt0_VLN4 - ModelDFSLt1_VLN4

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN4$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN4P because implies a one-off input of C
  ModelDFSL_VLN4P <- ModelDFSL1y_VLN4

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN4P,"VXP_Models\\ModelDFSL_R_VLN4P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN4 <- ggplot(ModelDFSL_VLN4P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN4, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN4, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN4, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN4, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN4, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN4, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN4, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN4
  ggplotly(P_CFluxI1y_VLN4)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN4P_YCO2 <- ModelDFSL_VLN4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN4P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN4")

  PA_CO21y_VLN4 <- ggplot(ModelDFSL_VLN4P_YCO2, aes(x = Year, y = AnnualCO2_VLN4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN4
  ggplotly(PA_CO21y_VLN4)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN4P_YCO2D <- merge(ModelDFSL_VLN4P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN4P_YCO2D$AnnualCO2D_VLN4 <- ModelDFSL_VLN4P_YCO2D$AnnualCO2_VLN4 * ModelDFSL_VLN4P_YCO2D$GWP100

  PA_CO21yD_VLN4 <- ggplot(ModelDFSL_VLN4P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN4
  ggplotly(PA_CO21yD_VLN4)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN4P_YC <- ModelDFSL_VLN4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN4P_YC) <- c("Year", "Months", "AnnualCTail_VLN4")

  PA_C1y_VLN4 <- ggplot(ModelDFSL_VLN4P_YC, aes(x = Year, y = AnnualCTail_VLN4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN4
  ggplotly(PA_C1y_VLN4)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN4P_YCT <- ModelDFSL_VLN4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN4)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN4P_YCT) <- c("Year", "Months", "AnnualCTail_VLN4")

  PA_CT1y_VLN4 <- ggplot(ModelDFSL_VLN4P_YCT, aes(x = Year, y = AnnualCTail_VLN4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN4
  ggplotly(PA_CT1y_VLN4)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN4P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN4P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN4P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN4P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN4P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN4P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN4P_YCT,"CTails_P\\ModelDFSL_R_C_VLN4P.xlsx") #Yearly C emissions



  #### 9.6 - VLN5) Litter; 50%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN5=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN5=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.5,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN5=data.frame(years,rep(fT_VLN5*fW_VLN5,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN5=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.5, #Percent clay in mineral soil
    xi=xi.frame_VLN5) #Loads the model

  Ct_VLN5=getC(Model_VLN5) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN5, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN5 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN5 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN5 <- as.data.frame(VEC_Lit_VLN5) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN5$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN5 <- as.data.frame(VEC_Man_VLN5) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN5$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN5, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN5, class) #Check that class is numeric
  LitterCinputs_VLN5=VEC_LitDF_VLN5   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN5=VEC_ManDF_VLN5 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN5 <- merge(LitterCinputs_VLN5, ManureCinputs_VLN5, by = "MNumber")
  MCinputs_VLN5$MInput_VLN5 <- MCinputs_VLN5$VEC_Lit_VLN5 + MCinputs_VLN5$VEC_Man_VLN5

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN5)[which(names(MCinputs_VLN5) == "VEC_Lit_VLN5")] <- "LitterC_VLN5"
  colnames(MCinputs_VLN5)[which(names(MCinputs_VLN5) == "VEC_Man_VLN5")] <- "ManureC_VLN5"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN5 <- as.data.frame(Ct_VLN5)
  colnames(ModelDF_VLN5) <- c('DPM_VLN5','RPM_VLN5','BIO_VLN5', 'HUM_VLN5', 'IOM_VLN5')
  ModelDFS_VLN5 <- ModelDF_VLN5[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN5$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN5$AllPools_VLN5 <- ModelDFS_VLN5$DPM_VLN5 + ModelDFS_VLN5$RPM_VLN5 + ModelDFS_VLN5$BIO_VLN5 + ModelDFS_VLN5$HUM_VLN5 + ModelDFS_VLN5$IOM_VLN5

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN5$AllPools_noIOM_VLN5 <- ModelDFS_VLN5$DPM_VLN5 + ModelDFS_VLN5$RPM_VLN5 + ModelDFS_VLN5$BIO_VLN5 + ModelDFS_VLN5$HUM_VLN5

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN5 <- merge(ModelDFS_VLN5, MCinputs_VLN5, by = "MNumber")

  ModelDFSL_VLN5$MInput_VLN5 <- ModelDFSL_VLN5$LitterC_VLN5 + ModelDFSL_VLN5$ManureC_VLN5
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN5$CTails_VLN5 <- ModelDFSL_VLN5$AllPools_noIOM_VLN5 + ModelDFSL_VLN5$MInput_VLN5

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN5$AccumInput_VLN5 = ModelDFSL_VLN5$AccumInput_VLN5=cumsum(ModelDFSL_VLN5$MInput_VLN5)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN5$MGrowth_DPM_VLN5 <- ave(ModelDFSL_VLN5$DPM_VLN5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN5$MGrowth_RPM_VLN5 <- ave(ModelDFSL_VLN5$RPM_VLN5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN5$MGrowth_BIO_VLN5 <- ave(ModelDFSL_VLN5$BIO_VLN5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN5$MGrowth_HUM_VLN5 <- ave(ModelDFSL_VLN5$HUM_VLN5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN5$MGrowth_IOM_VLN5 <- ave(ModelDFSL_VLN5$IOM_VLN5, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN5$M_CCO2_VLN5 <- ModelDFSL_VLN5$MInput_VLN5 - ModelDFSL_VLN5$MGrowth_DPM_VLN5 - ModelDFSL_VLN5$MGrowth_RPM_VLN5 - ModelDFSL_VLN5$MGrowth_BIO_VLN5 - ModelDFSL_VLN5$MGrowth_HUM_VLN5
  ModelDFSL_VLN5$Accum_CCO2_VLN5 <- ModelDFSL_VLN5$AccumInput_VLN5 - ModelDFSL_VLN5$AllPools_noIOM_VLN5

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN5$M_CCO2_VLN5[1] <- 0
  ModelDFSL_VLN5$Accum_CCO2_VLN5[1] <- 0

  #Balance validation
  ModelDFSL_VLN5$Balance_VLN5 <- ModelDFSL_VLN5$AccumInput_VLN5 - ModelDFSL_VLN5$Accum_CCO2_VLN5 - (ModelDFSL_VLN5$DPM_VLN5 + ModelDFSL_VLN5$RPM_VLN5 + ModelDFSL_VLN5$BIO_VLN5 + ModelDFSL_VLN5$HUM_VLN5)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN5$M_CO2_VLN5 <- ModelDFSL_VLN5$M_CCO2_VLN5 * 44/12
  ModelDFSL_VLN5$Accum_CO2_VLN5 <- ModelDFSL_VLN5$Accum_CCO2_VLN5 * 44/12

  #This model will be called VLN5C because implies a continuous input of C
  ModelDFSL_VLN5C <- ModelDFSL_VLN5

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN5C,"VXC_Models\\ModelDFSL_R_VLN5C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN5 <- ModelDFSL_VLN5 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN5.1 <- rbind(c(0:0), ModelDFSLt0_VLN5)
  ModelDFSLt1_VLN5.2 <- rbind(c(0:0), ModelDFSLt1_VLN5.1)
  ModelDFSLt1_VLN5.3 <- rbind(c(0:0), ModelDFSLt1_VLN5.2)
  ModelDFSLt1_VLN5.4 <- rbind(c(0:0), ModelDFSLt1_VLN5.3)
  ModelDFSLt1_VLN5.5 <- rbind(c(0:0), ModelDFSLt1_VLN5.4)
  ModelDFSLt1_VLN5.6 <- rbind(c(0:0), ModelDFSLt1_VLN5.5)
  ModelDFSLt1_VLN5.7 <- rbind(c(0:0), ModelDFSLt1_VLN5.6)
  ModelDFSLt1_VLN5.8 <- rbind(c(0:0), ModelDFSLt1_VLN5.7)
  ModelDFSLt1_VLN5.9 <- rbind(c(0:0), ModelDFSLt1_VLN5.8)
  ModelDFSLt1_VLN5.10 <- rbind(c(0:0), ModelDFSLt1_VLN5.9)
  ModelDFSLt1_VLN5.11 <- rbind(c(0:0), ModelDFSLt1_VLN5.10)
  ModelDFSLt1_VLN5.12 <- rbind(c(0:0), ModelDFSLt1_VLN5.11)
  ModelDFSLt1_VLN5.13 <- ModelDFSLt1_VLN5.12[-nrow(ModelDFSLt1_VLN5.12),]
  ModelDFSLt1_VLN5.14 <- ModelDFSLt1_VLN5.13[-nrow(ModelDFSLt1_VLN5.13),]
  ModelDFSLt1_VLN5.15 <- ModelDFSLt1_VLN5.14[-nrow(ModelDFSLt1_VLN5.14),]
  ModelDFSLt1_VLN5.16 <- ModelDFSLt1_VLN5.15[-nrow(ModelDFSLt1_VLN5.15),]
  ModelDFSLt1_VLN5.17 <- ModelDFSLt1_VLN5.16[-nrow(ModelDFSLt1_VLN5.16),]
  ModelDFSLt1_VLN5.18 <- ModelDFSLt1_VLN5.17[-nrow(ModelDFSLt1_VLN5.17),]
  ModelDFSLt1_VLN5.19 <- ModelDFSLt1_VLN5.18[-nrow(ModelDFSLt1_VLN5.18),]
  ModelDFSLt1_VLN5.20 <- ModelDFSLt1_VLN5.19[-nrow(ModelDFSLt1_VLN5.19),]
  ModelDFSLt1_VLN5.21 <- ModelDFSLt1_VLN5.20[-nrow(ModelDFSLt1_VLN5.20),]
  ModelDFSLt1_VLN5.22 <- ModelDFSLt1_VLN5.21[-nrow(ModelDFSLt1_VLN5.21),]
  ModelDFSLt1_VLN5.23 <- ModelDFSLt1_VLN5.22[-nrow(ModelDFSLt1_VLN5.22),]
  ModelDFSLt1_VLN5.24 <- ModelDFSLt1_VLN5.23[-nrow(ModelDFSLt1_VLN5.23),]

  ModelDFSLt1_VLN5 <- ModelDFSLt1_VLN5.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN5 <- ModelDFSLt0_VLN5 - ModelDFSLt1_VLN5

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN5$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN5P because implies a one-off input of C
  ModelDFSL_VLN5P <- ModelDFSL1y_VLN5

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN5P,"VXP_Models\\ModelDFSL_R_VLN5P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN5 <- ggplot(ModelDFSL_VLN5P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN5, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN5, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN5, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN5, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN5, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN5, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN5, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN5
  ggplotly(P_CFluxI1y_VLN5)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN5P_YCO2 <- ModelDFSL_VLN5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN5P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN5")

  PA_CO21y_VLN5 <- ggplot(ModelDFSL_VLN5P_YCO2, aes(x = Year, y = AnnualCO2_VLN5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN5
  ggplotly(PA_CO21y_VLN5)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN5P_YCO2D <- merge(ModelDFSL_VLN5P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN5P_YCO2D$AnnualCO2D_VLN5 <- ModelDFSL_VLN5P_YCO2D$AnnualCO2_VLN5 * ModelDFSL_VLN5P_YCO2D$GWP100

  PA_CO21yD_VLN5 <- ggplot(ModelDFSL_VLN5P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN5
  ggplotly(PA_CO21yD_VLN5)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN5P_YC <- ModelDFSL_VLN5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN5P_YC) <- c("Year", "Months", "AnnualCTail_VLN5")

  PA_C1y_VLN5 <- ggplot(ModelDFSL_VLN5P_YC, aes(x = Year, y = AnnualCTail_VLN5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN5
  ggplotly(PA_C1y_VLN5)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN5P_YCT <- ModelDFSL_VLN5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN5)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN5P_YCT) <- c("Year", "Months", "AnnualCTail_VLN5")

  PA_CT1y_VLN5 <- ggplot(ModelDFSL_VLN5P_YCT, aes(x = Year, y = AnnualCTail_VLN5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN5
  ggplotly(PA_CT1y_VLN5)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN5P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN5P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN5P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN5P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN5P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN5P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN5P_YCT,"CTails_P\\ModelDFSL_R_C_VLN5P.xlsx") #Yearly C emissions



  #### 9.7 - VLN6) Litter; 60%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN6=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN6=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.6,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN6=data.frame(years,rep(fT_VLN6*fW_VLN6,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN6=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.6, #Percent clay in mineral soil
    xi=xi.frame_VLN6) #Loads the model

  Ct_VLN6=getC(Model_VLN6) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN6, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN6 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN6 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN6 <- as.data.frame(VEC_Lit_VLN6) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN6$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN6 <- as.data.frame(VEC_Man_VLN6) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN6$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN6, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN6, class) #Check that class is numeric
  LitterCinputs_VLN6=VEC_LitDF_VLN6   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN6=VEC_ManDF_VLN6 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN6 <- merge(LitterCinputs_VLN6, ManureCinputs_VLN6, by = "MNumber")
  MCinputs_VLN6$MInput_VLN6 <- MCinputs_VLN6$VEC_Lit_VLN6 + MCinputs_VLN6$VEC_Man_VLN6

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN6)[which(names(MCinputs_VLN6) == "VEC_Lit_VLN6")] <- "LitterC_VLN6"
  colnames(MCinputs_VLN6)[which(names(MCinputs_VLN6) == "VEC_Man_VLN6")] <- "ManureC_VLN6"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN6 <- as.data.frame(Ct_VLN6)
  colnames(ModelDF_VLN6) <- c('DPM_VLN6','RPM_VLN6','BIO_VLN6', 'HUM_VLN6', 'IOM_VLN6')
  ModelDFS_VLN6 <- ModelDF_VLN6[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN6$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN6$AllPools_VLN6 <- ModelDFS_VLN6$DPM_VLN6 + ModelDFS_VLN6$RPM_VLN6 + ModelDFS_VLN6$BIO_VLN6 + ModelDFS_VLN6$HUM_VLN6 + ModelDFS_VLN6$IOM_VLN6

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN6$AllPools_noIOM_VLN6 <- ModelDFS_VLN6$DPM_VLN6 + ModelDFS_VLN6$RPM_VLN6 + ModelDFS_VLN6$BIO_VLN6 + ModelDFS_VLN6$HUM_VLN6

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN6 <- merge(ModelDFS_VLN6, MCinputs_VLN6, by = "MNumber")

  ModelDFSL_VLN6$MInput_VLN6 <- ModelDFSL_VLN6$LitterC_VLN6 + ModelDFSL_VLN6$ManureC_VLN6
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN6$CTails_VLN6 <- ModelDFSL_VLN6$AllPools_noIOM_VLN6 + ModelDFSL_VLN6$MInput_VLN6

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN6$AccumInput_VLN6 = ModelDFSL_VLN6$AccumInput_VLN6=cumsum(ModelDFSL_VLN6$MInput_VLN6)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN6$MGrowth_DPM_VLN6 <- ave(ModelDFSL_VLN6$DPM_VLN6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN6$MGrowth_RPM_VLN6 <- ave(ModelDFSL_VLN6$RPM_VLN6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN6$MGrowth_BIO_VLN6 <- ave(ModelDFSL_VLN6$BIO_VLN6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN6$MGrowth_HUM_VLN6 <- ave(ModelDFSL_VLN6$HUM_VLN6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN6$MGrowth_IOM_VLN6 <- ave(ModelDFSL_VLN6$IOM_VLN6, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN6$M_CCO2_VLN6 <- ModelDFSL_VLN6$MInput_VLN6 - ModelDFSL_VLN6$MGrowth_DPM_VLN6 - ModelDFSL_VLN6$MGrowth_RPM_VLN6 - ModelDFSL_VLN6$MGrowth_BIO_VLN6 - ModelDFSL_VLN6$MGrowth_HUM_VLN6
  ModelDFSL_VLN6$Accum_CCO2_VLN6 <- ModelDFSL_VLN6$AccumInput_VLN6 - ModelDFSL_VLN6$AllPools_noIOM_VLN6

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN6$M_CCO2_VLN6[1] <- 0
  ModelDFSL_VLN6$Accum_CCO2_VLN6[1] <- 0

  #Balance validation
  ModelDFSL_VLN6$Balance_VLN6 <- ModelDFSL_VLN6$AccumInput_VLN6 - ModelDFSL_VLN6$Accum_CCO2_VLN6 - (ModelDFSL_VLN6$DPM_VLN6 + ModelDFSL_VLN6$RPM_VLN6 + ModelDFSL_VLN6$BIO_VLN6 + ModelDFSL_VLN6$HUM_VLN6)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN6$M_CO2_VLN6 <- ModelDFSL_VLN6$M_CCO2_VLN6 * 44/12
  ModelDFSL_VLN6$Accum_CO2_VLN6 <- ModelDFSL_VLN6$Accum_CCO2_VLN6 * 44/12

  #This model will be called VLN6C because implies a continuous input of C
  ModelDFSL_VLN6C <- ModelDFSL_VLN6

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN6C,"VXC_Models\\ModelDFSL_R_VLN6C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN6 <- ModelDFSL_VLN6 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN6.1 <- rbind(c(0:0), ModelDFSLt0_VLN6)
  ModelDFSLt1_VLN6.2 <- rbind(c(0:0), ModelDFSLt1_VLN6.1)
  ModelDFSLt1_VLN6.3 <- rbind(c(0:0), ModelDFSLt1_VLN6.2)
  ModelDFSLt1_VLN6.4 <- rbind(c(0:0), ModelDFSLt1_VLN6.3)
  ModelDFSLt1_VLN6.5 <- rbind(c(0:0), ModelDFSLt1_VLN6.4)
  ModelDFSLt1_VLN6.6 <- rbind(c(0:0), ModelDFSLt1_VLN6.5)
  ModelDFSLt1_VLN6.7 <- rbind(c(0:0), ModelDFSLt1_VLN6.6)
  ModelDFSLt1_VLN6.8 <- rbind(c(0:0), ModelDFSLt1_VLN6.7)
  ModelDFSLt1_VLN6.9 <- rbind(c(0:0), ModelDFSLt1_VLN6.8)
  ModelDFSLt1_VLN6.10 <- rbind(c(0:0), ModelDFSLt1_VLN6.9)
  ModelDFSLt1_VLN6.11 <- rbind(c(0:0), ModelDFSLt1_VLN6.10)
  ModelDFSLt1_VLN6.12 <- rbind(c(0:0), ModelDFSLt1_VLN6.11)
  ModelDFSLt1_VLN6.13 <- ModelDFSLt1_VLN6.12[-nrow(ModelDFSLt1_VLN6.12),]
  ModelDFSLt1_VLN6.14 <- ModelDFSLt1_VLN6.13[-nrow(ModelDFSLt1_VLN6.13),]
  ModelDFSLt1_VLN6.15 <- ModelDFSLt1_VLN6.14[-nrow(ModelDFSLt1_VLN6.14),]
  ModelDFSLt1_VLN6.16 <- ModelDFSLt1_VLN6.15[-nrow(ModelDFSLt1_VLN6.15),]
  ModelDFSLt1_VLN6.17 <- ModelDFSLt1_VLN6.16[-nrow(ModelDFSLt1_VLN6.16),]
  ModelDFSLt1_VLN6.18 <- ModelDFSLt1_VLN6.17[-nrow(ModelDFSLt1_VLN6.17),]
  ModelDFSLt1_VLN6.19 <- ModelDFSLt1_VLN6.18[-nrow(ModelDFSLt1_VLN6.18),]
  ModelDFSLt1_VLN6.20 <- ModelDFSLt1_VLN6.19[-nrow(ModelDFSLt1_VLN6.19),]
  ModelDFSLt1_VLN6.21 <- ModelDFSLt1_VLN6.20[-nrow(ModelDFSLt1_VLN6.20),]
  ModelDFSLt1_VLN6.22 <- ModelDFSLt1_VLN6.21[-nrow(ModelDFSLt1_VLN6.21),]
  ModelDFSLt1_VLN6.23 <- ModelDFSLt1_VLN6.22[-nrow(ModelDFSLt1_VLN6.22),]
  ModelDFSLt1_VLN6.24 <- ModelDFSLt1_VLN6.23[-nrow(ModelDFSLt1_VLN6.23),]

  ModelDFSLt1_VLN6 <- ModelDFSLt1_VLN6.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN6 <- ModelDFSLt0_VLN6 - ModelDFSLt1_VLN6

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN6$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN6P because implies a one-off input of C
  ModelDFSL_VLN6P <- ModelDFSL1y_VLN6

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN6P,"VXP_Models\\ModelDFSL_R_VLN6P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN6 <- ggplot(ModelDFSL_VLN6P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN6, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN6, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN6, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN6, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN6, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN6, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN6, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN6
  ggplotly(P_CFluxI1y_VLN6)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN6P_YCO2 <- ModelDFSL_VLN6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN6P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN6")

  PA_CO21y_VLN6 <- ggplot(ModelDFSL_VLN6P_YCO2, aes(x = Year, y = AnnualCO2_VLN6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN6
  ggplotly(PA_CO21y_VLN6)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN6P_YCO2D <- merge(ModelDFSL_VLN6P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN6P_YCO2D$AnnualCO2D_VLN6 <- ModelDFSL_VLN6P_YCO2D$AnnualCO2_VLN6 * ModelDFSL_VLN6P_YCO2D$GWP100

  PA_CO21yD_VLN6 <- ggplot(ModelDFSL_VLN6P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN6
  ggplotly(PA_CO21yD_VLN6)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN6P_YC <- ModelDFSL_VLN6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN6P_YC) <- c("Year", "Months", "AnnualCTail_VLN6")

  PA_C1y_VLN6 <- ggplot(ModelDFSL_VLN6P_YC, aes(x = Year, y = AnnualCTail_VLN6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN6
  ggplotly(PA_C1y_VLN6)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN6P_YCT <- ModelDFSL_VLN6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN6)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN6P_YCT) <- c("Year", "Months", "AnnualCTail_VLN6")

  PA_CT1y_VLN6 <- ggplot(ModelDFSL_VLN6P_YCT, aes(x = Year, y = AnnualCTail_VLN6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN6
  ggplotly(PA_CT1y_VLN6)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN6P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN6P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN6P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN6P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN6P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN6P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN6P_YCT,"CTails_P\\ModelDFSL_R_C_VLN6P.xlsx") #Yearly C emissions



  #### 9.8 - VLN7) Litter; 70%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN7=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN7=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.7,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN7=data.frame(years,rep(fT_VLN7*fW_VLN7,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN7=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.7, #Percent clay in mineral soil
    xi=xi.frame_VLN7) #Loads the model

  Ct_VLN7=getC(Model_VLN7) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN7, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN7 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN7 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN7 <- as.data.frame(VEC_Lit_VLN7) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN7$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN7 <- as.data.frame(VEC_Man_VLN7) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN7$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN7, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN7, class) #Check that class is numeric
  LitterCinputs_VLN7=VEC_LitDF_VLN7   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN7=VEC_ManDF_VLN7 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN7 <- merge(LitterCinputs_VLN7, ManureCinputs_VLN7, by = "MNumber")
  MCinputs_VLN7$MInput_VLN7 <- MCinputs_VLN7$VEC_Lit_VLN7 + MCinputs_VLN7$VEC_Man_VLN7

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN7)[which(names(MCinputs_VLN7) == "VEC_Lit_VLN7")] <- "LitterC_VLN7"
  colnames(MCinputs_VLN7)[which(names(MCinputs_VLN7) == "VEC_Man_VLN7")] <- "ManureC_VLN7"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN7 <- as.data.frame(Ct_VLN7)
  colnames(ModelDF_VLN7) <- c('DPM_VLN7','RPM_VLN7','BIO_VLN7', 'HUM_VLN7', 'IOM_VLN7')
  ModelDFS_VLN7 <- ModelDF_VLN7[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN7$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN7$AllPools_VLN7 <- ModelDFS_VLN7$DPM_VLN7 + ModelDFS_VLN7$RPM_VLN7 + ModelDFS_VLN7$BIO_VLN7 + ModelDFS_VLN7$HUM_VLN7 + ModelDFS_VLN7$IOM_VLN7

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN7$AllPools_noIOM_VLN7 <- ModelDFS_VLN7$DPM_VLN7 + ModelDFS_VLN7$RPM_VLN7 + ModelDFS_VLN7$BIO_VLN7 + ModelDFS_VLN7$HUM_VLN7

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN7 <- merge(ModelDFS_VLN7, MCinputs_VLN7, by = "MNumber")

  ModelDFSL_VLN7$MInput_VLN7 <- ModelDFSL_VLN7$LitterC_VLN7 + ModelDFSL_VLN7$ManureC_VLN7
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN7$CTails_VLN7 <- ModelDFSL_VLN7$AllPools_noIOM_VLN7 + ModelDFSL_VLN7$MInput_VLN7

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN7$AccumInput_VLN7 = ModelDFSL_VLN7$AccumInput_VLN7=cumsum(ModelDFSL_VLN7$MInput_VLN7)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN7$MGrowth_DPM_VLN7 <- ave(ModelDFSL_VLN7$DPM_VLN7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN7$MGrowth_RPM_VLN7 <- ave(ModelDFSL_VLN7$RPM_VLN7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN7$MGrowth_BIO_VLN7 <- ave(ModelDFSL_VLN7$BIO_VLN7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN7$MGrowth_HUM_VLN7 <- ave(ModelDFSL_VLN7$HUM_VLN7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN7$MGrowth_IOM_VLN7 <- ave(ModelDFSL_VLN7$IOM_VLN7, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN7$M_CCO2_VLN7 <- ModelDFSL_VLN7$MInput_VLN7 - ModelDFSL_VLN7$MGrowth_DPM_VLN7 - ModelDFSL_VLN7$MGrowth_RPM_VLN7 - ModelDFSL_VLN7$MGrowth_BIO_VLN7 - ModelDFSL_VLN7$MGrowth_HUM_VLN7
  ModelDFSL_VLN7$Accum_CCO2_VLN7 <- ModelDFSL_VLN7$AccumInput_VLN7 - ModelDFSL_VLN7$AllPools_noIOM_VLN7

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN7$M_CCO2_VLN7[1] <- 0
  ModelDFSL_VLN7$Accum_CCO2_VLN7[1] <- 0

  #Balance validation
  ModelDFSL_VLN7$Balance_VLN7 <- ModelDFSL_VLN7$AccumInput_VLN7 - ModelDFSL_VLN7$Accum_CCO2_VLN7 - (ModelDFSL_VLN7$DPM_VLN7 + ModelDFSL_VLN7$RPM_VLN7 + ModelDFSL_VLN7$BIO_VLN7 + ModelDFSL_VLN7$HUM_VLN7)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN7$M_CO2_VLN7 <- ModelDFSL_VLN7$M_CCO2_VLN7 * 44/12
  ModelDFSL_VLN7$Accum_CO2_VLN7 <- ModelDFSL_VLN7$Accum_CCO2_VLN7 * 44/12

  #This model will be called VLN7C because implies a continuous input of C
  ModelDFSL_VLN7C <- ModelDFSL_VLN7

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN7C,"VXC_Models\\ModelDFSL_R_VLN7C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN7 <- ModelDFSL_VLN7 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN7.1 <- rbind(c(0:0), ModelDFSLt0_VLN7)
  ModelDFSLt1_VLN7.2 <- rbind(c(0:0), ModelDFSLt1_VLN7.1)
  ModelDFSLt1_VLN7.3 <- rbind(c(0:0), ModelDFSLt1_VLN7.2)
  ModelDFSLt1_VLN7.4 <- rbind(c(0:0), ModelDFSLt1_VLN7.3)
  ModelDFSLt1_VLN7.5 <- rbind(c(0:0), ModelDFSLt1_VLN7.4)
  ModelDFSLt1_VLN7.6 <- rbind(c(0:0), ModelDFSLt1_VLN7.5)
  ModelDFSLt1_VLN7.7 <- rbind(c(0:0), ModelDFSLt1_VLN7.6)
  ModelDFSLt1_VLN7.8 <- rbind(c(0:0), ModelDFSLt1_VLN7.7)
  ModelDFSLt1_VLN7.9 <- rbind(c(0:0), ModelDFSLt1_VLN7.8)
  ModelDFSLt1_VLN7.10 <- rbind(c(0:0), ModelDFSLt1_VLN7.9)
  ModelDFSLt1_VLN7.11 <- rbind(c(0:0), ModelDFSLt1_VLN7.10)
  ModelDFSLt1_VLN7.12 <- rbind(c(0:0), ModelDFSLt1_VLN7.11)
  ModelDFSLt1_VLN7.13 <- ModelDFSLt1_VLN7.12[-nrow(ModelDFSLt1_VLN7.12),]
  ModelDFSLt1_VLN7.14 <- ModelDFSLt1_VLN7.13[-nrow(ModelDFSLt1_VLN7.13),]
  ModelDFSLt1_VLN7.15 <- ModelDFSLt1_VLN7.14[-nrow(ModelDFSLt1_VLN7.14),]
  ModelDFSLt1_VLN7.16 <- ModelDFSLt1_VLN7.15[-nrow(ModelDFSLt1_VLN7.15),]
  ModelDFSLt1_VLN7.17 <- ModelDFSLt1_VLN7.16[-nrow(ModelDFSLt1_VLN7.16),]
  ModelDFSLt1_VLN7.18 <- ModelDFSLt1_VLN7.17[-nrow(ModelDFSLt1_VLN7.17),]
  ModelDFSLt1_VLN7.19 <- ModelDFSLt1_VLN7.18[-nrow(ModelDFSLt1_VLN7.18),]
  ModelDFSLt1_VLN7.20 <- ModelDFSLt1_VLN7.19[-nrow(ModelDFSLt1_VLN7.19),]
  ModelDFSLt1_VLN7.21 <- ModelDFSLt1_VLN7.20[-nrow(ModelDFSLt1_VLN7.20),]
  ModelDFSLt1_VLN7.22 <- ModelDFSLt1_VLN7.21[-nrow(ModelDFSLt1_VLN7.21),]
  ModelDFSLt1_VLN7.23 <- ModelDFSLt1_VLN7.22[-nrow(ModelDFSLt1_VLN7.22),]
  ModelDFSLt1_VLN7.24 <- ModelDFSLt1_VLN7.23[-nrow(ModelDFSLt1_VLN7.23),]

  ModelDFSLt1_VLN7 <- ModelDFSLt1_VLN7.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN7 <- ModelDFSLt0_VLN7 - ModelDFSLt1_VLN7

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN7$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN7P because implies a one-off input of C
  ModelDFSL_VLN7P <- ModelDFSL1y_VLN7

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN7P,"VXP_Models\\ModelDFSL_R_VLN7P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN7 <- ggplot(ModelDFSL_VLN7P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN7, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN7, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN7, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN7, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN7, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN7, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN7, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN7
  ggplotly(P_CFluxI1y_VLN7)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN7P_YCO2 <- ModelDFSL_VLN7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN7P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN7")

  PA_CO21y_VLN7 <- ggplot(ModelDFSL_VLN7P_YCO2, aes(x = Year, y = AnnualCO2_VLN7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN7
  ggplotly(PA_CO21y_VLN7)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN7P_YCO2D <- merge(ModelDFSL_VLN7P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN7P_YCO2D$AnnualCO2D_VLN7 <- ModelDFSL_VLN7P_YCO2D$AnnualCO2_VLN7 * ModelDFSL_VLN7P_YCO2D$GWP100

  PA_CO21yD_VLN7 <- ggplot(ModelDFSL_VLN7P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN7
  ggplotly(PA_CO21yD_VLN7)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN7P_YC <- ModelDFSL_VLN7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN7P_YC) <- c("Year", "Months", "AnnualCTail_VLN7")

  PA_C1y_VLN7 <- ggplot(ModelDFSL_VLN7P_YC, aes(x = Year, y = AnnualCTail_VLN7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN7
  ggplotly(PA_C1y_VLN7)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN7P_YCT <- ModelDFSL_VLN7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN7)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN7P_YCT) <- c("Year", "Months", "AnnualCTail_VLN7")

  PA_CT1y_VLN7 <- ggplot(ModelDFSL_VLN7P_YCT, aes(x = Year, y = AnnualCTail_VLN7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN7
  ggplotly(PA_CT1y_VLN7)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN7P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN7P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN7P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN7P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN7P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN7P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN7P_YCT,"CTails_P\\ModelDFSL_R_C_VLN7P.xlsx") #Yearly C emissions



  #### 9.9 - VLN8) Litter; 80%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN8=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN8=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.8,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN8=data.frame(years,rep(fT_VLN8*fW_VLN8,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN8=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.8, #Percent clay in mineral soil
    xi=xi.frame_VLN8) #Loads the model

  Ct_VLN8=getC(Model_VLN8) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN8, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN8 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN8 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN8 <- as.data.frame(VEC_Lit_VLN8) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN8$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN8 <- as.data.frame(VEC_Man_VLN8) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN8$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN8, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN8, class) #Check that class is numeric
  LitterCinputs_VLN8=VEC_LitDF_VLN8   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN8=VEC_ManDF_VLN8 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN8 <- merge(LitterCinputs_VLN8, ManureCinputs_VLN8, by = "MNumber")
  MCinputs_VLN8$MInput_VLN8 <- MCinputs_VLN8$VEC_Lit_VLN8 + MCinputs_VLN8$VEC_Man_VLN8

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN8)[which(names(MCinputs_VLN8) == "VEC_Lit_VLN8")] <- "LitterC_VLN8"
  colnames(MCinputs_VLN8)[which(names(MCinputs_VLN8) == "VEC_Man_VLN8")] <- "ManureC_VLN8"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN8 <- as.data.frame(Ct_VLN8)
  colnames(ModelDF_VLN8) <- c('DPM_VLN8','RPM_VLN8','BIO_VLN8', 'HUM_VLN8', 'IOM_VLN8')
  ModelDFS_VLN8 <- ModelDF_VLN8[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN8$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN8$AllPools_VLN8 <- ModelDFS_VLN8$DPM_VLN8 + ModelDFS_VLN8$RPM_VLN8 + ModelDFS_VLN8$BIO_VLN8 + ModelDFS_VLN8$HUM_VLN8 + ModelDFS_VLN8$IOM_VLN8

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN8$AllPools_noIOM_VLN8 <- ModelDFS_VLN8$DPM_VLN8 + ModelDFS_VLN8$RPM_VLN8 + ModelDFS_VLN8$BIO_VLN8 + ModelDFS_VLN8$HUM_VLN8

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN8 <- merge(ModelDFS_VLN8, MCinputs_VLN8, by = "MNumber")

  ModelDFSL_VLN8$MInput_VLN8 <- ModelDFSL_VLN8$LitterC_VLN8 + ModelDFSL_VLN8$ManureC_VLN8
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN8$CTails_VLN8 <- ModelDFSL_VLN8$AllPools_noIOM_VLN8 + ModelDFSL_VLN8$MInput_VLN8

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN8$AccumInput_VLN8 = ModelDFSL_VLN8$AccumInput_VLN8=cumsum(ModelDFSL_VLN8$MInput_VLN8)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN8$MGrowth_DPM_VLN8 <- ave(ModelDFSL_VLN8$DPM_VLN8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN8$MGrowth_RPM_VLN8 <- ave(ModelDFSL_VLN8$RPM_VLN8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN8$MGrowth_BIO_VLN8 <- ave(ModelDFSL_VLN8$BIO_VLN8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN8$MGrowth_HUM_VLN8 <- ave(ModelDFSL_VLN8$HUM_VLN8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN8$MGrowth_IOM_VLN8 <- ave(ModelDFSL_VLN8$IOM_VLN8, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN8$M_CCO2_VLN8 <- ModelDFSL_VLN8$MInput_VLN8 - ModelDFSL_VLN8$MGrowth_DPM_VLN8 - ModelDFSL_VLN8$MGrowth_RPM_VLN8 - ModelDFSL_VLN8$MGrowth_BIO_VLN8 - ModelDFSL_VLN8$MGrowth_HUM_VLN8
  ModelDFSL_VLN8$Accum_CCO2_VLN8 <- ModelDFSL_VLN8$AccumInput_VLN8 - ModelDFSL_VLN8$AllPools_noIOM_VLN8

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN8$M_CCO2_VLN8[1] <- 0
  ModelDFSL_VLN8$Accum_CCO2_VLN8[1] <- 0

  #Balance validation
  ModelDFSL_VLN8$Balance_VLN8 <- ModelDFSL_VLN8$AccumInput_VLN8 - ModelDFSL_VLN8$Accum_CCO2_VLN8 - (ModelDFSL_VLN8$DPM_VLN8 + ModelDFSL_VLN8$RPM_VLN8 + ModelDFSL_VLN8$BIO_VLN8 + ModelDFSL_VLN8$HUM_VLN8)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN8$M_CO2_VLN8 <- ModelDFSL_VLN8$M_CCO2_VLN8 * 44/12
  ModelDFSL_VLN8$Accum_CO2_VLN8 <- ModelDFSL_VLN8$Accum_CCO2_VLN8 * 44/12

  #This model will be called VLN8C because implies a continuous input of C
  ModelDFSL_VLN8C <- ModelDFSL_VLN8

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN8C,"VXC_Models\\ModelDFSL_R_VLN8C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN8 <- ModelDFSL_VLN8 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN8.1 <- rbind(c(0:0), ModelDFSLt0_VLN8)
  ModelDFSLt1_VLN8.2 <- rbind(c(0:0), ModelDFSLt1_VLN8.1)
  ModelDFSLt1_VLN8.3 <- rbind(c(0:0), ModelDFSLt1_VLN8.2)
  ModelDFSLt1_VLN8.4 <- rbind(c(0:0), ModelDFSLt1_VLN8.3)
  ModelDFSLt1_VLN8.5 <- rbind(c(0:0), ModelDFSLt1_VLN8.4)
  ModelDFSLt1_VLN8.6 <- rbind(c(0:0), ModelDFSLt1_VLN8.5)
  ModelDFSLt1_VLN8.7 <- rbind(c(0:0), ModelDFSLt1_VLN8.6)
  ModelDFSLt1_VLN8.8 <- rbind(c(0:0), ModelDFSLt1_VLN8.7)
  ModelDFSLt1_VLN8.9 <- rbind(c(0:0), ModelDFSLt1_VLN8.8)
  ModelDFSLt1_VLN8.10 <- rbind(c(0:0), ModelDFSLt1_VLN8.9)
  ModelDFSLt1_VLN8.11 <- rbind(c(0:0), ModelDFSLt1_VLN8.10)
  ModelDFSLt1_VLN8.12 <- rbind(c(0:0), ModelDFSLt1_VLN8.11)
  ModelDFSLt1_VLN8.13 <- ModelDFSLt1_VLN8.12[-nrow(ModelDFSLt1_VLN8.12),]
  ModelDFSLt1_VLN8.14 <- ModelDFSLt1_VLN8.13[-nrow(ModelDFSLt1_VLN8.13),]
  ModelDFSLt1_VLN8.15 <- ModelDFSLt1_VLN8.14[-nrow(ModelDFSLt1_VLN8.14),]
  ModelDFSLt1_VLN8.16 <- ModelDFSLt1_VLN8.15[-nrow(ModelDFSLt1_VLN8.15),]
  ModelDFSLt1_VLN8.17 <- ModelDFSLt1_VLN8.16[-nrow(ModelDFSLt1_VLN8.16),]
  ModelDFSLt1_VLN8.18 <- ModelDFSLt1_VLN8.17[-nrow(ModelDFSLt1_VLN8.17),]
  ModelDFSLt1_VLN8.19 <- ModelDFSLt1_VLN8.18[-nrow(ModelDFSLt1_VLN8.18),]
  ModelDFSLt1_VLN8.20 <- ModelDFSLt1_VLN8.19[-nrow(ModelDFSLt1_VLN8.19),]
  ModelDFSLt1_VLN8.21 <- ModelDFSLt1_VLN8.20[-nrow(ModelDFSLt1_VLN8.20),]
  ModelDFSLt1_VLN8.22 <- ModelDFSLt1_VLN8.21[-nrow(ModelDFSLt1_VLN8.21),]
  ModelDFSLt1_VLN8.23 <- ModelDFSLt1_VLN8.22[-nrow(ModelDFSLt1_VLN8.22),]
  ModelDFSLt1_VLN8.24 <- ModelDFSLt1_VLN8.23[-nrow(ModelDFSLt1_VLN8.23),]

  ModelDFSLt1_VLN8 <- ModelDFSLt1_VLN8.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN8 <- ModelDFSLt0_VLN8 - ModelDFSLt1_VLN8

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN8$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN8P because implies a one-off input of C
  ModelDFSL_VLN8P <- ModelDFSL1y_VLN8

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN8P,"VXP_Models\\ModelDFSL_R_VLN8P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN8 <- ggplot(ModelDFSL_VLN8P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN8, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN8, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN8, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN8, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN8, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN8, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN8, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN8
  ggplotly(P_CFluxI1y_VLN8)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN8P_YCO2 <- ModelDFSL_VLN8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN8P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN8")

  PA_CO21y_VLN8 <- ggplot(ModelDFSL_VLN8P_YCO2, aes(x = Year, y = AnnualCO2_VLN8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN8
  ggplotly(PA_CO21y_VLN8)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN8P_YCO2D <- merge(ModelDFSL_VLN8P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN8P_YCO2D$AnnualCO2D_VLN8 <- ModelDFSL_VLN8P_YCO2D$AnnualCO2_VLN8 * ModelDFSL_VLN8P_YCO2D$GWP100

  PA_CO21yD_VLN8 <- ggplot(ModelDFSL_VLN8P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN8
  ggplotly(PA_CO21yD_VLN8)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN8P_YC <- ModelDFSL_VLN8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN8P_YC) <- c("Year", "Months", "AnnualCTail_VLN8")

  PA_C1y_VLN8 <- ggplot(ModelDFSL_VLN8P_YC, aes(x = Year, y = AnnualCTail_VLN8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN8
  ggplotly(PA_C1y_VLN8)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN8P_YCT <- ModelDFSL_VLN8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN8)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN8P_YCT) <- c("Year", "Months", "AnnualCTail_VLN8")

  PA_CT1y_VLN8 <- ggplot(ModelDFSL_VLN8P_YCT, aes(x = Year, y = AnnualCTail_VLN8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN8
  ggplotly(PA_CT1y_VLN8)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN8P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN8P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN8P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN8P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN8P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN8P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN8P_YCT,"CTails_P\\ModelDFSL_R_C_VLN8P.xlsx") #Yearly C emissions



  #### 9.10 - VLN9) Litter; 90%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN9=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN9=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.9,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN9=data.frame(years,rep(fT_VLN9*fW_VLN9,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN9=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100*0.9, #Percent clay in mineral soil
    xi=xi.frame_VLN9) #Loads the model

  Ct_VLN9=getC(Model_VLN9) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN9, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN9 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN9 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN9 <- as.data.frame(VEC_Lit_VLN9) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN9$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN9 <- as.data.frame(VEC_Man_VLN9) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN9$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN9, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN9, class) #Check that class is numeric
  LitterCinputs_VLN9=VEC_LitDF_VLN9   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN9=VEC_ManDF_VLN9 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN9 <- merge(LitterCinputs_VLN9, ManureCinputs_VLN9, by = "MNumber")
  MCinputs_VLN9$MInput_VLN9 <- MCinputs_VLN9$VEC_Lit_VLN9 + MCinputs_VLN9$VEC_Man_VLN9

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN9)[which(names(MCinputs_VLN9) == "VEC_Lit_VLN9")] <- "LitterC_VLN9"
  colnames(MCinputs_VLN9)[which(names(MCinputs_VLN9) == "VEC_Man_VLN9")] <- "ManureC_VLN9"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN9 <- as.data.frame(Ct_VLN9)
  colnames(ModelDF_VLN9) <- c('DPM_VLN9','RPM_VLN9','BIO_VLN9', 'HUM_VLN9', 'IOM_VLN9')
  ModelDFS_VLN9 <- ModelDF_VLN9[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN9$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN9$AllPools_VLN9 <- ModelDFS_VLN9$DPM_VLN9 + ModelDFS_VLN9$RPM_VLN9 + ModelDFS_VLN9$BIO_VLN9 + ModelDFS_VLN9$HUM_VLN9 + ModelDFS_VLN9$IOM_VLN9

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN9$AllPools_noIOM_VLN9 <- ModelDFS_VLN9$DPM_VLN9 + ModelDFS_VLN9$RPM_VLN9 + ModelDFS_VLN9$BIO_VLN9 + ModelDFS_VLN9$HUM_VLN9

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN9 <- merge(ModelDFS_VLN9, MCinputs_VLN9, by = "MNumber")

  ModelDFSL_VLN9$MInput_VLN9 <- ModelDFSL_VLN9$LitterC_VLN9 + ModelDFSL_VLN9$ManureC_VLN9
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN9$CTails_VLN9 <- ModelDFSL_VLN9$AllPools_noIOM_VLN9 + ModelDFSL_VLN9$MInput_VLN9

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN9$AccumInput_VLN9 = ModelDFSL_VLN9$AccumInput_VLN9=cumsum(ModelDFSL_VLN9$MInput_VLN9)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN9$MGrowth_DPM_VLN9 <- ave(ModelDFSL_VLN9$DPM_VLN9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN9$MGrowth_RPM_VLN9 <- ave(ModelDFSL_VLN9$RPM_VLN9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN9$MGrowth_BIO_VLN9 <- ave(ModelDFSL_VLN9$BIO_VLN9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN9$MGrowth_HUM_VLN9 <- ave(ModelDFSL_VLN9$HUM_VLN9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN9$MGrowth_IOM_VLN9 <- ave(ModelDFSL_VLN9$IOM_VLN9, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN9$M_CCO2_VLN9 <- ModelDFSL_VLN9$MInput_VLN9 - ModelDFSL_VLN9$MGrowth_DPM_VLN9 - ModelDFSL_VLN9$MGrowth_RPM_VLN9 - ModelDFSL_VLN9$MGrowth_BIO_VLN9 - ModelDFSL_VLN9$MGrowth_HUM_VLN9
  ModelDFSL_VLN9$Accum_CCO2_VLN9 <- ModelDFSL_VLN9$AccumInput_VLN9 - ModelDFSL_VLN9$AllPools_noIOM_VLN9

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN9$M_CCO2_VLN9[1] <- 0
  ModelDFSL_VLN9$Accum_CCO2_VLN9[1] <- 0

  #Balance validation
  ModelDFSL_VLN9$Balance_VLN9 <- ModelDFSL_VLN9$AccumInput_VLN9 - ModelDFSL_VLN9$Accum_CCO2_VLN9 - (ModelDFSL_VLN9$DPM_VLN9 + ModelDFSL_VLN9$RPM_VLN9 + ModelDFSL_VLN9$BIO_VLN9 + ModelDFSL_VLN9$HUM_VLN9)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN9$M_CO2_VLN9 <- ModelDFSL_VLN9$M_CCO2_VLN9 * 44/12
  ModelDFSL_VLN9$Accum_CO2_VLN9 <- ModelDFSL_VLN9$Accum_CCO2_VLN9 * 44/12

  #This model will be called VLN9C because implies a continuous input of C
  ModelDFSL_VLN9C <- ModelDFSL_VLN9

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN9C,"VXC_Models\\ModelDFSL_R_VLN9C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN9 <- ModelDFSL_VLN9 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN9.1 <- rbind(c(0:0), ModelDFSLt0_VLN9)
  ModelDFSLt1_VLN9.2 <- rbind(c(0:0), ModelDFSLt1_VLN9.1)
  ModelDFSLt1_VLN9.3 <- rbind(c(0:0), ModelDFSLt1_VLN9.2)
  ModelDFSLt1_VLN9.4 <- rbind(c(0:0), ModelDFSLt1_VLN9.3)
  ModelDFSLt1_VLN9.5 <- rbind(c(0:0), ModelDFSLt1_VLN9.4)
  ModelDFSLt1_VLN9.6 <- rbind(c(0:0), ModelDFSLt1_VLN9.5)
  ModelDFSLt1_VLN9.7 <- rbind(c(0:0), ModelDFSLt1_VLN9.6)
  ModelDFSLt1_VLN9.8 <- rbind(c(0:0), ModelDFSLt1_VLN9.7)
  ModelDFSLt1_VLN9.9 <- rbind(c(0:0), ModelDFSLt1_VLN9.8)
  ModelDFSLt1_VLN9.10 <- rbind(c(0:0), ModelDFSLt1_VLN9.9)
  ModelDFSLt1_VLN9.11 <- rbind(c(0:0), ModelDFSLt1_VLN9.10)
  ModelDFSLt1_VLN9.12 <- rbind(c(0:0), ModelDFSLt1_VLN9.11)
  ModelDFSLt1_VLN9.13 <- ModelDFSLt1_VLN9.12[-nrow(ModelDFSLt1_VLN9.12),]
  ModelDFSLt1_VLN9.14 <- ModelDFSLt1_VLN9.13[-nrow(ModelDFSLt1_VLN9.13),]
  ModelDFSLt1_VLN9.15 <- ModelDFSLt1_VLN9.14[-nrow(ModelDFSLt1_VLN9.14),]
  ModelDFSLt1_VLN9.16 <- ModelDFSLt1_VLN9.15[-nrow(ModelDFSLt1_VLN9.15),]
  ModelDFSLt1_VLN9.17 <- ModelDFSLt1_VLN9.16[-nrow(ModelDFSLt1_VLN9.16),]
  ModelDFSLt1_VLN9.18 <- ModelDFSLt1_VLN9.17[-nrow(ModelDFSLt1_VLN9.17),]
  ModelDFSLt1_VLN9.19 <- ModelDFSLt1_VLN9.18[-nrow(ModelDFSLt1_VLN9.18),]
  ModelDFSLt1_VLN9.20 <- ModelDFSLt1_VLN9.19[-nrow(ModelDFSLt1_VLN9.19),]
  ModelDFSLt1_VLN9.21 <- ModelDFSLt1_VLN9.20[-nrow(ModelDFSLt1_VLN9.20),]
  ModelDFSLt1_VLN9.22 <- ModelDFSLt1_VLN9.21[-nrow(ModelDFSLt1_VLN9.21),]
  ModelDFSLt1_VLN9.23 <- ModelDFSLt1_VLN9.22[-nrow(ModelDFSLt1_VLN9.22),]
  ModelDFSLt1_VLN9.24 <- ModelDFSLt1_VLN9.23[-nrow(ModelDFSLt1_VLN9.23),]

  ModelDFSLt1_VLN9 <- ModelDFSLt1_VLN9.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN9 <- ModelDFSLt0_VLN9 - ModelDFSLt1_VLN9

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN9$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN9P because implies a one-off input of C
  ModelDFSL_VLN9P <- ModelDFSL1y_VLN9

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN9P,"VXP_Models\\ModelDFSL_R_VLN9P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN9 <- ggplot(ModelDFSL_VLN9P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN9, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN9, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN9, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN9, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN9, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN9, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN9, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN9
  ggplotly(P_CFluxI1y_VLN9)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN9P_YCO2 <- ModelDFSL_VLN9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN9P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN9")

  PA_CO21y_VLN9 <- ggplot(ModelDFSL_VLN9P_YCO2, aes(x = Year, y = AnnualCO2_VLN9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN9
  ggplotly(PA_CO21y_VLN9)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN9P_YCO2D <- merge(ModelDFSL_VLN9P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN9P_YCO2D$AnnualCO2D_VLN9 <- ModelDFSL_VLN9P_YCO2D$AnnualCO2_VLN9 * ModelDFSL_VLN9P_YCO2D$GWP100

  PA_CO21yD_VLN9 <- ggplot(ModelDFSL_VLN9P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN9
  ggplotly(PA_CO21yD_VLN9)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN9P_YC <- ModelDFSL_VLN9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN9P_YC) <- c("Year", "Months", "AnnualCTail_VLN9")

  PA_C1y_VLN9 <- ggplot(ModelDFSL_VLN9P_YC, aes(x = Year, y = AnnualCTail_VLN9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN9
  ggplotly(PA_C1y_VLN9)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN9P_YCT <- ModelDFSL_VLN9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN9)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN9P_YCT) <- c("Year", "Months", "AnnualCTail_VLN9")

  PA_CT1y_VLN9 <- ggplot(ModelDFSL_VLN9P_YCT, aes(x = Year, y = AnnualCTail_VLN9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN9
  ggplotly(PA_CT1y_VLN9)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN9P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN9P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN9P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN9P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN9P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN9P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN9P_YCT,"CTails_P\\ModelDFSL_R_C_VLN9P.xlsx") #Yearly C emissions



  #### 9.11 - VLN10) Litter; 100%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VLN10=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VLN10=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                           S.Thick = soil.thick, pClay = clay100,
                           pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VLN10=data.frame(years,rep(fT_VLN10*fW_VLN10,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VLN10=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
    clay=clay100, #Percent clay in mineral soil
    xi=xi.frame_VLN10) #Loads the model

  Ct_VLN10=getC(Model_VLN10) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VLN10, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VLN10 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VLN10 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VLN10 <- as.data.frame(VEC_Lit_VLN10) #Converting the Litter vector to a data frame
  VEC_LitDF_VLN10$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VLN10 <- as.data.frame(VEC_Man_VLN10) #Converting the Manure vector to a data frame
  VEC_ManDF_VLN10$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VLN10, class) #Check that class is numeric
  sapply(VEC_ManDF_VLN10, class) #Check that class is numeric
  LitterCinputs_VLN10=VEC_LitDF_VLN10   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VLN10=VEC_ManDF_VLN10 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VLN10 <- merge(LitterCinputs_VLN10, ManureCinputs_VLN10, by = "MNumber")
  MCinputs_VLN10$MInput_VLN10 <- MCinputs_VLN10$VEC_Lit_VLN10 + MCinputs_VLN10$VEC_Man_VLN10

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VLN10)[which(names(MCinputs_VLN10) == "VEC_Lit_VLN10")] <- "LitterC_VLN10"
  colnames(MCinputs_VLN10)[which(names(MCinputs_VLN10) == "VEC_Man_VLN10")] <- "ManureC_VLN10"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VLN10 <- as.data.frame(Ct_VLN10)
  colnames(ModelDF_VLN10) <- c('DPM_VLN10','RPM_VLN10','BIO_VLN10', 'HUM_VLN10', 'IOM_VLN10')
  ModelDFS_VLN10 <- ModelDF_VLN10[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VLN10$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VLN10$AllPools_VLN10 <- ModelDFS_VLN10$DPM_VLN10 + ModelDFS_VLN10$RPM_VLN10 + ModelDFS_VLN10$BIO_VLN10 + ModelDFS_VLN10$HUM_VLN10 + ModelDFS_VLN10$IOM_VLN10

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VLN10$AllPools_noIOM_VLN10 <- ModelDFS_VLN10$DPM_VLN10 + ModelDFS_VLN10$RPM_VLN10 + ModelDFS_VLN10$BIO_VLN10 + ModelDFS_VLN10$HUM_VLN10

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VLN10 <- merge(ModelDFS_VLN10, MCinputs_VLN10, by = "MNumber")

  ModelDFSL_VLN10$MInput_VLN10 <- ModelDFSL_VLN10$LitterC_VLN10 + ModelDFSL_VLN10$ManureC_VLN10
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VLN10$CTails_VLN10 <- ModelDFSL_VLN10$AllPools_noIOM_VLN10 + ModelDFSL_VLN10$MInput_VLN10

  #Create Monthly Accumulated input of C
  ModelDFSL_VLN10$AccumInput_VLN10 = ModelDFSL_VLN10$AccumInput_VLN10=cumsum(ModelDFSL_VLN10$MInput_VLN10)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VLN10$MGrowth_DPM_VLN10 <- ave(ModelDFSL_VLN10$DPM_VLN10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN10$MGrowth_RPM_VLN10 <- ave(ModelDFSL_VLN10$RPM_VLN10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN10$MGrowth_BIO_VLN10 <- ave(ModelDFSL_VLN10$BIO_VLN10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN10$MGrowth_HUM_VLN10 <- ave(ModelDFSL_VLN10$HUM_VLN10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VLN10$MGrowth_IOM_VLN10 <- ave(ModelDFSL_VLN10$IOM_VLN10, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VLN10$M_CCO2_VLN10 <- ModelDFSL_VLN10$MInput_VLN10 - ModelDFSL_VLN10$MGrowth_DPM_VLN10 - ModelDFSL_VLN10$MGrowth_RPM_VLN10 - ModelDFSL_VLN10$MGrowth_BIO_VLN10 - ModelDFSL_VLN10$MGrowth_HUM_VLN10
  ModelDFSL_VLN10$Accum_CCO2_VLN10 <- ModelDFSL_VLN10$AccumInput_VLN10 - ModelDFSL_VLN10$AllPools_noIOM_VLN10

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VLN10$M_CCO2_VLN10[1] <- 0
  ModelDFSL_VLN10$Accum_CCO2_VLN10[1] <- 0

  #Balance validation
  ModelDFSL_VLN10$Balance_VLN10 <- ModelDFSL_VLN10$AccumInput_VLN10 - ModelDFSL_VLN10$Accum_CCO2_VLN10 - (ModelDFSL_VLN10$DPM_VLN10 + ModelDFSL_VLN10$RPM_VLN10 + ModelDFSL_VLN10$BIO_VLN10 + ModelDFSL_VLN10$HUM_VLN10)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VLN10$M_CO2_VLN10 <- ModelDFSL_VLN10$M_CCO2_VLN10 * 44/12
  ModelDFSL_VLN10$Accum_CO2_VLN10 <- ModelDFSL_VLN10$Accum_CCO2_VLN10 * 44/12

  #This model will be called VLN10C because implies a continuous input of C
  ModelDFSL_VLN10C <- ModelDFSL_VLN10

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN10C,"VXC_Models\\ModelDFSL_R_VLN10C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VLN10 <- ModelDFSL_VLN10 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VLN10.1 <- rbind(c(0:0), ModelDFSLt0_VLN10)
  ModelDFSLt1_VLN10.2 <- rbind(c(0:0), ModelDFSLt1_VLN10.1)
  ModelDFSLt1_VLN10.3 <- rbind(c(0:0), ModelDFSLt1_VLN10.2)
  ModelDFSLt1_VLN10.4 <- rbind(c(0:0), ModelDFSLt1_VLN10.3)
  ModelDFSLt1_VLN10.5 <- rbind(c(0:0), ModelDFSLt1_VLN10.4)
  ModelDFSLt1_VLN10.6 <- rbind(c(0:0), ModelDFSLt1_VLN10.5)
  ModelDFSLt1_VLN10.7 <- rbind(c(0:0), ModelDFSLt1_VLN10.6)
  ModelDFSLt1_VLN10.8 <- rbind(c(0:0), ModelDFSLt1_VLN10.7)
  ModelDFSLt1_VLN10.9 <- rbind(c(0:0), ModelDFSLt1_VLN10.8)
  ModelDFSLt1_VLN10.10 <- rbind(c(0:0), ModelDFSLt1_VLN10.9)
  ModelDFSLt1_VLN10.11 <- rbind(c(0:0), ModelDFSLt1_VLN10.10)
  ModelDFSLt1_VLN10.12 <- rbind(c(0:0), ModelDFSLt1_VLN10.11)
  ModelDFSLt1_VLN10.13 <- ModelDFSLt1_VLN10.12[-nrow(ModelDFSLt1_VLN10.12),]
  ModelDFSLt1_VLN10.14 <- ModelDFSLt1_VLN10.13[-nrow(ModelDFSLt1_VLN10.13),]
  ModelDFSLt1_VLN10.15 <- ModelDFSLt1_VLN10.14[-nrow(ModelDFSLt1_VLN10.14),]
  ModelDFSLt1_VLN10.16 <- ModelDFSLt1_VLN10.15[-nrow(ModelDFSLt1_VLN10.15),]
  ModelDFSLt1_VLN10.17 <- ModelDFSLt1_VLN10.16[-nrow(ModelDFSLt1_VLN10.16),]
  ModelDFSLt1_VLN10.18 <- ModelDFSLt1_VLN10.17[-nrow(ModelDFSLt1_VLN10.17),]
  ModelDFSLt1_VLN10.19 <- ModelDFSLt1_VLN10.18[-nrow(ModelDFSLt1_VLN10.18),]
  ModelDFSLt1_VLN10.20 <- ModelDFSLt1_VLN10.19[-nrow(ModelDFSLt1_VLN10.19),]
  ModelDFSLt1_VLN10.21 <- ModelDFSLt1_VLN10.20[-nrow(ModelDFSLt1_VLN10.20),]
  ModelDFSLt1_VLN10.22 <- ModelDFSLt1_VLN10.21[-nrow(ModelDFSLt1_VLN10.21),]
  ModelDFSLt1_VLN10.23 <- ModelDFSLt1_VLN10.22[-nrow(ModelDFSLt1_VLN10.22),]
  ModelDFSLt1_VLN10.24 <- ModelDFSLt1_VLN10.23[-nrow(ModelDFSLt1_VLN10.23),]

  ModelDFSLt1_VLN10 <- ModelDFSLt1_VLN10.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VLN10 <- ModelDFSLt0_VLN10 - ModelDFSLt1_VLN10

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VLN10$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VLN10P because implies a one-off input of C
  ModelDFSL_VLN10P <- ModelDFSL1y_VLN10

  #Export the dataframe
  write_xlsx(ModelDFSL_VLN10P,"VXP_Models\\ModelDFSL_R_VLN10P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VLN10 <- ggplot(ModelDFSL_VLN10P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VLN10, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VLN10, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VLN10, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VLN10, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VLN10, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VLN10, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VLN10, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VLN10
  ggplotly(P_CFluxI1y_VLN10)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VLN10P_YCO2 <- ModelDFSL_VLN10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLN10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN10P_YCO2) <- c("Year", "Months", "AnnualCO2_VLN10")

  PA_CO21y_VLN10 <- ggplot(ModelDFSL_VLN10P_YCO2, aes(x = Year, y = AnnualCO2_VLN10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VLN10
  ggplotly(PA_CO21y_VLN10)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VLN10P_YCO2D <- merge(ModelDFSL_VLN10P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VLN10P_YCO2D$AnnualCO2D_VLN10 <- ModelDFSL_VLN10P_YCO2D$AnnualCO2_VLN10 * ModelDFSL_VLN10P_YCO2D$GWP100

  PA_CO21yD_VLN10 <- ggplot(ModelDFSL_VLN10P_YCO2D, aes(x = Year, y = AnnualCO2D_VLN10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VLN10
  ggplotly(PA_CO21yD_VLN10)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VLN10P_YC <- ModelDFSL_VLN10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLN10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN10P_YC) <- c("Year", "Months", "AnnualCTail_VLN10")

  PA_C1y_VLN10 <- ggplot(ModelDFSL_VLN10P_YC, aes(x = Year, y = AnnualCTail_VLN10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VLN10
  ggplotly(PA_C1y_VLN10)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VLN10P_YCT <- ModelDFSL_VLN10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLN10)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VLN10P_YCT) <- c("Year", "Months", "AnnualCTail_VLN10")

  PA_CT1y_VLN10 <- ggplot(ModelDFSL_VLN10P_YCT, aes(x = Year, y = AnnualCTail_VLN10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VLN10
  ggplotly(PA_CT1y_VLN10)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VLN10P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLN10P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VLN10P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLN10P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VLN10P_YC,"CEmissions_P\\ModelDFSL_R_C_VLN10P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VLN10P_YCT,"CTails_P\\ModelDFSL_R_C_VLN10P.xlsx") #Yearly C emissions


















}
