#' Run RothC_VMN
#'
#' The Merge_VXC function merge all VXC models created by the functions Run_RothC_[].
#'
#' @return A dataframe with all VXC models merged together
#' @import SoilR ggplot2 stringr
#' @importFrom plotly ggplotly
#' @export

#Function to Run and Create the multiple RothC Combinations
Run_RothC_VMN <- function(SL_years = 100,
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

  #### 12) Model Combinations - Manure; No Tillage, 0-100% (+10%) Clay ####
  #### 12.1 - VMN0) Manure; 0%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN0=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN0=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay0,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN0=data.frame(years,rep(fT_VMN0*fW_VMN0,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN0=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay0, #Percent clay in mineral soil
    xi=xi.frame_VMN0) #Loads the model

  Ct_VMN0=getC(Model_VMN0) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN0, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN0 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN0 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN0 <- as.data.frame(VEC_Lit_VMN0) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN0$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN0 <- as.data.frame(VEC_Man_VMN0) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN0$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN0, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN0, class) #Check that class is numeric
  LitterCinputs_VMN0=VEC_LitDF_VMN0   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN0=VEC_ManDF_VMN0 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN0 <- merge(LitterCinputs_VMN0, ManureCinputs_VMN0, by = "MNumber")
  MCinputs_VMN0$MInput_VMN0 <- MCinputs_VMN0$VEC_Lit_VMN0 + MCinputs_VMN0$VEC_Man_VMN0

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN0)[which(names(MCinputs_VMN0) == "VEC_Lit_VMN0")] <- "LitterC_VMN0"
  colnames(MCinputs_VMN0)[which(names(MCinputs_VMN0) == "VEC_Man_VMN0")] <- "ManureC_VMN0"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN0 <- as.data.frame(Ct_VMN0)
  colnames(ModelDF_VMN0) <- c('DPM_VMN0','RPM_VMN0','BIO_VMN0', 'HUM_VMN0', 'IOM_VMN0')
  ModelDFS_VMN0 <- ModelDF_VMN0[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN0$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN0$AllPools_VMN0 <- ModelDFS_VMN0$DPM_VMN0 + ModelDFS_VMN0$RPM_VMN0 + ModelDFS_VMN0$BIO_VMN0 + ModelDFS_VMN0$HUM_VMN0 + ModelDFS_VMN0$IOM_VMN0

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN0$AllPools_noIOM_VMN0 <- ModelDFS_VMN0$DPM_VMN0 + ModelDFS_VMN0$RPM_VMN0 + ModelDFS_VMN0$BIO_VMN0 + ModelDFS_VMN0$HUM_VMN0

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN0 <- merge(ModelDFS_VMN0, MCinputs_VMN0, by = "MNumber")

  ModelDFSL_VMN0$MInput_VMN0 <- ModelDFSL_VMN0$LitterC_VMN0 + ModelDFSL_VMN0$ManureC_VMN0
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN0$CTails_VMN0 <- ModelDFSL_VMN0$AllPools_noIOM_VMN0 + ModelDFSL_VMN0$MInput_VMN0

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN0$AccumInput_VMN0 = ModelDFSL_VMN0$AccumInput_VMN0=cumsum(ModelDFSL_VMN0$MInput_VMN0)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN0$MGrowth_DPM_VMN0 <- ave(ModelDFSL_VMN0$DPM_VMN0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN0$MGrowth_RPM_VMN0 <- ave(ModelDFSL_VMN0$RPM_VMN0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN0$MGrowth_BIO_VMN0 <- ave(ModelDFSL_VMN0$BIO_VMN0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN0$MGrowth_HUM_VMN0 <- ave(ModelDFSL_VMN0$HUM_VMN0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN0$MGrowth_IOM_VMN0 <- ave(ModelDFSL_VMN0$IOM_VMN0, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN0$M_CCO2_VMN0 <- ModelDFSL_VMN0$MInput_VMN0 - ModelDFSL_VMN0$MGrowth_DPM_VMN0 - ModelDFSL_VMN0$MGrowth_RPM_VMN0 - ModelDFSL_VMN0$MGrowth_BIO_VMN0 - ModelDFSL_VMN0$MGrowth_HUM_VMN0
  ModelDFSL_VMN0$Accum_CCO2_VMN0 <- ModelDFSL_VMN0$AccumInput_VMN0 - ModelDFSL_VMN0$AllPools_noIOM_VMN0

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN0$M_CCO2_VMN0[1] <- 0
  ModelDFSL_VMN0$Accum_CCO2_VMN0[1] <- 0

  #Balance validation
  ModelDFSL_VMN0$Balance_VMN0 <- ModelDFSL_VMN0$AccumInput_VMN0 - ModelDFSL_VMN0$Accum_CCO2_VMN0 - (ModelDFSL_VMN0$DPM_VMN0 + ModelDFSL_VMN0$RPM_VMN0 + ModelDFSL_VMN0$BIO_VMN0 + ModelDFSL_VMN0$HUM_VMN0)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN0$M_CO2_VMN0 <- ModelDFSL_VMN0$M_CCO2_VMN0 * 44/12
  ModelDFSL_VMN0$Accum_CO2_VMN0 <- ModelDFSL_VMN0$Accum_CCO2_VMN0 * 44/12

  #This model will be called VMN0C because implies a continuous input of C
  ModelDFSL_VMN0C <- ModelDFSL_VMN0

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN0C,"VXC_Models\\ModelDFSL_R_VMN0C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN0 <- ModelDFSL_VMN0 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN0.1 <- rbind(c(0:0), ModelDFSLt0_VMN0)
  ModelDFSLt1_VMN0.2 <- rbind(c(0:0), ModelDFSLt1_VMN0.1)
  ModelDFSLt1_VMN0.3 <- rbind(c(0:0), ModelDFSLt1_VMN0.2)
  ModelDFSLt1_VMN0.4 <- rbind(c(0:0), ModelDFSLt1_VMN0.3)
  ModelDFSLt1_VMN0.5 <- rbind(c(0:0), ModelDFSLt1_VMN0.4)
  ModelDFSLt1_VMN0.6 <- rbind(c(0:0), ModelDFSLt1_VMN0.5)
  ModelDFSLt1_VMN0.7 <- rbind(c(0:0), ModelDFSLt1_VMN0.6)
  ModelDFSLt1_VMN0.8 <- rbind(c(0:0), ModelDFSLt1_VMN0.7)
  ModelDFSLt1_VMN0.9 <- rbind(c(0:0), ModelDFSLt1_VMN0.8)
  ModelDFSLt1_VMN0.10 <- rbind(c(0:0), ModelDFSLt1_VMN0.9)
  ModelDFSLt1_VMN0.11 <- rbind(c(0:0), ModelDFSLt1_VMN0.10)
  ModelDFSLt1_VMN0.12 <- rbind(c(0:0), ModelDFSLt1_VMN0.11)
  ModelDFSLt1_VMN0.13 <- ModelDFSLt1_VMN0.12[-nrow(ModelDFSLt1_VMN0.12),]
  ModelDFSLt1_VMN0.14 <- ModelDFSLt1_VMN0.13[-nrow(ModelDFSLt1_VMN0.13),]
  ModelDFSLt1_VMN0.15 <- ModelDFSLt1_VMN0.14[-nrow(ModelDFSLt1_VMN0.14),]
  ModelDFSLt1_VMN0.16 <- ModelDFSLt1_VMN0.15[-nrow(ModelDFSLt1_VMN0.15),]
  ModelDFSLt1_VMN0.17 <- ModelDFSLt1_VMN0.16[-nrow(ModelDFSLt1_VMN0.16),]
  ModelDFSLt1_VMN0.18 <- ModelDFSLt1_VMN0.17[-nrow(ModelDFSLt1_VMN0.17),]
  ModelDFSLt1_VMN0.19 <- ModelDFSLt1_VMN0.18[-nrow(ModelDFSLt1_VMN0.18),]
  ModelDFSLt1_VMN0.20 <- ModelDFSLt1_VMN0.19[-nrow(ModelDFSLt1_VMN0.19),]
  ModelDFSLt1_VMN0.21 <- ModelDFSLt1_VMN0.20[-nrow(ModelDFSLt1_VMN0.20),]
  ModelDFSLt1_VMN0.22 <- ModelDFSLt1_VMN0.21[-nrow(ModelDFSLt1_VMN0.21),]
  ModelDFSLt1_VMN0.23 <- ModelDFSLt1_VMN0.22[-nrow(ModelDFSLt1_VMN0.22),]
  ModelDFSLt1_VMN0.24 <- ModelDFSLt1_VMN0.23[-nrow(ModelDFSLt1_VMN0.23),]

  ModelDFSLt1_VMN0 <- ModelDFSLt1_VMN0.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN0 <- ModelDFSLt0_VMN0 - ModelDFSLt1_VMN0

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN0$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN0P because implies a one-off input of C
  ModelDFSL_VMN0P <- ModelDFSL1y_VMN0

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN0P,"VXP_Models\\ModelDFSL_R_VMN0P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN0 <- ggplot(ModelDFSL_VMN0P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN0, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN0, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN0, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN0, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN0, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN0, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN0, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN0
  ggplotly(P_CFluxI1y_VMN0)

  #Plot 3.1: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN0P_YCO2 <- ModelDFSL_VMN0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN0P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN0")

  PA_CO21y_VMN0 <- ggplot(ModelDFSL_VMN0P_YCO2, aes(x = Year, y = AnnualCO2_VMN0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN0
  ggplotly(PA_CO21y_VMN0)

  #Plot 3.2: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN0P_YCO2D <- merge(ModelDFSL_VMN0P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN0P_YCO2D$AnnualCO2D_VMN0 <- ModelDFSL_VMN0P_YCO2D$AnnualCO2_VMN0 * ModelDFSL_VMN0P_YCO2D$GWP100

  PA_CO21yD_VMN0 <- ggplot(ModelDFSL_VMN0P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN0
  ggplotly(PA_CO21yD_VMN0)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN0P_YC <- ModelDFSL_VMN0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN0P_YC) <- c("Year", "Months", "AnnualCTail_VMN0")

  PA_C1y_VMN0 <- ggplot(ModelDFSL_VMN0P_YC, aes(x = Year, y = AnnualCTail_VMN0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN0
  ggplotly(PA_C1y_VMN0)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN0P_YCT <- ModelDFSL_VMN0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN0)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN0P_YCT) <- c("Year", "Months", "AnnualCTail_VMN0")

  PA_CT1y_VMN0 <- ggplot(ModelDFSL_VMN0P_YCT, aes(x = Year, y = AnnualCTail_VMN0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN0
  ggplotly(PA_CT1y_VMN0)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN0P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN0P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN0P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN0P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN0P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN0P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN0P_YCT,"CTails_P\\ModelDFSL_R_C_VMN0P.xlsx") #Yearly C emissions


  #### 12.2 - VMN1) Manure; 10%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN1=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN1=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.1,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN1=data.frame(years,rep(fT_VMN1*fW_VMN1,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN1=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.1, #Percent clay in mineral soil
    xi=xi.frame_VMN1) #Loads the model

  Ct_VMN1=getC(Model_VMN1) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN1, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN1 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN1 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN1 <- as.data.frame(VEC_Lit_VMN1) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN1$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN1 <- as.data.frame(VEC_Man_VMN1) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN1$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN1, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN1, class) #Check that class is numeric
  LitterCinputs_VMN1=VEC_LitDF_VMN1   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN1=VEC_ManDF_VMN1 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN1 <- merge(LitterCinputs_VMN1, ManureCinputs_VMN1, by = "MNumber")
  MCinputs_VMN1$MInput_VMN1 <- MCinputs_VMN1$VEC_Lit_VMN1 + MCinputs_VMN1$VEC_Man_VMN1

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN1)[which(names(MCinputs_VMN1) == "VEC_Lit_VMN1")] <- "LitterC_VMN1"
  colnames(MCinputs_VMN1)[which(names(MCinputs_VMN1) == "VEC_Man_VMN1")] <- "ManureC_VMN1"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN1 <- as.data.frame(Ct_VMN1)
  colnames(ModelDF_VMN1) <- c('DPM_VMN1','RPM_VMN1','BIO_VMN1', 'HUM_VMN1', 'IOM_VMN1')
  ModelDFS_VMN1 <- ModelDF_VMN1[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN1$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN1$AllPools_VMN1 <- ModelDFS_VMN1$DPM_VMN1 + ModelDFS_VMN1$RPM_VMN1 + ModelDFS_VMN1$BIO_VMN1 + ModelDFS_VMN1$HUM_VMN1 + ModelDFS_VMN1$IOM_VMN1

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN1$AllPools_noIOM_VMN1 <- ModelDFS_VMN1$DPM_VMN1 + ModelDFS_VMN1$RPM_VMN1 + ModelDFS_VMN1$BIO_VMN1 + ModelDFS_VMN1$HUM_VMN1

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN1 <- merge(ModelDFS_VMN1, MCinputs_VMN1, by = "MNumber")

  ModelDFSL_VMN1$MInput_VMN1 <- ModelDFSL_VMN1$LitterC_VMN1 + ModelDFSL_VMN1$ManureC_VMN1
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN1$CTails_VMN1 <- ModelDFSL_VMN1$AllPools_noIOM_VMN1 + ModelDFSL_VMN1$MInput_VMN1

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN1$AccumInput_VMN1 = ModelDFSL_VMN1$AccumInput_VMN1=cumsum(ModelDFSL_VMN1$MInput_VMN1)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN1$MGrowth_DPM_VMN1 <- ave(ModelDFSL_VMN1$DPM_VMN1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN1$MGrowth_RPM_VMN1 <- ave(ModelDFSL_VMN1$RPM_VMN1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN1$MGrowth_BIO_VMN1 <- ave(ModelDFSL_VMN1$BIO_VMN1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN1$MGrowth_HUM_VMN1 <- ave(ModelDFSL_VMN1$HUM_VMN1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN1$MGrowth_IOM_VMN1 <- ave(ModelDFSL_VMN1$IOM_VMN1, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN1$M_CCO2_VMN1 <- ModelDFSL_VMN1$MInput_VMN1 - ModelDFSL_VMN1$MGrowth_DPM_VMN1 - ModelDFSL_VMN1$MGrowth_RPM_VMN1 - ModelDFSL_VMN1$MGrowth_BIO_VMN1 - ModelDFSL_VMN1$MGrowth_HUM_VMN1
  ModelDFSL_VMN1$Accum_CCO2_VMN1 <- ModelDFSL_VMN1$AccumInput_VMN1 - ModelDFSL_VMN1$AllPools_noIOM_VMN1

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN1$M_CCO2_VMN1[1] <- 0
  ModelDFSL_VMN1$Accum_CCO2_VMN1[1] <- 0

  #Balance validation
  ModelDFSL_VMN1$Balance_VMN1 <- ModelDFSL_VMN1$AccumInput_VMN1 - ModelDFSL_VMN1$Accum_CCO2_VMN1 - (ModelDFSL_VMN1$DPM_VMN1 + ModelDFSL_VMN1$RPM_VMN1 + ModelDFSL_VMN1$BIO_VMN1 + ModelDFSL_VMN1$HUM_VMN1)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN1$M_CO2_VMN1 <- ModelDFSL_VMN1$M_CCO2_VMN1 * 44/12
  ModelDFSL_VMN1$Accum_CO2_VMN1 <- ModelDFSL_VMN1$Accum_CCO2_VMN1 * 44/12

  #This model will be called VMN1C because implies a continuous input of C
  ModelDFSL_VMN1C <- ModelDFSL_VMN1

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN1C,"VXC_Models\\ModelDFSL_R_VMN1C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN1 <- ModelDFSL_VMN1 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN1.1 <- rbind(c(0:0), ModelDFSLt0_VMN1)
  ModelDFSLt1_VMN1.2 <- rbind(c(0:0), ModelDFSLt1_VMN1.1)
  ModelDFSLt1_VMN1.3 <- rbind(c(0:0), ModelDFSLt1_VMN1.2)
  ModelDFSLt1_VMN1.4 <- rbind(c(0:0), ModelDFSLt1_VMN1.3)
  ModelDFSLt1_VMN1.5 <- rbind(c(0:0), ModelDFSLt1_VMN1.4)
  ModelDFSLt1_VMN1.6 <- rbind(c(0:0), ModelDFSLt1_VMN1.5)
  ModelDFSLt1_VMN1.7 <- rbind(c(0:0), ModelDFSLt1_VMN1.6)
  ModelDFSLt1_VMN1.8 <- rbind(c(0:0), ModelDFSLt1_VMN1.7)
  ModelDFSLt1_VMN1.9 <- rbind(c(0:0), ModelDFSLt1_VMN1.8)
  ModelDFSLt1_VMN1.10 <- rbind(c(0:0), ModelDFSLt1_VMN1.9)
  ModelDFSLt1_VMN1.11 <- rbind(c(0:0), ModelDFSLt1_VMN1.10)
  ModelDFSLt1_VMN1.12 <- rbind(c(0:0), ModelDFSLt1_VMN1.11)
  ModelDFSLt1_VMN1.13 <- ModelDFSLt1_VMN1.12[-nrow(ModelDFSLt1_VMN1.12),]
  ModelDFSLt1_VMN1.14 <- ModelDFSLt1_VMN1.13[-nrow(ModelDFSLt1_VMN1.13),]
  ModelDFSLt1_VMN1.15 <- ModelDFSLt1_VMN1.14[-nrow(ModelDFSLt1_VMN1.14),]
  ModelDFSLt1_VMN1.16 <- ModelDFSLt1_VMN1.15[-nrow(ModelDFSLt1_VMN1.15),]
  ModelDFSLt1_VMN1.17 <- ModelDFSLt1_VMN1.16[-nrow(ModelDFSLt1_VMN1.16),]
  ModelDFSLt1_VMN1.18 <- ModelDFSLt1_VMN1.17[-nrow(ModelDFSLt1_VMN1.17),]
  ModelDFSLt1_VMN1.19 <- ModelDFSLt1_VMN1.18[-nrow(ModelDFSLt1_VMN1.18),]
  ModelDFSLt1_VMN1.20 <- ModelDFSLt1_VMN1.19[-nrow(ModelDFSLt1_VMN1.19),]
  ModelDFSLt1_VMN1.21 <- ModelDFSLt1_VMN1.20[-nrow(ModelDFSLt1_VMN1.20),]
  ModelDFSLt1_VMN1.22 <- ModelDFSLt1_VMN1.21[-nrow(ModelDFSLt1_VMN1.21),]
  ModelDFSLt1_VMN1.23 <- ModelDFSLt1_VMN1.22[-nrow(ModelDFSLt1_VMN1.22),]
  ModelDFSLt1_VMN1.24 <- ModelDFSLt1_VMN1.23[-nrow(ModelDFSLt1_VMN1.23),]

  ModelDFSLt1_VMN1 <- ModelDFSLt1_VMN1.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN1 <- ModelDFSLt0_VMN1 - ModelDFSLt1_VMN1

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN1$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN1P because implies a one-off input of C
  ModelDFSL_VMN1P <- ModelDFSL1y_VMN1

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN1P,"VXP_Models\\ModelDFSL_R_VMN1P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN1 <- ggplot(ModelDFSL_VMN1P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN1, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN1, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN1, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN1, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN1, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN1, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN1, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN1
  ggplotly(P_CFluxI1y_VMN1)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN1P_YCO2 <- ModelDFSL_VMN1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN1P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN1")

  PA_CO21y_VMN1 <- ggplot(ModelDFSL_VMN1P_YCO2, aes(x = Year, y = AnnualCO2_VMN1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN1
  ggplotly(PA_CO21y_VMN1)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN1P_YCO2D <- merge(ModelDFSL_VMN1P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN1P_YCO2D$AnnualCO2D_VMN1 <- ModelDFSL_VMN1P_YCO2D$AnnualCO2_VMN1 * ModelDFSL_VMN1P_YCO2D$GWP100

  PA_CO21yD_VMN1 <- ggplot(ModelDFSL_VMN1P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN1
  ggplotly(PA_CO21yD_VMN1)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN1P_YC <- ModelDFSL_VMN1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN1P_YC) <- c("Year", "Months", "AnnualCTail_VMN1")

  PA_C1y_VMN1 <- ggplot(ModelDFSL_VMN1P_YC, aes(x = Year, y = AnnualCTail_VMN1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN1
  ggplotly(PA_C1y_VMN1)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN1P_YCT <- ModelDFSL_VMN1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN1)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN1P_YCT) <- c("Year", "Months", "AnnualCTail_VMN1")

  PA_CT1y_VMN1 <- ggplot(ModelDFSL_VMN1P_YCT, aes(x = Year, y = AnnualCTail_VMN1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN1
  ggplotly(PA_CT1y_VMN1)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN1P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN1P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN1P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN1P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN1P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN1P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN1P_YCT,"CTails_P\\ModelDFSL_R_C_VMN1P.xlsx") #Yearly C emissions



  #### 12.3 - VMN2) Manure; 20%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN2=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN2=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.2,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN2=data.frame(years,rep(fT_VMN2*fW_VMN2,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN2=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.2, #Percent clay in mineral soil
    xi=xi.frame_VMN2) #Loads the model

  Ct_VMN2=getC(Model_VMN2) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN2, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN2 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN2 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN2 <- as.data.frame(VEC_Lit_VMN2) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN2$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN2 <- as.data.frame(VEC_Man_VMN2) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN2$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN2, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN2, class) #Check that class is numeric
  LitterCinputs_VMN2=VEC_LitDF_VMN2   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN2=VEC_ManDF_VMN2 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN2 <- merge(LitterCinputs_VMN2, ManureCinputs_VMN2, by = "MNumber")
  MCinputs_VMN2$MInput_VMN2 <- MCinputs_VMN2$VEC_Lit_VMN2 + MCinputs_VMN2$VEC_Man_VMN2

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN2)[which(names(MCinputs_VMN2) == "VEC_Lit_VMN2")] <- "LitterC_VMN2"
  colnames(MCinputs_VMN2)[which(names(MCinputs_VMN2) == "VEC_Man_VMN2")] <- "ManureC_VMN2"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN2 <- as.data.frame(Ct_VMN2)
  colnames(ModelDF_VMN2) <- c('DPM_VMN2','RPM_VMN2','BIO_VMN2', 'HUM_VMN2', 'IOM_VMN2')
  ModelDFS_VMN2 <- ModelDF_VMN2[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN2$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN2$AllPools_VMN2 <- ModelDFS_VMN2$DPM_VMN2 + ModelDFS_VMN2$RPM_VMN2 + ModelDFS_VMN2$BIO_VMN2 + ModelDFS_VMN2$HUM_VMN2 + ModelDFS_VMN2$IOM_VMN2

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN2$AllPools_noIOM_VMN2 <- ModelDFS_VMN2$DPM_VMN2 + ModelDFS_VMN2$RPM_VMN2 + ModelDFS_VMN2$BIO_VMN2 + ModelDFS_VMN2$HUM_VMN2

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN2 <- merge(ModelDFS_VMN2, MCinputs_VMN2, by = "MNumber")

  ModelDFSL_VMN2$MInput_VMN2 <- ModelDFSL_VMN2$LitterC_VMN2 + ModelDFSL_VMN2$ManureC_VMN2
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN2$CTails_VMN2 <- ModelDFSL_VMN2$AllPools_noIOM_VMN2 + ModelDFSL_VMN2$MInput_VMN2

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN2$AccumInput_VMN2 = ModelDFSL_VMN2$AccumInput_VMN2=cumsum(ModelDFSL_VMN2$MInput_VMN2)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN2$MGrowth_DPM_VMN2 <- ave(ModelDFSL_VMN2$DPM_VMN2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN2$MGrowth_RPM_VMN2 <- ave(ModelDFSL_VMN2$RPM_VMN2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN2$MGrowth_BIO_VMN2 <- ave(ModelDFSL_VMN2$BIO_VMN2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN2$MGrowth_HUM_VMN2 <- ave(ModelDFSL_VMN2$HUM_VMN2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN2$MGrowth_IOM_VMN2 <- ave(ModelDFSL_VMN2$IOM_VMN2, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN2$M_CCO2_VMN2 <- ModelDFSL_VMN2$MInput_VMN2 - ModelDFSL_VMN2$MGrowth_DPM_VMN2 - ModelDFSL_VMN2$MGrowth_RPM_VMN2 - ModelDFSL_VMN2$MGrowth_BIO_VMN2 - ModelDFSL_VMN2$MGrowth_HUM_VMN2
  ModelDFSL_VMN2$Accum_CCO2_VMN2 <- ModelDFSL_VMN2$AccumInput_VMN2 - ModelDFSL_VMN2$AllPools_noIOM_VMN2

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN2$M_CCO2_VMN2[1] <- 0
  ModelDFSL_VMN2$Accum_CCO2_VMN2[1] <- 0

  #Balance validation
  ModelDFSL_VMN2$Balance_VMN2 <- ModelDFSL_VMN2$AccumInput_VMN2 - ModelDFSL_VMN2$Accum_CCO2_VMN2 - (ModelDFSL_VMN2$DPM_VMN2 + ModelDFSL_VMN2$RPM_VMN2 + ModelDFSL_VMN2$BIO_VMN2 + ModelDFSL_VMN2$HUM_VMN2)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN2$M_CO2_VMN2 <- ModelDFSL_VMN2$M_CCO2_VMN2 * 44/12
  ModelDFSL_VMN2$Accum_CO2_VMN2 <- ModelDFSL_VMN2$Accum_CCO2_VMN2 * 44/12

  #This model will be called VMN2C because implies a continuous input of C
  ModelDFSL_VMN2C <- ModelDFSL_VMN2

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN2C,"VXC_Models\\ModelDFSL_R_VMN2C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN2 <- ModelDFSL_VMN2 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN2.1 <- rbind(c(0:0), ModelDFSLt0_VMN2)
  ModelDFSLt1_VMN2.2 <- rbind(c(0:0), ModelDFSLt1_VMN2.1)
  ModelDFSLt1_VMN2.3 <- rbind(c(0:0), ModelDFSLt1_VMN2.2)
  ModelDFSLt1_VMN2.4 <- rbind(c(0:0), ModelDFSLt1_VMN2.3)
  ModelDFSLt1_VMN2.5 <- rbind(c(0:0), ModelDFSLt1_VMN2.4)
  ModelDFSLt1_VMN2.6 <- rbind(c(0:0), ModelDFSLt1_VMN2.5)
  ModelDFSLt1_VMN2.7 <- rbind(c(0:0), ModelDFSLt1_VMN2.6)
  ModelDFSLt1_VMN2.8 <- rbind(c(0:0), ModelDFSLt1_VMN2.7)
  ModelDFSLt1_VMN2.9 <- rbind(c(0:0), ModelDFSLt1_VMN2.8)
  ModelDFSLt1_VMN2.10 <- rbind(c(0:0), ModelDFSLt1_VMN2.9)
  ModelDFSLt1_VMN2.11 <- rbind(c(0:0), ModelDFSLt1_VMN2.10)
  ModelDFSLt1_VMN2.12 <- rbind(c(0:0), ModelDFSLt1_VMN2.11)
  ModelDFSLt1_VMN2.13 <- ModelDFSLt1_VMN2.12[-nrow(ModelDFSLt1_VMN2.12),]
  ModelDFSLt1_VMN2.14 <- ModelDFSLt1_VMN2.13[-nrow(ModelDFSLt1_VMN2.13),]
  ModelDFSLt1_VMN2.15 <- ModelDFSLt1_VMN2.14[-nrow(ModelDFSLt1_VMN2.14),]
  ModelDFSLt1_VMN2.16 <- ModelDFSLt1_VMN2.15[-nrow(ModelDFSLt1_VMN2.15),]
  ModelDFSLt1_VMN2.17 <- ModelDFSLt1_VMN2.16[-nrow(ModelDFSLt1_VMN2.16),]
  ModelDFSLt1_VMN2.18 <- ModelDFSLt1_VMN2.17[-nrow(ModelDFSLt1_VMN2.17),]
  ModelDFSLt1_VMN2.19 <- ModelDFSLt1_VMN2.18[-nrow(ModelDFSLt1_VMN2.18),]
  ModelDFSLt1_VMN2.20 <- ModelDFSLt1_VMN2.19[-nrow(ModelDFSLt1_VMN2.19),]
  ModelDFSLt1_VMN2.21 <- ModelDFSLt1_VMN2.20[-nrow(ModelDFSLt1_VMN2.20),]
  ModelDFSLt1_VMN2.22 <- ModelDFSLt1_VMN2.21[-nrow(ModelDFSLt1_VMN2.21),]
  ModelDFSLt1_VMN2.23 <- ModelDFSLt1_VMN2.22[-nrow(ModelDFSLt1_VMN2.22),]
  ModelDFSLt1_VMN2.24 <- ModelDFSLt1_VMN2.23[-nrow(ModelDFSLt1_VMN2.23),]

  ModelDFSLt1_VMN2 <- ModelDFSLt1_VMN2.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN2 <- ModelDFSLt0_VMN2 - ModelDFSLt1_VMN2

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN2$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN2P because implies a one-off input of C
  ModelDFSL_VMN2P <- ModelDFSL1y_VMN2

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN2P,"VXP_Models\\ModelDFSL_R_VMN2P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN2 <- ggplot(ModelDFSL_VMN2P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN2, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN2, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN2, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN2, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN2, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN2, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN2, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN2
  ggplotly(P_CFluxI1y_VMN2)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN2P_YCO2 <- ModelDFSL_VMN2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN2P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN2")

  PA_CO21y_VMN2 <- ggplot(ModelDFSL_VMN2P_YCO2, aes(x = Year, y = AnnualCO2_VMN2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN2
  ggplotly(PA_CO21y_VMN2)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN2P_YCO2D <- merge(ModelDFSL_VMN2P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN2P_YCO2D$AnnualCO2D_VMN2 <- ModelDFSL_VMN2P_YCO2D$AnnualCO2_VMN2 * ModelDFSL_VMN2P_YCO2D$GWP100

  PA_CO21yD_VMN2 <- ggplot(ModelDFSL_VMN2P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN2
  ggplotly(PA_CO21yD_VMN2)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN2P_YC <- ModelDFSL_VMN2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN2P_YC) <- c("Year", "Months", "AnnualCTail_VMN2")

  PA_C1y_VMN2 <- ggplot(ModelDFSL_VMN2P_YC, aes(x = Year, y = AnnualCTail_VMN2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN2
  ggplotly(PA_C1y_VMN2)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN2P_YCT <- ModelDFSL_VMN2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN2)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN2P_YCT) <- c("Year", "Months", "AnnualCTail_VMN2")

  PA_CT1y_VMN2 <- ggplot(ModelDFSL_VMN2P_YCT, aes(x = Year, y = AnnualCTail_VMN2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN2
  ggplotly(PA_CT1y_VMN2)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN2P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN2P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN2P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN2P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN2P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN2P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN2P_YCT,"CTails_P\\ModelDFSL_R_C_VMN2P.xlsx") #Yearly C emissions



  #### 12.4 - VMN3) Manure; 30%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN3=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN3=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.3,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN3=data.frame(years,rep(fT_VMN3*fW_VMN3,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN3=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.3, #Percent clay in mineral soil
    xi=xi.frame_VMN3) #Loads the model

  Ct_VMN3=getC(Model_VMN3) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN3, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN3 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN3 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN3 <- as.data.frame(VEC_Lit_VMN3) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN3$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN3 <- as.data.frame(VEC_Man_VMN3) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN3$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN3, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN3, class) #Check that class is numeric
  LitterCinputs_VMN3=VEC_LitDF_VMN3   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN3=VEC_ManDF_VMN3 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN3 <- merge(LitterCinputs_VMN3, ManureCinputs_VMN3, by = "MNumber")
  MCinputs_VMN3$MInput_VMN3 <- MCinputs_VMN3$VEC_Lit_VMN3 + MCinputs_VMN3$VEC_Man_VMN3

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN3)[which(names(MCinputs_VMN3) == "VEC_Lit_VMN3")] <- "LitterC_VMN3"
  colnames(MCinputs_VMN3)[which(names(MCinputs_VMN3) == "VEC_Man_VMN3")] <- "ManureC_VMN3"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN3 <- as.data.frame(Ct_VMN3)
  colnames(ModelDF_VMN3) <- c('DPM_VMN3','RPM_VMN3','BIO_VMN3', 'HUM_VMN3', 'IOM_VMN3')
  ModelDFS_VMN3 <- ModelDF_VMN3[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN3$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN3$AllPools_VMN3 <- ModelDFS_VMN3$DPM_VMN3 + ModelDFS_VMN3$RPM_VMN3 + ModelDFS_VMN3$BIO_VMN3 + ModelDFS_VMN3$HUM_VMN3 + ModelDFS_VMN3$IOM_VMN3

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN3$AllPools_noIOM_VMN3 <- ModelDFS_VMN3$DPM_VMN3 + ModelDFS_VMN3$RPM_VMN3 + ModelDFS_VMN3$BIO_VMN3 + ModelDFS_VMN3$HUM_VMN3

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN3 <- merge(ModelDFS_VMN3, MCinputs_VMN3, by = "MNumber")

  ModelDFSL_VMN3$MInput_VMN3 <- ModelDFSL_VMN3$LitterC_VMN3 + ModelDFSL_VMN3$ManureC_VMN3
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN3$CTails_VMN3 <- ModelDFSL_VMN3$AllPools_noIOM_VMN3 + ModelDFSL_VMN3$MInput_VMN3

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN3$AccumInput_VMN3 = ModelDFSL_VMN3$AccumInput_VMN3=cumsum(ModelDFSL_VMN3$MInput_VMN3)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN3$MGrowth_DPM_VMN3 <- ave(ModelDFSL_VMN3$DPM_VMN3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN3$MGrowth_RPM_VMN3 <- ave(ModelDFSL_VMN3$RPM_VMN3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN3$MGrowth_BIO_VMN3 <- ave(ModelDFSL_VMN3$BIO_VMN3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN3$MGrowth_HUM_VMN3 <- ave(ModelDFSL_VMN3$HUM_VMN3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN3$MGrowth_IOM_VMN3 <- ave(ModelDFSL_VMN3$IOM_VMN3, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN3$M_CCO2_VMN3 <- ModelDFSL_VMN3$MInput_VMN3 - ModelDFSL_VMN3$MGrowth_DPM_VMN3 - ModelDFSL_VMN3$MGrowth_RPM_VMN3 - ModelDFSL_VMN3$MGrowth_BIO_VMN3 - ModelDFSL_VMN3$MGrowth_HUM_VMN3
  ModelDFSL_VMN3$Accum_CCO2_VMN3 <- ModelDFSL_VMN3$AccumInput_VMN3 - ModelDFSL_VMN3$AllPools_noIOM_VMN3

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN3$M_CCO2_VMN3[1] <- 0
  ModelDFSL_VMN3$Accum_CCO2_VMN3[1] <- 0

  #Balance validation
  ModelDFSL_VMN3$Balance_VMN3 <- ModelDFSL_VMN3$AccumInput_VMN3 - ModelDFSL_VMN3$Accum_CCO2_VMN3 - (ModelDFSL_VMN3$DPM_VMN3 + ModelDFSL_VMN3$RPM_VMN3 + ModelDFSL_VMN3$BIO_VMN3 + ModelDFSL_VMN3$HUM_VMN3)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN3$M_CO2_VMN3 <- ModelDFSL_VMN3$M_CCO2_VMN3 * 44/12
  ModelDFSL_VMN3$Accum_CO2_VMN3 <- ModelDFSL_VMN3$Accum_CCO2_VMN3 * 44/12

  #This model will be called VMN3C because implies a continuous input of C
  ModelDFSL_VMN3C <- ModelDFSL_VMN3

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN3C,"VXC_Models\\ModelDFSL_R_VMN3C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN3 <- ModelDFSL_VMN3 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN3.1 <- rbind(c(0:0), ModelDFSLt0_VMN3)
  ModelDFSLt1_VMN3.2 <- rbind(c(0:0), ModelDFSLt1_VMN3.1)
  ModelDFSLt1_VMN3.3 <- rbind(c(0:0), ModelDFSLt1_VMN3.2)
  ModelDFSLt1_VMN3.4 <- rbind(c(0:0), ModelDFSLt1_VMN3.3)
  ModelDFSLt1_VMN3.5 <- rbind(c(0:0), ModelDFSLt1_VMN3.4)
  ModelDFSLt1_VMN3.6 <- rbind(c(0:0), ModelDFSLt1_VMN3.5)
  ModelDFSLt1_VMN3.7 <- rbind(c(0:0), ModelDFSLt1_VMN3.6)
  ModelDFSLt1_VMN3.8 <- rbind(c(0:0), ModelDFSLt1_VMN3.7)
  ModelDFSLt1_VMN3.9 <- rbind(c(0:0), ModelDFSLt1_VMN3.8)
  ModelDFSLt1_VMN3.10 <- rbind(c(0:0), ModelDFSLt1_VMN3.9)
  ModelDFSLt1_VMN3.11 <- rbind(c(0:0), ModelDFSLt1_VMN3.10)
  ModelDFSLt1_VMN3.12 <- rbind(c(0:0), ModelDFSLt1_VMN3.11)
  ModelDFSLt1_VMN3.13 <- ModelDFSLt1_VMN3.12[-nrow(ModelDFSLt1_VMN3.12),]
  ModelDFSLt1_VMN3.14 <- ModelDFSLt1_VMN3.13[-nrow(ModelDFSLt1_VMN3.13),]
  ModelDFSLt1_VMN3.15 <- ModelDFSLt1_VMN3.14[-nrow(ModelDFSLt1_VMN3.14),]
  ModelDFSLt1_VMN3.16 <- ModelDFSLt1_VMN3.15[-nrow(ModelDFSLt1_VMN3.15),]
  ModelDFSLt1_VMN3.17 <- ModelDFSLt1_VMN3.16[-nrow(ModelDFSLt1_VMN3.16),]
  ModelDFSLt1_VMN3.18 <- ModelDFSLt1_VMN3.17[-nrow(ModelDFSLt1_VMN3.17),]
  ModelDFSLt1_VMN3.19 <- ModelDFSLt1_VMN3.18[-nrow(ModelDFSLt1_VMN3.18),]
  ModelDFSLt1_VMN3.20 <- ModelDFSLt1_VMN3.19[-nrow(ModelDFSLt1_VMN3.19),]
  ModelDFSLt1_VMN3.21 <- ModelDFSLt1_VMN3.20[-nrow(ModelDFSLt1_VMN3.20),]
  ModelDFSLt1_VMN3.22 <- ModelDFSLt1_VMN3.21[-nrow(ModelDFSLt1_VMN3.21),]
  ModelDFSLt1_VMN3.23 <- ModelDFSLt1_VMN3.22[-nrow(ModelDFSLt1_VMN3.22),]
  ModelDFSLt1_VMN3.24 <- ModelDFSLt1_VMN3.23[-nrow(ModelDFSLt1_VMN3.23),]

  ModelDFSLt1_VMN3 <- ModelDFSLt1_VMN3.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN3 <- ModelDFSLt0_VMN3 - ModelDFSLt1_VMN3

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN3$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN3P because implies a one-off input of C
  ModelDFSL_VMN3P <- ModelDFSL1y_VMN3

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN3P,"VXP_Models\\ModelDFSL_R_VMN3P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN3 <- ggplot(ModelDFSL_VMN3P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN3, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN3, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN3, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN3, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN3, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN3, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN3, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN3
  ggplotly(P_CFluxI1y_VMN3)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN3P_YCO2 <- ModelDFSL_VMN3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN3P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN3")

  PA_CO21y_VMN3 <- ggplot(ModelDFSL_VMN3P_YCO2, aes(x = Year, y = AnnualCO2_VMN3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN3
  ggplotly(PA_CO21y_VMN3)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN3P_YCO2D <- merge(ModelDFSL_VMN3P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN3P_YCO2D$AnnualCO2D_VMN3 <- ModelDFSL_VMN3P_YCO2D$AnnualCO2_VMN3 * ModelDFSL_VMN3P_YCO2D$GWP100

  PA_CO21yD_VMN3 <- ggplot(ModelDFSL_VMN3P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN3
  ggplotly(PA_CO21yD_VMN3)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN3P_YC <- ModelDFSL_VMN3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN3P_YC) <- c("Year", "Months", "AnnualCTail_VMN3")

  PA_C1y_VMN3 <- ggplot(ModelDFSL_VMN3P_YC, aes(x = Year, y = AnnualCTail_VMN3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN3
  ggplotly(PA_C1y_VMN3)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN3P_YCT <- ModelDFSL_VMN3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN3)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN3P_YCT) <- c("Year", "Months", "AnnualCTail_VMN3")

  PA_CT1y_VMN3 <- ggplot(ModelDFSL_VMN3P_YCT, aes(x = Year, y = AnnualCTail_VMN3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN3
  ggplotly(PA_CT1y_VMN3)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN3P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN3P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN3P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN3P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN3P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN3P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN3P_YCT,"CTails_P\\ModelDFSL_R_C_VMN3P.xlsx") #Yearly C emissions



  #### 12.5 - VMN4) Manure; 40%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN4=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN4=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.4,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN4=data.frame(years,rep(fT_VMN4*fW_VMN4,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN4=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.4, #Percent clay in mineral soil
    xi=xi.frame_VMN4) #Loads the model

  Ct_VMN4=getC(Model_VMN4) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN4, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN4 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN4 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN4 <- as.data.frame(VEC_Lit_VMN4) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN4$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN4 <- as.data.frame(VEC_Man_VMN4) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN4$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN4, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN4, class) #Check that class is numeric
  LitterCinputs_VMN4=VEC_LitDF_VMN4   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN4=VEC_ManDF_VMN4 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN4 <- merge(LitterCinputs_VMN4, ManureCinputs_VMN4, by = "MNumber")
  MCinputs_VMN4$MInput_VMN4 <- MCinputs_VMN4$VEC_Lit_VMN4 + MCinputs_VMN4$VEC_Man_VMN4

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN4)[which(names(MCinputs_VMN4) == "VEC_Lit_VMN4")] <- "LitterC_VMN4"
  colnames(MCinputs_VMN4)[which(names(MCinputs_VMN4) == "VEC_Man_VMN4")] <- "ManureC_VMN4"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN4 <- as.data.frame(Ct_VMN4)
  colnames(ModelDF_VMN4) <- c('DPM_VMN4','RPM_VMN4','BIO_VMN4', 'HUM_VMN4', 'IOM_VMN4')
  ModelDFS_VMN4 <- ModelDF_VMN4[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN4$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN4$AllPools_VMN4 <- ModelDFS_VMN4$DPM_VMN4 + ModelDFS_VMN4$RPM_VMN4 + ModelDFS_VMN4$BIO_VMN4 + ModelDFS_VMN4$HUM_VMN4 + ModelDFS_VMN4$IOM_VMN4

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN4$AllPools_noIOM_VMN4 <- ModelDFS_VMN4$DPM_VMN4 + ModelDFS_VMN4$RPM_VMN4 + ModelDFS_VMN4$BIO_VMN4 + ModelDFS_VMN4$HUM_VMN4

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN4 <- merge(ModelDFS_VMN4, MCinputs_VMN4, by = "MNumber")

  ModelDFSL_VMN4$MInput_VMN4 <- ModelDFSL_VMN4$LitterC_VMN4 + ModelDFSL_VMN4$ManureC_VMN4
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN4$CTails_VMN4 <- ModelDFSL_VMN4$AllPools_noIOM_VMN4 + ModelDFSL_VMN4$MInput_VMN4

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN4$AccumInput_VMN4 = ModelDFSL_VMN4$AccumInput_VMN4=cumsum(ModelDFSL_VMN4$MInput_VMN4)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN4$MGrowth_DPM_VMN4 <- ave(ModelDFSL_VMN4$DPM_VMN4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN4$MGrowth_RPM_VMN4 <- ave(ModelDFSL_VMN4$RPM_VMN4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN4$MGrowth_BIO_VMN4 <- ave(ModelDFSL_VMN4$BIO_VMN4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN4$MGrowth_HUM_VMN4 <- ave(ModelDFSL_VMN4$HUM_VMN4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN4$MGrowth_IOM_VMN4 <- ave(ModelDFSL_VMN4$IOM_VMN4, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN4$M_CCO2_VMN4 <- ModelDFSL_VMN4$MInput_VMN4 - ModelDFSL_VMN4$MGrowth_DPM_VMN4 - ModelDFSL_VMN4$MGrowth_RPM_VMN4 - ModelDFSL_VMN4$MGrowth_BIO_VMN4 - ModelDFSL_VMN4$MGrowth_HUM_VMN4
  ModelDFSL_VMN4$Accum_CCO2_VMN4 <- ModelDFSL_VMN4$AccumInput_VMN4 - ModelDFSL_VMN4$AllPools_noIOM_VMN4

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN4$M_CCO2_VMN4[1] <- 0
  ModelDFSL_VMN4$Accum_CCO2_VMN4[1] <- 0

  #Balance validation
  ModelDFSL_VMN4$Balance_VMN4 <- ModelDFSL_VMN4$AccumInput_VMN4 - ModelDFSL_VMN4$Accum_CCO2_VMN4 - (ModelDFSL_VMN4$DPM_VMN4 + ModelDFSL_VMN4$RPM_VMN4 + ModelDFSL_VMN4$BIO_VMN4 + ModelDFSL_VMN4$HUM_VMN4)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN4$M_CO2_VMN4 <- ModelDFSL_VMN4$M_CCO2_VMN4 * 44/12
  ModelDFSL_VMN4$Accum_CO2_VMN4 <- ModelDFSL_VMN4$Accum_CCO2_VMN4 * 44/12

  #This model will be called VMN4C because implies a continuous input of C
  ModelDFSL_VMN4C <- ModelDFSL_VMN4

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN4C,"VXC_Models\\ModelDFSL_R_VMN4C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN4 <- ModelDFSL_VMN4 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN4.1 <- rbind(c(0:0), ModelDFSLt0_VMN4)
  ModelDFSLt1_VMN4.2 <- rbind(c(0:0), ModelDFSLt1_VMN4.1)
  ModelDFSLt1_VMN4.3 <- rbind(c(0:0), ModelDFSLt1_VMN4.2)
  ModelDFSLt1_VMN4.4 <- rbind(c(0:0), ModelDFSLt1_VMN4.3)
  ModelDFSLt1_VMN4.5 <- rbind(c(0:0), ModelDFSLt1_VMN4.4)
  ModelDFSLt1_VMN4.6 <- rbind(c(0:0), ModelDFSLt1_VMN4.5)
  ModelDFSLt1_VMN4.7 <- rbind(c(0:0), ModelDFSLt1_VMN4.6)
  ModelDFSLt1_VMN4.8 <- rbind(c(0:0), ModelDFSLt1_VMN4.7)
  ModelDFSLt1_VMN4.9 <- rbind(c(0:0), ModelDFSLt1_VMN4.8)
  ModelDFSLt1_VMN4.10 <- rbind(c(0:0), ModelDFSLt1_VMN4.9)
  ModelDFSLt1_VMN4.11 <- rbind(c(0:0), ModelDFSLt1_VMN4.10)
  ModelDFSLt1_VMN4.12 <- rbind(c(0:0), ModelDFSLt1_VMN4.11)
  ModelDFSLt1_VMN4.13 <- ModelDFSLt1_VMN4.12[-nrow(ModelDFSLt1_VMN4.12),]
  ModelDFSLt1_VMN4.14 <- ModelDFSLt1_VMN4.13[-nrow(ModelDFSLt1_VMN4.13),]
  ModelDFSLt1_VMN4.15 <- ModelDFSLt1_VMN4.14[-nrow(ModelDFSLt1_VMN4.14),]
  ModelDFSLt1_VMN4.16 <- ModelDFSLt1_VMN4.15[-nrow(ModelDFSLt1_VMN4.15),]
  ModelDFSLt1_VMN4.17 <- ModelDFSLt1_VMN4.16[-nrow(ModelDFSLt1_VMN4.16),]
  ModelDFSLt1_VMN4.18 <- ModelDFSLt1_VMN4.17[-nrow(ModelDFSLt1_VMN4.17),]
  ModelDFSLt1_VMN4.19 <- ModelDFSLt1_VMN4.18[-nrow(ModelDFSLt1_VMN4.18),]
  ModelDFSLt1_VMN4.20 <- ModelDFSLt1_VMN4.19[-nrow(ModelDFSLt1_VMN4.19),]
  ModelDFSLt1_VMN4.21 <- ModelDFSLt1_VMN4.20[-nrow(ModelDFSLt1_VMN4.20),]
  ModelDFSLt1_VMN4.22 <- ModelDFSLt1_VMN4.21[-nrow(ModelDFSLt1_VMN4.21),]
  ModelDFSLt1_VMN4.23 <- ModelDFSLt1_VMN4.22[-nrow(ModelDFSLt1_VMN4.22),]
  ModelDFSLt1_VMN4.24 <- ModelDFSLt1_VMN4.23[-nrow(ModelDFSLt1_VMN4.23),]

  ModelDFSLt1_VMN4 <- ModelDFSLt1_VMN4.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN4 <- ModelDFSLt0_VMN4 - ModelDFSLt1_VMN4

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN4$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN4P because implies a one-off input of C
  ModelDFSL_VMN4P <- ModelDFSL1y_VMN4

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN4P,"VXP_Models\\ModelDFSL_R_VMN4P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN4 <- ggplot(ModelDFSL_VMN4P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN4, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN4, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN4, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN4, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN4, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN4, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN4, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN4
  ggplotly(P_CFluxI1y_VMN4)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN4P_YCO2 <- ModelDFSL_VMN4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN4P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN4")

  PA_CO21y_VMN4 <- ggplot(ModelDFSL_VMN4P_YCO2, aes(x = Year, y = AnnualCO2_VMN4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN4
  ggplotly(PA_CO21y_VMN4)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN4P_YCO2D <- merge(ModelDFSL_VMN4P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN4P_YCO2D$AnnualCO2D_VMN4 <- ModelDFSL_VMN4P_YCO2D$AnnualCO2_VMN4 * ModelDFSL_VMN4P_YCO2D$GWP100

  PA_CO21yD_VMN4 <- ggplot(ModelDFSL_VMN4P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN4
  ggplotly(PA_CO21yD_VMN4)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN4P_YC <- ModelDFSL_VMN4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN4P_YC) <- c("Year", "Months", "AnnualCTail_VMN4")

  PA_C1y_VMN4 <- ggplot(ModelDFSL_VMN4P_YC, aes(x = Year, y = AnnualCTail_VMN4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN4
  ggplotly(PA_C1y_VMN4)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN4P_YCT <- ModelDFSL_VMN4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN4)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN4P_YCT) <- c("Year", "Months", "AnnualCTail_VMN4")

  PA_CT1y_VMN4 <- ggplot(ModelDFSL_VMN4P_YCT, aes(x = Year, y = AnnualCTail_VMN4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN4
  ggplotly(PA_CT1y_VMN4)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN4P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN4P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN4P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN4P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN4P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN4P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN4P_YCT,"CTails_P\\ModelDFSL_R_C_VMN4P.xlsx") #Yearly C emissions



  #### 12.6 - VMN5) Manure; 50%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN5=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN5=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.5,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN5=data.frame(years,rep(fT_VMN5*fW_VMN5,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN5=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.5, #Percent clay in mineral soil
    xi=xi.frame_VMN5) #Loads the model

  Ct_VMN5=getC(Model_VMN5) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN5, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN5 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN5 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN5 <- as.data.frame(VEC_Lit_VMN5) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN5$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN5 <- as.data.frame(VEC_Man_VMN5) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN5$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN5, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN5, class) #Check that class is numeric
  LitterCinputs_VMN5=VEC_LitDF_VMN5   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN5=VEC_ManDF_VMN5 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN5 <- merge(LitterCinputs_VMN5, ManureCinputs_VMN5, by = "MNumber")
  MCinputs_VMN5$MInput_VMN5 <- MCinputs_VMN5$VEC_Lit_VMN5 + MCinputs_VMN5$VEC_Man_VMN5

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN5)[which(names(MCinputs_VMN5) == "VEC_Lit_VMN5")] <- "LitterC_VMN5"
  colnames(MCinputs_VMN5)[which(names(MCinputs_VMN5) == "VEC_Man_VMN5")] <- "ManureC_VMN5"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN5 <- as.data.frame(Ct_VMN5)
  colnames(ModelDF_VMN5) <- c('DPM_VMN5','RPM_VMN5','BIO_VMN5', 'HUM_VMN5', 'IOM_VMN5')
  ModelDFS_VMN5 <- ModelDF_VMN5[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN5$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN5$AllPools_VMN5 <- ModelDFS_VMN5$DPM_VMN5 + ModelDFS_VMN5$RPM_VMN5 + ModelDFS_VMN5$BIO_VMN5 + ModelDFS_VMN5$HUM_VMN5 + ModelDFS_VMN5$IOM_VMN5

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN5$AllPools_noIOM_VMN5 <- ModelDFS_VMN5$DPM_VMN5 + ModelDFS_VMN5$RPM_VMN5 + ModelDFS_VMN5$BIO_VMN5 + ModelDFS_VMN5$HUM_VMN5

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN5 <- merge(ModelDFS_VMN5, MCinputs_VMN5, by = "MNumber")

  ModelDFSL_VMN5$MInput_VMN5 <- ModelDFSL_VMN5$LitterC_VMN5 + ModelDFSL_VMN5$ManureC_VMN5
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN5$CTails_VMN5 <- ModelDFSL_VMN5$AllPools_noIOM_VMN5 + ModelDFSL_VMN5$MInput_VMN5

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN5$AccumInput_VMN5 = ModelDFSL_VMN5$AccumInput_VMN5=cumsum(ModelDFSL_VMN5$MInput_VMN5)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN5$MGrowth_DPM_VMN5 <- ave(ModelDFSL_VMN5$DPM_VMN5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN5$MGrowth_RPM_VMN5 <- ave(ModelDFSL_VMN5$RPM_VMN5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN5$MGrowth_BIO_VMN5 <- ave(ModelDFSL_VMN5$BIO_VMN5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN5$MGrowth_HUM_VMN5 <- ave(ModelDFSL_VMN5$HUM_VMN5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN5$MGrowth_IOM_VMN5 <- ave(ModelDFSL_VMN5$IOM_VMN5, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN5$M_CCO2_VMN5 <- ModelDFSL_VMN5$MInput_VMN5 - ModelDFSL_VMN5$MGrowth_DPM_VMN5 - ModelDFSL_VMN5$MGrowth_RPM_VMN5 - ModelDFSL_VMN5$MGrowth_BIO_VMN5 - ModelDFSL_VMN5$MGrowth_HUM_VMN5
  ModelDFSL_VMN5$Accum_CCO2_VMN5 <- ModelDFSL_VMN5$AccumInput_VMN5 - ModelDFSL_VMN5$AllPools_noIOM_VMN5

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN5$M_CCO2_VMN5[1] <- 0
  ModelDFSL_VMN5$Accum_CCO2_VMN5[1] <- 0

  #Balance validation
  ModelDFSL_VMN5$Balance_VMN5 <- ModelDFSL_VMN5$AccumInput_VMN5 - ModelDFSL_VMN5$Accum_CCO2_VMN5 - (ModelDFSL_VMN5$DPM_VMN5 + ModelDFSL_VMN5$RPM_VMN5 + ModelDFSL_VMN5$BIO_VMN5 + ModelDFSL_VMN5$HUM_VMN5)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN5$M_CO2_VMN5 <- ModelDFSL_VMN5$M_CCO2_VMN5 * 44/12
  ModelDFSL_VMN5$Accum_CO2_VMN5 <- ModelDFSL_VMN5$Accum_CCO2_VMN5 * 44/12

  #This model will be called VMN5C because implies a continuous input of C
  ModelDFSL_VMN5C <- ModelDFSL_VMN5

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN5C,"VXC_Models\\ModelDFSL_R_VMN5C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN5 <- ModelDFSL_VMN5 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN5.1 <- rbind(c(0:0), ModelDFSLt0_VMN5)
  ModelDFSLt1_VMN5.2 <- rbind(c(0:0), ModelDFSLt1_VMN5.1)
  ModelDFSLt1_VMN5.3 <- rbind(c(0:0), ModelDFSLt1_VMN5.2)
  ModelDFSLt1_VMN5.4 <- rbind(c(0:0), ModelDFSLt1_VMN5.3)
  ModelDFSLt1_VMN5.5 <- rbind(c(0:0), ModelDFSLt1_VMN5.4)
  ModelDFSLt1_VMN5.6 <- rbind(c(0:0), ModelDFSLt1_VMN5.5)
  ModelDFSLt1_VMN5.7 <- rbind(c(0:0), ModelDFSLt1_VMN5.6)
  ModelDFSLt1_VMN5.8 <- rbind(c(0:0), ModelDFSLt1_VMN5.7)
  ModelDFSLt1_VMN5.9 <- rbind(c(0:0), ModelDFSLt1_VMN5.8)
  ModelDFSLt1_VMN5.10 <- rbind(c(0:0), ModelDFSLt1_VMN5.9)
  ModelDFSLt1_VMN5.11 <- rbind(c(0:0), ModelDFSLt1_VMN5.10)
  ModelDFSLt1_VMN5.12 <- rbind(c(0:0), ModelDFSLt1_VMN5.11)
  ModelDFSLt1_VMN5.13 <- ModelDFSLt1_VMN5.12[-nrow(ModelDFSLt1_VMN5.12),]
  ModelDFSLt1_VMN5.14 <- ModelDFSLt1_VMN5.13[-nrow(ModelDFSLt1_VMN5.13),]
  ModelDFSLt1_VMN5.15 <- ModelDFSLt1_VMN5.14[-nrow(ModelDFSLt1_VMN5.14),]
  ModelDFSLt1_VMN5.16 <- ModelDFSLt1_VMN5.15[-nrow(ModelDFSLt1_VMN5.15),]
  ModelDFSLt1_VMN5.17 <- ModelDFSLt1_VMN5.16[-nrow(ModelDFSLt1_VMN5.16),]
  ModelDFSLt1_VMN5.18 <- ModelDFSLt1_VMN5.17[-nrow(ModelDFSLt1_VMN5.17),]
  ModelDFSLt1_VMN5.19 <- ModelDFSLt1_VMN5.18[-nrow(ModelDFSLt1_VMN5.18),]
  ModelDFSLt1_VMN5.20 <- ModelDFSLt1_VMN5.19[-nrow(ModelDFSLt1_VMN5.19),]
  ModelDFSLt1_VMN5.21 <- ModelDFSLt1_VMN5.20[-nrow(ModelDFSLt1_VMN5.20),]
  ModelDFSLt1_VMN5.22 <- ModelDFSLt1_VMN5.21[-nrow(ModelDFSLt1_VMN5.21),]
  ModelDFSLt1_VMN5.23 <- ModelDFSLt1_VMN5.22[-nrow(ModelDFSLt1_VMN5.22),]
  ModelDFSLt1_VMN5.24 <- ModelDFSLt1_VMN5.23[-nrow(ModelDFSLt1_VMN5.23),]

  ModelDFSLt1_VMN5 <- ModelDFSLt1_VMN5.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN5 <- ModelDFSLt0_VMN5 - ModelDFSLt1_VMN5

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN5$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN5P because implies a one-off input of C
  ModelDFSL_VMN5P <- ModelDFSL1y_VMN5

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN5P,"VXP_Models\\ModelDFSL_R_VMN5P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN5 <- ggplot(ModelDFSL_VMN5P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN5, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN5, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN5, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN5, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN5, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN5, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN5, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN5
  ggplotly(P_CFluxI1y_VMN5)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN5P_YCO2 <- ModelDFSL_VMN5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN5P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN5")

  PA_CO21y_VMN5 <- ggplot(ModelDFSL_VMN5P_YCO2, aes(x = Year, y = AnnualCO2_VMN5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN5
  ggplotly(PA_CO21y_VMN5)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN5P_YCO2D <- merge(ModelDFSL_VMN5P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN5P_YCO2D$AnnualCO2D_VMN5 <- ModelDFSL_VMN5P_YCO2D$AnnualCO2_VMN5 * ModelDFSL_VMN5P_YCO2D$GWP100

  PA_CO21yD_VMN5 <- ggplot(ModelDFSL_VMN5P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN5
  ggplotly(PA_CO21yD_VMN5)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN5P_YC <- ModelDFSL_VMN5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN5P_YC) <- c("Year", "Months", "AnnualCTail_VMN5")

  PA_C1y_VMN5 <- ggplot(ModelDFSL_VMN5P_YC, aes(x = Year, y = AnnualCTail_VMN5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN5
  ggplotly(PA_C1y_VMN5)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN5P_YCT <- ModelDFSL_VMN5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN5)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN5P_YCT) <- c("Year", "Months", "AnnualCTail_VMN5")

  PA_CT1y_VMN5 <- ggplot(ModelDFSL_VMN5P_YCT, aes(x = Year, y = AnnualCTail_VMN5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN5
  ggplotly(PA_CT1y_VMN5)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN5P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN5P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN5P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN5P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN5P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN5P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN5P_YCT,"CTails_P\\ModelDFSL_R_C_VMN5P.xlsx") #Yearly C emissions



  #### 12.7 - VMN6) Manure; 60%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN6=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN6=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.6,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN6=data.frame(years,rep(fT_VMN6*fW_VMN6,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN6=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.6, #Percent clay in mineral soil
    xi=xi.frame_VMN6) #Loads the model

  Ct_VMN6=getC(Model_VMN6) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN6, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN6 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN6 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN6 <- as.data.frame(VEC_Lit_VMN6) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN6$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN6 <- as.data.frame(VEC_Man_VMN6) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN6$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN6, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN6, class) #Check that class is numeric
  LitterCinputs_VMN6=VEC_LitDF_VMN6   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN6=VEC_ManDF_VMN6 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN6 <- merge(LitterCinputs_VMN6, ManureCinputs_VMN6, by = "MNumber")
  MCinputs_VMN6$MInput_VMN6 <- MCinputs_VMN6$VEC_Lit_VMN6 + MCinputs_VMN6$VEC_Man_VMN6

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN6)[which(names(MCinputs_VMN6) == "VEC_Lit_VMN6")] <- "LitterC_VMN6"
  colnames(MCinputs_VMN6)[which(names(MCinputs_VMN6) == "VEC_Man_VMN6")] <- "ManureC_VMN6"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN6 <- as.data.frame(Ct_VMN6)
  colnames(ModelDF_VMN6) <- c('DPM_VMN6','RPM_VMN6','BIO_VMN6', 'HUM_VMN6', 'IOM_VMN6')
  ModelDFS_VMN6 <- ModelDF_VMN6[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN6$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN6$AllPools_VMN6 <- ModelDFS_VMN6$DPM_VMN6 + ModelDFS_VMN6$RPM_VMN6 + ModelDFS_VMN6$BIO_VMN6 + ModelDFS_VMN6$HUM_VMN6 + ModelDFS_VMN6$IOM_VMN6

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN6$AllPools_noIOM_VMN6 <- ModelDFS_VMN6$DPM_VMN6 + ModelDFS_VMN6$RPM_VMN6 + ModelDFS_VMN6$BIO_VMN6 + ModelDFS_VMN6$HUM_VMN6

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN6 <- merge(ModelDFS_VMN6, MCinputs_VMN6, by = "MNumber")

  ModelDFSL_VMN6$MInput_VMN6 <- ModelDFSL_VMN6$LitterC_VMN6 + ModelDFSL_VMN6$ManureC_VMN6
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN6$CTails_VMN6 <- ModelDFSL_VMN6$AllPools_noIOM_VMN6 + ModelDFSL_VMN6$MInput_VMN6

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN6$AccumInput_VMN6 = ModelDFSL_VMN6$AccumInput_VMN6=cumsum(ModelDFSL_VMN6$MInput_VMN6)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN6$MGrowth_DPM_VMN6 <- ave(ModelDFSL_VMN6$DPM_VMN6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN6$MGrowth_RPM_VMN6 <- ave(ModelDFSL_VMN6$RPM_VMN6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN6$MGrowth_BIO_VMN6 <- ave(ModelDFSL_VMN6$BIO_VMN6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN6$MGrowth_HUM_VMN6 <- ave(ModelDFSL_VMN6$HUM_VMN6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN6$MGrowth_IOM_VMN6 <- ave(ModelDFSL_VMN6$IOM_VMN6, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN6$M_CCO2_VMN6 <- ModelDFSL_VMN6$MInput_VMN6 - ModelDFSL_VMN6$MGrowth_DPM_VMN6 - ModelDFSL_VMN6$MGrowth_RPM_VMN6 - ModelDFSL_VMN6$MGrowth_BIO_VMN6 - ModelDFSL_VMN6$MGrowth_HUM_VMN6
  ModelDFSL_VMN6$Accum_CCO2_VMN6 <- ModelDFSL_VMN6$AccumInput_VMN6 - ModelDFSL_VMN6$AllPools_noIOM_VMN6

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN6$M_CCO2_VMN6[1] <- 0
  ModelDFSL_VMN6$Accum_CCO2_VMN6[1] <- 0

  #Balance validation
  ModelDFSL_VMN6$Balance_VMN6 <- ModelDFSL_VMN6$AccumInput_VMN6 - ModelDFSL_VMN6$Accum_CCO2_VMN6 - (ModelDFSL_VMN6$DPM_VMN6 + ModelDFSL_VMN6$RPM_VMN6 + ModelDFSL_VMN6$BIO_VMN6 + ModelDFSL_VMN6$HUM_VMN6)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN6$M_CO2_VMN6 <- ModelDFSL_VMN6$M_CCO2_VMN6 * 44/12
  ModelDFSL_VMN6$Accum_CO2_VMN6 <- ModelDFSL_VMN6$Accum_CCO2_VMN6 * 44/12

  #This model will be called VMN6C because implies a continuous input of C
  ModelDFSL_VMN6C <- ModelDFSL_VMN6

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN6C,"VXC_Models\\ModelDFSL_R_VMN6C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN6 <- ModelDFSL_VMN6 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN6.1 <- rbind(c(0:0), ModelDFSLt0_VMN6)
  ModelDFSLt1_VMN6.2 <- rbind(c(0:0), ModelDFSLt1_VMN6.1)
  ModelDFSLt1_VMN6.3 <- rbind(c(0:0), ModelDFSLt1_VMN6.2)
  ModelDFSLt1_VMN6.4 <- rbind(c(0:0), ModelDFSLt1_VMN6.3)
  ModelDFSLt1_VMN6.5 <- rbind(c(0:0), ModelDFSLt1_VMN6.4)
  ModelDFSLt1_VMN6.6 <- rbind(c(0:0), ModelDFSLt1_VMN6.5)
  ModelDFSLt1_VMN6.7 <- rbind(c(0:0), ModelDFSLt1_VMN6.6)
  ModelDFSLt1_VMN6.8 <- rbind(c(0:0), ModelDFSLt1_VMN6.7)
  ModelDFSLt1_VMN6.9 <- rbind(c(0:0), ModelDFSLt1_VMN6.8)
  ModelDFSLt1_VMN6.10 <- rbind(c(0:0), ModelDFSLt1_VMN6.9)
  ModelDFSLt1_VMN6.11 <- rbind(c(0:0), ModelDFSLt1_VMN6.10)
  ModelDFSLt1_VMN6.12 <- rbind(c(0:0), ModelDFSLt1_VMN6.11)
  ModelDFSLt1_VMN6.13 <- ModelDFSLt1_VMN6.12[-nrow(ModelDFSLt1_VMN6.12),]
  ModelDFSLt1_VMN6.14 <- ModelDFSLt1_VMN6.13[-nrow(ModelDFSLt1_VMN6.13),]
  ModelDFSLt1_VMN6.15 <- ModelDFSLt1_VMN6.14[-nrow(ModelDFSLt1_VMN6.14),]
  ModelDFSLt1_VMN6.16 <- ModelDFSLt1_VMN6.15[-nrow(ModelDFSLt1_VMN6.15),]
  ModelDFSLt1_VMN6.17 <- ModelDFSLt1_VMN6.16[-nrow(ModelDFSLt1_VMN6.16),]
  ModelDFSLt1_VMN6.18 <- ModelDFSLt1_VMN6.17[-nrow(ModelDFSLt1_VMN6.17),]
  ModelDFSLt1_VMN6.19 <- ModelDFSLt1_VMN6.18[-nrow(ModelDFSLt1_VMN6.18),]
  ModelDFSLt1_VMN6.20 <- ModelDFSLt1_VMN6.19[-nrow(ModelDFSLt1_VMN6.19),]
  ModelDFSLt1_VMN6.21 <- ModelDFSLt1_VMN6.20[-nrow(ModelDFSLt1_VMN6.20),]
  ModelDFSLt1_VMN6.22 <- ModelDFSLt1_VMN6.21[-nrow(ModelDFSLt1_VMN6.21),]
  ModelDFSLt1_VMN6.23 <- ModelDFSLt1_VMN6.22[-nrow(ModelDFSLt1_VMN6.22),]
  ModelDFSLt1_VMN6.24 <- ModelDFSLt1_VMN6.23[-nrow(ModelDFSLt1_VMN6.23),]

  ModelDFSLt1_VMN6 <- ModelDFSLt1_VMN6.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN6 <- ModelDFSLt0_VMN6 - ModelDFSLt1_VMN6

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN6$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN6P because implies a one-off input of C
  ModelDFSL_VMN6P <- ModelDFSL1y_VMN6

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN6P,"VXP_Models\\ModelDFSL_R_VMN6P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN6 <- ggplot(ModelDFSL_VMN6P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN6, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN6, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN6, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN6, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN6, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN6, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN6, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN6
  ggplotly(P_CFluxI1y_VMN6)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN6P_YCO2 <- ModelDFSL_VMN6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN6P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN6")

  PA_CO21y_VMN6 <- ggplot(ModelDFSL_VMN6P_YCO2, aes(x = Year, y = AnnualCO2_VMN6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN6
  ggplotly(PA_CO21y_VMN6)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN6P_YCO2D <- merge(ModelDFSL_VMN6P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN6P_YCO2D$AnnualCO2D_VMN6 <- ModelDFSL_VMN6P_YCO2D$AnnualCO2_VMN6 * ModelDFSL_VMN6P_YCO2D$GWP100

  PA_CO21yD_VMN6 <- ggplot(ModelDFSL_VMN6P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN6
  ggplotly(PA_CO21yD_VMN6)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN6P_YC <- ModelDFSL_VMN6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN6P_YC) <- c("Year", "Months", "AnnualCTail_VMN6")

  PA_C1y_VMN6 <- ggplot(ModelDFSL_VMN6P_YC, aes(x = Year, y = AnnualCTail_VMN6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN6
  ggplotly(PA_C1y_VMN6)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN6P_YCT <- ModelDFSL_VMN6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN6)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN6P_YCT) <- c("Year", "Months", "AnnualCTail_VMN6")

  PA_CT1y_VMN6 <- ggplot(ModelDFSL_VMN6P_YCT, aes(x = Year, y = AnnualCTail_VMN6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN6
  ggplotly(PA_CT1y_VMN6)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN6P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN6P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN6P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN6P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN6P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN6P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN6P_YCT,"CTails_P\\ModelDFSL_R_C_VMN6P.xlsx") #Yearly C emissions



  #### 12.8 - VMN7) Manure; 70%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN7=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN7=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.7,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN7=data.frame(years,rep(fT_VMN7*fW_VMN7,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN7=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.7, #Percent clay in mineral soil
    xi=xi.frame_VMN7) #Loads the model

  Ct_VMN7=getC(Model_VMN7) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN7, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN7 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN7 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN7 <- as.data.frame(VEC_Lit_VMN7) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN7$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN7 <- as.data.frame(VEC_Man_VMN7) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN7$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN7, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN7, class) #Check that class is numeric
  LitterCinputs_VMN7=VEC_LitDF_VMN7   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN7=VEC_ManDF_VMN7 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN7 <- merge(LitterCinputs_VMN7, ManureCinputs_VMN7, by = "MNumber")
  MCinputs_VMN7$MInput_VMN7 <- MCinputs_VMN7$VEC_Lit_VMN7 + MCinputs_VMN7$VEC_Man_VMN7

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN7)[which(names(MCinputs_VMN7) == "VEC_Lit_VMN7")] <- "LitterC_VMN7"
  colnames(MCinputs_VMN7)[which(names(MCinputs_VMN7) == "VEC_Man_VMN7")] <- "ManureC_VMN7"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN7 <- as.data.frame(Ct_VMN7)
  colnames(ModelDF_VMN7) <- c('DPM_VMN7','RPM_VMN7','BIO_VMN7', 'HUM_VMN7', 'IOM_VMN7')
  ModelDFS_VMN7 <- ModelDF_VMN7[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN7$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN7$AllPools_VMN7 <- ModelDFS_VMN7$DPM_VMN7 + ModelDFS_VMN7$RPM_VMN7 + ModelDFS_VMN7$BIO_VMN7 + ModelDFS_VMN7$HUM_VMN7 + ModelDFS_VMN7$IOM_VMN7

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN7$AllPools_noIOM_VMN7 <- ModelDFS_VMN7$DPM_VMN7 + ModelDFS_VMN7$RPM_VMN7 + ModelDFS_VMN7$BIO_VMN7 + ModelDFS_VMN7$HUM_VMN7

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN7 <- merge(ModelDFS_VMN7, MCinputs_VMN7, by = "MNumber")

  ModelDFSL_VMN7$MInput_VMN7 <- ModelDFSL_VMN7$LitterC_VMN7 + ModelDFSL_VMN7$ManureC_VMN7
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN7$CTails_VMN7 <- ModelDFSL_VMN7$AllPools_noIOM_VMN7 + ModelDFSL_VMN7$MInput_VMN7

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN7$AccumInput_VMN7 = ModelDFSL_VMN7$AccumInput_VMN7=cumsum(ModelDFSL_VMN7$MInput_VMN7)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN7$MGrowth_DPM_VMN7 <- ave(ModelDFSL_VMN7$DPM_VMN7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN7$MGrowth_RPM_VMN7 <- ave(ModelDFSL_VMN7$RPM_VMN7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN7$MGrowth_BIO_VMN7 <- ave(ModelDFSL_VMN7$BIO_VMN7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN7$MGrowth_HUM_VMN7 <- ave(ModelDFSL_VMN7$HUM_VMN7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN7$MGrowth_IOM_VMN7 <- ave(ModelDFSL_VMN7$IOM_VMN7, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN7$M_CCO2_VMN7 <- ModelDFSL_VMN7$MInput_VMN7 - ModelDFSL_VMN7$MGrowth_DPM_VMN7 - ModelDFSL_VMN7$MGrowth_RPM_VMN7 - ModelDFSL_VMN7$MGrowth_BIO_VMN7 - ModelDFSL_VMN7$MGrowth_HUM_VMN7
  ModelDFSL_VMN7$Accum_CCO2_VMN7 <- ModelDFSL_VMN7$AccumInput_VMN7 - ModelDFSL_VMN7$AllPools_noIOM_VMN7

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN7$M_CCO2_VMN7[1] <- 0
  ModelDFSL_VMN7$Accum_CCO2_VMN7[1] <- 0

  #Balance validation
  ModelDFSL_VMN7$Balance_VMN7 <- ModelDFSL_VMN7$AccumInput_VMN7 - ModelDFSL_VMN7$Accum_CCO2_VMN7 - (ModelDFSL_VMN7$DPM_VMN7 + ModelDFSL_VMN7$RPM_VMN7 + ModelDFSL_VMN7$BIO_VMN7 + ModelDFSL_VMN7$HUM_VMN7)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN7$M_CO2_VMN7 <- ModelDFSL_VMN7$M_CCO2_VMN7 * 44/12
  ModelDFSL_VMN7$Accum_CO2_VMN7 <- ModelDFSL_VMN7$Accum_CCO2_VMN7 * 44/12

  #This model will be called VMN7C because implies a continuous input of C
  ModelDFSL_VMN7C <- ModelDFSL_VMN7

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN7C,"VXC_Models\\ModelDFSL_R_VMN7C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN7 <- ModelDFSL_VMN7 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN7.1 <- rbind(c(0:0), ModelDFSLt0_VMN7)
  ModelDFSLt1_VMN7.2 <- rbind(c(0:0), ModelDFSLt1_VMN7.1)
  ModelDFSLt1_VMN7.3 <- rbind(c(0:0), ModelDFSLt1_VMN7.2)
  ModelDFSLt1_VMN7.4 <- rbind(c(0:0), ModelDFSLt1_VMN7.3)
  ModelDFSLt1_VMN7.5 <- rbind(c(0:0), ModelDFSLt1_VMN7.4)
  ModelDFSLt1_VMN7.6 <- rbind(c(0:0), ModelDFSLt1_VMN7.5)
  ModelDFSLt1_VMN7.7 <- rbind(c(0:0), ModelDFSLt1_VMN7.6)
  ModelDFSLt1_VMN7.8 <- rbind(c(0:0), ModelDFSLt1_VMN7.7)
  ModelDFSLt1_VMN7.9 <- rbind(c(0:0), ModelDFSLt1_VMN7.8)
  ModelDFSLt1_VMN7.10 <- rbind(c(0:0), ModelDFSLt1_VMN7.9)
  ModelDFSLt1_VMN7.11 <- rbind(c(0:0), ModelDFSLt1_VMN7.10)
  ModelDFSLt1_VMN7.12 <- rbind(c(0:0), ModelDFSLt1_VMN7.11)
  ModelDFSLt1_VMN7.13 <- ModelDFSLt1_VMN7.12[-nrow(ModelDFSLt1_VMN7.12),]
  ModelDFSLt1_VMN7.14 <- ModelDFSLt1_VMN7.13[-nrow(ModelDFSLt1_VMN7.13),]
  ModelDFSLt1_VMN7.15 <- ModelDFSLt1_VMN7.14[-nrow(ModelDFSLt1_VMN7.14),]
  ModelDFSLt1_VMN7.16 <- ModelDFSLt1_VMN7.15[-nrow(ModelDFSLt1_VMN7.15),]
  ModelDFSLt1_VMN7.17 <- ModelDFSLt1_VMN7.16[-nrow(ModelDFSLt1_VMN7.16),]
  ModelDFSLt1_VMN7.18 <- ModelDFSLt1_VMN7.17[-nrow(ModelDFSLt1_VMN7.17),]
  ModelDFSLt1_VMN7.19 <- ModelDFSLt1_VMN7.18[-nrow(ModelDFSLt1_VMN7.18),]
  ModelDFSLt1_VMN7.20 <- ModelDFSLt1_VMN7.19[-nrow(ModelDFSLt1_VMN7.19),]
  ModelDFSLt1_VMN7.21 <- ModelDFSLt1_VMN7.20[-nrow(ModelDFSLt1_VMN7.20),]
  ModelDFSLt1_VMN7.22 <- ModelDFSLt1_VMN7.21[-nrow(ModelDFSLt1_VMN7.21),]
  ModelDFSLt1_VMN7.23 <- ModelDFSLt1_VMN7.22[-nrow(ModelDFSLt1_VMN7.22),]
  ModelDFSLt1_VMN7.24 <- ModelDFSLt1_VMN7.23[-nrow(ModelDFSLt1_VMN7.23),]

  ModelDFSLt1_VMN7 <- ModelDFSLt1_VMN7.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN7 <- ModelDFSLt0_VMN7 - ModelDFSLt1_VMN7

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN7$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN7P because implies a one-off input of C
  ModelDFSL_VMN7P <- ModelDFSL1y_VMN7

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN7P,"VXP_Models\\ModelDFSL_R_VMN7P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN7 <- ggplot(ModelDFSL_VMN7P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN7, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN7, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN7, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN7, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN7, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN7, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN7, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN7
  ggplotly(P_CFluxI1y_VMN7)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN7P_YCO2 <- ModelDFSL_VMN7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN7P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN7")

  PA_CO21y_VMN7 <- ggplot(ModelDFSL_VMN7P_YCO2, aes(x = Year, y = AnnualCO2_VMN7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN7
  ggplotly(PA_CO21y_VMN7)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN7P_YCO2D <- merge(ModelDFSL_VMN7P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN7P_YCO2D$AnnualCO2D_VMN7 <- ModelDFSL_VMN7P_YCO2D$AnnualCO2_VMN7 * ModelDFSL_VMN7P_YCO2D$GWP100

  PA_CO21yD_VMN7 <- ggplot(ModelDFSL_VMN7P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN7
  ggplotly(PA_CO21yD_VMN7)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN7P_YC <- ModelDFSL_VMN7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN7P_YC) <- c("Year", "Months", "AnnualCTail_VMN7")

  PA_C1y_VMN7 <- ggplot(ModelDFSL_VMN7P_YC, aes(x = Year, y = AnnualCTail_VMN7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN7
  ggplotly(PA_C1y_VMN7)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN7P_YCT <- ModelDFSL_VMN7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN7)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN7P_YCT) <- c("Year", "Months", "AnnualCTail_VMN7")

  PA_CT1y_VMN7 <- ggplot(ModelDFSL_VMN7P_YCT, aes(x = Year, y = AnnualCTail_VMN7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN7
  ggplotly(PA_CT1y_VMN7)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN7P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN7P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN7P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN7P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN7P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN7P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN7P_YCT,"CTails_P\\ModelDFSL_R_C_VMN7P.xlsx") #Yearly C emissions



  #### 12.9 - VMN8) Manure; 80%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN8=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN8=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.8,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN8=data.frame(years,rep(fT_VMN8*fW_VMN8,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN8=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.8, #Percent clay in mineral soil
    xi=xi.frame_VMN8) #Loads the model

  Ct_VMN8=getC(Model_VMN8) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN8, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN8 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN8 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN8 <- as.data.frame(VEC_Lit_VMN8) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN8$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN8 <- as.data.frame(VEC_Man_VMN8) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN8$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN8, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN8, class) #Check that class is numeric
  LitterCinputs_VMN8=VEC_LitDF_VMN8   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN8=VEC_ManDF_VMN8 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN8 <- merge(LitterCinputs_VMN8, ManureCinputs_VMN8, by = "MNumber")
  MCinputs_VMN8$MInput_VMN8 <- MCinputs_VMN8$VEC_Lit_VMN8 + MCinputs_VMN8$VEC_Man_VMN8

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN8)[which(names(MCinputs_VMN8) == "VEC_Lit_VMN8")] <- "LitterC_VMN8"
  colnames(MCinputs_VMN8)[which(names(MCinputs_VMN8) == "VEC_Man_VMN8")] <- "ManureC_VMN8"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN8 <- as.data.frame(Ct_VMN8)
  colnames(ModelDF_VMN8) <- c('DPM_VMN8','RPM_VMN8','BIO_VMN8', 'HUM_VMN8', 'IOM_VMN8')
  ModelDFS_VMN8 <- ModelDF_VMN8[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN8$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN8$AllPools_VMN8 <- ModelDFS_VMN8$DPM_VMN8 + ModelDFS_VMN8$RPM_VMN8 + ModelDFS_VMN8$BIO_VMN8 + ModelDFS_VMN8$HUM_VMN8 + ModelDFS_VMN8$IOM_VMN8

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN8$AllPools_noIOM_VMN8 <- ModelDFS_VMN8$DPM_VMN8 + ModelDFS_VMN8$RPM_VMN8 + ModelDFS_VMN8$BIO_VMN8 + ModelDFS_VMN8$HUM_VMN8

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN8 <- merge(ModelDFS_VMN8, MCinputs_VMN8, by = "MNumber")

  ModelDFSL_VMN8$MInput_VMN8 <- ModelDFSL_VMN8$LitterC_VMN8 + ModelDFSL_VMN8$ManureC_VMN8
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN8$CTails_VMN8 <- ModelDFSL_VMN8$AllPools_noIOM_VMN8 + ModelDFSL_VMN8$MInput_VMN8

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN8$AccumInput_VMN8 = ModelDFSL_VMN8$AccumInput_VMN8=cumsum(ModelDFSL_VMN8$MInput_VMN8)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN8$MGrowth_DPM_VMN8 <- ave(ModelDFSL_VMN8$DPM_VMN8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN8$MGrowth_RPM_VMN8 <- ave(ModelDFSL_VMN8$RPM_VMN8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN8$MGrowth_BIO_VMN8 <- ave(ModelDFSL_VMN8$BIO_VMN8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN8$MGrowth_HUM_VMN8 <- ave(ModelDFSL_VMN8$HUM_VMN8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN8$MGrowth_IOM_VMN8 <- ave(ModelDFSL_VMN8$IOM_VMN8, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN8$M_CCO2_VMN8 <- ModelDFSL_VMN8$MInput_VMN8 - ModelDFSL_VMN8$MGrowth_DPM_VMN8 - ModelDFSL_VMN8$MGrowth_RPM_VMN8 - ModelDFSL_VMN8$MGrowth_BIO_VMN8 - ModelDFSL_VMN8$MGrowth_HUM_VMN8
  ModelDFSL_VMN8$Accum_CCO2_VMN8 <- ModelDFSL_VMN8$AccumInput_VMN8 - ModelDFSL_VMN8$AllPools_noIOM_VMN8

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN8$M_CCO2_VMN8[1] <- 0
  ModelDFSL_VMN8$Accum_CCO2_VMN8[1] <- 0

  #Balance validation
  ModelDFSL_VMN8$Balance_VMN8 <- ModelDFSL_VMN8$AccumInput_VMN8 - ModelDFSL_VMN8$Accum_CCO2_VMN8 - (ModelDFSL_VMN8$DPM_VMN8 + ModelDFSL_VMN8$RPM_VMN8 + ModelDFSL_VMN8$BIO_VMN8 + ModelDFSL_VMN8$HUM_VMN8)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN8$M_CO2_VMN8 <- ModelDFSL_VMN8$M_CCO2_VMN8 * 44/12
  ModelDFSL_VMN8$Accum_CO2_VMN8 <- ModelDFSL_VMN8$Accum_CCO2_VMN8 * 44/12

  #This model will be called VMN8C because implies a continuous input of C
  ModelDFSL_VMN8C <- ModelDFSL_VMN8

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN8C,"VXC_Models\\ModelDFSL_R_VMN8C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN8 <- ModelDFSL_VMN8 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN8.1 <- rbind(c(0:0), ModelDFSLt0_VMN8)
  ModelDFSLt1_VMN8.2 <- rbind(c(0:0), ModelDFSLt1_VMN8.1)
  ModelDFSLt1_VMN8.3 <- rbind(c(0:0), ModelDFSLt1_VMN8.2)
  ModelDFSLt1_VMN8.4 <- rbind(c(0:0), ModelDFSLt1_VMN8.3)
  ModelDFSLt1_VMN8.5 <- rbind(c(0:0), ModelDFSLt1_VMN8.4)
  ModelDFSLt1_VMN8.6 <- rbind(c(0:0), ModelDFSLt1_VMN8.5)
  ModelDFSLt1_VMN8.7 <- rbind(c(0:0), ModelDFSLt1_VMN8.6)
  ModelDFSLt1_VMN8.8 <- rbind(c(0:0), ModelDFSLt1_VMN8.7)
  ModelDFSLt1_VMN8.9 <- rbind(c(0:0), ModelDFSLt1_VMN8.8)
  ModelDFSLt1_VMN8.10 <- rbind(c(0:0), ModelDFSLt1_VMN8.9)
  ModelDFSLt1_VMN8.11 <- rbind(c(0:0), ModelDFSLt1_VMN8.10)
  ModelDFSLt1_VMN8.12 <- rbind(c(0:0), ModelDFSLt1_VMN8.11)
  ModelDFSLt1_VMN8.13 <- ModelDFSLt1_VMN8.12[-nrow(ModelDFSLt1_VMN8.12),]
  ModelDFSLt1_VMN8.14 <- ModelDFSLt1_VMN8.13[-nrow(ModelDFSLt1_VMN8.13),]
  ModelDFSLt1_VMN8.15 <- ModelDFSLt1_VMN8.14[-nrow(ModelDFSLt1_VMN8.14),]
  ModelDFSLt1_VMN8.16 <- ModelDFSLt1_VMN8.15[-nrow(ModelDFSLt1_VMN8.15),]
  ModelDFSLt1_VMN8.17 <- ModelDFSLt1_VMN8.16[-nrow(ModelDFSLt1_VMN8.16),]
  ModelDFSLt1_VMN8.18 <- ModelDFSLt1_VMN8.17[-nrow(ModelDFSLt1_VMN8.17),]
  ModelDFSLt1_VMN8.19 <- ModelDFSLt1_VMN8.18[-nrow(ModelDFSLt1_VMN8.18),]
  ModelDFSLt1_VMN8.20 <- ModelDFSLt1_VMN8.19[-nrow(ModelDFSLt1_VMN8.19),]
  ModelDFSLt1_VMN8.21 <- ModelDFSLt1_VMN8.20[-nrow(ModelDFSLt1_VMN8.20),]
  ModelDFSLt1_VMN8.22 <- ModelDFSLt1_VMN8.21[-nrow(ModelDFSLt1_VMN8.21),]
  ModelDFSLt1_VMN8.23 <- ModelDFSLt1_VMN8.22[-nrow(ModelDFSLt1_VMN8.22),]
  ModelDFSLt1_VMN8.24 <- ModelDFSLt1_VMN8.23[-nrow(ModelDFSLt1_VMN8.23),]

  ModelDFSLt1_VMN8 <- ModelDFSLt1_VMN8.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN8 <- ModelDFSLt0_VMN8 - ModelDFSLt1_VMN8

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN8$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN8P because implies a one-off input of C
  ModelDFSL_VMN8P <- ModelDFSL1y_VMN8

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN8P,"VXP_Models\\ModelDFSL_R_VMN8P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN8 <- ggplot(ModelDFSL_VMN8P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN8, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN8, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN8, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN8, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN8, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN8, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN8, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN8
  ggplotly(P_CFluxI1y_VMN8)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN8P_YCO2 <- ModelDFSL_VMN8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN8P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN8")

  PA_CO21y_VMN8 <- ggplot(ModelDFSL_VMN8P_YCO2, aes(x = Year, y = AnnualCO2_VMN8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN8
  ggplotly(PA_CO21y_VMN8)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN8P_YCO2D <- merge(ModelDFSL_VMN8P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN8P_YCO2D$AnnualCO2D_VMN8 <- ModelDFSL_VMN8P_YCO2D$AnnualCO2_VMN8 * ModelDFSL_VMN8P_YCO2D$GWP100

  PA_CO21yD_VMN8 <- ggplot(ModelDFSL_VMN8P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN8
  ggplotly(PA_CO21yD_VMN8)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN8P_YC <- ModelDFSL_VMN8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN8P_YC) <- c("Year", "Months", "AnnualCTail_VMN8")

  PA_C1y_VMN8 <- ggplot(ModelDFSL_VMN8P_YC, aes(x = Year, y = AnnualCTail_VMN8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN8
  ggplotly(PA_C1y_VMN8)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN8P_YCT <- ModelDFSL_VMN8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN8)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN8P_YCT) <- c("Year", "Months", "AnnualCTail_VMN8")

  PA_CT1y_VMN8 <- ggplot(ModelDFSL_VMN8P_YCT, aes(x = Year, y = AnnualCTail_VMN8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN8
  ggplotly(PA_CT1y_VMN8)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN8P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN8P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN8P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN8P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN8P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN8P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN8P_YCT,"CTails_P\\ModelDFSL_R_C_VMN8P.xlsx") #Yearly C emissions



  #### 12.10 - VMN9) Manure; 90%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN9=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN9=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.9,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN9=data.frame(years,rep(fT_VMN9*fW_VMN9,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN9=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.9, #Percent clay in mineral soil
    xi=xi.frame_VMN9) #Loads the model

  Ct_VMN9=getC(Model_VMN9) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN9, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN9 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN9 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN9 <- as.data.frame(VEC_Lit_VMN9) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN9$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN9 <- as.data.frame(VEC_Man_VMN9) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN9$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN9, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN9, class) #Check that class is numeric
  LitterCinputs_VMN9=VEC_LitDF_VMN9   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN9=VEC_ManDF_VMN9 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN9 <- merge(LitterCinputs_VMN9, ManureCinputs_VMN9, by = "MNumber")
  MCinputs_VMN9$MInput_VMN9 <- MCinputs_VMN9$VEC_Lit_VMN9 + MCinputs_VMN9$VEC_Man_VMN9

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN9)[which(names(MCinputs_VMN9) == "VEC_Lit_VMN9")] <- "LitterC_VMN9"
  colnames(MCinputs_VMN9)[which(names(MCinputs_VMN9) == "VEC_Man_VMN9")] <- "ManureC_VMN9"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN9 <- as.data.frame(Ct_VMN9)
  colnames(ModelDF_VMN9) <- c('DPM_VMN9','RPM_VMN9','BIO_VMN9', 'HUM_VMN9', 'IOM_VMN9')
  ModelDFS_VMN9 <- ModelDF_VMN9[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN9$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN9$AllPools_VMN9 <- ModelDFS_VMN9$DPM_VMN9 + ModelDFS_VMN9$RPM_VMN9 + ModelDFS_VMN9$BIO_VMN9 + ModelDFS_VMN9$HUM_VMN9 + ModelDFS_VMN9$IOM_VMN9

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN9$AllPools_noIOM_VMN9 <- ModelDFS_VMN9$DPM_VMN9 + ModelDFS_VMN9$RPM_VMN9 + ModelDFS_VMN9$BIO_VMN9 + ModelDFS_VMN9$HUM_VMN9

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN9 <- merge(ModelDFS_VMN9, MCinputs_VMN9, by = "MNumber")

  ModelDFSL_VMN9$MInput_VMN9 <- ModelDFSL_VMN9$LitterC_VMN9 + ModelDFSL_VMN9$ManureC_VMN9
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN9$CTails_VMN9 <- ModelDFSL_VMN9$AllPools_noIOM_VMN9 + ModelDFSL_VMN9$MInput_VMN9

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN9$AccumInput_VMN9 = ModelDFSL_VMN9$AccumInput_VMN9=cumsum(ModelDFSL_VMN9$MInput_VMN9)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN9$MGrowth_DPM_VMN9 <- ave(ModelDFSL_VMN9$DPM_VMN9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN9$MGrowth_RPM_VMN9 <- ave(ModelDFSL_VMN9$RPM_VMN9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN9$MGrowth_BIO_VMN9 <- ave(ModelDFSL_VMN9$BIO_VMN9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN9$MGrowth_HUM_VMN9 <- ave(ModelDFSL_VMN9$HUM_VMN9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN9$MGrowth_IOM_VMN9 <- ave(ModelDFSL_VMN9$IOM_VMN9, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN9$M_CCO2_VMN9 <- ModelDFSL_VMN9$MInput_VMN9 - ModelDFSL_VMN9$MGrowth_DPM_VMN9 - ModelDFSL_VMN9$MGrowth_RPM_VMN9 - ModelDFSL_VMN9$MGrowth_BIO_VMN9 - ModelDFSL_VMN9$MGrowth_HUM_VMN9
  ModelDFSL_VMN9$Accum_CCO2_VMN9 <- ModelDFSL_VMN9$AccumInput_VMN9 - ModelDFSL_VMN9$AllPools_noIOM_VMN9

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN9$M_CCO2_VMN9[1] <- 0
  ModelDFSL_VMN9$Accum_CCO2_VMN9[1] <- 0

  #Balance validation
  ModelDFSL_VMN9$Balance_VMN9 <- ModelDFSL_VMN9$AccumInput_VMN9 - ModelDFSL_VMN9$Accum_CCO2_VMN9 - (ModelDFSL_VMN9$DPM_VMN9 + ModelDFSL_VMN9$RPM_VMN9 + ModelDFSL_VMN9$BIO_VMN9 + ModelDFSL_VMN9$HUM_VMN9)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN9$M_CO2_VMN9 <- ModelDFSL_VMN9$M_CCO2_VMN9 * 44/12
  ModelDFSL_VMN9$Accum_CO2_VMN9 <- ModelDFSL_VMN9$Accum_CCO2_VMN9 * 44/12

  #This model will be called VMN9C because implies a continuous input of C
  ModelDFSL_VMN9C <- ModelDFSL_VMN9

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN9C,"VXC_Models\\ModelDFSL_R_VMN9C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN9 <- ModelDFSL_VMN9 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN9.1 <- rbind(c(0:0), ModelDFSLt0_VMN9)
  ModelDFSLt1_VMN9.2 <- rbind(c(0:0), ModelDFSLt1_VMN9.1)
  ModelDFSLt1_VMN9.3 <- rbind(c(0:0), ModelDFSLt1_VMN9.2)
  ModelDFSLt1_VMN9.4 <- rbind(c(0:0), ModelDFSLt1_VMN9.3)
  ModelDFSLt1_VMN9.5 <- rbind(c(0:0), ModelDFSLt1_VMN9.4)
  ModelDFSLt1_VMN9.6 <- rbind(c(0:0), ModelDFSLt1_VMN9.5)
  ModelDFSLt1_VMN9.7 <- rbind(c(0:0), ModelDFSLt1_VMN9.6)
  ModelDFSLt1_VMN9.8 <- rbind(c(0:0), ModelDFSLt1_VMN9.7)
  ModelDFSLt1_VMN9.9 <- rbind(c(0:0), ModelDFSLt1_VMN9.8)
  ModelDFSLt1_VMN9.10 <- rbind(c(0:0), ModelDFSLt1_VMN9.9)
  ModelDFSLt1_VMN9.11 <- rbind(c(0:0), ModelDFSLt1_VMN9.10)
  ModelDFSLt1_VMN9.12 <- rbind(c(0:0), ModelDFSLt1_VMN9.11)
  ModelDFSLt1_VMN9.13 <- ModelDFSLt1_VMN9.12[-nrow(ModelDFSLt1_VMN9.12),]
  ModelDFSLt1_VMN9.14 <- ModelDFSLt1_VMN9.13[-nrow(ModelDFSLt1_VMN9.13),]
  ModelDFSLt1_VMN9.15 <- ModelDFSLt1_VMN9.14[-nrow(ModelDFSLt1_VMN9.14),]
  ModelDFSLt1_VMN9.16 <- ModelDFSLt1_VMN9.15[-nrow(ModelDFSLt1_VMN9.15),]
  ModelDFSLt1_VMN9.17 <- ModelDFSLt1_VMN9.16[-nrow(ModelDFSLt1_VMN9.16),]
  ModelDFSLt1_VMN9.18 <- ModelDFSLt1_VMN9.17[-nrow(ModelDFSLt1_VMN9.17),]
  ModelDFSLt1_VMN9.19 <- ModelDFSLt1_VMN9.18[-nrow(ModelDFSLt1_VMN9.18),]
  ModelDFSLt1_VMN9.20 <- ModelDFSLt1_VMN9.19[-nrow(ModelDFSLt1_VMN9.19),]
  ModelDFSLt1_VMN9.21 <- ModelDFSLt1_VMN9.20[-nrow(ModelDFSLt1_VMN9.20),]
  ModelDFSLt1_VMN9.22 <- ModelDFSLt1_VMN9.21[-nrow(ModelDFSLt1_VMN9.21),]
  ModelDFSLt1_VMN9.23 <- ModelDFSLt1_VMN9.22[-nrow(ModelDFSLt1_VMN9.22),]
  ModelDFSLt1_VMN9.24 <- ModelDFSLt1_VMN9.23[-nrow(ModelDFSLt1_VMN9.23),]

  ModelDFSLt1_VMN9 <- ModelDFSLt1_VMN9.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN9 <- ModelDFSLt0_VMN9 - ModelDFSLt1_VMN9

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN9$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN9P because implies a one-off input of C
  ModelDFSL_VMN9P <- ModelDFSL1y_VMN9

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN9P,"VXP_Models\\ModelDFSL_R_VMN9P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN9 <- ggplot(ModelDFSL_VMN9P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN9, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN9, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN9, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN9, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN9, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN9, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN9, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN9
  ggplotly(P_CFluxI1y_VMN9)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN9P_YCO2 <- ModelDFSL_VMN9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN9P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN9")

  PA_CO21y_VMN9 <- ggplot(ModelDFSL_VMN9P_YCO2, aes(x = Year, y = AnnualCO2_VMN9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN9
  ggplotly(PA_CO21y_VMN9)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN9P_YCO2D <- merge(ModelDFSL_VMN9P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN9P_YCO2D$AnnualCO2D_VMN9 <- ModelDFSL_VMN9P_YCO2D$AnnualCO2_VMN9 * ModelDFSL_VMN9P_YCO2D$GWP100

  PA_CO21yD_VMN9 <- ggplot(ModelDFSL_VMN9P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN9
  ggplotly(PA_CO21yD_VMN9)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN9P_YC <- ModelDFSL_VMN9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN9P_YC) <- c("Year", "Months", "AnnualCTail_VMN9")

  PA_C1y_VMN9 <- ggplot(ModelDFSL_VMN9P_YC, aes(x = Year, y = AnnualCTail_VMN9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN9
  ggplotly(PA_C1y_VMN9)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN9P_YCT <- ModelDFSL_VMN9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN9)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN9P_YCT) <- c("Year", "Months", "AnnualCTail_VMN9")

  PA_CT1y_VMN9 <- ggplot(ModelDFSL_VMN9P_YCT, aes(x = Year, y = AnnualCTail_VMN9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN9
  ggplotly(PA_CT1y_VMN9)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN9P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN9P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN9P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN9P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN9P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN9P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN9P_YCT,"CTails_P\\ModelDFSL_R_C_VMN9P.xlsx") #Yearly C emissions



  #### 12.11 - VMN10) Manure; 100%clay; No tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMN10=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMN10=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                           S.Thick = soil.thick, pClay = clay100,
                           pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMN10=data.frame(years,rep(fT_VMN10*fW_VMN10,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMN10=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*NTRM, k.RPM = 0.3*NTRM, k.BIO = 0.66*NTRM, k.HUM = 0.02*NTRM, k.IOM = 0*NTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100, #Percent clay in mineral soil
    xi=xi.frame_VMN10) #Loads the model

  Ct_VMN10=getC(Model_VMN10) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMN10, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMN10 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMN10 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMN10 <- as.data.frame(VEC_Lit_VMN10) #Converting the Litter vector to a data frame
  VEC_LitDF_VMN10$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMN10 <- as.data.frame(VEC_Man_VMN10) #Converting the Manure vector to a data frame
  VEC_ManDF_VMN10$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMN10, class) #Check that class is numeric
  sapply(VEC_ManDF_VMN10, class) #Check that class is numeric
  LitterCinputs_VMN10=VEC_LitDF_VMN10   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMN10=VEC_ManDF_VMN10 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMN10 <- merge(LitterCinputs_VMN10, ManureCinputs_VMN10, by = "MNumber")
  MCinputs_VMN10$MInput_VMN10 <- MCinputs_VMN10$VEC_Lit_VMN10 + MCinputs_VMN10$VEC_Man_VMN10

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMN10)[which(names(MCinputs_VMN10) == "VEC_Lit_VMN10")] <- "LitterC_VMN10"
  colnames(MCinputs_VMN10)[which(names(MCinputs_VMN10) == "VEC_Man_VMN10")] <- "ManureC_VMN10"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMN10 <- as.data.frame(Ct_VMN10)
  colnames(ModelDF_VMN10) <- c('DPM_VMN10','RPM_VMN10','BIO_VMN10', 'HUM_VMN10', 'IOM_VMN10')
  ModelDFS_VMN10 <- ModelDF_VMN10[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMN10$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMN10$AllPools_VMN10 <- ModelDFS_VMN10$DPM_VMN10 + ModelDFS_VMN10$RPM_VMN10 + ModelDFS_VMN10$BIO_VMN10 + ModelDFS_VMN10$HUM_VMN10 + ModelDFS_VMN10$IOM_VMN10

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMN10$AllPools_noIOM_VMN10 <- ModelDFS_VMN10$DPM_VMN10 + ModelDFS_VMN10$RPM_VMN10 + ModelDFS_VMN10$BIO_VMN10 + ModelDFS_VMN10$HUM_VMN10

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMN10 <- merge(ModelDFS_VMN10, MCinputs_VMN10, by = "MNumber")

  ModelDFSL_VMN10$MInput_VMN10 <- ModelDFSL_VMN10$LitterC_VMN10 + ModelDFSL_VMN10$ManureC_VMN10
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMN10$CTails_VMN10 <- ModelDFSL_VMN10$AllPools_noIOM_VMN10 + ModelDFSL_VMN10$MInput_VMN10

  #Create Monthly Accumulated input of C
  ModelDFSL_VMN10$AccumInput_VMN10 = ModelDFSL_VMN10$AccumInput_VMN10=cumsum(ModelDFSL_VMN10$MInput_VMN10)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMN10$MGrowth_DPM_VMN10 <- ave(ModelDFSL_VMN10$DPM_VMN10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN10$MGrowth_RPM_VMN10 <- ave(ModelDFSL_VMN10$RPM_VMN10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN10$MGrowth_BIO_VMN10 <- ave(ModelDFSL_VMN10$BIO_VMN10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN10$MGrowth_HUM_VMN10 <- ave(ModelDFSL_VMN10$HUM_VMN10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMN10$MGrowth_IOM_VMN10 <- ave(ModelDFSL_VMN10$IOM_VMN10, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMN10$M_CCO2_VMN10 <- ModelDFSL_VMN10$MInput_VMN10 - ModelDFSL_VMN10$MGrowth_DPM_VMN10 - ModelDFSL_VMN10$MGrowth_RPM_VMN10 - ModelDFSL_VMN10$MGrowth_BIO_VMN10 - ModelDFSL_VMN10$MGrowth_HUM_VMN10
  ModelDFSL_VMN10$Accum_CCO2_VMN10 <- ModelDFSL_VMN10$AccumInput_VMN10 - ModelDFSL_VMN10$AllPools_noIOM_VMN10

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMN10$M_CCO2_VMN10[1] <- 0
  ModelDFSL_VMN10$Accum_CCO2_VMN10[1] <- 0

  #Balance validation
  ModelDFSL_VMN10$Balance_VMN10 <- ModelDFSL_VMN10$AccumInput_VMN10 - ModelDFSL_VMN10$Accum_CCO2_VMN10 - (ModelDFSL_VMN10$DPM_VMN10 + ModelDFSL_VMN10$RPM_VMN10 + ModelDFSL_VMN10$BIO_VMN10 + ModelDFSL_VMN10$HUM_VMN10)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMN10$M_CO2_VMN10 <- ModelDFSL_VMN10$M_CCO2_VMN10 * 44/12
  ModelDFSL_VMN10$Accum_CO2_VMN10 <- ModelDFSL_VMN10$Accum_CCO2_VMN10 * 44/12

  #This model will be called VMN10C because implies a continuous input of C
  ModelDFSL_VMN10C <- ModelDFSL_VMN10

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN10C,"VXC_Models\\ModelDFSL_R_VMN10C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMN10 <- ModelDFSL_VMN10 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMN10.1 <- rbind(c(0:0), ModelDFSLt0_VMN10)
  ModelDFSLt1_VMN10.2 <- rbind(c(0:0), ModelDFSLt1_VMN10.1)
  ModelDFSLt1_VMN10.3 <- rbind(c(0:0), ModelDFSLt1_VMN10.2)
  ModelDFSLt1_VMN10.4 <- rbind(c(0:0), ModelDFSLt1_VMN10.3)
  ModelDFSLt1_VMN10.5 <- rbind(c(0:0), ModelDFSLt1_VMN10.4)
  ModelDFSLt1_VMN10.6 <- rbind(c(0:0), ModelDFSLt1_VMN10.5)
  ModelDFSLt1_VMN10.7 <- rbind(c(0:0), ModelDFSLt1_VMN10.6)
  ModelDFSLt1_VMN10.8 <- rbind(c(0:0), ModelDFSLt1_VMN10.7)
  ModelDFSLt1_VMN10.9 <- rbind(c(0:0), ModelDFSLt1_VMN10.8)
  ModelDFSLt1_VMN10.10 <- rbind(c(0:0), ModelDFSLt1_VMN10.9)
  ModelDFSLt1_VMN10.11 <- rbind(c(0:0), ModelDFSLt1_VMN10.10)
  ModelDFSLt1_VMN10.12 <- rbind(c(0:0), ModelDFSLt1_VMN10.11)
  ModelDFSLt1_VMN10.13 <- ModelDFSLt1_VMN10.12[-nrow(ModelDFSLt1_VMN10.12),]
  ModelDFSLt1_VMN10.14 <- ModelDFSLt1_VMN10.13[-nrow(ModelDFSLt1_VMN10.13),]
  ModelDFSLt1_VMN10.15 <- ModelDFSLt1_VMN10.14[-nrow(ModelDFSLt1_VMN10.14),]
  ModelDFSLt1_VMN10.16 <- ModelDFSLt1_VMN10.15[-nrow(ModelDFSLt1_VMN10.15),]
  ModelDFSLt1_VMN10.17 <- ModelDFSLt1_VMN10.16[-nrow(ModelDFSLt1_VMN10.16),]
  ModelDFSLt1_VMN10.18 <- ModelDFSLt1_VMN10.17[-nrow(ModelDFSLt1_VMN10.17),]
  ModelDFSLt1_VMN10.19 <- ModelDFSLt1_VMN10.18[-nrow(ModelDFSLt1_VMN10.18),]
  ModelDFSLt1_VMN10.20 <- ModelDFSLt1_VMN10.19[-nrow(ModelDFSLt1_VMN10.19),]
  ModelDFSLt1_VMN10.21 <- ModelDFSLt1_VMN10.20[-nrow(ModelDFSLt1_VMN10.20),]
  ModelDFSLt1_VMN10.22 <- ModelDFSLt1_VMN10.21[-nrow(ModelDFSLt1_VMN10.21),]
  ModelDFSLt1_VMN10.23 <- ModelDFSLt1_VMN10.22[-nrow(ModelDFSLt1_VMN10.22),]
  ModelDFSLt1_VMN10.24 <- ModelDFSLt1_VMN10.23[-nrow(ModelDFSLt1_VMN10.23),]

  ModelDFSLt1_VMN10 <- ModelDFSLt1_VMN10.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMN10 <- ModelDFSLt0_VMN10 - ModelDFSLt1_VMN10

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMN10$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMN10P because implies a one-off input of C
  ModelDFSL_VMN10P <- ModelDFSL1y_VMN10

  #Export the dataframe
  write_xlsx(ModelDFSL_VMN10P,"VXP_Models\\ModelDFSL_R_VMN10P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMN10 <- ggplot(ModelDFSL_VMN10P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMN10, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMN10, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMN10, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMN10, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMN10, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMN10, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMN10, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMN10
  ggplotly(P_CFluxI1y_VMN10)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMN10P_YCO2 <- ModelDFSL_VMN10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMN10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN10P_YCO2) <- c("Year", "Months", "AnnualCO2_VMN10")

  PA_CO21y_VMN10 <- ggplot(ModelDFSL_VMN10P_YCO2, aes(x = Year, y = AnnualCO2_VMN10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMN10
  ggplotly(PA_CO21y_VMN10)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMN10P_YCO2D <- merge(ModelDFSL_VMN10P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMN10P_YCO2D$AnnualCO2D_VMN10 <- ModelDFSL_VMN10P_YCO2D$AnnualCO2_VMN10 * ModelDFSL_VMN10P_YCO2D$GWP100

  PA_CO21yD_VMN10 <- ggplot(ModelDFSL_VMN10P_YCO2D, aes(x = Year, y = AnnualCO2D_VMN10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMN10
  ggplotly(PA_CO21yD_VMN10)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMN10P_YC <- ModelDFSL_VMN10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMN10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN10P_YC) <- c("Year", "Months", "AnnualCTail_VMN10")

  PA_C1y_VMN10 <- ggplot(ModelDFSL_VMN10P_YC, aes(x = Year, y = AnnualCTail_VMN10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMN10
  ggplotly(PA_C1y_VMN10)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMN10P_YCT <- ModelDFSL_VMN10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMN10)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMN10P_YCT) <- c("Year", "Months", "AnnualCTail_VMN10")

  PA_CT1y_VMN10 <- ggplot(ModelDFSL_VMN10P_YCT, aes(x = Year, y = AnnualCTail_VMN10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMN10
  ggplotly(PA_CT1y_VMN10)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMN10P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMN10P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMN10P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMN10P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMN10P_YC,"CEmissions_P\\ModelDFSL_R_C_VMN10P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMN10P_YCT,"CTails_P\\ModelDFSL_R_C_VMN10P.xlsx") #Yearly C emissions




















}
