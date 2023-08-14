#' Run RothC_VMC
#'
#' The Merge_VXC function merge all VXC models created by the functions Run_RothC_[].
#'
#' @return A dataframe with all VXC models merged together
#' @import SoilR ggplot2 stringr
#' @importFrom plotly ggplotly
#' @export

#Function to Run and Create the multiple RothC Combinations


Run_RothC_VMC <- function(SL_years = 100,
                          soil.thick = 23,
                          Tem = Temp,
                          Pre = Precip,
                          Eva = Evp,
                          CTRM = 1.00)

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

  #### 10) Model Combinations - Manure; Conventional Tillage, 0-100% (+10%) Clay ####
  #### 10.1 - VMC0) Manure; 0%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC0=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC0=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay0,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC0=data.frame(years,rep(fT_VMC0*fW_VMC0,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC0=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay0, #Percent clay in mineral soil
    xi=xi.frame_VMC0) #Loads the model

  Ct_VMC0=getC(Model_VMC0) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC0, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC0 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC0 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC0 <- as.data.frame(VEC_Lit_VMC0) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC0$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC0 <- as.data.frame(VEC_Man_VMC0) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC0$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC0, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC0, class) #Check that class is numeric
  LitterCinputs_VMC0=VEC_LitDF_VMC0   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC0=VEC_ManDF_VMC0 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC0 <- merge(LitterCinputs_VMC0, ManureCinputs_VMC0, by = "MNumber")
  MCinputs_VMC0$MInput_VMC0 <- MCinputs_VMC0$VEC_Lit_VMC0 + MCinputs_VMC0$VEC_Man_VMC0

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC0)[which(names(MCinputs_VMC0) == "VEC_Lit_VMC0")] <- "LitterC_VMC0"
  colnames(MCinputs_VMC0)[which(names(MCinputs_VMC0) == "VEC_Man_VMC0")] <- "ManureC_VMC0"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC0 <- as.data.frame(Ct_VMC0)
  colnames(ModelDF_VMC0) <- c('DPM_VMC0','RPM_VMC0','BIO_VMC0', 'HUM_VMC0', 'IOM_VMC0')
  ModelDFS_VMC0 <- ModelDF_VMC0[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC0$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC0$AllPools_VMC0 <- ModelDFS_VMC0$DPM_VMC0 + ModelDFS_VMC0$RPM_VMC0 + ModelDFS_VMC0$BIO_VMC0 + ModelDFS_VMC0$HUM_VMC0 + ModelDFS_VMC0$IOM_VMC0

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC0$AllPools_noIOM_VMC0 <- ModelDFS_VMC0$DPM_VMC0 + ModelDFS_VMC0$RPM_VMC0 + ModelDFS_VMC0$BIO_VMC0 + ModelDFS_VMC0$HUM_VMC0

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC0 <- merge(ModelDFS_VMC0, MCinputs_VMC0, by = "MNumber")

  ModelDFSL_VMC0$MInput_VMC0 <- ModelDFSL_VMC0$LitterC_VMC0 + ModelDFSL_VMC0$ManureC_VMC0
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC0$CTails_VMC0 <- ModelDFSL_VMC0$AllPools_noIOM_VMC0 + ModelDFSL_VMC0$MInput_VMC0

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC0$AccumInput_VMC0 = ModelDFSL_VMC0$AccumInput_VMC0=cumsum(ModelDFSL_VMC0$MInput_VMC0)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC0$MGrowth_DPM_VMC0 <- ave(ModelDFSL_VMC0$DPM_VMC0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC0$MGrowth_RPM_VMC0 <- ave(ModelDFSL_VMC0$RPM_VMC0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC0$MGrowth_BIO_VMC0 <- ave(ModelDFSL_VMC0$BIO_VMC0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC0$MGrowth_HUM_VMC0 <- ave(ModelDFSL_VMC0$HUM_VMC0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC0$MGrowth_IOM_VMC0 <- ave(ModelDFSL_VMC0$IOM_VMC0, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC0$M_CCO2_VMC0 <- ModelDFSL_VMC0$MInput_VMC0 - ModelDFSL_VMC0$MGrowth_DPM_VMC0 - ModelDFSL_VMC0$MGrowth_RPM_VMC0 - ModelDFSL_VMC0$MGrowth_BIO_VMC0 - ModelDFSL_VMC0$MGrowth_HUM_VMC0
  ModelDFSL_VMC0$Accum_CCO2_VMC0 <- ModelDFSL_VMC0$AccumInput_VMC0 - ModelDFSL_VMC0$AllPools_noIOM_VMC0

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC0$M_CCO2_VMC0[1] <- 0
  ModelDFSL_VMC0$Accum_CCO2_VMC0[1] <- 0

  #Balance validation
  ModelDFSL_VMC0$Balance_VMC0 <- ModelDFSL_VMC0$AccumInput_VMC0 - ModelDFSL_VMC0$Accum_CCO2_VMC0 - (ModelDFSL_VMC0$DPM_VMC0 + ModelDFSL_VMC0$RPM_VMC0 + ModelDFSL_VMC0$BIO_VMC0 + ModelDFSL_VMC0$HUM_VMC0)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC0$M_CO2_VMC0 <- ModelDFSL_VMC0$M_CCO2_VMC0 * 44/12
  ModelDFSL_VMC0$Accum_CO2_VMC0 <- ModelDFSL_VMC0$Accum_CCO2_VMC0 * 44/12

  #This model will be called VMC0C because implies a continuous input of C
  ModelDFSL_VMC0C <- ModelDFSL_VMC0

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC0C,"VXC_Models\\ModelDFSL_R_VMC0C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC0 <- ModelDFSL_VMC0 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC0.1 <- rbind(c(0:0), ModelDFSLt0_VMC0)
  ModelDFSLt1_VMC0.2 <- rbind(c(0:0), ModelDFSLt1_VMC0.1)
  ModelDFSLt1_VMC0.3 <- rbind(c(0:0), ModelDFSLt1_VMC0.2)
  ModelDFSLt1_VMC0.4 <- rbind(c(0:0), ModelDFSLt1_VMC0.3)
  ModelDFSLt1_VMC0.5 <- rbind(c(0:0), ModelDFSLt1_VMC0.4)
  ModelDFSLt1_VMC0.6 <- rbind(c(0:0), ModelDFSLt1_VMC0.5)
  ModelDFSLt1_VMC0.7 <- rbind(c(0:0), ModelDFSLt1_VMC0.6)
  ModelDFSLt1_VMC0.8 <- rbind(c(0:0), ModelDFSLt1_VMC0.7)
  ModelDFSLt1_VMC0.9 <- rbind(c(0:0), ModelDFSLt1_VMC0.8)
  ModelDFSLt1_VMC0.10 <- rbind(c(0:0), ModelDFSLt1_VMC0.9)
  ModelDFSLt1_VMC0.11 <- rbind(c(0:0), ModelDFSLt1_VMC0.10)
  ModelDFSLt1_VMC0.12 <- rbind(c(0:0), ModelDFSLt1_VMC0.11)
  ModelDFSLt1_VMC0.13 <- ModelDFSLt1_VMC0.12[-nrow(ModelDFSLt1_VMC0.12),]
  ModelDFSLt1_VMC0.14 <- ModelDFSLt1_VMC0.13[-nrow(ModelDFSLt1_VMC0.13),]
  ModelDFSLt1_VMC0.15 <- ModelDFSLt1_VMC0.14[-nrow(ModelDFSLt1_VMC0.14),]
  ModelDFSLt1_VMC0.16 <- ModelDFSLt1_VMC0.15[-nrow(ModelDFSLt1_VMC0.15),]
  ModelDFSLt1_VMC0.17 <- ModelDFSLt1_VMC0.16[-nrow(ModelDFSLt1_VMC0.16),]
  ModelDFSLt1_VMC0.18 <- ModelDFSLt1_VMC0.17[-nrow(ModelDFSLt1_VMC0.17),]
  ModelDFSLt1_VMC0.19 <- ModelDFSLt1_VMC0.18[-nrow(ModelDFSLt1_VMC0.18),]
  ModelDFSLt1_VMC0.20 <- ModelDFSLt1_VMC0.19[-nrow(ModelDFSLt1_VMC0.19),]
  ModelDFSLt1_VMC0.21 <- ModelDFSLt1_VMC0.20[-nrow(ModelDFSLt1_VMC0.20),]
  ModelDFSLt1_VMC0.22 <- ModelDFSLt1_VMC0.21[-nrow(ModelDFSLt1_VMC0.21),]
  ModelDFSLt1_VMC0.23 <- ModelDFSLt1_VMC0.22[-nrow(ModelDFSLt1_VMC0.22),]
  ModelDFSLt1_VMC0.24 <- ModelDFSLt1_VMC0.23[-nrow(ModelDFSLt1_VMC0.23),]

  ModelDFSLt1_VMC0 <- ModelDFSLt1_VMC0.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC0 <- ModelDFSLt0_VMC0 - ModelDFSLt1_VMC0

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC0$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC0P because implies a one-off input of C
  ModelDFSL_VMC0P <- ModelDFSL1y_VMC0

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC0P,"VXP_Models\\ModelDFSL_R_VMC0P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC0 <- ggplot(ModelDFSL_VMC0P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC0, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC0, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC0, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC0, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC0, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC0, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC0, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC0
  ggplotly(P_CFluxI1y_VMC0)

  #Plot 3.1: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC0P_YCO2 <- ModelDFSL_VMC0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC0P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC0")

  PA_CO21y_VMC0 <- ggplot(ModelDFSL_VMC0P_YCO2, aes(x = Year, y = AnnualCO2_VMC0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC0
  ggplotly(PA_CO21y_VMC0)

  #Plot 3.2: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC0P_YCO2D <- merge(ModelDFSL_VMC0P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC0P_YCO2D$AnnualCO2D_VMC0 <- ModelDFSL_VMC0P_YCO2D$AnnualCO2_VMC0 * ModelDFSL_VMC0P_YCO2D$GWP100

  PA_CO21yD_VMC0 <- ggplot(ModelDFSL_VMC0P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC0
  ggplotly(PA_CO21yD_VMC0)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC0P_YC <- ModelDFSL_VMC0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC0P_YC) <- c("Year", "Months", "AnnualCTail_VMC0")

  PA_C1y_VMC0 <- ggplot(ModelDFSL_VMC0P_YC, aes(x = Year, y = AnnualCTail_VMC0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC0
  ggplotly(PA_C1y_VMC0)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC0P_YCT <- ModelDFSL_VMC0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC0)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC0P_YCT) <- c("Year", "Months", "AnnualCTail_VMC0")

  PA_CT1y_VMC0 <- ggplot(ModelDFSL_VMC0P_YCT, aes(x = Year, y = AnnualCTail_VMC0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC0
  ggplotly(PA_CT1y_VMC0)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC0P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC0P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC0P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC0P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC0P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC0P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC0P_YCT,"CTails_P\\ModelDFSL_R_C_VMC0P.xlsx") #Yearly C emissions


  #### 10.2 - VMC1) Manure; 10%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC1=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC1=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.1,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC1=data.frame(years,rep(fT_VMC1*fW_VMC1,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC1=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.1, #Percent clay in mineral soil
    xi=xi.frame_VMC1) #Loads the model

  Ct_VMC1=getC(Model_VMC1) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC1, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC1 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC1 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC1 <- as.data.frame(VEC_Lit_VMC1) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC1$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC1 <- as.data.frame(VEC_Man_VMC1) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC1$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC1, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC1, class) #Check that class is numeric
  LitterCinputs_VMC1=VEC_LitDF_VMC1   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC1=VEC_ManDF_VMC1 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC1 <- merge(LitterCinputs_VMC1, ManureCinputs_VMC1, by = "MNumber")
  MCinputs_VMC1$MInput_VMC1 <- MCinputs_VMC1$VEC_Lit_VMC1 + MCinputs_VMC1$VEC_Man_VMC1

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC1)[which(names(MCinputs_VMC1) == "VEC_Lit_VMC1")] <- "LitterC_VMC1"
  colnames(MCinputs_VMC1)[which(names(MCinputs_VMC1) == "VEC_Man_VMC1")] <- "ManureC_VMC1"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC1 <- as.data.frame(Ct_VMC1)
  colnames(ModelDF_VMC1) <- c('DPM_VMC1','RPM_VMC1','BIO_VMC1', 'HUM_VMC1', 'IOM_VMC1')
  ModelDFS_VMC1 <- ModelDF_VMC1[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC1$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC1$AllPools_VMC1 <- ModelDFS_VMC1$DPM_VMC1 + ModelDFS_VMC1$RPM_VMC1 + ModelDFS_VMC1$BIO_VMC1 + ModelDFS_VMC1$HUM_VMC1 + ModelDFS_VMC1$IOM_VMC1

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC1$AllPools_noIOM_VMC1 <- ModelDFS_VMC1$DPM_VMC1 + ModelDFS_VMC1$RPM_VMC1 + ModelDFS_VMC1$BIO_VMC1 + ModelDFS_VMC1$HUM_VMC1

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC1 <- merge(ModelDFS_VMC1, MCinputs_VMC1, by = "MNumber")

  ModelDFSL_VMC1$MInput_VMC1 <- ModelDFSL_VMC1$LitterC_VMC1 + ModelDFSL_VMC1$ManureC_VMC1
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC1$CTails_VMC1 <- ModelDFSL_VMC1$AllPools_noIOM_VMC1 + ModelDFSL_VMC1$MInput_VMC1

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC1$AccumInput_VMC1 = ModelDFSL_VMC1$AccumInput_VMC1=cumsum(ModelDFSL_VMC1$MInput_VMC1)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC1$MGrowth_DPM_VMC1 <- ave(ModelDFSL_VMC1$DPM_VMC1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC1$MGrowth_RPM_VMC1 <- ave(ModelDFSL_VMC1$RPM_VMC1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC1$MGrowth_BIO_VMC1 <- ave(ModelDFSL_VMC1$BIO_VMC1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC1$MGrowth_HUM_VMC1 <- ave(ModelDFSL_VMC1$HUM_VMC1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC1$MGrowth_IOM_VMC1 <- ave(ModelDFSL_VMC1$IOM_VMC1, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC1$M_CCO2_VMC1 <- ModelDFSL_VMC1$MInput_VMC1 - ModelDFSL_VMC1$MGrowth_DPM_VMC1 - ModelDFSL_VMC1$MGrowth_RPM_VMC1 - ModelDFSL_VMC1$MGrowth_BIO_VMC1 - ModelDFSL_VMC1$MGrowth_HUM_VMC1
  ModelDFSL_VMC1$Accum_CCO2_VMC1 <- ModelDFSL_VMC1$AccumInput_VMC1 - ModelDFSL_VMC1$AllPools_noIOM_VMC1

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC1$M_CCO2_VMC1[1] <- 0
  ModelDFSL_VMC1$Accum_CCO2_VMC1[1] <- 0

  #Balance validation
  ModelDFSL_VMC1$Balance_VMC1 <- ModelDFSL_VMC1$AccumInput_VMC1 - ModelDFSL_VMC1$Accum_CCO2_VMC1 - (ModelDFSL_VMC1$DPM_VMC1 + ModelDFSL_VMC1$RPM_VMC1 + ModelDFSL_VMC1$BIO_VMC1 + ModelDFSL_VMC1$HUM_VMC1)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC1$M_CO2_VMC1 <- ModelDFSL_VMC1$M_CCO2_VMC1 * 44/12
  ModelDFSL_VMC1$Accum_CO2_VMC1 <- ModelDFSL_VMC1$Accum_CCO2_VMC1 * 44/12

  #This model will be called VMC1C because implies a continuous input of C
  ModelDFSL_VMC1C <- ModelDFSL_VMC1

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC1C,"VXC_Models\\ModelDFSL_R_VMC1C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC1 <- ModelDFSL_VMC1 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC1.1 <- rbind(c(0:0), ModelDFSLt0_VMC1)
  ModelDFSLt1_VMC1.2 <- rbind(c(0:0), ModelDFSLt1_VMC1.1)
  ModelDFSLt1_VMC1.3 <- rbind(c(0:0), ModelDFSLt1_VMC1.2)
  ModelDFSLt1_VMC1.4 <- rbind(c(0:0), ModelDFSLt1_VMC1.3)
  ModelDFSLt1_VMC1.5 <- rbind(c(0:0), ModelDFSLt1_VMC1.4)
  ModelDFSLt1_VMC1.6 <- rbind(c(0:0), ModelDFSLt1_VMC1.5)
  ModelDFSLt1_VMC1.7 <- rbind(c(0:0), ModelDFSLt1_VMC1.6)
  ModelDFSLt1_VMC1.8 <- rbind(c(0:0), ModelDFSLt1_VMC1.7)
  ModelDFSLt1_VMC1.9 <- rbind(c(0:0), ModelDFSLt1_VMC1.8)
  ModelDFSLt1_VMC1.10 <- rbind(c(0:0), ModelDFSLt1_VMC1.9)
  ModelDFSLt1_VMC1.11 <- rbind(c(0:0), ModelDFSLt1_VMC1.10)
  ModelDFSLt1_VMC1.12 <- rbind(c(0:0), ModelDFSLt1_VMC1.11)
  ModelDFSLt1_VMC1.13 <- ModelDFSLt1_VMC1.12[-nrow(ModelDFSLt1_VMC1.12),]
  ModelDFSLt1_VMC1.14 <- ModelDFSLt1_VMC1.13[-nrow(ModelDFSLt1_VMC1.13),]
  ModelDFSLt1_VMC1.15 <- ModelDFSLt1_VMC1.14[-nrow(ModelDFSLt1_VMC1.14),]
  ModelDFSLt1_VMC1.16 <- ModelDFSLt1_VMC1.15[-nrow(ModelDFSLt1_VMC1.15),]
  ModelDFSLt1_VMC1.17 <- ModelDFSLt1_VMC1.16[-nrow(ModelDFSLt1_VMC1.16),]
  ModelDFSLt1_VMC1.18 <- ModelDFSLt1_VMC1.17[-nrow(ModelDFSLt1_VMC1.17),]
  ModelDFSLt1_VMC1.19 <- ModelDFSLt1_VMC1.18[-nrow(ModelDFSLt1_VMC1.18),]
  ModelDFSLt1_VMC1.20 <- ModelDFSLt1_VMC1.19[-nrow(ModelDFSLt1_VMC1.19),]
  ModelDFSLt1_VMC1.21 <- ModelDFSLt1_VMC1.20[-nrow(ModelDFSLt1_VMC1.20),]
  ModelDFSLt1_VMC1.22 <- ModelDFSLt1_VMC1.21[-nrow(ModelDFSLt1_VMC1.21),]
  ModelDFSLt1_VMC1.23 <- ModelDFSLt1_VMC1.22[-nrow(ModelDFSLt1_VMC1.22),]
  ModelDFSLt1_VMC1.24 <- ModelDFSLt1_VMC1.23[-nrow(ModelDFSLt1_VMC1.23),]

  ModelDFSLt1_VMC1 <- ModelDFSLt1_VMC1.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC1 <- ModelDFSLt0_VMC1 - ModelDFSLt1_VMC1

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC1$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC1P because implies a one-off input of C
  ModelDFSL_VMC1P <- ModelDFSL1y_VMC1

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC1P,"VXP_Models\\ModelDFSL_R_VMC1P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC1 <- ggplot(ModelDFSL_VMC1P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC1, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC1, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC1, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC1, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC1, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC1, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC1, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC1
  ggplotly(P_CFluxI1y_VMC1)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC1P_YCO2 <- ModelDFSL_VMC1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC1P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC1")

  PA_CO21y_VMC1 <- ggplot(ModelDFSL_VMC1P_YCO2, aes(x = Year, y = AnnualCO2_VMC1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC1
  ggplotly(PA_CO21y_VMC1)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC1P_YCO2D <- merge(ModelDFSL_VMC1P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC1P_YCO2D$AnnualCO2D_VMC1 <- ModelDFSL_VMC1P_YCO2D$AnnualCO2_VMC1 * ModelDFSL_VMC1P_YCO2D$GWP100

  PA_CO21yD_VMC1 <- ggplot(ModelDFSL_VMC1P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC1
  ggplotly(PA_CO21yD_VMC1)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC1P_YC <- ModelDFSL_VMC1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC1P_YC) <- c("Year", "Months", "AnnualCTail_VMC1")

  PA_C1y_VMC1 <- ggplot(ModelDFSL_VMC1P_YC, aes(x = Year, y = AnnualCTail_VMC1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC1
  ggplotly(PA_C1y_VMC1)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC1P_YCT <- ModelDFSL_VMC1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC1)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC1P_YCT) <- c("Year", "Months", "AnnualCTail_VMC1")

  PA_CT1y_VMC1 <- ggplot(ModelDFSL_VMC1P_YCT, aes(x = Year, y = AnnualCTail_VMC1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC1
  ggplotly(PA_CT1y_VMC1)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC1P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC1P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC1P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC1P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC1P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC1P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC1P_YCT,"CTails_P\\ModelDFSL_R_C_VMC1P.xlsx") #Yearly C emissions



  #### 10.3 - VMC2) Manure; 20%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC2=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC2=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.2,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC2=data.frame(years,rep(fT_VMC2*fW_VMC2,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC2=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.2, #Percent clay in mineral soil
    xi=xi.frame_VMC2) #Loads the model

  Ct_VMC2=getC(Model_VMC2) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC2, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC2 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC2 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC2 <- as.data.frame(VEC_Lit_VMC2) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC2$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC2 <- as.data.frame(VEC_Man_VMC2) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC2$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC2, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC2, class) #Check that class is numeric
  LitterCinputs_VMC2=VEC_LitDF_VMC2   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC2=VEC_ManDF_VMC2 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC2 <- merge(LitterCinputs_VMC2, ManureCinputs_VMC2, by = "MNumber")
  MCinputs_VMC2$MInput_VMC2 <- MCinputs_VMC2$VEC_Lit_VMC2 + MCinputs_VMC2$VEC_Man_VMC2

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC2)[which(names(MCinputs_VMC2) == "VEC_Lit_VMC2")] <- "LitterC_VMC2"
  colnames(MCinputs_VMC2)[which(names(MCinputs_VMC2) == "VEC_Man_VMC2")] <- "ManureC_VMC2"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC2 <- as.data.frame(Ct_VMC2)
  colnames(ModelDF_VMC2) <- c('DPM_VMC2','RPM_VMC2','BIO_VMC2', 'HUM_VMC2', 'IOM_VMC2')
  ModelDFS_VMC2 <- ModelDF_VMC2[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC2$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC2$AllPools_VMC2 <- ModelDFS_VMC2$DPM_VMC2 + ModelDFS_VMC2$RPM_VMC2 + ModelDFS_VMC2$BIO_VMC2 + ModelDFS_VMC2$HUM_VMC2 + ModelDFS_VMC2$IOM_VMC2

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC2$AllPools_noIOM_VMC2 <- ModelDFS_VMC2$DPM_VMC2 + ModelDFS_VMC2$RPM_VMC2 + ModelDFS_VMC2$BIO_VMC2 + ModelDFS_VMC2$HUM_VMC2

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC2 <- merge(ModelDFS_VMC2, MCinputs_VMC2, by = "MNumber")

  ModelDFSL_VMC2$MInput_VMC2 <- ModelDFSL_VMC2$LitterC_VMC2 + ModelDFSL_VMC2$ManureC_VMC2
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC2$CTails_VMC2 <- ModelDFSL_VMC2$AllPools_noIOM_VMC2 + ModelDFSL_VMC2$MInput_VMC2

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC2$AccumInput_VMC2 = ModelDFSL_VMC2$AccumInput_VMC2=cumsum(ModelDFSL_VMC2$MInput_VMC2)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC2$MGrowth_DPM_VMC2 <- ave(ModelDFSL_VMC2$DPM_VMC2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC2$MGrowth_RPM_VMC2 <- ave(ModelDFSL_VMC2$RPM_VMC2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC2$MGrowth_BIO_VMC2 <- ave(ModelDFSL_VMC2$BIO_VMC2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC2$MGrowth_HUM_VMC2 <- ave(ModelDFSL_VMC2$HUM_VMC2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC2$MGrowth_IOM_VMC2 <- ave(ModelDFSL_VMC2$IOM_VMC2, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC2$M_CCO2_VMC2 <- ModelDFSL_VMC2$MInput_VMC2 - ModelDFSL_VMC2$MGrowth_DPM_VMC2 - ModelDFSL_VMC2$MGrowth_RPM_VMC2 - ModelDFSL_VMC2$MGrowth_BIO_VMC2 - ModelDFSL_VMC2$MGrowth_HUM_VMC2
  ModelDFSL_VMC2$Accum_CCO2_VMC2 <- ModelDFSL_VMC2$AccumInput_VMC2 - ModelDFSL_VMC2$AllPools_noIOM_VMC2

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC2$M_CCO2_VMC2[1] <- 0
  ModelDFSL_VMC2$Accum_CCO2_VMC2[1] <- 0

  #Balance validation
  ModelDFSL_VMC2$Balance_VMC2 <- ModelDFSL_VMC2$AccumInput_VMC2 - ModelDFSL_VMC2$Accum_CCO2_VMC2 - (ModelDFSL_VMC2$DPM_VMC2 + ModelDFSL_VMC2$RPM_VMC2 + ModelDFSL_VMC2$BIO_VMC2 + ModelDFSL_VMC2$HUM_VMC2)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC2$M_CO2_VMC2 <- ModelDFSL_VMC2$M_CCO2_VMC2 * 44/12
  ModelDFSL_VMC2$Accum_CO2_VMC2 <- ModelDFSL_VMC2$Accum_CCO2_VMC2 * 44/12

  #This model will be called VMC2C because implies a continuous input of C
  ModelDFSL_VMC2C <- ModelDFSL_VMC2

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC2C,"VXC_Models\\ModelDFSL_R_VMC2C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC2 <- ModelDFSL_VMC2 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC2.1 <- rbind(c(0:0), ModelDFSLt0_VMC2)
  ModelDFSLt1_VMC2.2 <- rbind(c(0:0), ModelDFSLt1_VMC2.1)
  ModelDFSLt1_VMC2.3 <- rbind(c(0:0), ModelDFSLt1_VMC2.2)
  ModelDFSLt1_VMC2.4 <- rbind(c(0:0), ModelDFSLt1_VMC2.3)
  ModelDFSLt1_VMC2.5 <- rbind(c(0:0), ModelDFSLt1_VMC2.4)
  ModelDFSLt1_VMC2.6 <- rbind(c(0:0), ModelDFSLt1_VMC2.5)
  ModelDFSLt1_VMC2.7 <- rbind(c(0:0), ModelDFSLt1_VMC2.6)
  ModelDFSLt1_VMC2.8 <- rbind(c(0:0), ModelDFSLt1_VMC2.7)
  ModelDFSLt1_VMC2.9 <- rbind(c(0:0), ModelDFSLt1_VMC2.8)
  ModelDFSLt1_VMC2.10 <- rbind(c(0:0), ModelDFSLt1_VMC2.9)
  ModelDFSLt1_VMC2.11 <- rbind(c(0:0), ModelDFSLt1_VMC2.10)
  ModelDFSLt1_VMC2.12 <- rbind(c(0:0), ModelDFSLt1_VMC2.11)
  ModelDFSLt1_VMC2.13 <- ModelDFSLt1_VMC2.12[-nrow(ModelDFSLt1_VMC2.12),]
  ModelDFSLt1_VMC2.14 <- ModelDFSLt1_VMC2.13[-nrow(ModelDFSLt1_VMC2.13),]
  ModelDFSLt1_VMC2.15 <- ModelDFSLt1_VMC2.14[-nrow(ModelDFSLt1_VMC2.14),]
  ModelDFSLt1_VMC2.16 <- ModelDFSLt1_VMC2.15[-nrow(ModelDFSLt1_VMC2.15),]
  ModelDFSLt1_VMC2.17 <- ModelDFSLt1_VMC2.16[-nrow(ModelDFSLt1_VMC2.16),]
  ModelDFSLt1_VMC2.18 <- ModelDFSLt1_VMC2.17[-nrow(ModelDFSLt1_VMC2.17),]
  ModelDFSLt1_VMC2.19 <- ModelDFSLt1_VMC2.18[-nrow(ModelDFSLt1_VMC2.18),]
  ModelDFSLt1_VMC2.20 <- ModelDFSLt1_VMC2.19[-nrow(ModelDFSLt1_VMC2.19),]
  ModelDFSLt1_VMC2.21 <- ModelDFSLt1_VMC2.20[-nrow(ModelDFSLt1_VMC2.20),]
  ModelDFSLt1_VMC2.22 <- ModelDFSLt1_VMC2.21[-nrow(ModelDFSLt1_VMC2.21),]
  ModelDFSLt1_VMC2.23 <- ModelDFSLt1_VMC2.22[-nrow(ModelDFSLt1_VMC2.22),]
  ModelDFSLt1_VMC2.24 <- ModelDFSLt1_VMC2.23[-nrow(ModelDFSLt1_VMC2.23),]

  ModelDFSLt1_VMC2 <- ModelDFSLt1_VMC2.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC2 <- ModelDFSLt0_VMC2 - ModelDFSLt1_VMC2

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC2$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC2P because implies a one-off input of C
  ModelDFSL_VMC2P <- ModelDFSL1y_VMC2

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC2P,"VXP_Models\\ModelDFSL_R_VMC2P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC2 <- ggplot(ModelDFSL_VMC2P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC2, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC2, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC2, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC2, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC2, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC2, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC2, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC2
  ggplotly(P_CFluxI1y_VMC2)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC2P_YCO2 <- ModelDFSL_VMC2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC2P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC2")

  PA_CO21y_VMC2 <- ggplot(ModelDFSL_VMC2P_YCO2, aes(x = Year, y = AnnualCO2_VMC2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC2
  ggplotly(PA_CO21y_VMC2)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC2P_YCO2D <- merge(ModelDFSL_VMC2P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC2P_YCO2D$AnnualCO2D_VMC2 <- ModelDFSL_VMC2P_YCO2D$AnnualCO2_VMC2 * ModelDFSL_VMC2P_YCO2D$GWP100

  PA_CO21yD_VMC2 <- ggplot(ModelDFSL_VMC2P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC2
  ggplotly(PA_CO21yD_VMC2)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC2P_YC <- ModelDFSL_VMC2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC2P_YC) <- c("Year", "Months", "AnnualCTail_VMC2")

  PA_C1y_VMC2 <- ggplot(ModelDFSL_VMC2P_YC, aes(x = Year, y = AnnualCTail_VMC2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC2
  ggplotly(PA_C1y_VMC2)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC2P_YCT <- ModelDFSL_VMC2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC2)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC2P_YCT) <- c("Year", "Months", "AnnualCTail_VMC2")

  PA_CT1y_VMC2 <- ggplot(ModelDFSL_VMC2P_YCT, aes(x = Year, y = AnnualCTail_VMC2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC2
  ggplotly(PA_CT1y_VMC2)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC2P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC2P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC2P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC2P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC2P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC2P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC2P_YCT,"CTails_P\\ModelDFSL_R_C_VMC2P.xlsx") #Yearly C emissions



  #### 10.4 - VMC3) Manure; 30%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC3=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC3=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.3,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC3=data.frame(years,rep(fT_VMC3*fW_VMC3,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC3=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.3, #Percent clay in mineral soil
    xi=xi.frame_VMC3) #Loads the model

  Ct_VMC3=getC(Model_VMC3) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC3, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC3 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC3 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC3 <- as.data.frame(VEC_Lit_VMC3) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC3$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC3 <- as.data.frame(VEC_Man_VMC3) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC3$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC3, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC3, class) #Check that class is numeric
  LitterCinputs_VMC3=VEC_LitDF_VMC3   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC3=VEC_ManDF_VMC3 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC3 <- merge(LitterCinputs_VMC3, ManureCinputs_VMC3, by = "MNumber")
  MCinputs_VMC3$MInput_VMC3 <- MCinputs_VMC3$VEC_Lit_VMC3 + MCinputs_VMC3$VEC_Man_VMC3

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC3)[which(names(MCinputs_VMC3) == "VEC_Lit_VMC3")] <- "LitterC_VMC3"
  colnames(MCinputs_VMC3)[which(names(MCinputs_VMC3) == "VEC_Man_VMC3")] <- "ManureC_VMC3"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC3 <- as.data.frame(Ct_VMC3)
  colnames(ModelDF_VMC3) <- c('DPM_VMC3','RPM_VMC3','BIO_VMC3', 'HUM_VMC3', 'IOM_VMC3')
  ModelDFS_VMC3 <- ModelDF_VMC3[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC3$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC3$AllPools_VMC3 <- ModelDFS_VMC3$DPM_VMC3 + ModelDFS_VMC3$RPM_VMC3 + ModelDFS_VMC3$BIO_VMC3 + ModelDFS_VMC3$HUM_VMC3 + ModelDFS_VMC3$IOM_VMC3

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC3$AllPools_noIOM_VMC3 <- ModelDFS_VMC3$DPM_VMC3 + ModelDFS_VMC3$RPM_VMC3 + ModelDFS_VMC3$BIO_VMC3 + ModelDFS_VMC3$HUM_VMC3

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC3 <- merge(ModelDFS_VMC3, MCinputs_VMC3, by = "MNumber")

  ModelDFSL_VMC3$MInput_VMC3 <- ModelDFSL_VMC3$LitterC_VMC3 + ModelDFSL_VMC3$ManureC_VMC3
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC3$CTails_VMC3 <- ModelDFSL_VMC3$AllPools_noIOM_VMC3 + ModelDFSL_VMC3$MInput_VMC3

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC3$AccumInput_VMC3 = ModelDFSL_VMC3$AccumInput_VMC3=cumsum(ModelDFSL_VMC3$MInput_VMC3)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC3$MGrowth_DPM_VMC3 <- ave(ModelDFSL_VMC3$DPM_VMC3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC3$MGrowth_RPM_VMC3 <- ave(ModelDFSL_VMC3$RPM_VMC3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC3$MGrowth_BIO_VMC3 <- ave(ModelDFSL_VMC3$BIO_VMC3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC3$MGrowth_HUM_VMC3 <- ave(ModelDFSL_VMC3$HUM_VMC3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC3$MGrowth_IOM_VMC3 <- ave(ModelDFSL_VMC3$IOM_VMC3, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC3$M_CCO2_VMC3 <- ModelDFSL_VMC3$MInput_VMC3 - ModelDFSL_VMC3$MGrowth_DPM_VMC3 - ModelDFSL_VMC3$MGrowth_RPM_VMC3 - ModelDFSL_VMC3$MGrowth_BIO_VMC3 - ModelDFSL_VMC3$MGrowth_HUM_VMC3
  ModelDFSL_VMC3$Accum_CCO2_VMC3 <- ModelDFSL_VMC3$AccumInput_VMC3 - ModelDFSL_VMC3$AllPools_noIOM_VMC3

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC3$M_CCO2_VMC3[1] <- 0
  ModelDFSL_VMC3$Accum_CCO2_VMC3[1] <- 0

  #Balance validation
  ModelDFSL_VMC3$Balance_VMC3 <- ModelDFSL_VMC3$AccumInput_VMC3 - ModelDFSL_VMC3$Accum_CCO2_VMC3 - (ModelDFSL_VMC3$DPM_VMC3 + ModelDFSL_VMC3$RPM_VMC3 + ModelDFSL_VMC3$BIO_VMC3 + ModelDFSL_VMC3$HUM_VMC3)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC3$M_CO2_VMC3 <- ModelDFSL_VMC3$M_CCO2_VMC3 * 44/12
  ModelDFSL_VMC3$Accum_CO2_VMC3 <- ModelDFSL_VMC3$Accum_CCO2_VMC3 * 44/12

  #This model will be called VMC3C because implies a continuous input of C
  ModelDFSL_VMC3C <- ModelDFSL_VMC3

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC3C,"VXC_Models\\ModelDFSL_R_VMC3C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC3 <- ModelDFSL_VMC3 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC3.1 <- rbind(c(0:0), ModelDFSLt0_VMC3)
  ModelDFSLt1_VMC3.2 <- rbind(c(0:0), ModelDFSLt1_VMC3.1)
  ModelDFSLt1_VMC3.3 <- rbind(c(0:0), ModelDFSLt1_VMC3.2)
  ModelDFSLt1_VMC3.4 <- rbind(c(0:0), ModelDFSLt1_VMC3.3)
  ModelDFSLt1_VMC3.5 <- rbind(c(0:0), ModelDFSLt1_VMC3.4)
  ModelDFSLt1_VMC3.6 <- rbind(c(0:0), ModelDFSLt1_VMC3.5)
  ModelDFSLt1_VMC3.7 <- rbind(c(0:0), ModelDFSLt1_VMC3.6)
  ModelDFSLt1_VMC3.8 <- rbind(c(0:0), ModelDFSLt1_VMC3.7)
  ModelDFSLt1_VMC3.9 <- rbind(c(0:0), ModelDFSLt1_VMC3.8)
  ModelDFSLt1_VMC3.10 <- rbind(c(0:0), ModelDFSLt1_VMC3.9)
  ModelDFSLt1_VMC3.11 <- rbind(c(0:0), ModelDFSLt1_VMC3.10)
  ModelDFSLt1_VMC3.12 <- rbind(c(0:0), ModelDFSLt1_VMC3.11)
  ModelDFSLt1_VMC3.13 <- ModelDFSLt1_VMC3.12[-nrow(ModelDFSLt1_VMC3.12),]
  ModelDFSLt1_VMC3.14 <- ModelDFSLt1_VMC3.13[-nrow(ModelDFSLt1_VMC3.13),]
  ModelDFSLt1_VMC3.15 <- ModelDFSLt1_VMC3.14[-nrow(ModelDFSLt1_VMC3.14),]
  ModelDFSLt1_VMC3.16 <- ModelDFSLt1_VMC3.15[-nrow(ModelDFSLt1_VMC3.15),]
  ModelDFSLt1_VMC3.17 <- ModelDFSLt1_VMC3.16[-nrow(ModelDFSLt1_VMC3.16),]
  ModelDFSLt1_VMC3.18 <- ModelDFSLt1_VMC3.17[-nrow(ModelDFSLt1_VMC3.17),]
  ModelDFSLt1_VMC3.19 <- ModelDFSLt1_VMC3.18[-nrow(ModelDFSLt1_VMC3.18),]
  ModelDFSLt1_VMC3.20 <- ModelDFSLt1_VMC3.19[-nrow(ModelDFSLt1_VMC3.19),]
  ModelDFSLt1_VMC3.21 <- ModelDFSLt1_VMC3.20[-nrow(ModelDFSLt1_VMC3.20),]
  ModelDFSLt1_VMC3.22 <- ModelDFSLt1_VMC3.21[-nrow(ModelDFSLt1_VMC3.21),]
  ModelDFSLt1_VMC3.23 <- ModelDFSLt1_VMC3.22[-nrow(ModelDFSLt1_VMC3.22),]
  ModelDFSLt1_VMC3.24 <- ModelDFSLt1_VMC3.23[-nrow(ModelDFSLt1_VMC3.23),]

  ModelDFSLt1_VMC3 <- ModelDFSLt1_VMC3.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC3 <- ModelDFSLt0_VMC3 - ModelDFSLt1_VMC3

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC3$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC3P because implies a one-off input of C
  ModelDFSL_VMC3P <- ModelDFSL1y_VMC3

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC3P,"VXP_Models\\ModelDFSL_R_VMC3P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC3 <- ggplot(ModelDFSL_VMC3P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC3, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC3, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC3, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC3, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC3, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC3, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC3, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC3
  ggplotly(P_CFluxI1y_VMC3)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC3P_YCO2 <- ModelDFSL_VMC3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC3P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC3")

  PA_CO21y_VMC3 <- ggplot(ModelDFSL_VMC3P_YCO2, aes(x = Year, y = AnnualCO2_VMC3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC3
  ggplotly(PA_CO21y_VMC3)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC3P_YCO2D <- merge(ModelDFSL_VMC3P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC3P_YCO2D$AnnualCO2D_VMC3 <- ModelDFSL_VMC3P_YCO2D$AnnualCO2_VMC3 * ModelDFSL_VMC3P_YCO2D$GWP100

  PA_CO21yD_VMC3 <- ggplot(ModelDFSL_VMC3P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC3
  ggplotly(PA_CO21yD_VMC3)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC3P_YC <- ModelDFSL_VMC3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC3P_YC) <- c("Year", "Months", "AnnualCTail_VMC3")

  PA_C1y_VMC3 <- ggplot(ModelDFSL_VMC3P_YC, aes(x = Year, y = AnnualCTail_VMC3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC3
  ggplotly(PA_C1y_VMC3)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC3P_YCT <- ModelDFSL_VMC3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC3)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC3P_YCT) <- c("Year", "Months", "AnnualCTail_VMC3")

  PA_CT1y_VMC3 <- ggplot(ModelDFSL_VMC3P_YCT, aes(x = Year, y = AnnualCTail_VMC3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC3
  ggplotly(PA_CT1y_VMC3)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC3P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC3P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC3P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC3P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC3P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC3P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC3P_YCT,"CTails_P\\ModelDFSL_R_C_VMC3P.xlsx") #Yearly C emissions



  #### 10.5 - VMC4) Manure; 40%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC4=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC4=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.4,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC4=data.frame(years,rep(fT_VMC4*fW_VMC4,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC4=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.4, #Percent clay in mineral soil
    xi=xi.frame_VMC4) #Loads the model

  Ct_VMC4=getC(Model_VMC4) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC4, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC4 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC4 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC4 <- as.data.frame(VEC_Lit_VMC4) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC4$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC4 <- as.data.frame(VEC_Man_VMC4) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC4$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC4, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC4, class) #Check that class is numeric
  LitterCinputs_VMC4=VEC_LitDF_VMC4   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC4=VEC_ManDF_VMC4 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC4 <- merge(LitterCinputs_VMC4, ManureCinputs_VMC4, by = "MNumber")
  MCinputs_VMC4$MInput_VMC4 <- MCinputs_VMC4$VEC_Lit_VMC4 + MCinputs_VMC4$VEC_Man_VMC4

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC4)[which(names(MCinputs_VMC4) == "VEC_Lit_VMC4")] <- "LitterC_VMC4"
  colnames(MCinputs_VMC4)[which(names(MCinputs_VMC4) == "VEC_Man_VMC4")] <- "ManureC_VMC4"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC4 <- as.data.frame(Ct_VMC4)
  colnames(ModelDF_VMC4) <- c('DPM_VMC4','RPM_VMC4','BIO_VMC4', 'HUM_VMC4', 'IOM_VMC4')
  ModelDFS_VMC4 <- ModelDF_VMC4[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC4$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC4$AllPools_VMC4 <- ModelDFS_VMC4$DPM_VMC4 + ModelDFS_VMC4$RPM_VMC4 + ModelDFS_VMC4$BIO_VMC4 + ModelDFS_VMC4$HUM_VMC4 + ModelDFS_VMC4$IOM_VMC4

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC4$AllPools_noIOM_VMC4 <- ModelDFS_VMC4$DPM_VMC4 + ModelDFS_VMC4$RPM_VMC4 + ModelDFS_VMC4$BIO_VMC4 + ModelDFS_VMC4$HUM_VMC4

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC4 <- merge(ModelDFS_VMC4, MCinputs_VMC4, by = "MNumber")

  ModelDFSL_VMC4$MInput_VMC4 <- ModelDFSL_VMC4$LitterC_VMC4 + ModelDFSL_VMC4$ManureC_VMC4
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC4$CTails_VMC4 <- ModelDFSL_VMC4$AllPools_noIOM_VMC4 + ModelDFSL_VMC4$MInput_VMC4

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC4$AccumInput_VMC4 = ModelDFSL_VMC4$AccumInput_VMC4=cumsum(ModelDFSL_VMC4$MInput_VMC4)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC4$MGrowth_DPM_VMC4 <- ave(ModelDFSL_VMC4$DPM_VMC4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC4$MGrowth_RPM_VMC4 <- ave(ModelDFSL_VMC4$RPM_VMC4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC4$MGrowth_BIO_VMC4 <- ave(ModelDFSL_VMC4$BIO_VMC4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC4$MGrowth_HUM_VMC4 <- ave(ModelDFSL_VMC4$HUM_VMC4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC4$MGrowth_IOM_VMC4 <- ave(ModelDFSL_VMC4$IOM_VMC4, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC4$M_CCO2_VMC4 <- ModelDFSL_VMC4$MInput_VMC4 - ModelDFSL_VMC4$MGrowth_DPM_VMC4 - ModelDFSL_VMC4$MGrowth_RPM_VMC4 - ModelDFSL_VMC4$MGrowth_BIO_VMC4 - ModelDFSL_VMC4$MGrowth_HUM_VMC4
  ModelDFSL_VMC4$Accum_CCO2_VMC4 <- ModelDFSL_VMC4$AccumInput_VMC4 - ModelDFSL_VMC4$AllPools_noIOM_VMC4

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC4$M_CCO2_VMC4[1] <- 0
  ModelDFSL_VMC4$Accum_CCO2_VMC4[1] <- 0

  #Balance validation
  ModelDFSL_VMC4$Balance_VMC4 <- ModelDFSL_VMC4$AccumInput_VMC4 - ModelDFSL_VMC4$Accum_CCO2_VMC4 - (ModelDFSL_VMC4$DPM_VMC4 + ModelDFSL_VMC4$RPM_VMC4 + ModelDFSL_VMC4$BIO_VMC4 + ModelDFSL_VMC4$HUM_VMC4)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC4$M_CO2_VMC4 <- ModelDFSL_VMC4$M_CCO2_VMC4 * 44/12
  ModelDFSL_VMC4$Accum_CO2_VMC4 <- ModelDFSL_VMC4$Accum_CCO2_VMC4 * 44/12

  #This model will be called VMC4C because implies a continuous input of C
  ModelDFSL_VMC4C <- ModelDFSL_VMC4

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC4C,"VXC_Models\\ModelDFSL_R_VMC4C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC4 <- ModelDFSL_VMC4 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC4.1 <- rbind(c(0:0), ModelDFSLt0_VMC4)
  ModelDFSLt1_VMC4.2 <- rbind(c(0:0), ModelDFSLt1_VMC4.1)
  ModelDFSLt1_VMC4.3 <- rbind(c(0:0), ModelDFSLt1_VMC4.2)
  ModelDFSLt1_VMC4.4 <- rbind(c(0:0), ModelDFSLt1_VMC4.3)
  ModelDFSLt1_VMC4.5 <- rbind(c(0:0), ModelDFSLt1_VMC4.4)
  ModelDFSLt1_VMC4.6 <- rbind(c(0:0), ModelDFSLt1_VMC4.5)
  ModelDFSLt1_VMC4.7 <- rbind(c(0:0), ModelDFSLt1_VMC4.6)
  ModelDFSLt1_VMC4.8 <- rbind(c(0:0), ModelDFSLt1_VMC4.7)
  ModelDFSLt1_VMC4.9 <- rbind(c(0:0), ModelDFSLt1_VMC4.8)
  ModelDFSLt1_VMC4.10 <- rbind(c(0:0), ModelDFSLt1_VMC4.9)
  ModelDFSLt1_VMC4.11 <- rbind(c(0:0), ModelDFSLt1_VMC4.10)
  ModelDFSLt1_VMC4.12 <- rbind(c(0:0), ModelDFSLt1_VMC4.11)
  ModelDFSLt1_VMC4.13 <- ModelDFSLt1_VMC4.12[-nrow(ModelDFSLt1_VMC4.12),]
  ModelDFSLt1_VMC4.14 <- ModelDFSLt1_VMC4.13[-nrow(ModelDFSLt1_VMC4.13),]
  ModelDFSLt1_VMC4.15 <- ModelDFSLt1_VMC4.14[-nrow(ModelDFSLt1_VMC4.14),]
  ModelDFSLt1_VMC4.16 <- ModelDFSLt1_VMC4.15[-nrow(ModelDFSLt1_VMC4.15),]
  ModelDFSLt1_VMC4.17 <- ModelDFSLt1_VMC4.16[-nrow(ModelDFSLt1_VMC4.16),]
  ModelDFSLt1_VMC4.18 <- ModelDFSLt1_VMC4.17[-nrow(ModelDFSLt1_VMC4.17),]
  ModelDFSLt1_VMC4.19 <- ModelDFSLt1_VMC4.18[-nrow(ModelDFSLt1_VMC4.18),]
  ModelDFSLt1_VMC4.20 <- ModelDFSLt1_VMC4.19[-nrow(ModelDFSLt1_VMC4.19),]
  ModelDFSLt1_VMC4.21 <- ModelDFSLt1_VMC4.20[-nrow(ModelDFSLt1_VMC4.20),]
  ModelDFSLt1_VMC4.22 <- ModelDFSLt1_VMC4.21[-nrow(ModelDFSLt1_VMC4.21),]
  ModelDFSLt1_VMC4.23 <- ModelDFSLt1_VMC4.22[-nrow(ModelDFSLt1_VMC4.22),]
  ModelDFSLt1_VMC4.24 <- ModelDFSLt1_VMC4.23[-nrow(ModelDFSLt1_VMC4.23),]

  ModelDFSLt1_VMC4 <- ModelDFSLt1_VMC4.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC4 <- ModelDFSLt0_VMC4 - ModelDFSLt1_VMC4

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC4$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC4P because implies a one-off input of C
  ModelDFSL_VMC4P <- ModelDFSL1y_VMC4

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC4P,"VXP_Models\\ModelDFSL_R_VMC4P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC4 <- ggplot(ModelDFSL_VMC4P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC4, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC4, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC4, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC4, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC4, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC4, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC4, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC4
  ggplotly(P_CFluxI1y_VMC4)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC4P_YCO2 <- ModelDFSL_VMC4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC4P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC4")

  PA_CO21y_VMC4 <- ggplot(ModelDFSL_VMC4P_YCO2, aes(x = Year, y = AnnualCO2_VMC4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC4
  ggplotly(PA_CO21y_VMC4)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC4P_YCO2D <- merge(ModelDFSL_VMC4P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC4P_YCO2D$AnnualCO2D_VMC4 <- ModelDFSL_VMC4P_YCO2D$AnnualCO2_VMC4 * ModelDFSL_VMC4P_YCO2D$GWP100

  PA_CO21yD_VMC4 <- ggplot(ModelDFSL_VMC4P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC4
  ggplotly(PA_CO21yD_VMC4)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC4P_YC <- ModelDFSL_VMC4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC4P_YC) <- c("Year", "Months", "AnnualCTail_VMC4")

  PA_C1y_VMC4 <- ggplot(ModelDFSL_VMC4P_YC, aes(x = Year, y = AnnualCTail_VMC4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC4
  ggplotly(PA_C1y_VMC4)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC4P_YCT <- ModelDFSL_VMC4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC4)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC4P_YCT) <- c("Year", "Months", "AnnualCTail_VMC4")

  PA_CT1y_VMC4 <- ggplot(ModelDFSL_VMC4P_YCT, aes(x = Year, y = AnnualCTail_VMC4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC4
  ggplotly(PA_CT1y_VMC4)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC4P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC4P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC4P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC4P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC4P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC4P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC4P_YCT,"CTails_P\\ModelDFSL_R_C_VMC4P.xlsx") #Yearly C emissions



  #### 10.6 - VMC5) Manure; 50%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC5=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC5=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.5,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC5=data.frame(years,rep(fT_VMC5*fW_VMC5,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC5=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.5, #Percent clay in mineral soil
    xi=xi.frame_VMC5) #Loads the model

  Ct_VMC5=getC(Model_VMC5) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC5, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC5 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC5 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC5 <- as.data.frame(VEC_Lit_VMC5) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC5$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC5 <- as.data.frame(VEC_Man_VMC5) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC5$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC5, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC5, class) #Check that class is numeric
  LitterCinputs_VMC5=VEC_LitDF_VMC5   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC5=VEC_ManDF_VMC5 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC5 <- merge(LitterCinputs_VMC5, ManureCinputs_VMC5, by = "MNumber")
  MCinputs_VMC5$MInput_VMC5 <- MCinputs_VMC5$VEC_Lit_VMC5 + MCinputs_VMC5$VEC_Man_VMC5

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC5)[which(names(MCinputs_VMC5) == "VEC_Lit_VMC5")] <- "LitterC_VMC5"
  colnames(MCinputs_VMC5)[which(names(MCinputs_VMC5) == "VEC_Man_VMC5")] <- "ManureC_VMC5"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC5 <- as.data.frame(Ct_VMC5)
  colnames(ModelDF_VMC5) <- c('DPM_VMC5','RPM_VMC5','BIO_VMC5', 'HUM_VMC5', 'IOM_VMC5')
  ModelDFS_VMC5 <- ModelDF_VMC5[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC5$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC5$AllPools_VMC5 <- ModelDFS_VMC5$DPM_VMC5 + ModelDFS_VMC5$RPM_VMC5 + ModelDFS_VMC5$BIO_VMC5 + ModelDFS_VMC5$HUM_VMC5 + ModelDFS_VMC5$IOM_VMC5

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC5$AllPools_noIOM_VMC5 <- ModelDFS_VMC5$DPM_VMC5 + ModelDFS_VMC5$RPM_VMC5 + ModelDFS_VMC5$BIO_VMC5 + ModelDFS_VMC5$HUM_VMC5

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC5 <- merge(ModelDFS_VMC5, MCinputs_VMC5, by = "MNumber")

  ModelDFSL_VMC5$MInput_VMC5 <- ModelDFSL_VMC5$LitterC_VMC5 + ModelDFSL_VMC5$ManureC_VMC5
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC5$CTails_VMC5 <- ModelDFSL_VMC5$AllPools_noIOM_VMC5 + ModelDFSL_VMC5$MInput_VMC5

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC5$AccumInput_VMC5 = ModelDFSL_VMC5$AccumInput_VMC5=cumsum(ModelDFSL_VMC5$MInput_VMC5)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC5$MGrowth_DPM_VMC5 <- ave(ModelDFSL_VMC5$DPM_VMC5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC5$MGrowth_RPM_VMC5 <- ave(ModelDFSL_VMC5$RPM_VMC5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC5$MGrowth_BIO_VMC5 <- ave(ModelDFSL_VMC5$BIO_VMC5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC5$MGrowth_HUM_VMC5 <- ave(ModelDFSL_VMC5$HUM_VMC5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC5$MGrowth_IOM_VMC5 <- ave(ModelDFSL_VMC5$IOM_VMC5, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC5$M_CCO2_VMC5 <- ModelDFSL_VMC5$MInput_VMC5 - ModelDFSL_VMC5$MGrowth_DPM_VMC5 - ModelDFSL_VMC5$MGrowth_RPM_VMC5 - ModelDFSL_VMC5$MGrowth_BIO_VMC5 - ModelDFSL_VMC5$MGrowth_HUM_VMC5
  ModelDFSL_VMC5$Accum_CCO2_VMC5 <- ModelDFSL_VMC5$AccumInput_VMC5 - ModelDFSL_VMC5$AllPools_noIOM_VMC5

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC5$M_CCO2_VMC5[1] <- 0
  ModelDFSL_VMC5$Accum_CCO2_VMC5[1] <- 0

  #Balance validation
  ModelDFSL_VMC5$Balance_VMC5 <- ModelDFSL_VMC5$AccumInput_VMC5 - ModelDFSL_VMC5$Accum_CCO2_VMC5 - (ModelDFSL_VMC5$DPM_VMC5 + ModelDFSL_VMC5$RPM_VMC5 + ModelDFSL_VMC5$BIO_VMC5 + ModelDFSL_VMC5$HUM_VMC5)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC5$M_CO2_VMC5 <- ModelDFSL_VMC5$M_CCO2_VMC5 * 44/12
  ModelDFSL_VMC5$Accum_CO2_VMC5 <- ModelDFSL_VMC5$Accum_CCO2_VMC5 * 44/12

  #This model will be called VMC5C because implies a continuous input of C
  ModelDFSL_VMC5C <- ModelDFSL_VMC5

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC5C,"VXC_Models\\ModelDFSL_R_VMC5C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC5 <- ModelDFSL_VMC5 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC5.1 <- rbind(c(0:0), ModelDFSLt0_VMC5)
  ModelDFSLt1_VMC5.2 <- rbind(c(0:0), ModelDFSLt1_VMC5.1)
  ModelDFSLt1_VMC5.3 <- rbind(c(0:0), ModelDFSLt1_VMC5.2)
  ModelDFSLt1_VMC5.4 <- rbind(c(0:0), ModelDFSLt1_VMC5.3)
  ModelDFSLt1_VMC5.5 <- rbind(c(0:0), ModelDFSLt1_VMC5.4)
  ModelDFSLt1_VMC5.6 <- rbind(c(0:0), ModelDFSLt1_VMC5.5)
  ModelDFSLt1_VMC5.7 <- rbind(c(0:0), ModelDFSLt1_VMC5.6)
  ModelDFSLt1_VMC5.8 <- rbind(c(0:0), ModelDFSLt1_VMC5.7)
  ModelDFSLt1_VMC5.9 <- rbind(c(0:0), ModelDFSLt1_VMC5.8)
  ModelDFSLt1_VMC5.10 <- rbind(c(0:0), ModelDFSLt1_VMC5.9)
  ModelDFSLt1_VMC5.11 <- rbind(c(0:0), ModelDFSLt1_VMC5.10)
  ModelDFSLt1_VMC5.12 <- rbind(c(0:0), ModelDFSLt1_VMC5.11)
  ModelDFSLt1_VMC5.13 <- ModelDFSLt1_VMC5.12[-nrow(ModelDFSLt1_VMC5.12),]
  ModelDFSLt1_VMC5.14 <- ModelDFSLt1_VMC5.13[-nrow(ModelDFSLt1_VMC5.13),]
  ModelDFSLt1_VMC5.15 <- ModelDFSLt1_VMC5.14[-nrow(ModelDFSLt1_VMC5.14),]
  ModelDFSLt1_VMC5.16 <- ModelDFSLt1_VMC5.15[-nrow(ModelDFSLt1_VMC5.15),]
  ModelDFSLt1_VMC5.17 <- ModelDFSLt1_VMC5.16[-nrow(ModelDFSLt1_VMC5.16),]
  ModelDFSLt1_VMC5.18 <- ModelDFSLt1_VMC5.17[-nrow(ModelDFSLt1_VMC5.17),]
  ModelDFSLt1_VMC5.19 <- ModelDFSLt1_VMC5.18[-nrow(ModelDFSLt1_VMC5.18),]
  ModelDFSLt1_VMC5.20 <- ModelDFSLt1_VMC5.19[-nrow(ModelDFSLt1_VMC5.19),]
  ModelDFSLt1_VMC5.21 <- ModelDFSLt1_VMC5.20[-nrow(ModelDFSLt1_VMC5.20),]
  ModelDFSLt1_VMC5.22 <- ModelDFSLt1_VMC5.21[-nrow(ModelDFSLt1_VMC5.21),]
  ModelDFSLt1_VMC5.23 <- ModelDFSLt1_VMC5.22[-nrow(ModelDFSLt1_VMC5.22),]
  ModelDFSLt1_VMC5.24 <- ModelDFSLt1_VMC5.23[-nrow(ModelDFSLt1_VMC5.23),]

  ModelDFSLt1_VMC5 <- ModelDFSLt1_VMC5.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC5 <- ModelDFSLt0_VMC5 - ModelDFSLt1_VMC5

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC5$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC5P because implies a one-off input of C
  ModelDFSL_VMC5P <- ModelDFSL1y_VMC5

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC5P,"VXP_Models\\ModelDFSL_R_VMC5P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC5 <- ggplot(ModelDFSL_VMC5P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC5, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC5, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC5, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC5, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC5, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC5, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC5, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC5
  ggplotly(P_CFluxI1y_VMC5)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC5P_YCO2 <- ModelDFSL_VMC5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC5P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC5")

  PA_CO21y_VMC5 <- ggplot(ModelDFSL_VMC5P_YCO2, aes(x = Year, y = AnnualCO2_VMC5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC5
  ggplotly(PA_CO21y_VMC5)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC5P_YCO2D <- merge(ModelDFSL_VMC5P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC5P_YCO2D$AnnualCO2D_VMC5 <- ModelDFSL_VMC5P_YCO2D$AnnualCO2_VMC5 * ModelDFSL_VMC5P_YCO2D$GWP100

  PA_CO21yD_VMC5 <- ggplot(ModelDFSL_VMC5P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC5
  ggplotly(PA_CO21yD_VMC5)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC5P_YC <- ModelDFSL_VMC5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC5P_YC) <- c("Year", "Months", "AnnualCTail_VMC5")

  PA_C1y_VMC5 <- ggplot(ModelDFSL_VMC5P_YC, aes(x = Year, y = AnnualCTail_VMC5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC5
  ggplotly(PA_C1y_VMC5)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC5P_YCT <- ModelDFSL_VMC5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC5)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC5P_YCT) <- c("Year", "Months", "AnnualCTail_VMC5")

  PA_CT1y_VMC5 <- ggplot(ModelDFSL_VMC5P_YCT, aes(x = Year, y = AnnualCTail_VMC5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC5
  ggplotly(PA_CT1y_VMC5)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC5P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC5P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC5P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC5P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC5P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC5P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC5P_YCT,"CTails_P\\ModelDFSL_R_C_VMC5P.xlsx") #Yearly C emissions



  #### 10.7 - VMC6) Manure; 60%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC6=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC6=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.6,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC6=data.frame(years,rep(fT_VMC6*fW_VMC6,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC6=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.6, #Percent clay in mineral soil
    xi=xi.frame_VMC6) #Loads the model

  Ct_VMC6=getC(Model_VMC6) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC6, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC6 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC6 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC6 <- as.data.frame(VEC_Lit_VMC6) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC6$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC6 <- as.data.frame(VEC_Man_VMC6) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC6$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC6, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC6, class) #Check that class is numeric
  LitterCinputs_VMC6=VEC_LitDF_VMC6   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC6=VEC_ManDF_VMC6 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC6 <- merge(LitterCinputs_VMC6, ManureCinputs_VMC6, by = "MNumber")
  MCinputs_VMC6$MInput_VMC6 <- MCinputs_VMC6$VEC_Lit_VMC6 + MCinputs_VMC6$VEC_Man_VMC6

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC6)[which(names(MCinputs_VMC6) == "VEC_Lit_VMC6")] <- "LitterC_VMC6"
  colnames(MCinputs_VMC6)[which(names(MCinputs_VMC6) == "VEC_Man_VMC6")] <- "ManureC_VMC6"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC6 <- as.data.frame(Ct_VMC6)
  colnames(ModelDF_VMC6) <- c('DPM_VMC6','RPM_VMC6','BIO_VMC6', 'HUM_VMC6', 'IOM_VMC6')
  ModelDFS_VMC6 <- ModelDF_VMC6[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC6$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC6$AllPools_VMC6 <- ModelDFS_VMC6$DPM_VMC6 + ModelDFS_VMC6$RPM_VMC6 + ModelDFS_VMC6$BIO_VMC6 + ModelDFS_VMC6$HUM_VMC6 + ModelDFS_VMC6$IOM_VMC6

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC6$AllPools_noIOM_VMC6 <- ModelDFS_VMC6$DPM_VMC6 + ModelDFS_VMC6$RPM_VMC6 + ModelDFS_VMC6$BIO_VMC6 + ModelDFS_VMC6$HUM_VMC6

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC6 <- merge(ModelDFS_VMC6, MCinputs_VMC6, by = "MNumber")

  ModelDFSL_VMC6$MInput_VMC6 <- ModelDFSL_VMC6$LitterC_VMC6 + ModelDFSL_VMC6$ManureC_VMC6
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC6$CTails_VMC6 <- ModelDFSL_VMC6$AllPools_noIOM_VMC6 + ModelDFSL_VMC6$MInput_VMC6

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC6$AccumInput_VMC6 = ModelDFSL_VMC6$AccumInput_VMC6=cumsum(ModelDFSL_VMC6$MInput_VMC6)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC6$MGrowth_DPM_VMC6 <- ave(ModelDFSL_VMC6$DPM_VMC6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC6$MGrowth_RPM_VMC6 <- ave(ModelDFSL_VMC6$RPM_VMC6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC6$MGrowth_BIO_VMC6 <- ave(ModelDFSL_VMC6$BIO_VMC6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC6$MGrowth_HUM_VMC6 <- ave(ModelDFSL_VMC6$HUM_VMC6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC6$MGrowth_IOM_VMC6 <- ave(ModelDFSL_VMC6$IOM_VMC6, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC6$M_CCO2_VMC6 <- ModelDFSL_VMC6$MInput_VMC6 - ModelDFSL_VMC6$MGrowth_DPM_VMC6 - ModelDFSL_VMC6$MGrowth_RPM_VMC6 - ModelDFSL_VMC6$MGrowth_BIO_VMC6 - ModelDFSL_VMC6$MGrowth_HUM_VMC6
  ModelDFSL_VMC6$Accum_CCO2_VMC6 <- ModelDFSL_VMC6$AccumInput_VMC6 - ModelDFSL_VMC6$AllPools_noIOM_VMC6

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC6$M_CCO2_VMC6[1] <- 0
  ModelDFSL_VMC6$Accum_CCO2_VMC6[1] <- 0

  #Balance validation
  ModelDFSL_VMC6$Balance_VMC6 <- ModelDFSL_VMC6$AccumInput_VMC6 - ModelDFSL_VMC6$Accum_CCO2_VMC6 - (ModelDFSL_VMC6$DPM_VMC6 + ModelDFSL_VMC6$RPM_VMC6 + ModelDFSL_VMC6$BIO_VMC6 + ModelDFSL_VMC6$HUM_VMC6)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC6$M_CO2_VMC6 <- ModelDFSL_VMC6$M_CCO2_VMC6 * 44/12
  ModelDFSL_VMC6$Accum_CO2_VMC6 <- ModelDFSL_VMC6$Accum_CCO2_VMC6 * 44/12

  #This model will be called VMC6C because implies a continuous input of C
  ModelDFSL_VMC6C <- ModelDFSL_VMC6

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC6C,"VXC_Models\\ModelDFSL_R_VMC6C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC6 <- ModelDFSL_VMC6 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC6.1 <- rbind(c(0:0), ModelDFSLt0_VMC6)
  ModelDFSLt1_VMC6.2 <- rbind(c(0:0), ModelDFSLt1_VMC6.1)
  ModelDFSLt1_VMC6.3 <- rbind(c(0:0), ModelDFSLt1_VMC6.2)
  ModelDFSLt1_VMC6.4 <- rbind(c(0:0), ModelDFSLt1_VMC6.3)
  ModelDFSLt1_VMC6.5 <- rbind(c(0:0), ModelDFSLt1_VMC6.4)
  ModelDFSLt1_VMC6.6 <- rbind(c(0:0), ModelDFSLt1_VMC6.5)
  ModelDFSLt1_VMC6.7 <- rbind(c(0:0), ModelDFSLt1_VMC6.6)
  ModelDFSLt1_VMC6.8 <- rbind(c(0:0), ModelDFSLt1_VMC6.7)
  ModelDFSLt1_VMC6.9 <- rbind(c(0:0), ModelDFSLt1_VMC6.8)
  ModelDFSLt1_VMC6.10 <- rbind(c(0:0), ModelDFSLt1_VMC6.9)
  ModelDFSLt1_VMC6.11 <- rbind(c(0:0), ModelDFSLt1_VMC6.10)
  ModelDFSLt1_VMC6.12 <- rbind(c(0:0), ModelDFSLt1_VMC6.11)
  ModelDFSLt1_VMC6.13 <- ModelDFSLt1_VMC6.12[-nrow(ModelDFSLt1_VMC6.12),]
  ModelDFSLt1_VMC6.14 <- ModelDFSLt1_VMC6.13[-nrow(ModelDFSLt1_VMC6.13),]
  ModelDFSLt1_VMC6.15 <- ModelDFSLt1_VMC6.14[-nrow(ModelDFSLt1_VMC6.14),]
  ModelDFSLt1_VMC6.16 <- ModelDFSLt1_VMC6.15[-nrow(ModelDFSLt1_VMC6.15),]
  ModelDFSLt1_VMC6.17 <- ModelDFSLt1_VMC6.16[-nrow(ModelDFSLt1_VMC6.16),]
  ModelDFSLt1_VMC6.18 <- ModelDFSLt1_VMC6.17[-nrow(ModelDFSLt1_VMC6.17),]
  ModelDFSLt1_VMC6.19 <- ModelDFSLt1_VMC6.18[-nrow(ModelDFSLt1_VMC6.18),]
  ModelDFSLt1_VMC6.20 <- ModelDFSLt1_VMC6.19[-nrow(ModelDFSLt1_VMC6.19),]
  ModelDFSLt1_VMC6.21 <- ModelDFSLt1_VMC6.20[-nrow(ModelDFSLt1_VMC6.20),]
  ModelDFSLt1_VMC6.22 <- ModelDFSLt1_VMC6.21[-nrow(ModelDFSLt1_VMC6.21),]
  ModelDFSLt1_VMC6.23 <- ModelDFSLt1_VMC6.22[-nrow(ModelDFSLt1_VMC6.22),]
  ModelDFSLt1_VMC6.24 <- ModelDFSLt1_VMC6.23[-nrow(ModelDFSLt1_VMC6.23),]

  ModelDFSLt1_VMC6 <- ModelDFSLt1_VMC6.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC6 <- ModelDFSLt0_VMC6 - ModelDFSLt1_VMC6

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC6$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC6P because implies a one-off input of C
  ModelDFSL_VMC6P <- ModelDFSL1y_VMC6

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC6P,"VXP_Models\\ModelDFSL_R_VMC6P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC6 <- ggplot(ModelDFSL_VMC6P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC6, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC6, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC6, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC6, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC6, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC6, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC6, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC6
  ggplotly(P_CFluxI1y_VMC6)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC6P_YCO2 <- ModelDFSL_VMC6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC6P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC6")

  PA_CO21y_VMC6 <- ggplot(ModelDFSL_VMC6P_YCO2, aes(x = Year, y = AnnualCO2_VMC6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC6
  ggplotly(PA_CO21y_VMC6)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC6P_YCO2D <- merge(ModelDFSL_VMC6P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC6P_YCO2D$AnnualCO2D_VMC6 <- ModelDFSL_VMC6P_YCO2D$AnnualCO2_VMC6 * ModelDFSL_VMC6P_YCO2D$GWP100

  PA_CO21yD_VMC6 <- ggplot(ModelDFSL_VMC6P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC6
  ggplotly(PA_CO21yD_VMC6)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC6P_YC <- ModelDFSL_VMC6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC6P_YC) <- c("Year", "Months", "AnnualCTail_VMC6")

  PA_C1y_VMC6 <- ggplot(ModelDFSL_VMC6P_YC, aes(x = Year, y = AnnualCTail_VMC6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC6
  ggplotly(PA_C1y_VMC6)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC6P_YCT <- ModelDFSL_VMC6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC6)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC6P_YCT) <- c("Year", "Months", "AnnualCTail_VMC6")

  PA_CT1y_VMC6 <- ggplot(ModelDFSL_VMC6P_YCT, aes(x = Year, y = AnnualCTail_VMC6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC6
  ggplotly(PA_CT1y_VMC6)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC6P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC6P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC6P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC6P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC6P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC6P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC6P_YCT,"CTails_P\\ModelDFSL_R_C_VMC6P.xlsx") #Yearly C emissions



  #### 10.8 - VMC7) Manure; 70%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC7=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC7=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.7,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC7=data.frame(years,rep(fT_VMC7*fW_VMC7,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC7=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.7, #Percent clay in mineral soil
    xi=xi.frame_VMC7) #Loads the model

  Ct_VMC7=getC(Model_VMC7) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC7, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC7 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC7 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC7 <- as.data.frame(VEC_Lit_VMC7) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC7$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC7 <- as.data.frame(VEC_Man_VMC7) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC7$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC7, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC7, class) #Check that class is numeric
  LitterCinputs_VMC7=VEC_LitDF_VMC7   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC7=VEC_ManDF_VMC7 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC7 <- merge(LitterCinputs_VMC7, ManureCinputs_VMC7, by = "MNumber")
  MCinputs_VMC7$MInput_VMC7 <- MCinputs_VMC7$VEC_Lit_VMC7 + MCinputs_VMC7$VEC_Man_VMC7

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC7)[which(names(MCinputs_VMC7) == "VEC_Lit_VMC7")] <- "LitterC_VMC7"
  colnames(MCinputs_VMC7)[which(names(MCinputs_VMC7) == "VEC_Man_VMC7")] <- "ManureC_VMC7"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC7 <- as.data.frame(Ct_VMC7)
  colnames(ModelDF_VMC7) <- c('DPM_VMC7','RPM_VMC7','BIO_VMC7', 'HUM_VMC7', 'IOM_VMC7')
  ModelDFS_VMC7 <- ModelDF_VMC7[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC7$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC7$AllPools_VMC7 <- ModelDFS_VMC7$DPM_VMC7 + ModelDFS_VMC7$RPM_VMC7 + ModelDFS_VMC7$BIO_VMC7 + ModelDFS_VMC7$HUM_VMC7 + ModelDFS_VMC7$IOM_VMC7

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC7$AllPools_noIOM_VMC7 <- ModelDFS_VMC7$DPM_VMC7 + ModelDFS_VMC7$RPM_VMC7 + ModelDFS_VMC7$BIO_VMC7 + ModelDFS_VMC7$HUM_VMC7

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC7 <- merge(ModelDFS_VMC7, MCinputs_VMC7, by = "MNumber")

  ModelDFSL_VMC7$MInput_VMC7 <- ModelDFSL_VMC7$LitterC_VMC7 + ModelDFSL_VMC7$ManureC_VMC7
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC7$CTails_VMC7 <- ModelDFSL_VMC7$AllPools_noIOM_VMC7 + ModelDFSL_VMC7$MInput_VMC7

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC7$AccumInput_VMC7 = ModelDFSL_VMC7$AccumInput_VMC7=cumsum(ModelDFSL_VMC7$MInput_VMC7)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC7$MGrowth_DPM_VMC7 <- ave(ModelDFSL_VMC7$DPM_VMC7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC7$MGrowth_RPM_VMC7 <- ave(ModelDFSL_VMC7$RPM_VMC7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC7$MGrowth_BIO_VMC7 <- ave(ModelDFSL_VMC7$BIO_VMC7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC7$MGrowth_HUM_VMC7 <- ave(ModelDFSL_VMC7$HUM_VMC7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC7$MGrowth_IOM_VMC7 <- ave(ModelDFSL_VMC7$IOM_VMC7, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC7$M_CCO2_VMC7 <- ModelDFSL_VMC7$MInput_VMC7 - ModelDFSL_VMC7$MGrowth_DPM_VMC7 - ModelDFSL_VMC7$MGrowth_RPM_VMC7 - ModelDFSL_VMC7$MGrowth_BIO_VMC7 - ModelDFSL_VMC7$MGrowth_HUM_VMC7
  ModelDFSL_VMC7$Accum_CCO2_VMC7 <- ModelDFSL_VMC7$AccumInput_VMC7 - ModelDFSL_VMC7$AllPools_noIOM_VMC7

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC7$M_CCO2_VMC7[1] <- 0
  ModelDFSL_VMC7$Accum_CCO2_VMC7[1] <- 0

  #Balance validation
  ModelDFSL_VMC7$Balance_VMC7 <- ModelDFSL_VMC7$AccumInput_VMC7 - ModelDFSL_VMC7$Accum_CCO2_VMC7 - (ModelDFSL_VMC7$DPM_VMC7 + ModelDFSL_VMC7$RPM_VMC7 + ModelDFSL_VMC7$BIO_VMC7 + ModelDFSL_VMC7$HUM_VMC7)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC7$M_CO2_VMC7 <- ModelDFSL_VMC7$M_CCO2_VMC7 * 44/12
  ModelDFSL_VMC7$Accum_CO2_VMC7 <- ModelDFSL_VMC7$Accum_CCO2_VMC7 * 44/12

  #This model will be called VMC7C because implies a continuous input of C
  ModelDFSL_VMC7C <- ModelDFSL_VMC7

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC7C,"VXC_Models\\ModelDFSL_R_VMC7C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC7 <- ModelDFSL_VMC7 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC7.1 <- rbind(c(0:0), ModelDFSLt0_VMC7)
  ModelDFSLt1_VMC7.2 <- rbind(c(0:0), ModelDFSLt1_VMC7.1)
  ModelDFSLt1_VMC7.3 <- rbind(c(0:0), ModelDFSLt1_VMC7.2)
  ModelDFSLt1_VMC7.4 <- rbind(c(0:0), ModelDFSLt1_VMC7.3)
  ModelDFSLt1_VMC7.5 <- rbind(c(0:0), ModelDFSLt1_VMC7.4)
  ModelDFSLt1_VMC7.6 <- rbind(c(0:0), ModelDFSLt1_VMC7.5)
  ModelDFSLt1_VMC7.7 <- rbind(c(0:0), ModelDFSLt1_VMC7.6)
  ModelDFSLt1_VMC7.8 <- rbind(c(0:0), ModelDFSLt1_VMC7.7)
  ModelDFSLt1_VMC7.9 <- rbind(c(0:0), ModelDFSLt1_VMC7.8)
  ModelDFSLt1_VMC7.10 <- rbind(c(0:0), ModelDFSLt1_VMC7.9)
  ModelDFSLt1_VMC7.11 <- rbind(c(0:0), ModelDFSLt1_VMC7.10)
  ModelDFSLt1_VMC7.12 <- rbind(c(0:0), ModelDFSLt1_VMC7.11)
  ModelDFSLt1_VMC7.13 <- ModelDFSLt1_VMC7.12[-nrow(ModelDFSLt1_VMC7.12),]
  ModelDFSLt1_VMC7.14 <- ModelDFSLt1_VMC7.13[-nrow(ModelDFSLt1_VMC7.13),]
  ModelDFSLt1_VMC7.15 <- ModelDFSLt1_VMC7.14[-nrow(ModelDFSLt1_VMC7.14),]
  ModelDFSLt1_VMC7.16 <- ModelDFSLt1_VMC7.15[-nrow(ModelDFSLt1_VMC7.15),]
  ModelDFSLt1_VMC7.17 <- ModelDFSLt1_VMC7.16[-nrow(ModelDFSLt1_VMC7.16),]
  ModelDFSLt1_VMC7.18 <- ModelDFSLt1_VMC7.17[-nrow(ModelDFSLt1_VMC7.17),]
  ModelDFSLt1_VMC7.19 <- ModelDFSLt1_VMC7.18[-nrow(ModelDFSLt1_VMC7.18),]
  ModelDFSLt1_VMC7.20 <- ModelDFSLt1_VMC7.19[-nrow(ModelDFSLt1_VMC7.19),]
  ModelDFSLt1_VMC7.21 <- ModelDFSLt1_VMC7.20[-nrow(ModelDFSLt1_VMC7.20),]
  ModelDFSLt1_VMC7.22 <- ModelDFSLt1_VMC7.21[-nrow(ModelDFSLt1_VMC7.21),]
  ModelDFSLt1_VMC7.23 <- ModelDFSLt1_VMC7.22[-nrow(ModelDFSLt1_VMC7.22),]
  ModelDFSLt1_VMC7.24 <- ModelDFSLt1_VMC7.23[-nrow(ModelDFSLt1_VMC7.23),]

  ModelDFSLt1_VMC7 <- ModelDFSLt1_VMC7.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC7 <- ModelDFSLt0_VMC7 - ModelDFSLt1_VMC7

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC7$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC7P because implies a one-off input of C
  ModelDFSL_VMC7P <- ModelDFSL1y_VMC7

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC7P,"VXP_Models\\ModelDFSL_R_VMC7P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC7 <- ggplot(ModelDFSL_VMC7P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC7, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC7, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC7, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC7, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC7, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC7, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC7, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC7
  ggplotly(P_CFluxI1y_VMC7)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC7P_YCO2 <- ModelDFSL_VMC7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC7P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC7")

  PA_CO21y_VMC7 <- ggplot(ModelDFSL_VMC7P_YCO2, aes(x = Year, y = AnnualCO2_VMC7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC7
  ggplotly(PA_CO21y_VMC7)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC7P_YCO2D <- merge(ModelDFSL_VMC7P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC7P_YCO2D$AnnualCO2D_VMC7 <- ModelDFSL_VMC7P_YCO2D$AnnualCO2_VMC7 * ModelDFSL_VMC7P_YCO2D$GWP100

  PA_CO21yD_VMC7 <- ggplot(ModelDFSL_VMC7P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC7
  ggplotly(PA_CO21yD_VMC7)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC7P_YC <- ModelDFSL_VMC7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC7P_YC) <- c("Year", "Months", "AnnualCTail_VMC7")

  PA_C1y_VMC7 <- ggplot(ModelDFSL_VMC7P_YC, aes(x = Year, y = AnnualCTail_VMC7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC7
  ggplotly(PA_C1y_VMC7)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC7P_YCT <- ModelDFSL_VMC7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC7)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC7P_YCT) <- c("Year", "Months", "AnnualCTail_VMC7")

  PA_CT1y_VMC7 <- ggplot(ModelDFSL_VMC7P_YCT, aes(x = Year, y = AnnualCTail_VMC7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC7
  ggplotly(PA_CT1y_VMC7)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC7P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC7P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC7P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC7P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC7P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC7P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC7P_YCT,"CTails_P\\ModelDFSL_R_C_VMC7P.xlsx") #Yearly C emissions



  #### 10.9 - VMC8) Manure; 80%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC8=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC8=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.8,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC8=data.frame(years,rep(fT_VMC8*fW_VMC8,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC8=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.8, #Percent clay in mineral soil
    xi=xi.frame_VMC8) #Loads the model

  Ct_VMC8=getC(Model_VMC8) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC8, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC8 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC8 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC8 <- as.data.frame(VEC_Lit_VMC8) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC8$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC8 <- as.data.frame(VEC_Man_VMC8) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC8$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC8, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC8, class) #Check that class is numeric
  LitterCinputs_VMC8=VEC_LitDF_VMC8   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC8=VEC_ManDF_VMC8 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC8 <- merge(LitterCinputs_VMC8, ManureCinputs_VMC8, by = "MNumber")
  MCinputs_VMC8$MInput_VMC8 <- MCinputs_VMC8$VEC_Lit_VMC8 + MCinputs_VMC8$VEC_Man_VMC8

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC8)[which(names(MCinputs_VMC8) == "VEC_Lit_VMC8")] <- "LitterC_VMC8"
  colnames(MCinputs_VMC8)[which(names(MCinputs_VMC8) == "VEC_Man_VMC8")] <- "ManureC_VMC8"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC8 <- as.data.frame(Ct_VMC8)
  colnames(ModelDF_VMC8) <- c('DPM_VMC8','RPM_VMC8','BIO_VMC8', 'HUM_VMC8', 'IOM_VMC8')
  ModelDFS_VMC8 <- ModelDF_VMC8[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC8$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC8$AllPools_VMC8 <- ModelDFS_VMC8$DPM_VMC8 + ModelDFS_VMC8$RPM_VMC8 + ModelDFS_VMC8$BIO_VMC8 + ModelDFS_VMC8$HUM_VMC8 + ModelDFS_VMC8$IOM_VMC8

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC8$AllPools_noIOM_VMC8 <- ModelDFS_VMC8$DPM_VMC8 + ModelDFS_VMC8$RPM_VMC8 + ModelDFS_VMC8$BIO_VMC8 + ModelDFS_VMC8$HUM_VMC8

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC8 <- merge(ModelDFS_VMC8, MCinputs_VMC8, by = "MNumber")

  ModelDFSL_VMC8$MInput_VMC8 <- ModelDFSL_VMC8$LitterC_VMC8 + ModelDFSL_VMC8$ManureC_VMC8
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC8$CTails_VMC8 <- ModelDFSL_VMC8$AllPools_noIOM_VMC8 + ModelDFSL_VMC8$MInput_VMC8

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC8$AccumInput_VMC8 = ModelDFSL_VMC8$AccumInput_VMC8=cumsum(ModelDFSL_VMC8$MInput_VMC8)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC8$MGrowth_DPM_VMC8 <- ave(ModelDFSL_VMC8$DPM_VMC8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC8$MGrowth_RPM_VMC8 <- ave(ModelDFSL_VMC8$RPM_VMC8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC8$MGrowth_BIO_VMC8 <- ave(ModelDFSL_VMC8$BIO_VMC8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC8$MGrowth_HUM_VMC8 <- ave(ModelDFSL_VMC8$HUM_VMC8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC8$MGrowth_IOM_VMC8 <- ave(ModelDFSL_VMC8$IOM_VMC8, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC8$M_CCO2_VMC8 <- ModelDFSL_VMC8$MInput_VMC8 - ModelDFSL_VMC8$MGrowth_DPM_VMC8 - ModelDFSL_VMC8$MGrowth_RPM_VMC8 - ModelDFSL_VMC8$MGrowth_BIO_VMC8 - ModelDFSL_VMC8$MGrowth_HUM_VMC8
  ModelDFSL_VMC8$Accum_CCO2_VMC8 <- ModelDFSL_VMC8$AccumInput_VMC8 - ModelDFSL_VMC8$AllPools_noIOM_VMC8

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC8$M_CCO2_VMC8[1] <- 0
  ModelDFSL_VMC8$Accum_CCO2_VMC8[1] <- 0

  #Balance validation
  ModelDFSL_VMC8$Balance_VMC8 <- ModelDFSL_VMC8$AccumInput_VMC8 - ModelDFSL_VMC8$Accum_CCO2_VMC8 - (ModelDFSL_VMC8$DPM_VMC8 + ModelDFSL_VMC8$RPM_VMC8 + ModelDFSL_VMC8$BIO_VMC8 + ModelDFSL_VMC8$HUM_VMC8)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC8$M_CO2_VMC8 <- ModelDFSL_VMC8$M_CCO2_VMC8 * 44/12
  ModelDFSL_VMC8$Accum_CO2_VMC8 <- ModelDFSL_VMC8$Accum_CCO2_VMC8 * 44/12

  #This model will be called VMC8C because implies a continuous input of C
  ModelDFSL_VMC8C <- ModelDFSL_VMC8

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC8C,"VXC_Models\\ModelDFSL_R_VMC8C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC8 <- ModelDFSL_VMC8 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC8.1 <- rbind(c(0:0), ModelDFSLt0_VMC8)
  ModelDFSLt1_VMC8.2 <- rbind(c(0:0), ModelDFSLt1_VMC8.1)
  ModelDFSLt1_VMC8.3 <- rbind(c(0:0), ModelDFSLt1_VMC8.2)
  ModelDFSLt1_VMC8.4 <- rbind(c(0:0), ModelDFSLt1_VMC8.3)
  ModelDFSLt1_VMC8.5 <- rbind(c(0:0), ModelDFSLt1_VMC8.4)
  ModelDFSLt1_VMC8.6 <- rbind(c(0:0), ModelDFSLt1_VMC8.5)
  ModelDFSLt1_VMC8.7 <- rbind(c(0:0), ModelDFSLt1_VMC8.6)
  ModelDFSLt1_VMC8.8 <- rbind(c(0:0), ModelDFSLt1_VMC8.7)
  ModelDFSLt1_VMC8.9 <- rbind(c(0:0), ModelDFSLt1_VMC8.8)
  ModelDFSLt1_VMC8.10 <- rbind(c(0:0), ModelDFSLt1_VMC8.9)
  ModelDFSLt1_VMC8.11 <- rbind(c(0:0), ModelDFSLt1_VMC8.10)
  ModelDFSLt1_VMC8.12 <- rbind(c(0:0), ModelDFSLt1_VMC8.11)
  ModelDFSLt1_VMC8.13 <- ModelDFSLt1_VMC8.12[-nrow(ModelDFSLt1_VMC8.12),]
  ModelDFSLt1_VMC8.14 <- ModelDFSLt1_VMC8.13[-nrow(ModelDFSLt1_VMC8.13),]
  ModelDFSLt1_VMC8.15 <- ModelDFSLt1_VMC8.14[-nrow(ModelDFSLt1_VMC8.14),]
  ModelDFSLt1_VMC8.16 <- ModelDFSLt1_VMC8.15[-nrow(ModelDFSLt1_VMC8.15),]
  ModelDFSLt1_VMC8.17 <- ModelDFSLt1_VMC8.16[-nrow(ModelDFSLt1_VMC8.16),]
  ModelDFSLt1_VMC8.18 <- ModelDFSLt1_VMC8.17[-nrow(ModelDFSLt1_VMC8.17),]
  ModelDFSLt1_VMC8.19 <- ModelDFSLt1_VMC8.18[-nrow(ModelDFSLt1_VMC8.18),]
  ModelDFSLt1_VMC8.20 <- ModelDFSLt1_VMC8.19[-nrow(ModelDFSLt1_VMC8.19),]
  ModelDFSLt1_VMC8.21 <- ModelDFSLt1_VMC8.20[-nrow(ModelDFSLt1_VMC8.20),]
  ModelDFSLt1_VMC8.22 <- ModelDFSLt1_VMC8.21[-nrow(ModelDFSLt1_VMC8.21),]
  ModelDFSLt1_VMC8.23 <- ModelDFSLt1_VMC8.22[-nrow(ModelDFSLt1_VMC8.22),]
  ModelDFSLt1_VMC8.24 <- ModelDFSLt1_VMC8.23[-nrow(ModelDFSLt1_VMC8.23),]

  ModelDFSLt1_VMC8 <- ModelDFSLt1_VMC8.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC8 <- ModelDFSLt0_VMC8 - ModelDFSLt1_VMC8

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC8$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC8P because implies a one-off input of C
  ModelDFSL_VMC8P <- ModelDFSL1y_VMC8

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC8P,"VXP_Models\\ModelDFSL_R_VMC8P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC8 <- ggplot(ModelDFSL_VMC8P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC8, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC8, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC8, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC8, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC8, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC8, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC8, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC8
  ggplotly(P_CFluxI1y_VMC8)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC8P_YCO2 <- ModelDFSL_VMC8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC8P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC8")

  PA_CO21y_VMC8 <- ggplot(ModelDFSL_VMC8P_YCO2, aes(x = Year, y = AnnualCO2_VMC8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC8
  ggplotly(PA_CO21y_VMC8)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC8P_YCO2D <- merge(ModelDFSL_VMC8P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC8P_YCO2D$AnnualCO2D_VMC8 <- ModelDFSL_VMC8P_YCO2D$AnnualCO2_VMC8 * ModelDFSL_VMC8P_YCO2D$GWP100

  PA_CO21yD_VMC8 <- ggplot(ModelDFSL_VMC8P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC8
  ggplotly(PA_CO21yD_VMC8)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC8P_YC <- ModelDFSL_VMC8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC8P_YC) <- c("Year", "Months", "AnnualCTail_VMC8")

  PA_C1y_VMC8 <- ggplot(ModelDFSL_VMC8P_YC, aes(x = Year, y = AnnualCTail_VMC8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC8
  ggplotly(PA_C1y_VMC8)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC8P_YCT <- ModelDFSL_VMC8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC8)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC8P_YCT) <- c("Year", "Months", "AnnualCTail_VMC8")

  PA_CT1y_VMC8 <- ggplot(ModelDFSL_VMC8P_YCT, aes(x = Year, y = AnnualCTail_VMC8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC8
  ggplotly(PA_CT1y_VMC8)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC8P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC8P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC8P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC8P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC8P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC8P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC8P_YCT,"CTails_P\\ModelDFSL_R_C_VMC8P.xlsx") #Yearly C emissions



  #### 10.10 - VMC9) Manure; 90%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC9=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC9=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.9,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC9=data.frame(years,rep(fT_VMC9*fW_VMC9,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC9=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.9, #Percent clay in mineral soil
    xi=xi.frame_VMC9) #Loads the model

  Ct_VMC9=getC(Model_VMC9) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC9, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC9 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC9 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC9 <- as.data.frame(VEC_Lit_VMC9) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC9$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC9 <- as.data.frame(VEC_Man_VMC9) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC9$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC9, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC9, class) #Check that class is numeric
  LitterCinputs_VMC9=VEC_LitDF_VMC9   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC9=VEC_ManDF_VMC9 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC9 <- merge(LitterCinputs_VMC9, ManureCinputs_VMC9, by = "MNumber")
  MCinputs_VMC9$MInput_VMC9 <- MCinputs_VMC9$VEC_Lit_VMC9 + MCinputs_VMC9$VEC_Man_VMC9

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC9)[which(names(MCinputs_VMC9) == "VEC_Lit_VMC9")] <- "LitterC_VMC9"
  colnames(MCinputs_VMC9)[which(names(MCinputs_VMC9) == "VEC_Man_VMC9")] <- "ManureC_VMC9"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC9 <- as.data.frame(Ct_VMC9)
  colnames(ModelDF_VMC9) <- c('DPM_VMC9','RPM_VMC9','BIO_VMC9', 'HUM_VMC9', 'IOM_VMC9')
  ModelDFS_VMC9 <- ModelDF_VMC9[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC9$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC9$AllPools_VMC9 <- ModelDFS_VMC9$DPM_VMC9 + ModelDFS_VMC9$RPM_VMC9 + ModelDFS_VMC9$BIO_VMC9 + ModelDFS_VMC9$HUM_VMC9 + ModelDFS_VMC9$IOM_VMC9

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC9$AllPools_noIOM_VMC9 <- ModelDFS_VMC9$DPM_VMC9 + ModelDFS_VMC9$RPM_VMC9 + ModelDFS_VMC9$BIO_VMC9 + ModelDFS_VMC9$HUM_VMC9

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC9 <- merge(ModelDFS_VMC9, MCinputs_VMC9, by = "MNumber")

  ModelDFSL_VMC9$MInput_VMC9 <- ModelDFSL_VMC9$LitterC_VMC9 + ModelDFSL_VMC9$ManureC_VMC9
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC9$CTails_VMC9 <- ModelDFSL_VMC9$AllPools_noIOM_VMC9 + ModelDFSL_VMC9$MInput_VMC9

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC9$AccumInput_VMC9 = ModelDFSL_VMC9$AccumInput_VMC9=cumsum(ModelDFSL_VMC9$MInput_VMC9)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC9$MGrowth_DPM_VMC9 <- ave(ModelDFSL_VMC9$DPM_VMC9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC9$MGrowth_RPM_VMC9 <- ave(ModelDFSL_VMC9$RPM_VMC9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC9$MGrowth_BIO_VMC9 <- ave(ModelDFSL_VMC9$BIO_VMC9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC9$MGrowth_HUM_VMC9 <- ave(ModelDFSL_VMC9$HUM_VMC9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC9$MGrowth_IOM_VMC9 <- ave(ModelDFSL_VMC9$IOM_VMC9, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC9$M_CCO2_VMC9 <- ModelDFSL_VMC9$MInput_VMC9 - ModelDFSL_VMC9$MGrowth_DPM_VMC9 - ModelDFSL_VMC9$MGrowth_RPM_VMC9 - ModelDFSL_VMC9$MGrowth_BIO_VMC9 - ModelDFSL_VMC9$MGrowth_HUM_VMC9
  ModelDFSL_VMC9$Accum_CCO2_VMC9 <- ModelDFSL_VMC9$AccumInput_VMC9 - ModelDFSL_VMC9$AllPools_noIOM_VMC9

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC9$M_CCO2_VMC9[1] <- 0
  ModelDFSL_VMC9$Accum_CCO2_VMC9[1] <- 0

  #Balance validation
  ModelDFSL_VMC9$Balance_VMC9 <- ModelDFSL_VMC9$AccumInput_VMC9 - ModelDFSL_VMC9$Accum_CCO2_VMC9 - (ModelDFSL_VMC9$DPM_VMC9 + ModelDFSL_VMC9$RPM_VMC9 + ModelDFSL_VMC9$BIO_VMC9 + ModelDFSL_VMC9$HUM_VMC9)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC9$M_CO2_VMC9 <- ModelDFSL_VMC9$M_CCO2_VMC9 * 44/12
  ModelDFSL_VMC9$Accum_CO2_VMC9 <- ModelDFSL_VMC9$Accum_CCO2_VMC9 * 44/12

  #This model will be called VMC9C because implies a continuous input of C
  ModelDFSL_VMC9C <- ModelDFSL_VMC9

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC9C,"VXC_Models\\ModelDFSL_R_VMC9C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC9 <- ModelDFSL_VMC9 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC9.1 <- rbind(c(0:0), ModelDFSLt0_VMC9)
  ModelDFSLt1_VMC9.2 <- rbind(c(0:0), ModelDFSLt1_VMC9.1)
  ModelDFSLt1_VMC9.3 <- rbind(c(0:0), ModelDFSLt1_VMC9.2)
  ModelDFSLt1_VMC9.4 <- rbind(c(0:0), ModelDFSLt1_VMC9.3)
  ModelDFSLt1_VMC9.5 <- rbind(c(0:0), ModelDFSLt1_VMC9.4)
  ModelDFSLt1_VMC9.6 <- rbind(c(0:0), ModelDFSLt1_VMC9.5)
  ModelDFSLt1_VMC9.7 <- rbind(c(0:0), ModelDFSLt1_VMC9.6)
  ModelDFSLt1_VMC9.8 <- rbind(c(0:0), ModelDFSLt1_VMC9.7)
  ModelDFSLt1_VMC9.9 <- rbind(c(0:0), ModelDFSLt1_VMC9.8)
  ModelDFSLt1_VMC9.10 <- rbind(c(0:0), ModelDFSLt1_VMC9.9)
  ModelDFSLt1_VMC9.11 <- rbind(c(0:0), ModelDFSLt1_VMC9.10)
  ModelDFSLt1_VMC9.12 <- rbind(c(0:0), ModelDFSLt1_VMC9.11)
  ModelDFSLt1_VMC9.13 <- ModelDFSLt1_VMC9.12[-nrow(ModelDFSLt1_VMC9.12),]
  ModelDFSLt1_VMC9.14 <- ModelDFSLt1_VMC9.13[-nrow(ModelDFSLt1_VMC9.13),]
  ModelDFSLt1_VMC9.15 <- ModelDFSLt1_VMC9.14[-nrow(ModelDFSLt1_VMC9.14),]
  ModelDFSLt1_VMC9.16 <- ModelDFSLt1_VMC9.15[-nrow(ModelDFSLt1_VMC9.15),]
  ModelDFSLt1_VMC9.17 <- ModelDFSLt1_VMC9.16[-nrow(ModelDFSLt1_VMC9.16),]
  ModelDFSLt1_VMC9.18 <- ModelDFSLt1_VMC9.17[-nrow(ModelDFSLt1_VMC9.17),]
  ModelDFSLt1_VMC9.19 <- ModelDFSLt1_VMC9.18[-nrow(ModelDFSLt1_VMC9.18),]
  ModelDFSLt1_VMC9.20 <- ModelDFSLt1_VMC9.19[-nrow(ModelDFSLt1_VMC9.19),]
  ModelDFSLt1_VMC9.21 <- ModelDFSLt1_VMC9.20[-nrow(ModelDFSLt1_VMC9.20),]
  ModelDFSLt1_VMC9.22 <- ModelDFSLt1_VMC9.21[-nrow(ModelDFSLt1_VMC9.21),]
  ModelDFSLt1_VMC9.23 <- ModelDFSLt1_VMC9.22[-nrow(ModelDFSLt1_VMC9.22),]
  ModelDFSLt1_VMC9.24 <- ModelDFSLt1_VMC9.23[-nrow(ModelDFSLt1_VMC9.23),]

  ModelDFSLt1_VMC9 <- ModelDFSLt1_VMC9.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC9 <- ModelDFSLt0_VMC9 - ModelDFSLt1_VMC9

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC9$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC9P because implies a one-off input of C
  ModelDFSL_VMC9P <- ModelDFSL1y_VMC9

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC9P,"VXP_Models\\ModelDFSL_R_VMC9P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC9 <- ggplot(ModelDFSL_VMC9P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC9, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC9, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC9, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC9, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC9, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC9, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC9, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC9
  ggplotly(P_CFluxI1y_VMC9)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC9P_YCO2 <- ModelDFSL_VMC9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC9P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC9")

  PA_CO21y_VMC9 <- ggplot(ModelDFSL_VMC9P_YCO2, aes(x = Year, y = AnnualCO2_VMC9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC9
  ggplotly(PA_CO21y_VMC9)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC9P_YCO2D <- merge(ModelDFSL_VMC9P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC9P_YCO2D$AnnualCO2D_VMC9 <- ModelDFSL_VMC9P_YCO2D$AnnualCO2_VMC9 * ModelDFSL_VMC9P_YCO2D$GWP100

  PA_CO21yD_VMC9 <- ggplot(ModelDFSL_VMC9P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC9
  ggplotly(PA_CO21yD_VMC9)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC9P_YC <- ModelDFSL_VMC9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC9P_YC) <- c("Year", "Months", "AnnualCTail_VMC9")

  PA_C1y_VMC9 <- ggplot(ModelDFSL_VMC9P_YC, aes(x = Year, y = AnnualCTail_VMC9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC9
  ggplotly(PA_C1y_VMC9)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC9P_YCT <- ModelDFSL_VMC9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC9)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC9P_YCT) <- c("Year", "Months", "AnnualCTail_VMC9")

  PA_CT1y_VMC9 <- ggplot(ModelDFSL_VMC9P_YCT, aes(x = Year, y = AnnualCTail_VMC9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC9
  ggplotly(PA_CT1y_VMC9)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC9P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC9P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC9P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC9P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC9P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC9P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC9P_YCT,"CTails_P\\ModelDFSL_R_C_VMC9P.xlsx") #Yearly C emissions

  #### 10.11 - VMC10) Manure; 100%clay; Conventional tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMC10=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMC10=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                           S.Thick = soil.thick, pClay = clay100,
                           pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMC10=data.frame(years,rep(fT_VMC10*fW_VMC10,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMC10=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100, #Percent clay in mineral soil
    xi=xi.frame_VMC10) #Loads the model

  Ct_VMC10=getC(Model_VMC10) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMC10, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMC10 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMC10 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMC10 <- as.data.frame(VEC_Lit_VMC10) #Converting the Litter vector to a data frame
  VEC_LitDF_VMC10$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMC10 <- as.data.frame(VEC_Man_VMC10) #Converting the Manure vector to a data frame
  VEC_ManDF_VMC10$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMC10, class) #Check that class is numeric
  sapply(VEC_ManDF_VMC10, class) #Check that class is numeric
  LitterCinputs_VMC10=VEC_LitDF_VMC10   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMC10=VEC_ManDF_VMC10 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMC10 <- merge(LitterCinputs_VMC10, ManureCinputs_VMC10, by = "MNumber")
  MCinputs_VMC10$MInput_VMC10 <- MCinputs_VMC10$VEC_Lit_VMC10 + MCinputs_VMC10$VEC_Man_VMC10

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMC10)[which(names(MCinputs_VMC10) == "VEC_Lit_VMC10")] <- "LitterC_VMC10"
  colnames(MCinputs_VMC10)[which(names(MCinputs_VMC10) == "VEC_Man_VMC10")] <- "ManureC_VMC10"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMC10 <- as.data.frame(Ct_VMC10)
  colnames(ModelDF_VMC10) <- c('DPM_VMC10','RPM_VMC10','BIO_VMC10', 'HUM_VMC10', 'IOM_VMC10')
  ModelDFS_VMC10 <- ModelDF_VMC10[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMC10$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMC10$AllPools_VMC10 <- ModelDFS_VMC10$DPM_VMC10 + ModelDFS_VMC10$RPM_VMC10 + ModelDFS_VMC10$BIO_VMC10 + ModelDFS_VMC10$HUM_VMC10 + ModelDFS_VMC10$IOM_VMC10

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMC10$AllPools_noIOM_VMC10 <- ModelDFS_VMC10$DPM_VMC10 + ModelDFS_VMC10$RPM_VMC10 + ModelDFS_VMC10$BIO_VMC10 + ModelDFS_VMC10$HUM_VMC10

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMC10 <- merge(ModelDFS_VMC10, MCinputs_VMC10, by = "MNumber")

  ModelDFSL_VMC10$MInput_VMC10 <- ModelDFSL_VMC10$LitterC_VMC10 + ModelDFSL_VMC10$ManureC_VMC10
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMC10$CTails_VMC10 <- ModelDFSL_VMC10$AllPools_noIOM_VMC10 + ModelDFSL_VMC10$MInput_VMC10

  #Create Monthly Accumulated input of C
  ModelDFSL_VMC10$AccumInput_VMC10 = ModelDFSL_VMC10$AccumInput_VMC10=cumsum(ModelDFSL_VMC10$MInput_VMC10)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMC10$MGrowth_DPM_VMC10 <- ave(ModelDFSL_VMC10$DPM_VMC10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC10$MGrowth_RPM_VMC10 <- ave(ModelDFSL_VMC10$RPM_VMC10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC10$MGrowth_BIO_VMC10 <- ave(ModelDFSL_VMC10$BIO_VMC10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC10$MGrowth_HUM_VMC10 <- ave(ModelDFSL_VMC10$HUM_VMC10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMC10$MGrowth_IOM_VMC10 <- ave(ModelDFSL_VMC10$IOM_VMC10, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMC10$M_CCO2_VMC10 <- ModelDFSL_VMC10$MInput_VMC10 - ModelDFSL_VMC10$MGrowth_DPM_VMC10 - ModelDFSL_VMC10$MGrowth_RPM_VMC10 - ModelDFSL_VMC10$MGrowth_BIO_VMC10 - ModelDFSL_VMC10$MGrowth_HUM_VMC10
  ModelDFSL_VMC10$Accum_CCO2_VMC10 <- ModelDFSL_VMC10$AccumInput_VMC10 - ModelDFSL_VMC10$AllPools_noIOM_VMC10

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMC10$M_CCO2_VMC10[1] <- 0
  ModelDFSL_VMC10$Accum_CCO2_VMC10[1] <- 0

  #Balance validation
  ModelDFSL_VMC10$Balance_VMC10 <- ModelDFSL_VMC10$AccumInput_VMC10 - ModelDFSL_VMC10$Accum_CCO2_VMC10 - (ModelDFSL_VMC10$DPM_VMC10 + ModelDFSL_VMC10$RPM_VMC10 + ModelDFSL_VMC10$BIO_VMC10 + ModelDFSL_VMC10$HUM_VMC10)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMC10$M_CO2_VMC10 <- ModelDFSL_VMC10$M_CCO2_VMC10 * 44/12
  ModelDFSL_VMC10$Accum_CO2_VMC10 <- ModelDFSL_VMC10$Accum_CCO2_VMC10 * 44/12

  #This model will be called VMC10C because implies a continuous input of C
  ModelDFSL_VMC10C <- ModelDFSL_VMC10

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC10C,"VXC_Models\\ModelDFSL_R_VMC10C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMC10 <- ModelDFSL_VMC10 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMC10.1 <- rbind(c(0:0), ModelDFSLt0_VMC10)
  ModelDFSLt1_VMC10.2 <- rbind(c(0:0), ModelDFSLt1_VMC10.1)
  ModelDFSLt1_VMC10.3 <- rbind(c(0:0), ModelDFSLt1_VMC10.2)
  ModelDFSLt1_VMC10.4 <- rbind(c(0:0), ModelDFSLt1_VMC10.3)
  ModelDFSLt1_VMC10.5 <- rbind(c(0:0), ModelDFSLt1_VMC10.4)
  ModelDFSLt1_VMC10.6 <- rbind(c(0:0), ModelDFSLt1_VMC10.5)
  ModelDFSLt1_VMC10.7 <- rbind(c(0:0), ModelDFSLt1_VMC10.6)
  ModelDFSLt1_VMC10.8 <- rbind(c(0:0), ModelDFSLt1_VMC10.7)
  ModelDFSLt1_VMC10.9 <- rbind(c(0:0), ModelDFSLt1_VMC10.8)
  ModelDFSLt1_VMC10.10 <- rbind(c(0:0), ModelDFSLt1_VMC10.9)
  ModelDFSLt1_VMC10.11 <- rbind(c(0:0), ModelDFSLt1_VMC10.10)
  ModelDFSLt1_VMC10.12 <- rbind(c(0:0), ModelDFSLt1_VMC10.11)
  ModelDFSLt1_VMC10.13 <- ModelDFSLt1_VMC10.12[-nrow(ModelDFSLt1_VMC10.12),]
  ModelDFSLt1_VMC10.14 <- ModelDFSLt1_VMC10.13[-nrow(ModelDFSLt1_VMC10.13),]
  ModelDFSLt1_VMC10.15 <- ModelDFSLt1_VMC10.14[-nrow(ModelDFSLt1_VMC10.14),]
  ModelDFSLt1_VMC10.16 <- ModelDFSLt1_VMC10.15[-nrow(ModelDFSLt1_VMC10.15),]
  ModelDFSLt1_VMC10.17 <- ModelDFSLt1_VMC10.16[-nrow(ModelDFSLt1_VMC10.16),]
  ModelDFSLt1_VMC10.18 <- ModelDFSLt1_VMC10.17[-nrow(ModelDFSLt1_VMC10.17),]
  ModelDFSLt1_VMC10.19 <- ModelDFSLt1_VMC10.18[-nrow(ModelDFSLt1_VMC10.18),]
  ModelDFSLt1_VMC10.20 <- ModelDFSLt1_VMC10.19[-nrow(ModelDFSLt1_VMC10.19),]
  ModelDFSLt1_VMC10.21 <- ModelDFSLt1_VMC10.20[-nrow(ModelDFSLt1_VMC10.20),]
  ModelDFSLt1_VMC10.22 <- ModelDFSLt1_VMC10.21[-nrow(ModelDFSLt1_VMC10.21),]
  ModelDFSLt1_VMC10.23 <- ModelDFSLt1_VMC10.22[-nrow(ModelDFSLt1_VMC10.22),]
  ModelDFSLt1_VMC10.24 <- ModelDFSLt1_VMC10.23[-nrow(ModelDFSLt1_VMC10.23),]

  ModelDFSLt1_VMC10 <- ModelDFSLt1_VMC10.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMC10 <- ModelDFSLt0_VMC10 - ModelDFSLt1_VMC10

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMC10$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMC10P because implies a one-off input of C
  ModelDFSL_VMC10P <- ModelDFSL1y_VMC10

  #Export the dataframe
  write_xlsx(ModelDFSL_VMC10P,"VXP_Models\\ModelDFSL_R_VMC10P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMC10 <- ggplot(ModelDFSL_VMC10P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMC10, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMC10, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMC10, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMC10, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMC10, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMC10, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMC10, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMC10
  ggplotly(P_CFluxI1y_VMC10)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMC10P_YCO2 <- ModelDFSL_VMC10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMC10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC10P_YCO2) <- c("Year", "Months", "AnnualCO2_VMC10")

  PA_CO21y_VMC10 <- ggplot(ModelDFSL_VMC10P_YCO2, aes(x = Year, y = AnnualCO2_VMC10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMC10
  ggplotly(PA_CO21y_VMC10)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMC10P_YCO2D <- merge(ModelDFSL_VMC10P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMC10P_YCO2D$AnnualCO2D_VMC10 <- ModelDFSL_VMC10P_YCO2D$AnnualCO2_VMC10 * ModelDFSL_VMC10P_YCO2D$GWP100

  PA_CO21yD_VMC10 <- ggplot(ModelDFSL_VMC10P_YCO2D, aes(x = Year, y = AnnualCO2D_VMC10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMC10
  ggplotly(PA_CO21yD_VMC10)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMC10P_YC <- ModelDFSL_VMC10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMC10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC10P_YC) <- c("Year", "Months", "AnnualCTail_VMC10")

  PA_C1y_VMC10 <- ggplot(ModelDFSL_VMC10P_YC, aes(x = Year, y = AnnualCTail_VMC10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMC10
  ggplotly(PA_C1y_VMC10)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMC10P_YCT <- ModelDFSL_VMC10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMC10)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMC10P_YCT) <- c("Year", "Months", "AnnualCTail_VMC10")

  PA_CT1y_VMC10 <- ggplot(ModelDFSL_VMC10P_YCT, aes(x = Year, y = AnnualCTail_VMC10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMC10
  ggplotly(PA_CT1y_VMC10)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMC10P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMC10P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMC10P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMC10P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMC10P_YC,"CEmissions_P\\ModelDFSL_R_C_VMC10P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMC10P_YCT,"CTails_P\\ModelDFSL_R_C_VMC10P.xlsx") #Yearly C emissions









}
