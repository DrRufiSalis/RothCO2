#' Run RothC_VMC
#'
#' The Merge_VXC function merge all VXC models created by the functions Run_RothC_[].
#'
#' @return A dataframe with all VXC models merged together
#' @import SoilR ggplot2 stringr
#' @importFrom plotly ggplotly
#' @export

#Function to Run and Create the multiple RothC Combinations


Run_RothC_VMR <- function(SL_years = 100,
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

  #### 11) Model Combinations - Manure; Reduced Tillage, 0-100% (+10%) Clay ####
  #### 11.1 - VMR0) Manure; 0%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR0=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR0=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay0,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR0=data.frame(years,rep(fT_VMR0*fW_VMR0,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR0=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay0, #Percent clay in mineral soil
    xi=xi.frame_VMR0) #Loads the model

  Ct_VMR0=getC(Model_VMR0) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR0, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR0 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR0 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR0 <- as.data.frame(VEC_Lit_VMR0) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR0$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR0 <- as.data.frame(VEC_Man_VMR0) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR0$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR0, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR0, class) #Check that class is numeric
  LitterCinputs_VMR0=VEC_LitDF_VMR0   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR0=VEC_ManDF_VMR0 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR0 <- merge(LitterCinputs_VMR0, ManureCinputs_VMR0, by = "MNumber")
  MCinputs_VMR0$MInput_VMR0 <- MCinputs_VMR0$VEC_Lit_VMR0 + MCinputs_VMR0$VEC_Man_VMR0

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR0)[which(names(MCinputs_VMR0) == "VEC_Lit_VMR0")] <- "LitterC_VMR0"
  colnames(MCinputs_VMR0)[which(names(MCinputs_VMR0) == "VEC_Man_VMR0")] <- "ManureC_VMR0"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR0 <- as.data.frame(Ct_VMR0)
  colnames(ModelDF_VMR0) <- c('DPM_VMR0','RPM_VMR0','BIO_VMR0', 'HUM_VMR0', 'IOM_VMR0')
  ModelDFS_VMR0 <- ModelDF_VMR0[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR0$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR0$AllPools_VMR0 <- ModelDFS_VMR0$DPM_VMR0 + ModelDFS_VMR0$RPM_VMR0 + ModelDFS_VMR0$BIO_VMR0 + ModelDFS_VMR0$HUM_VMR0 + ModelDFS_VMR0$IOM_VMR0

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR0$AllPools_noIOM_VMR0 <- ModelDFS_VMR0$DPM_VMR0 + ModelDFS_VMR0$RPM_VMR0 + ModelDFS_VMR0$BIO_VMR0 + ModelDFS_VMR0$HUM_VMR0

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR0 <- merge(ModelDFS_VMR0, MCinputs_VMR0, by = "MNumber")

  ModelDFSL_VMR0$MInput_VMR0 <- ModelDFSL_VMR0$LitterC_VMR0 + ModelDFSL_VMR0$ManureC_VMR0
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR0$CTails_VMR0 <- ModelDFSL_VMR0$AllPools_noIOM_VMR0 + ModelDFSL_VMR0$MInput_VMR0

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR0$AccumInput_VMR0 = ModelDFSL_VMR0$AccumInput_VMR0=cumsum(ModelDFSL_VMR0$MInput_VMR0)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR0$MGrowth_DPM_VMR0 <- ave(ModelDFSL_VMR0$DPM_VMR0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR0$MGrowth_RPM_VMR0 <- ave(ModelDFSL_VMR0$RPM_VMR0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR0$MGrowth_BIO_VMR0 <- ave(ModelDFSL_VMR0$BIO_VMR0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR0$MGrowth_HUM_VMR0 <- ave(ModelDFSL_VMR0$HUM_VMR0, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR0$MGrowth_IOM_VMR0 <- ave(ModelDFSL_VMR0$IOM_VMR0, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR0$M_CCO2_VMR0 <- ModelDFSL_VMR0$MInput_VMR0 - ModelDFSL_VMR0$MGrowth_DPM_VMR0 - ModelDFSL_VMR0$MGrowth_RPM_VMR0 - ModelDFSL_VMR0$MGrowth_BIO_VMR0 - ModelDFSL_VMR0$MGrowth_HUM_VMR0
  ModelDFSL_VMR0$Accum_CCO2_VMR0 <- ModelDFSL_VMR0$AccumInput_VMR0 - ModelDFSL_VMR0$AllPools_noIOM_VMR0

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR0$M_CCO2_VMR0[1] <- 0
  ModelDFSL_VMR0$Accum_CCO2_VMR0[1] <- 0

  #Balance validation
  ModelDFSL_VMR0$Balance_VMR0 <- ModelDFSL_VMR0$AccumInput_VMR0 - ModelDFSL_VMR0$Accum_CCO2_VMR0 - (ModelDFSL_VMR0$DPM_VMR0 + ModelDFSL_VMR0$RPM_VMR0 + ModelDFSL_VMR0$BIO_VMR0 + ModelDFSL_VMR0$HUM_VMR0)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR0$M_CO2_VMR0 <- ModelDFSL_VMR0$M_CCO2_VMR0 * 44/12
  ModelDFSL_VMR0$Accum_CO2_VMR0 <- ModelDFSL_VMR0$Accum_CCO2_VMR0 * 44/12

  #This model will be called VMR0C because implies a continuous input of C
  ModelDFSL_VMR0C <- ModelDFSL_VMR0

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR0C,"VXC_Models\\ModelDFSL_R_VMR0C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR0 <- ModelDFSL_VMR0 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR0.1 <- rbind(c(0:0), ModelDFSLt0_VMR0)
  ModelDFSLt1_VMR0.2 <- rbind(c(0:0), ModelDFSLt1_VMR0.1)
  ModelDFSLt1_VMR0.3 <- rbind(c(0:0), ModelDFSLt1_VMR0.2)
  ModelDFSLt1_VMR0.4 <- rbind(c(0:0), ModelDFSLt1_VMR0.3)
  ModelDFSLt1_VMR0.5 <- rbind(c(0:0), ModelDFSLt1_VMR0.4)
  ModelDFSLt1_VMR0.6 <- rbind(c(0:0), ModelDFSLt1_VMR0.5)
  ModelDFSLt1_VMR0.7 <- rbind(c(0:0), ModelDFSLt1_VMR0.6)
  ModelDFSLt1_VMR0.8 <- rbind(c(0:0), ModelDFSLt1_VMR0.7)
  ModelDFSLt1_VMR0.9 <- rbind(c(0:0), ModelDFSLt1_VMR0.8)
  ModelDFSLt1_VMR0.10 <- rbind(c(0:0), ModelDFSLt1_VMR0.9)
  ModelDFSLt1_VMR0.11 <- rbind(c(0:0), ModelDFSLt1_VMR0.10)
  ModelDFSLt1_VMR0.12 <- rbind(c(0:0), ModelDFSLt1_VMR0.11)
  ModelDFSLt1_VMR0.13 <- ModelDFSLt1_VMR0.12[-nrow(ModelDFSLt1_VMR0.12),]
  ModelDFSLt1_VMR0.14 <- ModelDFSLt1_VMR0.13[-nrow(ModelDFSLt1_VMR0.13),]
  ModelDFSLt1_VMR0.15 <- ModelDFSLt1_VMR0.14[-nrow(ModelDFSLt1_VMR0.14),]
  ModelDFSLt1_VMR0.16 <- ModelDFSLt1_VMR0.15[-nrow(ModelDFSLt1_VMR0.15),]
  ModelDFSLt1_VMR0.17 <- ModelDFSLt1_VMR0.16[-nrow(ModelDFSLt1_VMR0.16),]
  ModelDFSLt1_VMR0.18 <- ModelDFSLt1_VMR0.17[-nrow(ModelDFSLt1_VMR0.17),]
  ModelDFSLt1_VMR0.19 <- ModelDFSLt1_VMR0.18[-nrow(ModelDFSLt1_VMR0.18),]
  ModelDFSLt1_VMR0.20 <- ModelDFSLt1_VMR0.19[-nrow(ModelDFSLt1_VMR0.19),]
  ModelDFSLt1_VMR0.21 <- ModelDFSLt1_VMR0.20[-nrow(ModelDFSLt1_VMR0.20),]
  ModelDFSLt1_VMR0.22 <- ModelDFSLt1_VMR0.21[-nrow(ModelDFSLt1_VMR0.21),]
  ModelDFSLt1_VMR0.23 <- ModelDFSLt1_VMR0.22[-nrow(ModelDFSLt1_VMR0.22),]
  ModelDFSLt1_VMR0.24 <- ModelDFSLt1_VMR0.23[-nrow(ModelDFSLt1_VMR0.23),]

  ModelDFSLt1_VMR0 <- ModelDFSLt1_VMR0.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR0 <- ModelDFSLt0_VMR0 - ModelDFSLt1_VMR0

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR0$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR0P because implies a one-off input of C
  ModelDFSL_VMR0P <- ModelDFSL1y_VMR0

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR0P,"VXP_Models\\ModelDFSL_R_VMR0P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR0 <- ggplot(ModelDFSL_VMR0P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR0, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR0, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR0, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR0, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR0, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR0, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR0, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR0
  ggplotly(P_CFluxI1y_VMR0)

  #Plot 3.1: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR0P_YCO2 <- ModelDFSL_VMR0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR0P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR0")

  PA_CO21y_VMR0 <- ggplot(ModelDFSL_VMR0P_YCO2, aes(x = Year, y = AnnualCO2_VMR0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR0
  ggplotly(PA_CO21y_VMR0)

  #Plot 3.2: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR0P_YCO2D <- merge(ModelDFSL_VMR0P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR0P_YCO2D$AnnualCO2D_VMR0 <- ModelDFSL_VMR0P_YCO2D$AnnualCO2_VMR0 * ModelDFSL_VMR0P_YCO2D$GWP100

  PA_CO21yD_VMR0 <- ggplot(ModelDFSL_VMR0P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR0
  ggplotly(PA_CO21yD_VMR0)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR0P_YC <- ModelDFSL_VMR0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR0P_YC) <- c("Year", "Months", "AnnualCTail_VMR0")

  PA_C1y_VMR0 <- ggplot(ModelDFSL_VMR0P_YC, aes(x = Year, y = AnnualCTail_VMR0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR0
  ggplotly(PA_C1y_VMR0)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR0P_YCT <- ModelDFSL_VMR0P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR0)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR0P_YCT) <- c("Year", "Months", "AnnualCTail_VMR0")

  PA_CT1y_VMR0 <- ggplot(ModelDFSL_VMR0P_YCT, aes(x = Year, y = AnnualCTail_VMR0)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR0
  ggplotly(PA_CT1y_VMR0)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR0P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR0P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR0P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR0P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR0P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR0P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR0P_YCT,"CTails_P\\ModelDFSL_R_C_VMR0P.xlsx") #Yearly C emissions


  #### 11.2 - VMR1) Manure; 10%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR1=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR1=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.1,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR1=data.frame(years,rep(fT_VMR1*fW_VMR1,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR1=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.1, #Percent clay in mineral soil
    xi=xi.frame_VMR1) #Loads the model

  Ct_VMR1=getC(Model_VMR1) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR1, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR1 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR1 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR1 <- as.data.frame(VEC_Lit_VMR1) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR1$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR1 <- as.data.frame(VEC_Man_VMR1) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR1$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR1, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR1, class) #Check that class is numeric
  LitterCinputs_VMR1=VEC_LitDF_VMR1   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR1=VEC_ManDF_VMR1 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR1 <- merge(LitterCinputs_VMR1, ManureCinputs_VMR1, by = "MNumber")
  MCinputs_VMR1$MInput_VMR1 <- MCinputs_VMR1$VEC_Lit_VMR1 + MCinputs_VMR1$VEC_Man_VMR1

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR1)[which(names(MCinputs_VMR1) == "VEC_Lit_VMR1")] <- "LitterC_VMR1"
  colnames(MCinputs_VMR1)[which(names(MCinputs_VMR1) == "VEC_Man_VMR1")] <- "ManureC_VMR1"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR1 <- as.data.frame(Ct_VMR1)
  colnames(ModelDF_VMR1) <- c('DPM_VMR1','RPM_VMR1','BIO_VMR1', 'HUM_VMR1', 'IOM_VMR1')
  ModelDFS_VMR1 <- ModelDF_VMR1[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR1$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR1$AllPools_VMR1 <- ModelDFS_VMR1$DPM_VMR1 + ModelDFS_VMR1$RPM_VMR1 + ModelDFS_VMR1$BIO_VMR1 + ModelDFS_VMR1$HUM_VMR1 + ModelDFS_VMR1$IOM_VMR1

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR1$AllPools_noIOM_VMR1 <- ModelDFS_VMR1$DPM_VMR1 + ModelDFS_VMR1$RPM_VMR1 + ModelDFS_VMR1$BIO_VMR1 + ModelDFS_VMR1$HUM_VMR1

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR1 <- merge(ModelDFS_VMR1, MCinputs_VMR1, by = "MNumber")

  ModelDFSL_VMR1$MInput_VMR1 <- ModelDFSL_VMR1$LitterC_VMR1 + ModelDFSL_VMR1$ManureC_VMR1
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR1$CTails_VMR1 <- ModelDFSL_VMR1$AllPools_noIOM_VMR1 + ModelDFSL_VMR1$MInput_VMR1

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR1$AccumInput_VMR1 = ModelDFSL_VMR1$AccumInput_VMR1=cumsum(ModelDFSL_VMR1$MInput_VMR1)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR1$MGrowth_DPM_VMR1 <- ave(ModelDFSL_VMR1$DPM_VMR1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR1$MGrowth_RPM_VMR1 <- ave(ModelDFSL_VMR1$RPM_VMR1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR1$MGrowth_BIO_VMR1 <- ave(ModelDFSL_VMR1$BIO_VMR1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR1$MGrowth_HUM_VMR1 <- ave(ModelDFSL_VMR1$HUM_VMR1, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR1$MGrowth_IOM_VMR1 <- ave(ModelDFSL_VMR1$IOM_VMR1, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR1$M_CCO2_VMR1 <- ModelDFSL_VMR1$MInput_VMR1 - ModelDFSL_VMR1$MGrowth_DPM_VMR1 - ModelDFSL_VMR1$MGrowth_RPM_VMR1 - ModelDFSL_VMR1$MGrowth_BIO_VMR1 - ModelDFSL_VMR1$MGrowth_HUM_VMR1
  ModelDFSL_VMR1$Accum_CCO2_VMR1 <- ModelDFSL_VMR1$AccumInput_VMR1 - ModelDFSL_VMR1$AllPools_noIOM_VMR1

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR1$M_CCO2_VMR1[1] <- 0
  ModelDFSL_VMR1$Accum_CCO2_VMR1[1] <- 0

  #Balance validation
  ModelDFSL_VMR1$Balance_VMR1 <- ModelDFSL_VMR1$AccumInput_VMR1 - ModelDFSL_VMR1$Accum_CCO2_VMR1 - (ModelDFSL_VMR1$DPM_VMR1 + ModelDFSL_VMR1$RPM_VMR1 + ModelDFSL_VMR1$BIO_VMR1 + ModelDFSL_VMR1$HUM_VMR1)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR1$M_CO2_VMR1 <- ModelDFSL_VMR1$M_CCO2_VMR1 * 44/12
  ModelDFSL_VMR1$Accum_CO2_VMR1 <- ModelDFSL_VMR1$Accum_CCO2_VMR1 * 44/12

  #This model will be called VMR1C because implies a continuous input of C
  ModelDFSL_VMR1C <- ModelDFSL_VMR1

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR1C,"VXC_Models\\ModelDFSL_R_VMR1C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR1 <- ModelDFSL_VMR1 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR1.1 <- rbind(c(0:0), ModelDFSLt0_VMR1)
  ModelDFSLt1_VMR1.2 <- rbind(c(0:0), ModelDFSLt1_VMR1.1)
  ModelDFSLt1_VMR1.3 <- rbind(c(0:0), ModelDFSLt1_VMR1.2)
  ModelDFSLt1_VMR1.4 <- rbind(c(0:0), ModelDFSLt1_VMR1.3)
  ModelDFSLt1_VMR1.5 <- rbind(c(0:0), ModelDFSLt1_VMR1.4)
  ModelDFSLt1_VMR1.6 <- rbind(c(0:0), ModelDFSLt1_VMR1.5)
  ModelDFSLt1_VMR1.7 <- rbind(c(0:0), ModelDFSLt1_VMR1.6)
  ModelDFSLt1_VMR1.8 <- rbind(c(0:0), ModelDFSLt1_VMR1.7)
  ModelDFSLt1_VMR1.9 <- rbind(c(0:0), ModelDFSLt1_VMR1.8)
  ModelDFSLt1_VMR1.10 <- rbind(c(0:0), ModelDFSLt1_VMR1.9)
  ModelDFSLt1_VMR1.11 <- rbind(c(0:0), ModelDFSLt1_VMR1.10)
  ModelDFSLt1_VMR1.12 <- rbind(c(0:0), ModelDFSLt1_VMR1.11)
  ModelDFSLt1_VMR1.13 <- ModelDFSLt1_VMR1.12[-nrow(ModelDFSLt1_VMR1.12),]
  ModelDFSLt1_VMR1.14 <- ModelDFSLt1_VMR1.13[-nrow(ModelDFSLt1_VMR1.13),]
  ModelDFSLt1_VMR1.15 <- ModelDFSLt1_VMR1.14[-nrow(ModelDFSLt1_VMR1.14),]
  ModelDFSLt1_VMR1.16 <- ModelDFSLt1_VMR1.15[-nrow(ModelDFSLt1_VMR1.15),]
  ModelDFSLt1_VMR1.17 <- ModelDFSLt1_VMR1.16[-nrow(ModelDFSLt1_VMR1.16),]
  ModelDFSLt1_VMR1.18 <- ModelDFSLt1_VMR1.17[-nrow(ModelDFSLt1_VMR1.17),]
  ModelDFSLt1_VMR1.19 <- ModelDFSLt1_VMR1.18[-nrow(ModelDFSLt1_VMR1.18),]
  ModelDFSLt1_VMR1.20 <- ModelDFSLt1_VMR1.19[-nrow(ModelDFSLt1_VMR1.19),]
  ModelDFSLt1_VMR1.21 <- ModelDFSLt1_VMR1.20[-nrow(ModelDFSLt1_VMR1.20),]
  ModelDFSLt1_VMR1.22 <- ModelDFSLt1_VMR1.21[-nrow(ModelDFSLt1_VMR1.21),]
  ModelDFSLt1_VMR1.23 <- ModelDFSLt1_VMR1.22[-nrow(ModelDFSLt1_VMR1.22),]
  ModelDFSLt1_VMR1.24 <- ModelDFSLt1_VMR1.23[-nrow(ModelDFSLt1_VMR1.23),]

  ModelDFSLt1_VMR1 <- ModelDFSLt1_VMR1.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR1 <- ModelDFSLt0_VMR1 - ModelDFSLt1_VMR1

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR1$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR1P because implies a one-off input of C
  ModelDFSL_VMR1P <- ModelDFSL1y_VMR1

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR1P,"VXP_Models\\ModelDFSL_R_VMR1P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR1 <- ggplot(ModelDFSL_VMR1P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR1, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR1, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR1, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR1, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR1, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR1, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR1, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR1
  ggplotly(P_CFluxI1y_VMR1)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR1P_YCO2 <- ModelDFSL_VMR1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR1P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR1")

  PA_CO21y_VMR1 <- ggplot(ModelDFSL_VMR1P_YCO2, aes(x = Year, y = AnnualCO2_VMR1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR1
  ggplotly(PA_CO21y_VMR1)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR1P_YCO2D <- merge(ModelDFSL_VMR1P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR1P_YCO2D$AnnualCO2D_VMR1 <- ModelDFSL_VMR1P_YCO2D$AnnualCO2_VMR1 * ModelDFSL_VMR1P_YCO2D$GWP100

  PA_CO21yD_VMR1 <- ggplot(ModelDFSL_VMR1P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR1
  ggplotly(PA_CO21yD_VMR1)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR1P_YC <- ModelDFSL_VMR1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR1P_YC) <- c("Year", "Months", "AnnualCTail_VMR1")

  PA_C1y_VMR1 <- ggplot(ModelDFSL_VMR1P_YC, aes(x = Year, y = AnnualCTail_VMR1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR1
  ggplotly(PA_C1y_VMR1)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR1P_YCT <- ModelDFSL_VMR1P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR1)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR1P_YCT) <- c("Year", "Months", "AnnualCTail_VMR1")

  PA_CT1y_VMR1 <- ggplot(ModelDFSL_VMR1P_YCT, aes(x = Year, y = AnnualCTail_VMR1)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR1
  ggplotly(PA_CT1y_VMR1)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR1P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR1P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR1P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR1P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR1P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR1P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR1P_YCT,"CTails_P\\ModelDFSL_R_C_VMR1P.xlsx") #Yearly C emissions



  #### 11.3 - VMR2) Manure; 20%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR2=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR2=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.2,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR2=data.frame(years,rep(fT_VMR2*fW_VMR2,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR2=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.2, #Percent clay in mineral soil
    xi=xi.frame_VMR2) #Loads the model

  Ct_VMR2=getC(Model_VMR2) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR2, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR2 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR2 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR2 <- as.data.frame(VEC_Lit_VMR2) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR2$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR2 <- as.data.frame(VEC_Man_VMR2) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR2$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR2, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR2, class) #Check that class is numeric
  LitterCinputs_VMR2=VEC_LitDF_VMR2   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR2=VEC_ManDF_VMR2 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR2 <- merge(LitterCinputs_VMR2, ManureCinputs_VMR2, by = "MNumber")
  MCinputs_VMR2$MInput_VMR2 <- MCinputs_VMR2$VEC_Lit_VMR2 + MCinputs_VMR2$VEC_Man_VMR2

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR2)[which(names(MCinputs_VMR2) == "VEC_Lit_VMR2")] <- "LitterC_VMR2"
  colnames(MCinputs_VMR2)[which(names(MCinputs_VMR2) == "VEC_Man_VMR2")] <- "ManureC_VMR2"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR2 <- as.data.frame(Ct_VMR2)
  colnames(ModelDF_VMR2) <- c('DPM_VMR2','RPM_VMR2','BIO_VMR2', 'HUM_VMR2', 'IOM_VMR2')
  ModelDFS_VMR2 <- ModelDF_VMR2[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR2$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR2$AllPools_VMR2 <- ModelDFS_VMR2$DPM_VMR2 + ModelDFS_VMR2$RPM_VMR2 + ModelDFS_VMR2$BIO_VMR2 + ModelDFS_VMR2$HUM_VMR2 + ModelDFS_VMR2$IOM_VMR2

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR2$AllPools_noIOM_VMR2 <- ModelDFS_VMR2$DPM_VMR2 + ModelDFS_VMR2$RPM_VMR2 + ModelDFS_VMR2$BIO_VMR2 + ModelDFS_VMR2$HUM_VMR2

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR2 <- merge(ModelDFS_VMR2, MCinputs_VMR2, by = "MNumber")

  ModelDFSL_VMR2$MInput_VMR2 <- ModelDFSL_VMR2$LitterC_VMR2 + ModelDFSL_VMR2$ManureC_VMR2
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR2$CTails_VMR2 <- ModelDFSL_VMR2$AllPools_noIOM_VMR2 + ModelDFSL_VMR2$MInput_VMR2

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR2$AccumInput_VMR2 = ModelDFSL_VMR2$AccumInput_VMR2=cumsum(ModelDFSL_VMR2$MInput_VMR2)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR2$MGrowth_DPM_VMR2 <- ave(ModelDFSL_VMR2$DPM_VMR2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR2$MGrowth_RPM_VMR2 <- ave(ModelDFSL_VMR2$RPM_VMR2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR2$MGrowth_BIO_VMR2 <- ave(ModelDFSL_VMR2$BIO_VMR2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR2$MGrowth_HUM_VMR2 <- ave(ModelDFSL_VMR2$HUM_VMR2, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR2$MGrowth_IOM_VMR2 <- ave(ModelDFSL_VMR2$IOM_VMR2, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR2$M_CCO2_VMR2 <- ModelDFSL_VMR2$MInput_VMR2 - ModelDFSL_VMR2$MGrowth_DPM_VMR2 - ModelDFSL_VMR2$MGrowth_RPM_VMR2 - ModelDFSL_VMR2$MGrowth_BIO_VMR2 - ModelDFSL_VMR2$MGrowth_HUM_VMR2
  ModelDFSL_VMR2$Accum_CCO2_VMR2 <- ModelDFSL_VMR2$AccumInput_VMR2 - ModelDFSL_VMR2$AllPools_noIOM_VMR2

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR2$M_CCO2_VMR2[1] <- 0
  ModelDFSL_VMR2$Accum_CCO2_VMR2[1] <- 0

  #Balance validation
  ModelDFSL_VMR2$Balance_VMR2 <- ModelDFSL_VMR2$AccumInput_VMR2 - ModelDFSL_VMR2$Accum_CCO2_VMR2 - (ModelDFSL_VMR2$DPM_VMR2 + ModelDFSL_VMR2$RPM_VMR2 + ModelDFSL_VMR2$BIO_VMR2 + ModelDFSL_VMR2$HUM_VMR2)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR2$M_CO2_VMR2 <- ModelDFSL_VMR2$M_CCO2_VMR2 * 44/12
  ModelDFSL_VMR2$Accum_CO2_VMR2 <- ModelDFSL_VMR2$Accum_CCO2_VMR2 * 44/12

  #This model will be called VMR2C because implies a continuous input of C
  ModelDFSL_VMR2C <- ModelDFSL_VMR2

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR2C,"VXC_Models\\ModelDFSL_R_VMR2C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR2 <- ModelDFSL_VMR2 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR2.1 <- rbind(c(0:0), ModelDFSLt0_VMR2)
  ModelDFSLt1_VMR2.2 <- rbind(c(0:0), ModelDFSLt1_VMR2.1)
  ModelDFSLt1_VMR2.3 <- rbind(c(0:0), ModelDFSLt1_VMR2.2)
  ModelDFSLt1_VMR2.4 <- rbind(c(0:0), ModelDFSLt1_VMR2.3)
  ModelDFSLt1_VMR2.5 <- rbind(c(0:0), ModelDFSLt1_VMR2.4)
  ModelDFSLt1_VMR2.6 <- rbind(c(0:0), ModelDFSLt1_VMR2.5)
  ModelDFSLt1_VMR2.7 <- rbind(c(0:0), ModelDFSLt1_VMR2.6)
  ModelDFSLt1_VMR2.8 <- rbind(c(0:0), ModelDFSLt1_VMR2.7)
  ModelDFSLt1_VMR2.9 <- rbind(c(0:0), ModelDFSLt1_VMR2.8)
  ModelDFSLt1_VMR2.10 <- rbind(c(0:0), ModelDFSLt1_VMR2.9)
  ModelDFSLt1_VMR2.11 <- rbind(c(0:0), ModelDFSLt1_VMR2.10)
  ModelDFSLt1_VMR2.12 <- rbind(c(0:0), ModelDFSLt1_VMR2.11)
  ModelDFSLt1_VMR2.13 <- ModelDFSLt1_VMR2.12[-nrow(ModelDFSLt1_VMR2.12),]
  ModelDFSLt1_VMR2.14 <- ModelDFSLt1_VMR2.13[-nrow(ModelDFSLt1_VMR2.13),]
  ModelDFSLt1_VMR2.15 <- ModelDFSLt1_VMR2.14[-nrow(ModelDFSLt1_VMR2.14),]
  ModelDFSLt1_VMR2.16 <- ModelDFSLt1_VMR2.15[-nrow(ModelDFSLt1_VMR2.15),]
  ModelDFSLt1_VMR2.17 <- ModelDFSLt1_VMR2.16[-nrow(ModelDFSLt1_VMR2.16),]
  ModelDFSLt1_VMR2.18 <- ModelDFSLt1_VMR2.17[-nrow(ModelDFSLt1_VMR2.17),]
  ModelDFSLt1_VMR2.19 <- ModelDFSLt1_VMR2.18[-nrow(ModelDFSLt1_VMR2.18),]
  ModelDFSLt1_VMR2.20 <- ModelDFSLt1_VMR2.19[-nrow(ModelDFSLt1_VMR2.19),]
  ModelDFSLt1_VMR2.21 <- ModelDFSLt1_VMR2.20[-nrow(ModelDFSLt1_VMR2.20),]
  ModelDFSLt1_VMR2.22 <- ModelDFSLt1_VMR2.21[-nrow(ModelDFSLt1_VMR2.21),]
  ModelDFSLt1_VMR2.23 <- ModelDFSLt1_VMR2.22[-nrow(ModelDFSLt1_VMR2.22),]
  ModelDFSLt1_VMR2.24 <- ModelDFSLt1_VMR2.23[-nrow(ModelDFSLt1_VMR2.23),]

  ModelDFSLt1_VMR2 <- ModelDFSLt1_VMR2.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR2 <- ModelDFSLt0_VMR2 - ModelDFSLt1_VMR2

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR2$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR2P because implies a one-off input of C
  ModelDFSL_VMR2P <- ModelDFSL1y_VMR2

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR2P,"VXP_Models\\ModelDFSL_R_VMR2P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR2 <- ggplot(ModelDFSL_VMR2P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR2, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR2, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR2, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR2, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR2, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR2, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR2, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR2
  ggplotly(P_CFluxI1y_VMR2)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR2P_YCO2 <- ModelDFSL_VMR2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR2P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR2")

  PA_CO21y_VMR2 <- ggplot(ModelDFSL_VMR2P_YCO2, aes(x = Year, y = AnnualCO2_VMR2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR2
  ggplotly(PA_CO21y_VMR2)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR2P_YCO2D <- merge(ModelDFSL_VMR2P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR2P_YCO2D$AnnualCO2D_VMR2 <- ModelDFSL_VMR2P_YCO2D$AnnualCO2_VMR2 * ModelDFSL_VMR2P_YCO2D$GWP100

  PA_CO21yD_VMR2 <- ggplot(ModelDFSL_VMR2P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR2
  ggplotly(PA_CO21yD_VMR2)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR2P_YC <- ModelDFSL_VMR2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR2P_YC) <- c("Year", "Months", "AnnualCTail_VMR2")

  PA_C1y_VMR2 <- ggplot(ModelDFSL_VMR2P_YC, aes(x = Year, y = AnnualCTail_VMR2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR2
  ggplotly(PA_C1y_VMR2)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR2P_YCT <- ModelDFSL_VMR2P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR2)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR2P_YCT) <- c("Year", "Months", "AnnualCTail_VMR2")

  PA_CT1y_VMR2 <- ggplot(ModelDFSL_VMR2P_YCT, aes(x = Year, y = AnnualCTail_VMR2)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR2
  ggplotly(PA_CT1y_VMR2)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR2P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR2P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR2P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR2P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR2P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR2P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR2P_YCT,"CTails_P\\ModelDFSL_R_C_VMR2P.xlsx") #Yearly C emissions



  #### 11.4 - VMR3) Manure; 30%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR3=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR3=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.3,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR3=data.frame(years,rep(fT_VMR3*fW_VMR3,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR3=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.3, #Percent clay in mineral soil
    xi=xi.frame_VMR3) #Loads the model

  Ct_VMR3=getC(Model_VMR3) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR3, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR3 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR3 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR3 <- as.data.frame(VEC_Lit_VMR3) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR3$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR3 <- as.data.frame(VEC_Man_VMR3) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR3$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR3, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR3, class) #Check that class is numeric
  LitterCinputs_VMR3=VEC_LitDF_VMR3   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR3=VEC_ManDF_VMR3 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR3 <- merge(LitterCinputs_VMR3, ManureCinputs_VMR3, by = "MNumber")
  MCinputs_VMR3$MInput_VMR3 <- MCinputs_VMR3$VEC_Lit_VMR3 + MCinputs_VMR3$VEC_Man_VMR3

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR3)[which(names(MCinputs_VMR3) == "VEC_Lit_VMR3")] <- "LitterC_VMR3"
  colnames(MCinputs_VMR3)[which(names(MCinputs_VMR3) == "VEC_Man_VMR3")] <- "ManureC_VMR3"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR3 <- as.data.frame(Ct_VMR3)
  colnames(ModelDF_VMR3) <- c('DPM_VMR3','RPM_VMR3','BIO_VMR3', 'HUM_VMR3', 'IOM_VMR3')
  ModelDFS_VMR3 <- ModelDF_VMR3[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR3$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR3$AllPools_VMR3 <- ModelDFS_VMR3$DPM_VMR3 + ModelDFS_VMR3$RPM_VMR3 + ModelDFS_VMR3$BIO_VMR3 + ModelDFS_VMR3$HUM_VMR3 + ModelDFS_VMR3$IOM_VMR3

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR3$AllPools_noIOM_VMR3 <- ModelDFS_VMR3$DPM_VMR3 + ModelDFS_VMR3$RPM_VMR3 + ModelDFS_VMR3$BIO_VMR3 + ModelDFS_VMR3$HUM_VMR3

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR3 <- merge(ModelDFS_VMR3, MCinputs_VMR3, by = "MNumber")

  ModelDFSL_VMR3$MInput_VMR3 <- ModelDFSL_VMR3$LitterC_VMR3 + ModelDFSL_VMR3$ManureC_VMR3
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR3$CTails_VMR3 <- ModelDFSL_VMR3$AllPools_noIOM_VMR3 + ModelDFSL_VMR3$MInput_VMR3

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR3$AccumInput_VMR3 = ModelDFSL_VMR3$AccumInput_VMR3=cumsum(ModelDFSL_VMR3$MInput_VMR3)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR3$MGrowth_DPM_VMR3 <- ave(ModelDFSL_VMR3$DPM_VMR3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR3$MGrowth_RPM_VMR3 <- ave(ModelDFSL_VMR3$RPM_VMR3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR3$MGrowth_BIO_VMR3 <- ave(ModelDFSL_VMR3$BIO_VMR3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR3$MGrowth_HUM_VMR3 <- ave(ModelDFSL_VMR3$HUM_VMR3, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR3$MGrowth_IOM_VMR3 <- ave(ModelDFSL_VMR3$IOM_VMR3, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR3$M_CCO2_VMR3 <- ModelDFSL_VMR3$MInput_VMR3 - ModelDFSL_VMR3$MGrowth_DPM_VMR3 - ModelDFSL_VMR3$MGrowth_RPM_VMR3 - ModelDFSL_VMR3$MGrowth_BIO_VMR3 - ModelDFSL_VMR3$MGrowth_HUM_VMR3
  ModelDFSL_VMR3$Accum_CCO2_VMR3 <- ModelDFSL_VMR3$AccumInput_VMR3 - ModelDFSL_VMR3$AllPools_noIOM_VMR3

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR3$M_CCO2_VMR3[1] <- 0
  ModelDFSL_VMR3$Accum_CCO2_VMR3[1] <- 0

  #Balance validation
  ModelDFSL_VMR3$Balance_VMR3 <- ModelDFSL_VMR3$AccumInput_VMR3 - ModelDFSL_VMR3$Accum_CCO2_VMR3 - (ModelDFSL_VMR3$DPM_VMR3 + ModelDFSL_VMR3$RPM_VMR3 + ModelDFSL_VMR3$BIO_VMR3 + ModelDFSL_VMR3$HUM_VMR3)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR3$M_CO2_VMR3 <- ModelDFSL_VMR3$M_CCO2_VMR3 * 44/12
  ModelDFSL_VMR3$Accum_CO2_VMR3 <- ModelDFSL_VMR3$Accum_CCO2_VMR3 * 44/12

  #This model will be called VMR3C because implies a continuous input of C
  ModelDFSL_VMR3C <- ModelDFSL_VMR3

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR3C,"VXC_Models\\ModelDFSL_R_VMR3C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR3 <- ModelDFSL_VMR3 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR3.1 <- rbind(c(0:0), ModelDFSLt0_VMR3)
  ModelDFSLt1_VMR3.2 <- rbind(c(0:0), ModelDFSLt1_VMR3.1)
  ModelDFSLt1_VMR3.3 <- rbind(c(0:0), ModelDFSLt1_VMR3.2)
  ModelDFSLt1_VMR3.4 <- rbind(c(0:0), ModelDFSLt1_VMR3.3)
  ModelDFSLt1_VMR3.5 <- rbind(c(0:0), ModelDFSLt1_VMR3.4)
  ModelDFSLt1_VMR3.6 <- rbind(c(0:0), ModelDFSLt1_VMR3.5)
  ModelDFSLt1_VMR3.7 <- rbind(c(0:0), ModelDFSLt1_VMR3.6)
  ModelDFSLt1_VMR3.8 <- rbind(c(0:0), ModelDFSLt1_VMR3.7)
  ModelDFSLt1_VMR3.9 <- rbind(c(0:0), ModelDFSLt1_VMR3.8)
  ModelDFSLt1_VMR3.10 <- rbind(c(0:0), ModelDFSLt1_VMR3.9)
  ModelDFSLt1_VMR3.11 <- rbind(c(0:0), ModelDFSLt1_VMR3.10)
  ModelDFSLt1_VMR3.12 <- rbind(c(0:0), ModelDFSLt1_VMR3.11)
  ModelDFSLt1_VMR3.13 <- ModelDFSLt1_VMR3.12[-nrow(ModelDFSLt1_VMR3.12),]
  ModelDFSLt1_VMR3.14 <- ModelDFSLt1_VMR3.13[-nrow(ModelDFSLt1_VMR3.13),]
  ModelDFSLt1_VMR3.15 <- ModelDFSLt1_VMR3.14[-nrow(ModelDFSLt1_VMR3.14),]
  ModelDFSLt1_VMR3.16 <- ModelDFSLt1_VMR3.15[-nrow(ModelDFSLt1_VMR3.15),]
  ModelDFSLt1_VMR3.17 <- ModelDFSLt1_VMR3.16[-nrow(ModelDFSLt1_VMR3.16),]
  ModelDFSLt1_VMR3.18 <- ModelDFSLt1_VMR3.17[-nrow(ModelDFSLt1_VMR3.17),]
  ModelDFSLt1_VMR3.19 <- ModelDFSLt1_VMR3.18[-nrow(ModelDFSLt1_VMR3.18),]
  ModelDFSLt1_VMR3.20 <- ModelDFSLt1_VMR3.19[-nrow(ModelDFSLt1_VMR3.19),]
  ModelDFSLt1_VMR3.21 <- ModelDFSLt1_VMR3.20[-nrow(ModelDFSLt1_VMR3.20),]
  ModelDFSLt1_VMR3.22 <- ModelDFSLt1_VMR3.21[-nrow(ModelDFSLt1_VMR3.21),]
  ModelDFSLt1_VMR3.23 <- ModelDFSLt1_VMR3.22[-nrow(ModelDFSLt1_VMR3.22),]
  ModelDFSLt1_VMR3.24 <- ModelDFSLt1_VMR3.23[-nrow(ModelDFSLt1_VMR3.23),]

  ModelDFSLt1_VMR3 <- ModelDFSLt1_VMR3.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR3 <- ModelDFSLt0_VMR3 - ModelDFSLt1_VMR3

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR3$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR3P because implies a one-off input of C
  ModelDFSL_VMR3P <- ModelDFSL1y_VMR3

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR3P,"VXP_Models\\ModelDFSL_R_VMR3P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR3 <- ggplot(ModelDFSL_VMR3P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR3, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR3, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR3, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR3, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR3, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR3, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR3, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR3
  ggplotly(P_CFluxI1y_VMR3)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR3P_YCO2 <- ModelDFSL_VMR3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR3P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR3")

  PA_CO21y_VMR3 <- ggplot(ModelDFSL_VMR3P_YCO2, aes(x = Year, y = AnnualCO2_VMR3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR3
  ggplotly(PA_CO21y_VMR3)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR3P_YCO2D <- merge(ModelDFSL_VMR3P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR3P_YCO2D$AnnualCO2D_VMR3 <- ModelDFSL_VMR3P_YCO2D$AnnualCO2_VMR3 * ModelDFSL_VMR3P_YCO2D$GWP100

  PA_CO21yD_VMR3 <- ggplot(ModelDFSL_VMR3P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR3
  ggplotly(PA_CO21yD_VMR3)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR3P_YC <- ModelDFSL_VMR3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR3P_YC) <- c("Year", "Months", "AnnualCTail_VMR3")

  PA_C1y_VMR3 <- ggplot(ModelDFSL_VMR3P_YC, aes(x = Year, y = AnnualCTail_VMR3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR3
  ggplotly(PA_C1y_VMR3)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR3P_YCT <- ModelDFSL_VMR3P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR3)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR3P_YCT) <- c("Year", "Months", "AnnualCTail_VMR3")

  PA_CT1y_VMR3 <- ggplot(ModelDFSL_VMR3P_YCT, aes(x = Year, y = AnnualCTail_VMR3)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR3
  ggplotly(PA_CT1y_VMR3)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR3P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR3P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR3P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR3P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR3P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR3P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR3P_YCT,"CTails_P\\ModelDFSL_R_C_VMR3P.xlsx") #Yearly C emissions



  #### 11.5 - VMR4) Manure; 40%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR4=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR4=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.4,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR4=data.frame(years,rep(fT_VMR4*fW_VMR4,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR4=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.4, #Percent clay in mineral soil
    xi=xi.frame_VMR4) #Loads the model

  Ct_VMR4=getC(Model_VMR4) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR4, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR4 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR4 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR4 <- as.data.frame(VEC_Lit_VMR4) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR4$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR4 <- as.data.frame(VEC_Man_VMR4) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR4$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR4, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR4, class) #Check that class is numeric
  LitterCinputs_VMR4=VEC_LitDF_VMR4   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR4=VEC_ManDF_VMR4 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR4 <- merge(LitterCinputs_VMR4, ManureCinputs_VMR4, by = "MNumber")
  MCinputs_VMR4$MInput_VMR4 <- MCinputs_VMR4$VEC_Lit_VMR4 + MCinputs_VMR4$VEC_Man_VMR4

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR4)[which(names(MCinputs_VMR4) == "VEC_Lit_VMR4")] <- "LitterC_VMR4"
  colnames(MCinputs_VMR4)[which(names(MCinputs_VMR4) == "VEC_Man_VMR4")] <- "ManureC_VMR4"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR4 <- as.data.frame(Ct_VMR4)
  colnames(ModelDF_VMR4) <- c('DPM_VMR4','RPM_VMR4','BIO_VMR4', 'HUM_VMR4', 'IOM_VMR4')
  ModelDFS_VMR4 <- ModelDF_VMR4[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR4$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR4$AllPools_VMR4 <- ModelDFS_VMR4$DPM_VMR4 + ModelDFS_VMR4$RPM_VMR4 + ModelDFS_VMR4$BIO_VMR4 + ModelDFS_VMR4$HUM_VMR4 + ModelDFS_VMR4$IOM_VMR4

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR4$AllPools_noIOM_VMR4 <- ModelDFS_VMR4$DPM_VMR4 + ModelDFS_VMR4$RPM_VMR4 + ModelDFS_VMR4$BIO_VMR4 + ModelDFS_VMR4$HUM_VMR4

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR4 <- merge(ModelDFS_VMR4, MCinputs_VMR4, by = "MNumber")

  ModelDFSL_VMR4$MInput_VMR4 <- ModelDFSL_VMR4$LitterC_VMR4 + ModelDFSL_VMR4$ManureC_VMR4
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR4$CTails_VMR4 <- ModelDFSL_VMR4$AllPools_noIOM_VMR4 + ModelDFSL_VMR4$MInput_VMR4

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR4$AccumInput_VMR4 = ModelDFSL_VMR4$AccumInput_VMR4=cumsum(ModelDFSL_VMR4$MInput_VMR4)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR4$MGrowth_DPM_VMR4 <- ave(ModelDFSL_VMR4$DPM_VMR4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR4$MGrowth_RPM_VMR4 <- ave(ModelDFSL_VMR4$RPM_VMR4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR4$MGrowth_BIO_VMR4 <- ave(ModelDFSL_VMR4$BIO_VMR4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR4$MGrowth_HUM_VMR4 <- ave(ModelDFSL_VMR4$HUM_VMR4, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR4$MGrowth_IOM_VMR4 <- ave(ModelDFSL_VMR4$IOM_VMR4, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR4$M_CCO2_VMR4 <- ModelDFSL_VMR4$MInput_VMR4 - ModelDFSL_VMR4$MGrowth_DPM_VMR4 - ModelDFSL_VMR4$MGrowth_RPM_VMR4 - ModelDFSL_VMR4$MGrowth_BIO_VMR4 - ModelDFSL_VMR4$MGrowth_HUM_VMR4
  ModelDFSL_VMR4$Accum_CCO2_VMR4 <- ModelDFSL_VMR4$AccumInput_VMR4 - ModelDFSL_VMR4$AllPools_noIOM_VMR4

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR4$M_CCO2_VMR4[1] <- 0
  ModelDFSL_VMR4$Accum_CCO2_VMR4[1] <- 0

  #Balance validation
  ModelDFSL_VMR4$Balance_VMR4 <- ModelDFSL_VMR4$AccumInput_VMR4 - ModelDFSL_VMR4$Accum_CCO2_VMR4 - (ModelDFSL_VMR4$DPM_VMR4 + ModelDFSL_VMR4$RPM_VMR4 + ModelDFSL_VMR4$BIO_VMR4 + ModelDFSL_VMR4$HUM_VMR4)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR4$M_CO2_VMR4 <- ModelDFSL_VMR4$M_CCO2_VMR4 * 44/12
  ModelDFSL_VMR4$Accum_CO2_VMR4 <- ModelDFSL_VMR4$Accum_CCO2_VMR4 * 44/12

  #This model will be called VMR4C because implies a continuous input of C
  ModelDFSL_VMR4C <- ModelDFSL_VMR4

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR4C,"VXC_Models\\ModelDFSL_R_VMR4C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR4 <- ModelDFSL_VMR4 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR4.1 <- rbind(c(0:0), ModelDFSLt0_VMR4)
  ModelDFSLt1_VMR4.2 <- rbind(c(0:0), ModelDFSLt1_VMR4.1)
  ModelDFSLt1_VMR4.3 <- rbind(c(0:0), ModelDFSLt1_VMR4.2)
  ModelDFSLt1_VMR4.4 <- rbind(c(0:0), ModelDFSLt1_VMR4.3)
  ModelDFSLt1_VMR4.5 <- rbind(c(0:0), ModelDFSLt1_VMR4.4)
  ModelDFSLt1_VMR4.6 <- rbind(c(0:0), ModelDFSLt1_VMR4.5)
  ModelDFSLt1_VMR4.7 <- rbind(c(0:0), ModelDFSLt1_VMR4.6)
  ModelDFSLt1_VMR4.8 <- rbind(c(0:0), ModelDFSLt1_VMR4.7)
  ModelDFSLt1_VMR4.9 <- rbind(c(0:0), ModelDFSLt1_VMR4.8)
  ModelDFSLt1_VMR4.10 <- rbind(c(0:0), ModelDFSLt1_VMR4.9)
  ModelDFSLt1_VMR4.11 <- rbind(c(0:0), ModelDFSLt1_VMR4.10)
  ModelDFSLt1_VMR4.12 <- rbind(c(0:0), ModelDFSLt1_VMR4.11)
  ModelDFSLt1_VMR4.13 <- ModelDFSLt1_VMR4.12[-nrow(ModelDFSLt1_VMR4.12),]
  ModelDFSLt1_VMR4.14 <- ModelDFSLt1_VMR4.13[-nrow(ModelDFSLt1_VMR4.13),]
  ModelDFSLt1_VMR4.15 <- ModelDFSLt1_VMR4.14[-nrow(ModelDFSLt1_VMR4.14),]
  ModelDFSLt1_VMR4.16 <- ModelDFSLt1_VMR4.15[-nrow(ModelDFSLt1_VMR4.15),]
  ModelDFSLt1_VMR4.17 <- ModelDFSLt1_VMR4.16[-nrow(ModelDFSLt1_VMR4.16),]
  ModelDFSLt1_VMR4.18 <- ModelDFSLt1_VMR4.17[-nrow(ModelDFSLt1_VMR4.17),]
  ModelDFSLt1_VMR4.19 <- ModelDFSLt1_VMR4.18[-nrow(ModelDFSLt1_VMR4.18),]
  ModelDFSLt1_VMR4.20 <- ModelDFSLt1_VMR4.19[-nrow(ModelDFSLt1_VMR4.19),]
  ModelDFSLt1_VMR4.21 <- ModelDFSLt1_VMR4.20[-nrow(ModelDFSLt1_VMR4.20),]
  ModelDFSLt1_VMR4.22 <- ModelDFSLt1_VMR4.21[-nrow(ModelDFSLt1_VMR4.21),]
  ModelDFSLt1_VMR4.23 <- ModelDFSLt1_VMR4.22[-nrow(ModelDFSLt1_VMR4.22),]
  ModelDFSLt1_VMR4.24 <- ModelDFSLt1_VMR4.23[-nrow(ModelDFSLt1_VMR4.23),]

  ModelDFSLt1_VMR4 <- ModelDFSLt1_VMR4.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR4 <- ModelDFSLt0_VMR4 - ModelDFSLt1_VMR4

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR4$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR4P because implies a one-off input of C
  ModelDFSL_VMR4P <- ModelDFSL1y_VMR4

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR4P,"VXP_Models\\ModelDFSL_R_VMR4P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR4 <- ggplot(ModelDFSL_VMR4P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR4, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR4, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR4, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR4, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR4, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR4, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR4, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR4
  ggplotly(P_CFluxI1y_VMR4)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR4P_YCO2 <- ModelDFSL_VMR4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR4P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR4")

  PA_CO21y_VMR4 <- ggplot(ModelDFSL_VMR4P_YCO2, aes(x = Year, y = AnnualCO2_VMR4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR4
  ggplotly(PA_CO21y_VMR4)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR4P_YCO2D <- merge(ModelDFSL_VMR4P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR4P_YCO2D$AnnualCO2D_VMR4 <- ModelDFSL_VMR4P_YCO2D$AnnualCO2_VMR4 * ModelDFSL_VMR4P_YCO2D$GWP100

  PA_CO21yD_VMR4 <- ggplot(ModelDFSL_VMR4P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR4
  ggplotly(PA_CO21yD_VMR4)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR4P_YC <- ModelDFSL_VMR4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR4P_YC) <- c("Year", "Months", "AnnualCTail_VMR4")

  PA_C1y_VMR4 <- ggplot(ModelDFSL_VMR4P_YC, aes(x = Year, y = AnnualCTail_VMR4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR4
  ggplotly(PA_C1y_VMR4)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR4P_YCT <- ModelDFSL_VMR4P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR4)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR4P_YCT) <- c("Year", "Months", "AnnualCTail_VMR4")

  PA_CT1y_VMR4 <- ggplot(ModelDFSL_VMR4P_YCT, aes(x = Year, y = AnnualCTail_VMR4)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR4
  ggplotly(PA_CT1y_VMR4)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR4P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR4P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR4P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR4P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR4P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR4P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR4P_YCT,"CTails_P\\ModelDFSL_R_C_VMR4P.xlsx") #Yearly C emissions



  #### 11.6 - VMR5) Manure; 50%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR5=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR5=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.5,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR5=data.frame(years,rep(fT_VMR5*fW_VMR5,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR5=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.5, #Percent clay in mineral soil
    xi=xi.frame_VMR5) #Loads the model

  Ct_VMR5=getC(Model_VMR5) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR5, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR5 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR5 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR5 <- as.data.frame(VEC_Lit_VMR5) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR5$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR5 <- as.data.frame(VEC_Man_VMR5) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR5$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR5, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR5, class) #Check that class is numeric
  LitterCinputs_VMR5=VEC_LitDF_VMR5   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR5=VEC_ManDF_VMR5 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR5 <- merge(LitterCinputs_VMR5, ManureCinputs_VMR5, by = "MNumber")
  MCinputs_VMR5$MInput_VMR5 <- MCinputs_VMR5$VEC_Lit_VMR5 + MCinputs_VMR5$VEC_Man_VMR5

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR5)[which(names(MCinputs_VMR5) == "VEC_Lit_VMR5")] <- "LitterC_VMR5"
  colnames(MCinputs_VMR5)[which(names(MCinputs_VMR5) == "VEC_Man_VMR5")] <- "ManureC_VMR5"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR5 <- as.data.frame(Ct_VMR5)
  colnames(ModelDF_VMR5) <- c('DPM_VMR5','RPM_VMR5','BIO_VMR5', 'HUM_VMR5', 'IOM_VMR5')
  ModelDFS_VMR5 <- ModelDF_VMR5[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR5$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR5$AllPools_VMR5 <- ModelDFS_VMR5$DPM_VMR5 + ModelDFS_VMR5$RPM_VMR5 + ModelDFS_VMR5$BIO_VMR5 + ModelDFS_VMR5$HUM_VMR5 + ModelDFS_VMR5$IOM_VMR5

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR5$AllPools_noIOM_VMR5 <- ModelDFS_VMR5$DPM_VMR5 + ModelDFS_VMR5$RPM_VMR5 + ModelDFS_VMR5$BIO_VMR5 + ModelDFS_VMR5$HUM_VMR5

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR5 <- merge(ModelDFS_VMR5, MCinputs_VMR5, by = "MNumber")

  ModelDFSL_VMR5$MInput_VMR5 <- ModelDFSL_VMR5$LitterC_VMR5 + ModelDFSL_VMR5$ManureC_VMR5
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR5$CTails_VMR5 <- ModelDFSL_VMR5$AllPools_noIOM_VMR5 + ModelDFSL_VMR5$MInput_VMR5

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR5$AccumInput_VMR5 = ModelDFSL_VMR5$AccumInput_VMR5=cumsum(ModelDFSL_VMR5$MInput_VMR5)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR5$MGrowth_DPM_VMR5 <- ave(ModelDFSL_VMR5$DPM_VMR5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR5$MGrowth_RPM_VMR5 <- ave(ModelDFSL_VMR5$RPM_VMR5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR5$MGrowth_BIO_VMR5 <- ave(ModelDFSL_VMR5$BIO_VMR5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR5$MGrowth_HUM_VMR5 <- ave(ModelDFSL_VMR5$HUM_VMR5, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR5$MGrowth_IOM_VMR5 <- ave(ModelDFSL_VMR5$IOM_VMR5, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR5$M_CCO2_VMR5 <- ModelDFSL_VMR5$MInput_VMR5 - ModelDFSL_VMR5$MGrowth_DPM_VMR5 - ModelDFSL_VMR5$MGrowth_RPM_VMR5 - ModelDFSL_VMR5$MGrowth_BIO_VMR5 - ModelDFSL_VMR5$MGrowth_HUM_VMR5
  ModelDFSL_VMR5$Accum_CCO2_VMR5 <- ModelDFSL_VMR5$AccumInput_VMR5 - ModelDFSL_VMR5$AllPools_noIOM_VMR5

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR5$M_CCO2_VMR5[1] <- 0
  ModelDFSL_VMR5$Accum_CCO2_VMR5[1] <- 0

  #Balance validation
  ModelDFSL_VMR5$Balance_VMR5 <- ModelDFSL_VMR5$AccumInput_VMR5 - ModelDFSL_VMR5$Accum_CCO2_VMR5 - (ModelDFSL_VMR5$DPM_VMR5 + ModelDFSL_VMR5$RPM_VMR5 + ModelDFSL_VMR5$BIO_VMR5 + ModelDFSL_VMR5$HUM_VMR5)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR5$M_CO2_VMR5 <- ModelDFSL_VMR5$M_CCO2_VMR5 * 44/12
  ModelDFSL_VMR5$Accum_CO2_VMR5 <- ModelDFSL_VMR5$Accum_CCO2_VMR5 * 44/12

  #This model will be called VMR5C because implies a continuous input of C
  ModelDFSL_VMR5C <- ModelDFSL_VMR5

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR5C,"VXC_Models\\ModelDFSL_R_VMR5C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR5 <- ModelDFSL_VMR5 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR5.1 <- rbind(c(0:0), ModelDFSLt0_VMR5)
  ModelDFSLt1_VMR5.2 <- rbind(c(0:0), ModelDFSLt1_VMR5.1)
  ModelDFSLt1_VMR5.3 <- rbind(c(0:0), ModelDFSLt1_VMR5.2)
  ModelDFSLt1_VMR5.4 <- rbind(c(0:0), ModelDFSLt1_VMR5.3)
  ModelDFSLt1_VMR5.5 <- rbind(c(0:0), ModelDFSLt1_VMR5.4)
  ModelDFSLt1_VMR5.6 <- rbind(c(0:0), ModelDFSLt1_VMR5.5)
  ModelDFSLt1_VMR5.7 <- rbind(c(0:0), ModelDFSLt1_VMR5.6)
  ModelDFSLt1_VMR5.8 <- rbind(c(0:0), ModelDFSLt1_VMR5.7)
  ModelDFSLt1_VMR5.9 <- rbind(c(0:0), ModelDFSLt1_VMR5.8)
  ModelDFSLt1_VMR5.10 <- rbind(c(0:0), ModelDFSLt1_VMR5.9)
  ModelDFSLt1_VMR5.11 <- rbind(c(0:0), ModelDFSLt1_VMR5.10)
  ModelDFSLt1_VMR5.12 <- rbind(c(0:0), ModelDFSLt1_VMR5.11)
  ModelDFSLt1_VMR5.13 <- ModelDFSLt1_VMR5.12[-nrow(ModelDFSLt1_VMR5.12),]
  ModelDFSLt1_VMR5.14 <- ModelDFSLt1_VMR5.13[-nrow(ModelDFSLt1_VMR5.13),]
  ModelDFSLt1_VMR5.15 <- ModelDFSLt1_VMR5.14[-nrow(ModelDFSLt1_VMR5.14),]
  ModelDFSLt1_VMR5.16 <- ModelDFSLt1_VMR5.15[-nrow(ModelDFSLt1_VMR5.15),]
  ModelDFSLt1_VMR5.17 <- ModelDFSLt1_VMR5.16[-nrow(ModelDFSLt1_VMR5.16),]
  ModelDFSLt1_VMR5.18 <- ModelDFSLt1_VMR5.17[-nrow(ModelDFSLt1_VMR5.17),]
  ModelDFSLt1_VMR5.19 <- ModelDFSLt1_VMR5.18[-nrow(ModelDFSLt1_VMR5.18),]
  ModelDFSLt1_VMR5.20 <- ModelDFSLt1_VMR5.19[-nrow(ModelDFSLt1_VMR5.19),]
  ModelDFSLt1_VMR5.21 <- ModelDFSLt1_VMR5.20[-nrow(ModelDFSLt1_VMR5.20),]
  ModelDFSLt1_VMR5.22 <- ModelDFSLt1_VMR5.21[-nrow(ModelDFSLt1_VMR5.21),]
  ModelDFSLt1_VMR5.23 <- ModelDFSLt1_VMR5.22[-nrow(ModelDFSLt1_VMR5.22),]
  ModelDFSLt1_VMR5.24 <- ModelDFSLt1_VMR5.23[-nrow(ModelDFSLt1_VMR5.23),]

  ModelDFSLt1_VMR5 <- ModelDFSLt1_VMR5.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR5 <- ModelDFSLt0_VMR5 - ModelDFSLt1_VMR5

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR5$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR5P because implies a one-off input of C
  ModelDFSL_VMR5P <- ModelDFSL1y_VMR5

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR5P,"VXP_Models\\ModelDFSL_R_VMR5P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR5 <- ggplot(ModelDFSL_VMR5P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR5, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR5, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR5, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR5, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR5, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR5, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR5, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR5
  ggplotly(P_CFluxI1y_VMR5)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR5P_YCO2 <- ModelDFSL_VMR5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR5P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR5")

  PA_CO21y_VMR5 <- ggplot(ModelDFSL_VMR5P_YCO2, aes(x = Year, y = AnnualCO2_VMR5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR5
  ggplotly(PA_CO21y_VMR5)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR5P_YCO2D <- merge(ModelDFSL_VMR5P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR5P_YCO2D$AnnualCO2D_VMR5 <- ModelDFSL_VMR5P_YCO2D$AnnualCO2_VMR5 * ModelDFSL_VMR5P_YCO2D$GWP100

  PA_CO21yD_VMR5 <- ggplot(ModelDFSL_VMR5P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR5
  ggplotly(PA_CO21yD_VMR5)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR5P_YC <- ModelDFSL_VMR5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR5P_YC) <- c("Year", "Months", "AnnualCTail_VMR5")

  PA_C1y_VMR5 <- ggplot(ModelDFSL_VMR5P_YC, aes(x = Year, y = AnnualCTail_VMR5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR5
  ggplotly(PA_C1y_VMR5)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR5P_YCT <- ModelDFSL_VMR5P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR5)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR5P_YCT) <- c("Year", "Months", "AnnualCTail_VMR5")

  PA_CT1y_VMR5 <- ggplot(ModelDFSL_VMR5P_YCT, aes(x = Year, y = AnnualCTail_VMR5)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR5
  ggplotly(PA_CT1y_VMR5)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR5P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR5P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR5P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR5P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR5P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR5P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR5P_YCT,"CTails_P\\ModelDFSL_R_C_VMR5P.xlsx") #Yearly C emissions



  #### 11.7 - VMR6) Manure; 60%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR6=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR6=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.6,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR6=data.frame(years,rep(fT_VMR6*fW_VMR6,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR6=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.6, #Percent clay in mineral soil
    xi=xi.frame_VMR6) #Loads the model

  Ct_VMR6=getC(Model_VMR6) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR6, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR6 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR6 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR6 <- as.data.frame(VEC_Lit_VMR6) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR6$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR6 <- as.data.frame(VEC_Man_VMR6) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR6$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR6, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR6, class) #Check that class is numeric
  LitterCinputs_VMR6=VEC_LitDF_VMR6   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR6=VEC_ManDF_VMR6 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR6 <- merge(LitterCinputs_VMR6, ManureCinputs_VMR6, by = "MNumber")
  MCinputs_VMR6$MInput_VMR6 <- MCinputs_VMR6$VEC_Lit_VMR6 + MCinputs_VMR6$VEC_Man_VMR6

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR6)[which(names(MCinputs_VMR6) == "VEC_Lit_VMR6")] <- "LitterC_VMR6"
  colnames(MCinputs_VMR6)[which(names(MCinputs_VMR6) == "VEC_Man_VMR6")] <- "ManureC_VMR6"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR6 <- as.data.frame(Ct_VMR6)
  colnames(ModelDF_VMR6) <- c('DPM_VMR6','RPM_VMR6','BIO_VMR6', 'HUM_VMR6', 'IOM_VMR6')
  ModelDFS_VMR6 <- ModelDF_VMR6[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR6$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR6$AllPools_VMR6 <- ModelDFS_VMR6$DPM_VMR6 + ModelDFS_VMR6$RPM_VMR6 + ModelDFS_VMR6$BIO_VMR6 + ModelDFS_VMR6$HUM_VMR6 + ModelDFS_VMR6$IOM_VMR6

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR6$AllPools_noIOM_VMR6 <- ModelDFS_VMR6$DPM_VMR6 + ModelDFS_VMR6$RPM_VMR6 + ModelDFS_VMR6$BIO_VMR6 + ModelDFS_VMR6$HUM_VMR6

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR6 <- merge(ModelDFS_VMR6, MCinputs_VMR6, by = "MNumber")

  ModelDFSL_VMR6$MInput_VMR6 <- ModelDFSL_VMR6$LitterC_VMR6 + ModelDFSL_VMR6$ManureC_VMR6
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR6$CTails_VMR6 <- ModelDFSL_VMR6$AllPools_noIOM_VMR6 + ModelDFSL_VMR6$MInput_VMR6

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR6$AccumInput_VMR6 = ModelDFSL_VMR6$AccumInput_VMR6=cumsum(ModelDFSL_VMR6$MInput_VMR6)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR6$MGrowth_DPM_VMR6 <- ave(ModelDFSL_VMR6$DPM_VMR6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR6$MGrowth_RPM_VMR6 <- ave(ModelDFSL_VMR6$RPM_VMR6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR6$MGrowth_BIO_VMR6 <- ave(ModelDFSL_VMR6$BIO_VMR6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR6$MGrowth_HUM_VMR6 <- ave(ModelDFSL_VMR6$HUM_VMR6, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR6$MGrowth_IOM_VMR6 <- ave(ModelDFSL_VMR6$IOM_VMR6, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR6$M_CCO2_VMR6 <- ModelDFSL_VMR6$MInput_VMR6 - ModelDFSL_VMR6$MGrowth_DPM_VMR6 - ModelDFSL_VMR6$MGrowth_RPM_VMR6 - ModelDFSL_VMR6$MGrowth_BIO_VMR6 - ModelDFSL_VMR6$MGrowth_HUM_VMR6
  ModelDFSL_VMR6$Accum_CCO2_VMR6 <- ModelDFSL_VMR6$AccumInput_VMR6 - ModelDFSL_VMR6$AllPools_noIOM_VMR6

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR6$M_CCO2_VMR6[1] <- 0
  ModelDFSL_VMR6$Accum_CCO2_VMR6[1] <- 0

  #Balance validation
  ModelDFSL_VMR6$Balance_VMR6 <- ModelDFSL_VMR6$AccumInput_VMR6 - ModelDFSL_VMR6$Accum_CCO2_VMR6 - (ModelDFSL_VMR6$DPM_VMR6 + ModelDFSL_VMR6$RPM_VMR6 + ModelDFSL_VMR6$BIO_VMR6 + ModelDFSL_VMR6$HUM_VMR6)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR6$M_CO2_VMR6 <- ModelDFSL_VMR6$M_CCO2_VMR6 * 44/12
  ModelDFSL_VMR6$Accum_CO2_VMR6 <- ModelDFSL_VMR6$Accum_CCO2_VMR6 * 44/12

  #This model will be called VMR6C because implies a continuous input of C
  ModelDFSL_VMR6C <- ModelDFSL_VMR6

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR6C,"VXC_Models\\ModelDFSL_R_VMR6C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR6 <- ModelDFSL_VMR6 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR6.1 <- rbind(c(0:0), ModelDFSLt0_VMR6)
  ModelDFSLt1_VMR6.2 <- rbind(c(0:0), ModelDFSLt1_VMR6.1)
  ModelDFSLt1_VMR6.3 <- rbind(c(0:0), ModelDFSLt1_VMR6.2)
  ModelDFSLt1_VMR6.4 <- rbind(c(0:0), ModelDFSLt1_VMR6.3)
  ModelDFSLt1_VMR6.5 <- rbind(c(0:0), ModelDFSLt1_VMR6.4)
  ModelDFSLt1_VMR6.6 <- rbind(c(0:0), ModelDFSLt1_VMR6.5)
  ModelDFSLt1_VMR6.7 <- rbind(c(0:0), ModelDFSLt1_VMR6.6)
  ModelDFSLt1_VMR6.8 <- rbind(c(0:0), ModelDFSLt1_VMR6.7)
  ModelDFSLt1_VMR6.9 <- rbind(c(0:0), ModelDFSLt1_VMR6.8)
  ModelDFSLt1_VMR6.10 <- rbind(c(0:0), ModelDFSLt1_VMR6.9)
  ModelDFSLt1_VMR6.11 <- rbind(c(0:0), ModelDFSLt1_VMR6.10)
  ModelDFSLt1_VMR6.12 <- rbind(c(0:0), ModelDFSLt1_VMR6.11)
  ModelDFSLt1_VMR6.13 <- ModelDFSLt1_VMR6.12[-nrow(ModelDFSLt1_VMR6.12),]
  ModelDFSLt1_VMR6.14 <- ModelDFSLt1_VMR6.13[-nrow(ModelDFSLt1_VMR6.13),]
  ModelDFSLt1_VMR6.15 <- ModelDFSLt1_VMR6.14[-nrow(ModelDFSLt1_VMR6.14),]
  ModelDFSLt1_VMR6.16 <- ModelDFSLt1_VMR6.15[-nrow(ModelDFSLt1_VMR6.15),]
  ModelDFSLt1_VMR6.17 <- ModelDFSLt1_VMR6.16[-nrow(ModelDFSLt1_VMR6.16),]
  ModelDFSLt1_VMR6.18 <- ModelDFSLt1_VMR6.17[-nrow(ModelDFSLt1_VMR6.17),]
  ModelDFSLt1_VMR6.19 <- ModelDFSLt1_VMR6.18[-nrow(ModelDFSLt1_VMR6.18),]
  ModelDFSLt1_VMR6.20 <- ModelDFSLt1_VMR6.19[-nrow(ModelDFSLt1_VMR6.19),]
  ModelDFSLt1_VMR6.21 <- ModelDFSLt1_VMR6.20[-nrow(ModelDFSLt1_VMR6.20),]
  ModelDFSLt1_VMR6.22 <- ModelDFSLt1_VMR6.21[-nrow(ModelDFSLt1_VMR6.21),]
  ModelDFSLt1_VMR6.23 <- ModelDFSLt1_VMR6.22[-nrow(ModelDFSLt1_VMR6.22),]
  ModelDFSLt1_VMR6.24 <- ModelDFSLt1_VMR6.23[-nrow(ModelDFSLt1_VMR6.23),]

  ModelDFSLt1_VMR6 <- ModelDFSLt1_VMR6.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR6 <- ModelDFSLt0_VMR6 - ModelDFSLt1_VMR6

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR6$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR6P because implies a one-off input of C
  ModelDFSL_VMR6P <- ModelDFSL1y_VMR6

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR6P,"VXP_Models\\ModelDFSL_R_VMR6P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR6 <- ggplot(ModelDFSL_VMR6P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR6, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR6, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR6, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR6, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR6, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR6, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR6, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR6
  ggplotly(P_CFluxI1y_VMR6)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR6P_YCO2 <- ModelDFSL_VMR6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR6P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR6")

  PA_CO21y_VMR6 <- ggplot(ModelDFSL_VMR6P_YCO2, aes(x = Year, y = AnnualCO2_VMR6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR6
  ggplotly(PA_CO21y_VMR6)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR6P_YCO2D <- merge(ModelDFSL_VMR6P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR6P_YCO2D$AnnualCO2D_VMR6 <- ModelDFSL_VMR6P_YCO2D$AnnualCO2_VMR6 * ModelDFSL_VMR6P_YCO2D$GWP100

  PA_CO21yD_VMR6 <- ggplot(ModelDFSL_VMR6P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR6
  ggplotly(PA_CO21yD_VMR6)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR6P_YC <- ModelDFSL_VMR6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR6P_YC) <- c("Year", "Months", "AnnualCTail_VMR6")

  PA_C1y_VMR6 <- ggplot(ModelDFSL_VMR6P_YC, aes(x = Year, y = AnnualCTail_VMR6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR6
  ggplotly(PA_C1y_VMR6)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR6P_YCT <- ModelDFSL_VMR6P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR6)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR6P_YCT) <- c("Year", "Months", "AnnualCTail_VMR6")

  PA_CT1y_VMR6 <- ggplot(ModelDFSL_VMR6P_YCT, aes(x = Year, y = AnnualCTail_VMR6)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR6
  ggplotly(PA_CT1y_VMR6)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR6P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR6P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR6P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR6P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR6P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR6P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR6P_YCT,"CTails_P\\ModelDFSL_R_C_VMR6P.xlsx") #Yearly C emissions



  #### 11.8 - VMR7) Manure; 70%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR7=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR7=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.7,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR7=data.frame(years,rep(fT_VMR7*fW_VMR7,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR7=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.7, #Percent clay in mineral soil
    xi=xi.frame_VMR7) #Loads the model

  Ct_VMR7=getC(Model_VMR7) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR7, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR7 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR7 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR7 <- as.data.frame(VEC_Lit_VMR7) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR7$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR7 <- as.data.frame(VEC_Man_VMR7) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR7$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR7, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR7, class) #Check that class is numeric
  LitterCinputs_VMR7=VEC_LitDF_VMR7   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR7=VEC_ManDF_VMR7 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR7 <- merge(LitterCinputs_VMR7, ManureCinputs_VMR7, by = "MNumber")
  MCinputs_VMR7$MInput_VMR7 <- MCinputs_VMR7$VEC_Lit_VMR7 + MCinputs_VMR7$VEC_Man_VMR7

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR7)[which(names(MCinputs_VMR7) == "VEC_Lit_VMR7")] <- "LitterC_VMR7"
  colnames(MCinputs_VMR7)[which(names(MCinputs_VMR7) == "VEC_Man_VMR7")] <- "ManureC_VMR7"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR7 <- as.data.frame(Ct_VMR7)
  colnames(ModelDF_VMR7) <- c('DPM_VMR7','RPM_VMR7','BIO_VMR7', 'HUM_VMR7', 'IOM_VMR7')
  ModelDFS_VMR7 <- ModelDF_VMR7[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR7$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR7$AllPools_VMR7 <- ModelDFS_VMR7$DPM_VMR7 + ModelDFS_VMR7$RPM_VMR7 + ModelDFS_VMR7$BIO_VMR7 + ModelDFS_VMR7$HUM_VMR7 + ModelDFS_VMR7$IOM_VMR7

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR7$AllPools_noIOM_VMR7 <- ModelDFS_VMR7$DPM_VMR7 + ModelDFS_VMR7$RPM_VMR7 + ModelDFS_VMR7$BIO_VMR7 + ModelDFS_VMR7$HUM_VMR7

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR7 <- merge(ModelDFS_VMR7, MCinputs_VMR7, by = "MNumber")

  ModelDFSL_VMR7$MInput_VMR7 <- ModelDFSL_VMR7$LitterC_VMR7 + ModelDFSL_VMR7$ManureC_VMR7
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR7$CTails_VMR7 <- ModelDFSL_VMR7$AllPools_noIOM_VMR7 + ModelDFSL_VMR7$MInput_VMR7

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR7$AccumInput_VMR7 = ModelDFSL_VMR7$AccumInput_VMR7=cumsum(ModelDFSL_VMR7$MInput_VMR7)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR7$MGrowth_DPM_VMR7 <- ave(ModelDFSL_VMR7$DPM_VMR7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR7$MGrowth_RPM_VMR7 <- ave(ModelDFSL_VMR7$RPM_VMR7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR7$MGrowth_BIO_VMR7 <- ave(ModelDFSL_VMR7$BIO_VMR7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR7$MGrowth_HUM_VMR7 <- ave(ModelDFSL_VMR7$HUM_VMR7, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR7$MGrowth_IOM_VMR7 <- ave(ModelDFSL_VMR7$IOM_VMR7, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR7$M_CCO2_VMR7 <- ModelDFSL_VMR7$MInput_VMR7 - ModelDFSL_VMR7$MGrowth_DPM_VMR7 - ModelDFSL_VMR7$MGrowth_RPM_VMR7 - ModelDFSL_VMR7$MGrowth_BIO_VMR7 - ModelDFSL_VMR7$MGrowth_HUM_VMR7
  ModelDFSL_VMR7$Accum_CCO2_VMR7 <- ModelDFSL_VMR7$AccumInput_VMR7 - ModelDFSL_VMR7$AllPools_noIOM_VMR7

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR7$M_CCO2_VMR7[1] <- 0
  ModelDFSL_VMR7$Accum_CCO2_VMR7[1] <- 0

  #Balance validation
  ModelDFSL_VMR7$Balance_VMR7 <- ModelDFSL_VMR7$AccumInput_VMR7 - ModelDFSL_VMR7$Accum_CCO2_VMR7 - (ModelDFSL_VMR7$DPM_VMR7 + ModelDFSL_VMR7$RPM_VMR7 + ModelDFSL_VMR7$BIO_VMR7 + ModelDFSL_VMR7$HUM_VMR7)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR7$M_CO2_VMR7 <- ModelDFSL_VMR7$M_CCO2_VMR7 * 44/12
  ModelDFSL_VMR7$Accum_CO2_VMR7 <- ModelDFSL_VMR7$Accum_CCO2_VMR7 * 44/12

  #This model will be called VMR7C because implies a continuous input of C
  ModelDFSL_VMR7C <- ModelDFSL_VMR7

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR7C,"VXC_Models\\ModelDFSL_R_VMR7C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR7 <- ModelDFSL_VMR7 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR7.1 <- rbind(c(0:0), ModelDFSLt0_VMR7)
  ModelDFSLt1_VMR7.2 <- rbind(c(0:0), ModelDFSLt1_VMR7.1)
  ModelDFSLt1_VMR7.3 <- rbind(c(0:0), ModelDFSLt1_VMR7.2)
  ModelDFSLt1_VMR7.4 <- rbind(c(0:0), ModelDFSLt1_VMR7.3)
  ModelDFSLt1_VMR7.5 <- rbind(c(0:0), ModelDFSLt1_VMR7.4)
  ModelDFSLt1_VMR7.6 <- rbind(c(0:0), ModelDFSLt1_VMR7.5)
  ModelDFSLt1_VMR7.7 <- rbind(c(0:0), ModelDFSLt1_VMR7.6)
  ModelDFSLt1_VMR7.8 <- rbind(c(0:0), ModelDFSLt1_VMR7.7)
  ModelDFSLt1_VMR7.9 <- rbind(c(0:0), ModelDFSLt1_VMR7.8)
  ModelDFSLt1_VMR7.10 <- rbind(c(0:0), ModelDFSLt1_VMR7.9)
  ModelDFSLt1_VMR7.11 <- rbind(c(0:0), ModelDFSLt1_VMR7.10)
  ModelDFSLt1_VMR7.12 <- rbind(c(0:0), ModelDFSLt1_VMR7.11)
  ModelDFSLt1_VMR7.13 <- ModelDFSLt1_VMR7.12[-nrow(ModelDFSLt1_VMR7.12),]
  ModelDFSLt1_VMR7.14 <- ModelDFSLt1_VMR7.13[-nrow(ModelDFSLt1_VMR7.13),]
  ModelDFSLt1_VMR7.15 <- ModelDFSLt1_VMR7.14[-nrow(ModelDFSLt1_VMR7.14),]
  ModelDFSLt1_VMR7.16 <- ModelDFSLt1_VMR7.15[-nrow(ModelDFSLt1_VMR7.15),]
  ModelDFSLt1_VMR7.17 <- ModelDFSLt1_VMR7.16[-nrow(ModelDFSLt1_VMR7.16),]
  ModelDFSLt1_VMR7.18 <- ModelDFSLt1_VMR7.17[-nrow(ModelDFSLt1_VMR7.17),]
  ModelDFSLt1_VMR7.19 <- ModelDFSLt1_VMR7.18[-nrow(ModelDFSLt1_VMR7.18),]
  ModelDFSLt1_VMR7.20 <- ModelDFSLt1_VMR7.19[-nrow(ModelDFSLt1_VMR7.19),]
  ModelDFSLt1_VMR7.21 <- ModelDFSLt1_VMR7.20[-nrow(ModelDFSLt1_VMR7.20),]
  ModelDFSLt1_VMR7.22 <- ModelDFSLt1_VMR7.21[-nrow(ModelDFSLt1_VMR7.21),]
  ModelDFSLt1_VMR7.23 <- ModelDFSLt1_VMR7.22[-nrow(ModelDFSLt1_VMR7.22),]
  ModelDFSLt1_VMR7.24 <- ModelDFSLt1_VMR7.23[-nrow(ModelDFSLt1_VMR7.23),]

  ModelDFSLt1_VMR7 <- ModelDFSLt1_VMR7.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR7 <- ModelDFSLt0_VMR7 - ModelDFSLt1_VMR7

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR7$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR7P because implies a one-off input of C
  ModelDFSL_VMR7P <- ModelDFSL1y_VMR7

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR7P,"VXP_Models\\ModelDFSL_R_VMR7P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR7 <- ggplot(ModelDFSL_VMR7P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR7, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR7, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR7, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR7, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR7, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR7, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR7, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR7
  ggplotly(P_CFluxI1y_VMR7)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR7P_YCO2 <- ModelDFSL_VMR7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR7P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR7")

  PA_CO21y_VMR7 <- ggplot(ModelDFSL_VMR7P_YCO2, aes(x = Year, y = AnnualCO2_VMR7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR7
  ggplotly(PA_CO21y_VMR7)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR7P_YCO2D <- merge(ModelDFSL_VMR7P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR7P_YCO2D$AnnualCO2D_VMR7 <- ModelDFSL_VMR7P_YCO2D$AnnualCO2_VMR7 * ModelDFSL_VMR7P_YCO2D$GWP100

  PA_CO21yD_VMR7 <- ggplot(ModelDFSL_VMR7P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR7
  ggplotly(PA_CO21yD_VMR7)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR7P_YC <- ModelDFSL_VMR7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR7P_YC) <- c("Year", "Months", "AnnualCTail_VMR7")

  PA_C1y_VMR7 <- ggplot(ModelDFSL_VMR7P_YC, aes(x = Year, y = AnnualCTail_VMR7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR7
  ggplotly(PA_C1y_VMR7)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR7P_YCT <- ModelDFSL_VMR7P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR7)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR7P_YCT) <- c("Year", "Months", "AnnualCTail_VMR7")

  PA_CT1y_VMR7 <- ggplot(ModelDFSL_VMR7P_YCT, aes(x = Year, y = AnnualCTail_VMR7)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR7
  ggplotly(PA_CT1y_VMR7)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR7P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR7P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR7P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR7P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR7P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR7P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR7P_YCT,"CTails_P\\ModelDFSL_R_C_VMR7P.xlsx") #Yearly C emissions



  #### 11.9 - VMR8) Manure; 80%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR8=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR8=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.8,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR8=data.frame(years,rep(fT_VMR8*fW_VMR8,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR8=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.8, #Percent clay in mineral soil
    xi=xi.frame_VMR8) #Loads the model

  Ct_VMR8=getC(Model_VMR8) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR8, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR8 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR8 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR8 <- as.data.frame(VEC_Lit_VMR8) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR8$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR8 <- as.data.frame(VEC_Man_VMR8) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR8$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR8, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR8, class) #Check that class is numeric
  LitterCinputs_VMR8=VEC_LitDF_VMR8   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR8=VEC_ManDF_VMR8 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR8 <- merge(LitterCinputs_VMR8, ManureCinputs_VMR8, by = "MNumber")
  MCinputs_VMR8$MInput_VMR8 <- MCinputs_VMR8$VEC_Lit_VMR8 + MCinputs_VMR8$VEC_Man_VMR8

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR8)[which(names(MCinputs_VMR8) == "VEC_Lit_VMR8")] <- "LitterC_VMR8"
  colnames(MCinputs_VMR8)[which(names(MCinputs_VMR8) == "VEC_Man_VMR8")] <- "ManureC_VMR8"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR8 <- as.data.frame(Ct_VMR8)
  colnames(ModelDF_VMR8) <- c('DPM_VMR8','RPM_VMR8','BIO_VMR8', 'HUM_VMR8', 'IOM_VMR8')
  ModelDFS_VMR8 <- ModelDF_VMR8[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR8$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR8$AllPools_VMR8 <- ModelDFS_VMR8$DPM_VMR8 + ModelDFS_VMR8$RPM_VMR8 + ModelDFS_VMR8$BIO_VMR8 + ModelDFS_VMR8$HUM_VMR8 + ModelDFS_VMR8$IOM_VMR8

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR8$AllPools_noIOM_VMR8 <- ModelDFS_VMR8$DPM_VMR8 + ModelDFS_VMR8$RPM_VMR8 + ModelDFS_VMR8$BIO_VMR8 + ModelDFS_VMR8$HUM_VMR8

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR8 <- merge(ModelDFS_VMR8, MCinputs_VMR8, by = "MNumber")

  ModelDFSL_VMR8$MInput_VMR8 <- ModelDFSL_VMR8$LitterC_VMR8 + ModelDFSL_VMR8$ManureC_VMR8
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR8$CTails_VMR8 <- ModelDFSL_VMR8$AllPools_noIOM_VMR8 + ModelDFSL_VMR8$MInput_VMR8

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR8$AccumInput_VMR8 = ModelDFSL_VMR8$AccumInput_VMR8=cumsum(ModelDFSL_VMR8$MInput_VMR8)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR8$MGrowth_DPM_VMR8 <- ave(ModelDFSL_VMR8$DPM_VMR8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR8$MGrowth_RPM_VMR8 <- ave(ModelDFSL_VMR8$RPM_VMR8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR8$MGrowth_BIO_VMR8 <- ave(ModelDFSL_VMR8$BIO_VMR8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR8$MGrowth_HUM_VMR8 <- ave(ModelDFSL_VMR8$HUM_VMR8, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR8$MGrowth_IOM_VMR8 <- ave(ModelDFSL_VMR8$IOM_VMR8, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR8$M_CCO2_VMR8 <- ModelDFSL_VMR8$MInput_VMR8 - ModelDFSL_VMR8$MGrowth_DPM_VMR8 - ModelDFSL_VMR8$MGrowth_RPM_VMR8 - ModelDFSL_VMR8$MGrowth_BIO_VMR8 - ModelDFSL_VMR8$MGrowth_HUM_VMR8
  ModelDFSL_VMR8$Accum_CCO2_VMR8 <- ModelDFSL_VMR8$AccumInput_VMR8 - ModelDFSL_VMR8$AllPools_noIOM_VMR8

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR8$M_CCO2_VMR8[1] <- 0
  ModelDFSL_VMR8$Accum_CCO2_VMR8[1] <- 0

  #Balance validation
  ModelDFSL_VMR8$Balance_VMR8 <- ModelDFSL_VMR8$AccumInput_VMR8 - ModelDFSL_VMR8$Accum_CCO2_VMR8 - (ModelDFSL_VMR8$DPM_VMR8 + ModelDFSL_VMR8$RPM_VMR8 + ModelDFSL_VMR8$BIO_VMR8 + ModelDFSL_VMR8$HUM_VMR8)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR8$M_CO2_VMR8 <- ModelDFSL_VMR8$M_CCO2_VMR8 * 44/12
  ModelDFSL_VMR8$Accum_CO2_VMR8 <- ModelDFSL_VMR8$Accum_CCO2_VMR8 * 44/12

  #This model will be called VMR8C because implies a continuous input of C
  ModelDFSL_VMR8C <- ModelDFSL_VMR8

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR8C,"VXC_Models\\ModelDFSL_R_VMR8C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR8 <- ModelDFSL_VMR8 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR8.1 <- rbind(c(0:0), ModelDFSLt0_VMR8)
  ModelDFSLt1_VMR8.2 <- rbind(c(0:0), ModelDFSLt1_VMR8.1)
  ModelDFSLt1_VMR8.3 <- rbind(c(0:0), ModelDFSLt1_VMR8.2)
  ModelDFSLt1_VMR8.4 <- rbind(c(0:0), ModelDFSLt1_VMR8.3)
  ModelDFSLt1_VMR8.5 <- rbind(c(0:0), ModelDFSLt1_VMR8.4)
  ModelDFSLt1_VMR8.6 <- rbind(c(0:0), ModelDFSLt1_VMR8.5)
  ModelDFSLt1_VMR8.7 <- rbind(c(0:0), ModelDFSLt1_VMR8.6)
  ModelDFSLt1_VMR8.8 <- rbind(c(0:0), ModelDFSLt1_VMR8.7)
  ModelDFSLt1_VMR8.9 <- rbind(c(0:0), ModelDFSLt1_VMR8.8)
  ModelDFSLt1_VMR8.10 <- rbind(c(0:0), ModelDFSLt1_VMR8.9)
  ModelDFSLt1_VMR8.11 <- rbind(c(0:0), ModelDFSLt1_VMR8.10)
  ModelDFSLt1_VMR8.12 <- rbind(c(0:0), ModelDFSLt1_VMR8.11)
  ModelDFSLt1_VMR8.13 <- ModelDFSLt1_VMR8.12[-nrow(ModelDFSLt1_VMR8.12),]
  ModelDFSLt1_VMR8.14 <- ModelDFSLt1_VMR8.13[-nrow(ModelDFSLt1_VMR8.13),]
  ModelDFSLt1_VMR8.15 <- ModelDFSLt1_VMR8.14[-nrow(ModelDFSLt1_VMR8.14),]
  ModelDFSLt1_VMR8.16 <- ModelDFSLt1_VMR8.15[-nrow(ModelDFSLt1_VMR8.15),]
  ModelDFSLt1_VMR8.17 <- ModelDFSLt1_VMR8.16[-nrow(ModelDFSLt1_VMR8.16),]
  ModelDFSLt1_VMR8.18 <- ModelDFSLt1_VMR8.17[-nrow(ModelDFSLt1_VMR8.17),]
  ModelDFSLt1_VMR8.19 <- ModelDFSLt1_VMR8.18[-nrow(ModelDFSLt1_VMR8.18),]
  ModelDFSLt1_VMR8.20 <- ModelDFSLt1_VMR8.19[-nrow(ModelDFSLt1_VMR8.19),]
  ModelDFSLt1_VMR8.21 <- ModelDFSLt1_VMR8.20[-nrow(ModelDFSLt1_VMR8.20),]
  ModelDFSLt1_VMR8.22 <- ModelDFSLt1_VMR8.21[-nrow(ModelDFSLt1_VMR8.21),]
  ModelDFSLt1_VMR8.23 <- ModelDFSLt1_VMR8.22[-nrow(ModelDFSLt1_VMR8.22),]
  ModelDFSLt1_VMR8.24 <- ModelDFSLt1_VMR8.23[-nrow(ModelDFSLt1_VMR8.23),]

  ModelDFSLt1_VMR8 <- ModelDFSLt1_VMR8.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR8 <- ModelDFSLt0_VMR8 - ModelDFSLt1_VMR8

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR8$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR8P because implies a one-off input of C
  ModelDFSL_VMR8P <- ModelDFSL1y_VMR8

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR8P,"VXP_Models\\ModelDFSL_R_VMR8P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR8 <- ggplot(ModelDFSL_VMR8P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR8, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR8, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR8, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR8, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR8, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR8, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR8, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR8
  ggplotly(P_CFluxI1y_VMR8)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR8P_YCO2 <- ModelDFSL_VMR8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR8P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR8")

  PA_CO21y_VMR8 <- ggplot(ModelDFSL_VMR8P_YCO2, aes(x = Year, y = AnnualCO2_VMR8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR8
  ggplotly(PA_CO21y_VMR8)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR8P_YCO2D <- merge(ModelDFSL_VMR8P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR8P_YCO2D$AnnualCO2D_VMR8 <- ModelDFSL_VMR8P_YCO2D$AnnualCO2_VMR8 * ModelDFSL_VMR8P_YCO2D$GWP100

  PA_CO21yD_VMR8 <- ggplot(ModelDFSL_VMR8P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR8
  ggplotly(PA_CO21yD_VMR8)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR8P_YC <- ModelDFSL_VMR8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR8P_YC) <- c("Year", "Months", "AnnualCTail_VMR8")

  PA_C1y_VMR8 <- ggplot(ModelDFSL_VMR8P_YC, aes(x = Year, y = AnnualCTail_VMR8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR8
  ggplotly(PA_C1y_VMR8)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR8P_YCT <- ModelDFSL_VMR8P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR8)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR8P_YCT) <- c("Year", "Months", "AnnualCTail_VMR8")

  PA_CT1y_VMR8 <- ggplot(ModelDFSL_VMR8P_YCT, aes(x = Year, y = AnnualCTail_VMR8)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR8
  ggplotly(PA_CT1y_VMR8)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR8P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR8P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR8P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR8P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR8P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR8P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR8P_YCT,"CTails_P\\ModelDFSL_R_C_VMR8P.xlsx") #Yearly C emissions



  #### 11.10 - VMR9) Manure; 90%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR9=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR9=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                          S.Thick = soil.thick, pClay = clay100*0.9,
                          pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR9=data.frame(years,rep(fT_VMR9*fW_VMR9,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR9=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100*0.9, #Percent clay in mineral soil
    xi=xi.frame_VMR9) #Loads the model

  Ct_VMR9=getC(Model_VMR9) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR9, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR9 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR9 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR9 <- as.data.frame(VEC_Lit_VMR9) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR9$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR9 <- as.data.frame(VEC_Man_VMR9) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR9$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR9, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR9, class) #Check that class is numeric
  LitterCinputs_VMR9=VEC_LitDF_VMR9   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR9=VEC_ManDF_VMR9 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR9 <- merge(LitterCinputs_VMR9, ManureCinputs_VMR9, by = "MNumber")
  MCinputs_VMR9$MInput_VMR9 <- MCinputs_VMR9$VEC_Lit_VMR9 + MCinputs_VMR9$VEC_Man_VMR9

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR9)[which(names(MCinputs_VMR9) == "VEC_Lit_VMR9")] <- "LitterC_VMR9"
  colnames(MCinputs_VMR9)[which(names(MCinputs_VMR9) == "VEC_Man_VMR9")] <- "ManureC_VMR9"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR9 <- as.data.frame(Ct_VMR9)
  colnames(ModelDF_VMR9) <- c('DPM_VMR9','RPM_VMR9','BIO_VMR9', 'HUM_VMR9', 'IOM_VMR9')
  ModelDFS_VMR9 <- ModelDF_VMR9[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR9$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR9$AllPools_VMR9 <- ModelDFS_VMR9$DPM_VMR9 + ModelDFS_VMR9$RPM_VMR9 + ModelDFS_VMR9$BIO_VMR9 + ModelDFS_VMR9$HUM_VMR9 + ModelDFS_VMR9$IOM_VMR9

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR9$AllPools_noIOM_VMR9 <- ModelDFS_VMR9$DPM_VMR9 + ModelDFS_VMR9$RPM_VMR9 + ModelDFS_VMR9$BIO_VMR9 + ModelDFS_VMR9$HUM_VMR9

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR9 <- merge(ModelDFS_VMR9, MCinputs_VMR9, by = "MNumber")

  ModelDFSL_VMR9$MInput_VMR9 <- ModelDFSL_VMR9$LitterC_VMR9 + ModelDFSL_VMR9$ManureC_VMR9
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR9$CTails_VMR9 <- ModelDFSL_VMR9$AllPools_noIOM_VMR9 + ModelDFSL_VMR9$MInput_VMR9

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR9$AccumInput_VMR9 = ModelDFSL_VMR9$AccumInput_VMR9=cumsum(ModelDFSL_VMR9$MInput_VMR9)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR9$MGrowth_DPM_VMR9 <- ave(ModelDFSL_VMR9$DPM_VMR9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR9$MGrowth_RPM_VMR9 <- ave(ModelDFSL_VMR9$RPM_VMR9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR9$MGrowth_BIO_VMR9 <- ave(ModelDFSL_VMR9$BIO_VMR9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR9$MGrowth_HUM_VMR9 <- ave(ModelDFSL_VMR9$HUM_VMR9, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR9$MGrowth_IOM_VMR9 <- ave(ModelDFSL_VMR9$IOM_VMR9, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR9$M_CCO2_VMR9 <- ModelDFSL_VMR9$MInput_VMR9 - ModelDFSL_VMR9$MGrowth_DPM_VMR9 - ModelDFSL_VMR9$MGrowth_RPM_VMR9 - ModelDFSL_VMR9$MGrowth_BIO_VMR9 - ModelDFSL_VMR9$MGrowth_HUM_VMR9
  ModelDFSL_VMR9$Accum_CCO2_VMR9 <- ModelDFSL_VMR9$AccumInput_VMR9 - ModelDFSL_VMR9$AllPools_noIOM_VMR9

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR9$M_CCO2_VMR9[1] <- 0
  ModelDFSL_VMR9$Accum_CCO2_VMR9[1] <- 0

  #Balance validation
  ModelDFSL_VMR9$Balance_VMR9 <- ModelDFSL_VMR9$AccumInput_VMR9 - ModelDFSL_VMR9$Accum_CCO2_VMR9 - (ModelDFSL_VMR9$DPM_VMR9 + ModelDFSL_VMR9$RPM_VMR9 + ModelDFSL_VMR9$BIO_VMR9 + ModelDFSL_VMR9$HUM_VMR9)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR9$M_CO2_VMR9 <- ModelDFSL_VMR9$M_CCO2_VMR9 * 44/12
  ModelDFSL_VMR9$Accum_CO2_VMR9 <- ModelDFSL_VMR9$Accum_CCO2_VMR9 * 44/12

  #This model will be called VMR9C because implies a continuous input of C
  ModelDFSL_VMR9C <- ModelDFSL_VMR9

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR9C,"VXC_Models\\ModelDFSL_R_VMR9C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR9 <- ModelDFSL_VMR9 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR9.1 <- rbind(c(0:0), ModelDFSLt0_VMR9)
  ModelDFSLt1_VMR9.2 <- rbind(c(0:0), ModelDFSLt1_VMR9.1)
  ModelDFSLt1_VMR9.3 <- rbind(c(0:0), ModelDFSLt1_VMR9.2)
  ModelDFSLt1_VMR9.4 <- rbind(c(0:0), ModelDFSLt1_VMR9.3)
  ModelDFSLt1_VMR9.5 <- rbind(c(0:0), ModelDFSLt1_VMR9.4)
  ModelDFSLt1_VMR9.6 <- rbind(c(0:0), ModelDFSLt1_VMR9.5)
  ModelDFSLt1_VMR9.7 <- rbind(c(0:0), ModelDFSLt1_VMR9.6)
  ModelDFSLt1_VMR9.8 <- rbind(c(0:0), ModelDFSLt1_VMR9.7)
  ModelDFSLt1_VMR9.9 <- rbind(c(0:0), ModelDFSLt1_VMR9.8)
  ModelDFSLt1_VMR9.10 <- rbind(c(0:0), ModelDFSLt1_VMR9.9)
  ModelDFSLt1_VMR9.11 <- rbind(c(0:0), ModelDFSLt1_VMR9.10)
  ModelDFSLt1_VMR9.12 <- rbind(c(0:0), ModelDFSLt1_VMR9.11)
  ModelDFSLt1_VMR9.13 <- ModelDFSLt1_VMR9.12[-nrow(ModelDFSLt1_VMR9.12),]
  ModelDFSLt1_VMR9.14 <- ModelDFSLt1_VMR9.13[-nrow(ModelDFSLt1_VMR9.13),]
  ModelDFSLt1_VMR9.15 <- ModelDFSLt1_VMR9.14[-nrow(ModelDFSLt1_VMR9.14),]
  ModelDFSLt1_VMR9.16 <- ModelDFSLt1_VMR9.15[-nrow(ModelDFSLt1_VMR9.15),]
  ModelDFSLt1_VMR9.17 <- ModelDFSLt1_VMR9.16[-nrow(ModelDFSLt1_VMR9.16),]
  ModelDFSLt1_VMR9.18 <- ModelDFSLt1_VMR9.17[-nrow(ModelDFSLt1_VMR9.17),]
  ModelDFSLt1_VMR9.19 <- ModelDFSLt1_VMR9.18[-nrow(ModelDFSLt1_VMR9.18),]
  ModelDFSLt1_VMR9.20 <- ModelDFSLt1_VMR9.19[-nrow(ModelDFSLt1_VMR9.19),]
  ModelDFSLt1_VMR9.21 <- ModelDFSLt1_VMR9.20[-nrow(ModelDFSLt1_VMR9.20),]
  ModelDFSLt1_VMR9.22 <- ModelDFSLt1_VMR9.21[-nrow(ModelDFSLt1_VMR9.21),]
  ModelDFSLt1_VMR9.23 <- ModelDFSLt1_VMR9.22[-nrow(ModelDFSLt1_VMR9.22),]
  ModelDFSLt1_VMR9.24 <- ModelDFSLt1_VMR9.23[-nrow(ModelDFSLt1_VMR9.23),]

  ModelDFSLt1_VMR9 <- ModelDFSLt1_VMR9.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR9 <- ModelDFSLt0_VMR9 - ModelDFSLt1_VMR9

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR9$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR9P because implies a one-off input of C
  ModelDFSL_VMR9P <- ModelDFSL1y_VMR9

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR9P,"VXP_Models\\ModelDFSL_R_VMR9P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR9 <- ggplot(ModelDFSL_VMR9P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR9, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR9, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR9, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR9, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR9, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR9, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR9, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR9
  ggplotly(P_CFluxI1y_VMR9)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR9P_YCO2 <- ModelDFSL_VMR9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR9P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR9")

  PA_CO21y_VMR9 <- ggplot(ModelDFSL_VMR9P_YCO2, aes(x = Year, y = AnnualCO2_VMR9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR9
  ggplotly(PA_CO21y_VMR9)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR9P_YCO2D <- merge(ModelDFSL_VMR9P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR9P_YCO2D$AnnualCO2D_VMR9 <- ModelDFSL_VMR9P_YCO2D$AnnualCO2_VMR9 * ModelDFSL_VMR9P_YCO2D$GWP100

  PA_CO21yD_VMR9 <- ggplot(ModelDFSL_VMR9P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR9
  ggplotly(PA_CO21yD_VMR9)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR9P_YC <- ModelDFSL_VMR9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR9P_YC) <- c("Year", "Months", "AnnualCTail_VMR9")

  PA_C1y_VMR9 <- ggplot(ModelDFSL_VMR9P_YC, aes(x = Year, y = AnnualCTail_VMR9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR9
  ggplotly(PA_C1y_VMR9)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR9P_YCT <- ModelDFSL_VMR9P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR9)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR9P_YCT) <- c("Year", "Months", "AnnualCTail_VMR9")

  PA_CT1y_VMR9 <- ggplot(ModelDFSL_VMR9P_YCT, aes(x = Year, y = AnnualCTail_VMR9)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR9
  ggplotly(PA_CT1y_VMR9)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR9P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR9P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR9P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR9P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR9P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR9P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR9P_YCT,"CTails_P\\ModelDFSL_R_C_VMR9P.xlsx") #Yearly C emissions



  #### 11.11 - VMR10) Manure; 100%clay; Reduced tillage ####
  #The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
  fT_VMR10=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
  fW_VMR10=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
                           S.Thick = soil.thick, pClay = clay100,
                           pE = 1.0, bare = FALSE)$b #Moisture effects per month
  xi.frame_VMR10=data.frame(years,rep(fT_VMR10*fW_VMR10,length.out=length(years)))

  #To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
  Model_VMR10=SoilR::RothCModel(
    t=years, #Simulation Length
    ks=c(k.DPM = 10*RTRM, k.RPM = 0.3*RTRM, k.BIO = 0.66*RTRM, k.HUM = 0.02*RTRM, k.IOM = 0*RTRM), #Decomposition rates for the different pools
    C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
    In=Scalar_LitterCinputs*0, #Amount of litter inputs by time
    FYM = Scalar_ManureCinputs*1, #Amount of Farm Yard Manure by time
    clay=clay100, #Percent clay in mineral soil
    xi=xi.frame_VMR10) #Loads the model

  Ct_VMR10=getC(Model_VMR10) #Calculates stocks for each pool per month

  #The results can be plotted with the commands
  matplot(years, Ct_VMR10, type="l", lty=1, col=1:5,
          xlab="Time (years)", ylab="C stocks (Mg/ha)")
  legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
         lty=1, col=1:5, bty="n")

  VEC_Lit_VMR10 <- rep(M_Scalar_LitterCinputs*0, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
  VEC_Man_VMR10 <- rep(M_Scalar_ManureCinputs*1, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

  VEC_LitDF_VMR10 <- as.data.frame(VEC_Lit_VMR10) #Converting the Litter vector to a data frame
  VEC_LitDF_VMR10$MNumber <- seq(from = 1, to = SimulationLength_months)
  VEC_ManDF_VMR10 <- as.data.frame(VEC_Man_VMR10) #Converting the Manure vector to a data frame
  VEC_ManDF_VMR10$MNumber <- seq(from = 1, to = SimulationLength_months)

  sapply(VEC_LitDF_VMR10, class) #Check that class is numeric
  sapply(VEC_ManDF_VMR10, class) #Check that class is numeric
  LitterCinputs_VMR10=VEC_LitDF_VMR10   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  ManureCinputs_VMR10=VEC_ManDF_VMR10 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
  MCinputs_VMR10 <- merge(LitterCinputs_VMR10, ManureCinputs_VMR10, by = "MNumber")
  MCinputs_VMR10$MInput_VMR10 <- MCinputs_VMR10$VEC_Lit_VMR10 + MCinputs_VMR10$VEC_Man_VMR10

  #Change names of litter and manure columns for more common ones
  colnames(MCinputs_VMR10)[which(names(MCinputs_VMR10) == "VEC_Lit_VMR10")] <- "LitterC_VMR10"
  colnames(MCinputs_VMR10)[which(names(MCinputs_VMR10) == "VEC_Man_VMR10")] <- "ManureC_VMR10"

  #Create a dataframe with the data considering the simulation length
  ModelDF_VMR10 <- as.data.frame(Ct_VMR10)
  colnames(ModelDF_VMR10) <- c('DPM_VMR10','RPM_VMR10','BIO_VMR10', 'HUM_VMR10', 'IOM_VMR10')
  ModelDFS_VMR10 <- ModelDF_VMR10[1:SimulationLength_months, ]

  #Create a column with the number of month of analysis
  ModelDFS_VMR10$MNumber <- seq(from = 1, to = SimulationLength_months)

  #Create a column that sums all pools
  ModelDFS_VMR10$AllPools_VMR10 <- ModelDFS_VMR10$DPM_VMR10 + ModelDFS_VMR10$RPM_VMR10 + ModelDFS_VMR10$BIO_VMR10 + ModelDFS_VMR10$HUM_VMR10 + ModelDFS_VMR10$IOM_VMR10

  #Create a column that sums all pools except IOM (static)
  ModelDFS_VMR10$AllPools_noIOM_VMR10 <- ModelDFS_VMR10$DPM_VMR10 + ModelDFS_VMR10$RPM_VMR10 + ModelDFS_VMR10$BIO_VMR10 + ModelDFS_VMR10$HUM_VMR10

  #Create a column that add the monthly input of C (manure and litter separately)
  ModelDFSL_VMR10 <- merge(ModelDFS_VMR10, MCinputs_VMR10, by = "MNumber")

  ModelDFSL_VMR10$MInput_VMR10 <- ModelDFSL_VMR10$LitterC_VMR10 + ModelDFSL_VMR10$ManureC_VMR10
  #ModelDF_SL$MInput[1] <- 0

  #Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
  ModelDFSL_VMR10$CTails_VMR10 <- ModelDFSL_VMR10$AllPools_noIOM_VMR10 + ModelDFSL_VMR10$MInput_VMR10

  #Create Monthly Accumulated input of C
  ModelDFSL_VMR10$AccumInput_VMR10 = ModelDFSL_VMR10$AccumInput_VMR10=cumsum(ModelDFSL_VMR10$MInput_VMR10)

  #Create Monthly Growths for each carbon pool
  ModelDFSL_VMR10$MGrowth_DPM_VMR10 <- ave(ModelDFSL_VMR10$DPM_VMR10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR10$MGrowth_RPM_VMR10 <- ave(ModelDFSL_VMR10$RPM_VMR10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR10$MGrowth_BIO_VMR10 <- ave(ModelDFSL_VMR10$BIO_VMR10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR10$MGrowth_HUM_VMR10 <- ave(ModelDFSL_VMR10$HUM_VMR10, FUN = function(x) c(0, diff(x)))
  ModelDFSL_VMR10$MGrowth_IOM_VMR10 <- ave(ModelDFSL_VMR10$IOM_VMR10, FUN = function(x) c(0, diff(x)))

  #Create column for Monthly and Accumulated C-CO2 emissions
  ModelDFSL_VMR10$M_CCO2_VMR10 <- ModelDFSL_VMR10$MInput_VMR10 - ModelDFSL_VMR10$MGrowth_DPM_VMR10 - ModelDFSL_VMR10$MGrowth_RPM_VMR10 - ModelDFSL_VMR10$MGrowth_BIO_VMR10 - ModelDFSL_VMR10$MGrowth_HUM_VMR10
  ModelDFSL_VMR10$Accum_CCO2_VMR10 <- ModelDFSL_VMR10$AccumInput_VMR10 - ModelDFSL_VMR10$AllPools_noIOM_VMR10

  #Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
  ModelDFSL_VMR10$M_CCO2_VMR10[1] <- 0
  ModelDFSL_VMR10$Accum_CCO2_VMR10[1] <- 0

  #Balance validation
  ModelDFSL_VMR10$Balance_VMR10 <- ModelDFSL_VMR10$AccumInput_VMR10 - ModelDFSL_VMR10$Accum_CCO2_VMR10 - (ModelDFSL_VMR10$DPM_VMR10 + ModelDFSL_VMR10$RPM_VMR10 + ModelDFSL_VMR10$BIO_VMR10 + ModelDFSL_VMR10$HUM_VMR10)

  #Convert C-CO2 emissions into CO2 emissions
  ModelDFSL_VMR10$M_CO2_VMR10 <- ModelDFSL_VMR10$M_CCO2_VMR10 * 44/12
  ModelDFSL_VMR10$Accum_CO2_VMR10 <- ModelDFSL_VMR10$Accum_CCO2_VMR10 * 44/12

  #This model will be called VMR10C because implies a continuous input of C
  ModelDFSL_VMR10C <- ModelDFSL_VMR10

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR10C,"VXC_Models\\ModelDFSL_R_VMR10C.xlsx")

  #Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
  #Create two equal models by using the above general script
  #Create model t=x
  ModelDFSLt0_VMR10 <- ModelDFSL_VMR10 #Create model t=x

  #Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
  ModelDFSLt1_VMR10.1 <- rbind(c(0:0), ModelDFSLt0_VMR10)
  ModelDFSLt1_VMR10.2 <- rbind(c(0:0), ModelDFSLt1_VMR10.1)
  ModelDFSLt1_VMR10.3 <- rbind(c(0:0), ModelDFSLt1_VMR10.2)
  ModelDFSLt1_VMR10.4 <- rbind(c(0:0), ModelDFSLt1_VMR10.3)
  ModelDFSLt1_VMR10.5 <- rbind(c(0:0), ModelDFSLt1_VMR10.4)
  ModelDFSLt1_VMR10.6 <- rbind(c(0:0), ModelDFSLt1_VMR10.5)
  ModelDFSLt1_VMR10.7 <- rbind(c(0:0), ModelDFSLt1_VMR10.6)
  ModelDFSLt1_VMR10.8 <- rbind(c(0:0), ModelDFSLt1_VMR10.7)
  ModelDFSLt1_VMR10.9 <- rbind(c(0:0), ModelDFSLt1_VMR10.8)
  ModelDFSLt1_VMR10.10 <- rbind(c(0:0), ModelDFSLt1_VMR10.9)
  ModelDFSLt1_VMR10.11 <- rbind(c(0:0), ModelDFSLt1_VMR10.10)
  ModelDFSLt1_VMR10.12 <- rbind(c(0:0), ModelDFSLt1_VMR10.11)
  ModelDFSLt1_VMR10.13 <- ModelDFSLt1_VMR10.12[-nrow(ModelDFSLt1_VMR10.12),]
  ModelDFSLt1_VMR10.14 <- ModelDFSLt1_VMR10.13[-nrow(ModelDFSLt1_VMR10.13),]
  ModelDFSLt1_VMR10.15 <- ModelDFSLt1_VMR10.14[-nrow(ModelDFSLt1_VMR10.14),]
  ModelDFSLt1_VMR10.16 <- ModelDFSLt1_VMR10.15[-nrow(ModelDFSLt1_VMR10.15),]
  ModelDFSLt1_VMR10.17 <- ModelDFSLt1_VMR10.16[-nrow(ModelDFSLt1_VMR10.16),]
  ModelDFSLt1_VMR10.18 <- ModelDFSLt1_VMR10.17[-nrow(ModelDFSLt1_VMR10.17),]
  ModelDFSLt1_VMR10.19 <- ModelDFSLt1_VMR10.18[-nrow(ModelDFSLt1_VMR10.18),]
  ModelDFSLt1_VMR10.20 <- ModelDFSLt1_VMR10.19[-nrow(ModelDFSLt1_VMR10.19),]
  ModelDFSLt1_VMR10.21 <- ModelDFSLt1_VMR10.20[-nrow(ModelDFSLt1_VMR10.20),]
  ModelDFSLt1_VMR10.22 <- ModelDFSLt1_VMR10.21[-nrow(ModelDFSLt1_VMR10.21),]
  ModelDFSLt1_VMR10.23 <- ModelDFSLt1_VMR10.22[-nrow(ModelDFSLt1_VMR10.22),]
  ModelDFSLt1_VMR10.24 <- ModelDFSLt1_VMR10.23[-nrow(ModelDFSLt1_VMR10.23),]

  ModelDFSLt1_VMR10 <- ModelDFSLt1_VMR10.24 #Create model t=x+1

  #Subtract model (t=x) - (t=x+1)
  ModelDFSL1y_VMR10 <- ModelDFSLt0_VMR10 - ModelDFSLt1_VMR10

  #Re-do Month Number column because the substraction was also applied to it
  ModelDFSL1y_VMR10$MNumber <- seq(from = 1, to = SimulationLength_months)

  #This model will be called VMR10P because implies a one-off input of C
  ModelDFSL_VMR10P <- ModelDFSL1y_VMR10

  #Export the dataframe
  write_xlsx(ModelDFSL_VMR10P,"VXP_Models\\ModelDFSL_R_VMR10P.xlsx")

  #Plot 2: Geom_line with interactive plotly to represent carbon flows
  P_CFluxI1y_VMR10 <- ggplot(ModelDFSL_VMR10P, aes(x = MNumber)) +
    geom_line(aes(y = DPM_VMR10, color = "DPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = RPM_VMR10, color="RPM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = BIO_VMR10, color = "BIO", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = HUM_VMR10, color="HUM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = IOM_VMR10, color = "IOM", linetype = "Output"), size = 0.5) +
    geom_line(aes(y = AccumInput_VMR10, color="AccumInput", linetype="Input"), size = 1) +
    geom_line(aes(y = Accum_CCO2_VMR10, color = "Accum_CCO2", linetype="Output"), size = 1) +
    xlab("Month number") + ylab("Accumulated Carbon Flows") +
    guides(color=guide_legend(title="Flow")) +
    guides(linetype=guide_legend(title="Input/Output"))
  #facet_zoom(ylim = c(0, 25))

  P_CFluxI1y_VMR10
  ggplotly(P_CFluxI1y_VMR10)


  #Plot 3: Interactive Yearly CO2 emissions #
  MY <- 12
  ModelDFSL_VMR10P_YCO2 <- ModelDFSL_VMR10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VMR10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR10P_YCO2) <- c("Year", "Months", "AnnualCO2_VMR10")

  PA_CO21y_VMR10 <- ggplot(ModelDFSL_VMR10P_YCO2, aes(x = Year, y = AnnualCO2_VMR10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21y_VMR10
  ggplotly(PA_CO21y_VMR10)

  #Plot 3: Interactive Yearly CO2 emissions considering a delay
  GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
  ModelDFSL_VMR10P_YCO2D <- merge(ModelDFSL_VMR10P_YCO2, GWP100delay, by = "Year")
  ModelDFSL_VMR10P_YCO2D$AnnualCO2D_VMR10 <- ModelDFSL_VMR10P_YCO2D$AnnualCO2_VMR10 * ModelDFSL_VMR10P_YCO2D$GWP100

  PA_CO21yD_VMR10 <- ggplot(ModelDFSL_VMR10P_YCO2D, aes(x = Year, y = AnnualCO2D_VMR10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

  PA_CO21yD_VMR10
  ggplotly(PA_CO21yD_VMR10)

  #Plot 4: Interactive Yearly C emissions #
  MY <- 12
  ModelDFSL_VMR10P_YC <- ModelDFSL_VMR10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VMR10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR10P_YC) <- c("Year", "Months", "AnnualCTail_VMR10")

  PA_C1y_VMR10 <- ggplot(ModelDFSL_VMR10P_YC, aes(x = Year, y = AnnualCTail_VMR10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

  PA_C1y_VMR10
  ggplotly(PA_C1y_VMR10)

  #Plot 5: Interactive Yearly C tails (remaining C in Soil) #
  MY <- 12
  ModelDFSL_VMR10P_YCT <- ModelDFSL_VMR10P %>%
    group_by(grp = as.integer(gl(n(), MY, n()))) %>%
    summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VMR10)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
  colnames(ModelDFSL_VMR10P_YCT) <- c("Year", "Months", "AnnualCTail_VMR10")

  PA_CT1y_VMR10 <- ggplot(ModelDFSL_VMR10P_YCT, aes(x = Year, y = AnnualCTail_VMR10)) +
    geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

  PA_CT1y_VMR10
  ggplotly(PA_CT1y_VMR10)

  #Save Dataframes for Plots 3.1, 3.2, 4 and 5
  write_xlsx(ModelDFSL_VMR10P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VMR10P.xlsx") #Yearly CO2 emissions (no delay)
  write_xlsx(ModelDFSL_VMR10P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VMR10P.xlsx") #Yearly CO2 emissions (delay)
  write_xlsx(ModelDFSL_VMR10P_YC,"CEmissions_P\\ModelDFSL_R_C_VMR10P.xlsx") #Yearly C emissions
  write_xlsx(ModelDFSL_VMR10P_YCT,"CTails_P\\ModelDFSL_R_C_VMR10P.xlsx") #Yearly C emissions















}


