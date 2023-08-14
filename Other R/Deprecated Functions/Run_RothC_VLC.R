#' Run RothC_VLC
#'
#' The Merge_VXC function merge all VXC models created by the functions Run_RothC_[].
#'
#' @return A dataframe with all VXC models merged together
#' @import SoilR ggplot2 stringr
#' @importFrom plotly ggplotly
#' @export

#Function to Run and Create the multiple RothC Combinations
Run_RothC_VLC <- function(SL_years = 100,
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

#### 7) Model Combinations - Litter; Conventional Tillage, 0-100% (+10%) Clay ####
#### 7.1 - VLC0) Litter; 0%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC0=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC0=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
            S.Thick = soil.thick, pClay = clay0,
            pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC0=data.frame(years,rep(fT_VLC0*fW_VLC0,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC0=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay0, #Percent clay in mineral soil
  xi=xi.frame_VLC0) #Loads the model

Ct_VLC0=getC(Model_VLC0) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC0, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC0 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC0 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC0 <- as.data.frame(VEC_Lit_VLC0) #Converting the Litter vector to a data frame
VEC_LitDF_VLC0$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC0 <- as.data.frame(VEC_Man_VLC0) #Converting the Manure vector to a data frame
VEC_ManDF_VLC0$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC0, class) #Check that class is numeric
sapply(VEC_ManDF_VLC0, class) #Check that class is numeric
LitterCinputs_VLC0=VEC_LitDF_VLC0   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC0=VEC_ManDF_VLC0 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC0 <- merge(LitterCinputs_VLC0, ManureCinputs_VLC0, by = "MNumber")
MCinputs_VLC0$MInput_VLC0 <- MCinputs_VLC0$VEC_Lit_VLC0 + MCinputs_VLC0$VEC_Man_VLC0

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC0)[which(names(MCinputs_VLC0) == "VEC_Lit_VLC0")] <- "LitterC_VLC0"
colnames(MCinputs_VLC0)[which(names(MCinputs_VLC0) == "VEC_Man_VLC0")] <- "ManureC_VLC0"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC0 <- as.data.frame(Ct_VLC0)
colnames(ModelDF_VLC0) <- c('DPM_VLC0','RPM_VLC0','BIO_VLC0', 'HUM_VLC0', 'IOM_VLC0')
ModelDFS_VLC0 <- ModelDF_VLC0[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC0$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC0$AllPools_VLC0 <- ModelDFS_VLC0$DPM_VLC0 + ModelDFS_VLC0$RPM_VLC0 + ModelDFS_VLC0$BIO_VLC0 + ModelDFS_VLC0$HUM_VLC0 + ModelDFS_VLC0$IOM_VLC0

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC0$AllPools_noIOM_VLC0 <- ModelDFS_VLC0$DPM_VLC0 + ModelDFS_VLC0$RPM_VLC0 + ModelDFS_VLC0$BIO_VLC0 + ModelDFS_VLC0$HUM_VLC0

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC0 <- merge(ModelDFS_VLC0, MCinputs_VLC0, by = "MNumber")

ModelDFSL_VLC0$MInput_VLC0 <- ModelDFSL_VLC0$LitterC_VLC0 + ModelDFSL_VLC0$ManureC_VLC0
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC0$CTails_VLC0 <- ModelDFSL_VLC0$AllPools_noIOM_VLC0 + ModelDFSL_VLC0$MInput_VLC0

#Create Monthly Accumulated input of C
ModelDFSL_VLC0$AccumInput_VLC0 = ModelDFSL_VLC0$AccumInput_VLC0=cumsum(ModelDFSL_VLC0$MInput_VLC0)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC0$MGrowth_DPM_VLC0 <- ave(ModelDFSL_VLC0$DPM_VLC0, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC0$MGrowth_RPM_VLC0 <- ave(ModelDFSL_VLC0$RPM_VLC0, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC0$MGrowth_BIO_VLC0 <- ave(ModelDFSL_VLC0$BIO_VLC0, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC0$MGrowth_HUM_VLC0 <- ave(ModelDFSL_VLC0$HUM_VLC0, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC0$MGrowth_IOM_VLC0 <- ave(ModelDFSL_VLC0$IOM_VLC0, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC0$M_CCO2_VLC0 <- ModelDFSL_VLC0$MInput_VLC0 - ModelDFSL_VLC0$MGrowth_DPM_VLC0 - ModelDFSL_VLC0$MGrowth_RPM_VLC0 - ModelDFSL_VLC0$MGrowth_BIO_VLC0 - ModelDFSL_VLC0$MGrowth_HUM_VLC0
ModelDFSL_VLC0$Accum_CCO2_VLC0 <- ModelDFSL_VLC0$AccumInput_VLC0 - ModelDFSL_VLC0$AllPools_noIOM_VLC0

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC0$M_CCO2_VLC0[1] <- 0
ModelDFSL_VLC0$Accum_CCO2_VLC0[1] <- 0

#Balance validation
ModelDFSL_VLC0$Balance_VLC0 <- ModelDFSL_VLC0$AccumInput_VLC0 - ModelDFSL_VLC0$Accum_CCO2_VLC0 - (ModelDFSL_VLC0$DPM_VLC0 + ModelDFSL_VLC0$RPM_VLC0 + ModelDFSL_VLC0$BIO_VLC0 + ModelDFSL_VLC0$HUM_VLC0)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC0$M_CO2_VLC0 <- ModelDFSL_VLC0$M_CCO2_VLC0 * 44/12
ModelDFSL_VLC0$Accum_CO2_VLC0 <- ModelDFSL_VLC0$Accum_CCO2_VLC0 * 44/12

#This model will be called VLC0C because implies a continuous input of C
ModelDFSL_VLC0C <- ModelDFSL_VLC0

#Export the dataframe
write_xlsx(ModelDFSL_VLC0C,"VXC_Models\\ModelDFSL_R_VLC0C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC0 <- ModelDFSL_VLC0 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC0.1 <- rbind(c(0:0), ModelDFSLt0_VLC0)
ModelDFSLt1_VLC0.2 <- rbind(c(0:0), ModelDFSLt1_VLC0.1)
ModelDFSLt1_VLC0.3 <- rbind(c(0:0), ModelDFSLt1_VLC0.2)
ModelDFSLt1_VLC0.4 <- rbind(c(0:0), ModelDFSLt1_VLC0.3)
ModelDFSLt1_VLC0.5 <- rbind(c(0:0), ModelDFSLt1_VLC0.4)
ModelDFSLt1_VLC0.6 <- rbind(c(0:0), ModelDFSLt1_VLC0.5)
ModelDFSLt1_VLC0.7 <- rbind(c(0:0), ModelDFSLt1_VLC0.6)
ModelDFSLt1_VLC0.8 <- rbind(c(0:0), ModelDFSLt1_VLC0.7)
ModelDFSLt1_VLC0.9 <- rbind(c(0:0), ModelDFSLt1_VLC0.8)
ModelDFSLt1_VLC0.10 <- rbind(c(0:0), ModelDFSLt1_VLC0.9)
ModelDFSLt1_VLC0.11 <- rbind(c(0:0), ModelDFSLt1_VLC0.10)
ModelDFSLt1_VLC0.12 <- rbind(c(0:0), ModelDFSLt1_VLC0.11)
ModelDFSLt1_VLC0.13 <- ModelDFSLt1_VLC0.12[-nrow(ModelDFSLt1_VLC0.12),]
ModelDFSLt1_VLC0.14 <- ModelDFSLt1_VLC0.13[-nrow(ModelDFSLt1_VLC0.13),]
ModelDFSLt1_VLC0.15 <- ModelDFSLt1_VLC0.14[-nrow(ModelDFSLt1_VLC0.14),]
ModelDFSLt1_VLC0.16 <- ModelDFSLt1_VLC0.15[-nrow(ModelDFSLt1_VLC0.15),]
ModelDFSLt1_VLC0.17 <- ModelDFSLt1_VLC0.16[-nrow(ModelDFSLt1_VLC0.16),]
ModelDFSLt1_VLC0.18 <- ModelDFSLt1_VLC0.17[-nrow(ModelDFSLt1_VLC0.17),]
ModelDFSLt1_VLC0.19 <- ModelDFSLt1_VLC0.18[-nrow(ModelDFSLt1_VLC0.18),]
ModelDFSLt1_VLC0.20 <- ModelDFSLt1_VLC0.19[-nrow(ModelDFSLt1_VLC0.19),]
ModelDFSLt1_VLC0.21 <- ModelDFSLt1_VLC0.20[-nrow(ModelDFSLt1_VLC0.20),]
ModelDFSLt1_VLC0.22 <- ModelDFSLt1_VLC0.21[-nrow(ModelDFSLt1_VLC0.21),]
ModelDFSLt1_VLC0.23 <- ModelDFSLt1_VLC0.22[-nrow(ModelDFSLt1_VLC0.22),]
ModelDFSLt1_VLC0.24 <- ModelDFSLt1_VLC0.23[-nrow(ModelDFSLt1_VLC0.23),]

ModelDFSLt1_VLC0 <- ModelDFSLt1_VLC0.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC0 <- ModelDFSLt0_VLC0 - ModelDFSLt1_VLC0

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC0$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC0P because implies a one-off input of C
ModelDFSL_VLC0P <- ModelDFSL1y_VLC0

#Export the dataframe
write_xlsx(ModelDFSL_VLC0P,"VXP_Models\\ModelDFSL_R_VLC0P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC0 <- ggplot(ModelDFSL_VLC0P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC0, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC0, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC0, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC0, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC0, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC0, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC0, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC0
ggplotly(P_CFluxI1y_VLC0)

#Plot 3.1: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC0P_YCO2 <- ModelDFSL_VLC0P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC0P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC0")

PA_CO21y_VLC0 <- ggplot(ModelDFSL_VLC0P_YCO2, aes(x = Year, y = AnnualCO2_VLC0)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC0
ggplotly(PA_CO21y_VLC0)

#Plot 3.2: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC0P_YCO2D <- merge(ModelDFSL_VLC0P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC0P_YCO2D$AnnualCO2D_VLC0 <- ModelDFSL_VLC0P_YCO2D$AnnualCO2_VLC0 * ModelDFSL_VLC0P_YCO2D$GWP100

PA_CO21yD_VLC0 <- ggplot(ModelDFSL_VLC0P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC0)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC0
ggplotly(PA_CO21yD_VLC0)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC0P_YC <- ModelDFSL_VLC0P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC0)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC0P_YC) <- c("Year", "Months", "AnnualCTail_VLC0")

PA_C1y_VLC0 <- ggplot(ModelDFSL_VLC0P_YC, aes(x = Year, y = AnnualCTail_VLC0)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC0
ggplotly(PA_C1y_VLC0)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC0P_YCT <- ModelDFSL_VLC0P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC0)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC0P_YCT) <- c("Year", "Months", "AnnualCTail_VLC0")

PA_CT1y_VLC0 <- ggplot(ModelDFSL_VLC0P_YCT, aes(x = Year, y = AnnualCTail_VLC0)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC0
ggplotly(PA_CT1y_VLC0)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC0P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC0P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC0P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC0P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC0P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC0P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC0P_YCT,"CTails_P\\ModelDFSL_R_C_VLC0P.xlsx") #Yearly C emissions


#### 7.2 - VLC1) Litter; 10%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC1=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC1=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
               S.Thick = soil.thick, pClay = clay100*0.1,
               pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC1=data.frame(years,rep(fT_VLC1*fW_VLC1,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC1=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay100*0.1, #Percent clay in mineral soil
  xi=xi.frame_VLC1) #Loads the model

Ct_VLC1=getC(Model_VLC1) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC1, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC1 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC1 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC1 <- as.data.frame(VEC_Lit_VLC1) #Converting the Litter vector to a data frame
VEC_LitDF_VLC1$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC1 <- as.data.frame(VEC_Man_VLC1) #Converting the Manure vector to a data frame
VEC_ManDF_VLC1$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC1, class) #Check that class is numeric
sapply(VEC_ManDF_VLC1, class) #Check that class is numeric
LitterCinputs_VLC1=VEC_LitDF_VLC1   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC1=VEC_ManDF_VLC1 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC1 <- merge(LitterCinputs_VLC1, ManureCinputs_VLC1, by = "MNumber")
MCinputs_VLC1$MInput_VLC1 <- MCinputs_VLC1$VEC_Lit_VLC1 + MCinputs_VLC1$VEC_Man_VLC1

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC1)[which(names(MCinputs_VLC1) == "VEC_Lit_VLC1")] <- "LitterC_VLC1"
colnames(MCinputs_VLC1)[which(names(MCinputs_VLC1) == "VEC_Man_VLC1")] <- "ManureC_VLC1"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC1 <- as.data.frame(Ct_VLC1)
colnames(ModelDF_VLC1) <- c('DPM_VLC1','RPM_VLC1','BIO_VLC1', 'HUM_VLC1', 'IOM_VLC1')
ModelDFS_VLC1 <- ModelDF_VLC1[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC1$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC1$AllPools_VLC1 <- ModelDFS_VLC1$DPM_VLC1 + ModelDFS_VLC1$RPM_VLC1 + ModelDFS_VLC1$BIO_VLC1 + ModelDFS_VLC1$HUM_VLC1 + ModelDFS_VLC1$IOM_VLC1

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC1$AllPools_noIOM_VLC1 <- ModelDFS_VLC1$DPM_VLC1 + ModelDFS_VLC1$RPM_VLC1 + ModelDFS_VLC1$BIO_VLC1 + ModelDFS_VLC1$HUM_VLC1

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC1 <- merge(ModelDFS_VLC1, MCinputs_VLC1, by = "MNumber")

ModelDFSL_VLC1$MInput_VLC1 <- ModelDFSL_VLC1$LitterC_VLC1 + ModelDFSL_VLC1$ManureC_VLC1
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC1$CTails_VLC1 <- ModelDFSL_VLC1$AllPools_noIOM_VLC1 + ModelDFSL_VLC1$MInput_VLC1

#Create Monthly Accumulated input of C
ModelDFSL_VLC1$AccumInput_VLC1 = ModelDFSL_VLC1$AccumInput_VLC1=cumsum(ModelDFSL_VLC1$MInput_VLC1)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC1$MGrowth_DPM_VLC1 <- ave(ModelDFSL_VLC1$DPM_VLC1, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC1$MGrowth_RPM_VLC1 <- ave(ModelDFSL_VLC1$RPM_VLC1, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC1$MGrowth_BIO_VLC1 <- ave(ModelDFSL_VLC1$BIO_VLC1, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC1$MGrowth_HUM_VLC1 <- ave(ModelDFSL_VLC1$HUM_VLC1, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC1$MGrowth_IOM_VLC1 <- ave(ModelDFSL_VLC1$IOM_VLC1, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC1$M_CCO2_VLC1 <- ModelDFSL_VLC1$MInput_VLC1 - ModelDFSL_VLC1$MGrowth_DPM_VLC1 - ModelDFSL_VLC1$MGrowth_RPM_VLC1 - ModelDFSL_VLC1$MGrowth_BIO_VLC1 - ModelDFSL_VLC1$MGrowth_HUM_VLC1
ModelDFSL_VLC1$Accum_CCO2_VLC1 <- ModelDFSL_VLC1$AccumInput_VLC1 - ModelDFSL_VLC1$AllPools_noIOM_VLC1

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC1$M_CCO2_VLC1[1] <- 0
ModelDFSL_VLC1$Accum_CCO2_VLC1[1] <- 0

#Balance validation
ModelDFSL_VLC1$Balance_VLC1 <- ModelDFSL_VLC1$AccumInput_VLC1 - ModelDFSL_VLC1$Accum_CCO2_VLC1 - (ModelDFSL_VLC1$DPM_VLC1 + ModelDFSL_VLC1$RPM_VLC1 + ModelDFSL_VLC1$BIO_VLC1 + ModelDFSL_VLC1$HUM_VLC1)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC1$M_CO2_VLC1 <- ModelDFSL_VLC1$M_CCO2_VLC1 * 44/12
ModelDFSL_VLC1$Accum_CO2_VLC1 <- ModelDFSL_VLC1$Accum_CCO2_VLC1 * 44/12

#This model will be called VLC1C because implies a continuous input of C
ModelDFSL_VLC1C <- ModelDFSL_VLC1

#Export the dataframe
write_xlsx(ModelDFSL_VLC1C,"VXC_Models\\ModelDFSL_R_VLC1C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC1 <- ModelDFSL_VLC1 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC1.1 <- rbind(c(0:0), ModelDFSLt0_VLC1)
ModelDFSLt1_VLC1.2 <- rbind(c(0:0), ModelDFSLt1_VLC1.1)
ModelDFSLt1_VLC1.3 <- rbind(c(0:0), ModelDFSLt1_VLC1.2)
ModelDFSLt1_VLC1.4 <- rbind(c(0:0), ModelDFSLt1_VLC1.3)
ModelDFSLt1_VLC1.5 <- rbind(c(0:0), ModelDFSLt1_VLC1.4)
ModelDFSLt1_VLC1.6 <- rbind(c(0:0), ModelDFSLt1_VLC1.5)
ModelDFSLt1_VLC1.7 <- rbind(c(0:0), ModelDFSLt1_VLC1.6)
ModelDFSLt1_VLC1.8 <- rbind(c(0:0), ModelDFSLt1_VLC1.7)
ModelDFSLt1_VLC1.9 <- rbind(c(0:0), ModelDFSLt1_VLC1.8)
ModelDFSLt1_VLC1.10 <- rbind(c(0:0), ModelDFSLt1_VLC1.9)
ModelDFSLt1_VLC1.11 <- rbind(c(0:0), ModelDFSLt1_VLC1.10)
ModelDFSLt1_VLC1.12 <- rbind(c(0:0), ModelDFSLt1_VLC1.11)
ModelDFSLt1_VLC1.13 <- ModelDFSLt1_VLC1.12[-nrow(ModelDFSLt1_VLC1.12),]
ModelDFSLt1_VLC1.14 <- ModelDFSLt1_VLC1.13[-nrow(ModelDFSLt1_VLC1.13),]
ModelDFSLt1_VLC1.15 <- ModelDFSLt1_VLC1.14[-nrow(ModelDFSLt1_VLC1.14),]
ModelDFSLt1_VLC1.16 <- ModelDFSLt1_VLC1.15[-nrow(ModelDFSLt1_VLC1.15),]
ModelDFSLt1_VLC1.17 <- ModelDFSLt1_VLC1.16[-nrow(ModelDFSLt1_VLC1.16),]
ModelDFSLt1_VLC1.18 <- ModelDFSLt1_VLC1.17[-nrow(ModelDFSLt1_VLC1.17),]
ModelDFSLt1_VLC1.19 <- ModelDFSLt1_VLC1.18[-nrow(ModelDFSLt1_VLC1.18),]
ModelDFSLt1_VLC1.20 <- ModelDFSLt1_VLC1.19[-nrow(ModelDFSLt1_VLC1.19),]
ModelDFSLt1_VLC1.21 <- ModelDFSLt1_VLC1.20[-nrow(ModelDFSLt1_VLC1.20),]
ModelDFSLt1_VLC1.22 <- ModelDFSLt1_VLC1.21[-nrow(ModelDFSLt1_VLC1.21),]
ModelDFSLt1_VLC1.23 <- ModelDFSLt1_VLC1.22[-nrow(ModelDFSLt1_VLC1.22),]
ModelDFSLt1_VLC1.24 <- ModelDFSLt1_VLC1.23[-nrow(ModelDFSLt1_VLC1.23),]

ModelDFSLt1_VLC1 <- ModelDFSLt1_VLC1.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC1 <- ModelDFSLt0_VLC1 - ModelDFSLt1_VLC1

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC1$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC1P because implies a one-off input of C
ModelDFSL_VLC1P <- ModelDFSL1y_VLC1

#Export the dataframe
write_xlsx(ModelDFSL_VLC1P,"VXP_Models\\ModelDFSL_R_VLC1P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC1 <- ggplot(ModelDFSL_VLC1P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC1, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC1, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC1, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC1, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC1, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC1, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC1, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC1
ggplotly(P_CFluxI1y_VLC1)


#Plot 3: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC1P_YCO2 <- ModelDFSL_VLC1P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC1P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC1")

PA_CO21y_VLC1 <- ggplot(ModelDFSL_VLC1P_YCO2, aes(x = Year, y = AnnualCO2_VLC1)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC1
ggplotly(PA_CO21y_VLC1)

#Plot 3: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC1P_YCO2D <- merge(ModelDFSL_VLC1P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC1P_YCO2D$AnnualCO2D_VLC1 <- ModelDFSL_VLC1P_YCO2D$AnnualCO2_VLC1 * ModelDFSL_VLC1P_YCO2D$GWP100

PA_CO21yD_VLC1 <- ggplot(ModelDFSL_VLC1P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC1)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC1
ggplotly(PA_CO21yD_VLC1)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC1P_YC <- ModelDFSL_VLC1P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC1)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC1P_YC) <- c("Year", "Months", "AnnualCTail_VLC1")

PA_C1y_VLC1 <- ggplot(ModelDFSL_VLC1P_YC, aes(x = Year, y = AnnualCTail_VLC1)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC1
ggplotly(PA_C1y_VLC1)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC1P_YCT <- ModelDFSL_VLC1P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC1)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC1P_YCT) <- c("Year", "Months", "AnnualCTail_VLC1")

PA_CT1y_VLC1 <- ggplot(ModelDFSL_VLC1P_YCT, aes(x = Year, y = AnnualCTail_VLC1)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC1
ggplotly(PA_CT1y_VLC1)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC1P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC1P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC1P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC1P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC1P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC1P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC1P_YCT,"CTails_P\\ModelDFSL_R_C_VLC1P.xlsx") #Yearly C emissions



#### 7.3 - VLC2) Litter; 20%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC2=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC2=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
               S.Thick = soil.thick, pClay = clay100*0.2,
               pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC2=data.frame(years,rep(fT_VLC2*fW_VLC2,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC2=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay100*0.2, #Percent clay in mineral soil
  xi=xi.frame_VLC2) #Loads the model

Ct_VLC2=getC(Model_VLC2) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC2, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC2 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC2 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC2 <- as.data.frame(VEC_Lit_VLC2) #Converting the Litter vector to a data frame
VEC_LitDF_VLC2$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC2 <- as.data.frame(VEC_Man_VLC2) #Converting the Manure vector to a data frame
VEC_ManDF_VLC2$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC2, class) #Check that class is numeric
sapply(VEC_ManDF_VLC2, class) #Check that class is numeric
LitterCinputs_VLC2=VEC_LitDF_VLC2   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC2=VEC_ManDF_VLC2 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC2 <- merge(LitterCinputs_VLC2, ManureCinputs_VLC2, by = "MNumber")
MCinputs_VLC2$MInput_VLC2 <- MCinputs_VLC2$VEC_Lit_VLC2 + MCinputs_VLC2$VEC_Man_VLC2

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC2)[which(names(MCinputs_VLC2) == "VEC_Lit_VLC2")] <- "LitterC_VLC2"
colnames(MCinputs_VLC2)[which(names(MCinputs_VLC2) == "VEC_Man_VLC2")] <- "ManureC_VLC2"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC2 <- as.data.frame(Ct_VLC2)
colnames(ModelDF_VLC2) <- c('DPM_VLC2','RPM_VLC2','BIO_VLC2', 'HUM_VLC2', 'IOM_VLC2')
ModelDFS_VLC2 <- ModelDF_VLC2[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC2$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC2$AllPools_VLC2 <- ModelDFS_VLC2$DPM_VLC2 + ModelDFS_VLC2$RPM_VLC2 + ModelDFS_VLC2$BIO_VLC2 + ModelDFS_VLC2$HUM_VLC2 + ModelDFS_VLC2$IOM_VLC2

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC2$AllPools_noIOM_VLC2 <- ModelDFS_VLC2$DPM_VLC2 + ModelDFS_VLC2$RPM_VLC2 + ModelDFS_VLC2$BIO_VLC2 + ModelDFS_VLC2$HUM_VLC2

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC2 <- merge(ModelDFS_VLC2, MCinputs_VLC2, by = "MNumber")

ModelDFSL_VLC2$MInput_VLC2 <- ModelDFSL_VLC2$LitterC_VLC2 + ModelDFSL_VLC2$ManureC_VLC2
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC2$CTails_VLC2 <- ModelDFSL_VLC2$AllPools_noIOM_VLC2 + ModelDFSL_VLC2$MInput_VLC2

#Create Monthly Accumulated input of C
ModelDFSL_VLC2$AccumInput_VLC2 = ModelDFSL_VLC2$AccumInput_VLC2=cumsum(ModelDFSL_VLC2$MInput_VLC2)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC2$MGrowth_DPM_VLC2 <- ave(ModelDFSL_VLC2$DPM_VLC2, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC2$MGrowth_RPM_VLC2 <- ave(ModelDFSL_VLC2$RPM_VLC2, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC2$MGrowth_BIO_VLC2 <- ave(ModelDFSL_VLC2$BIO_VLC2, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC2$MGrowth_HUM_VLC2 <- ave(ModelDFSL_VLC2$HUM_VLC2, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC2$MGrowth_IOM_VLC2 <- ave(ModelDFSL_VLC2$IOM_VLC2, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC2$M_CCO2_VLC2 <- ModelDFSL_VLC2$MInput_VLC2 - ModelDFSL_VLC2$MGrowth_DPM_VLC2 - ModelDFSL_VLC2$MGrowth_RPM_VLC2 - ModelDFSL_VLC2$MGrowth_BIO_VLC2 - ModelDFSL_VLC2$MGrowth_HUM_VLC2
ModelDFSL_VLC2$Accum_CCO2_VLC2 <- ModelDFSL_VLC2$AccumInput_VLC2 - ModelDFSL_VLC2$AllPools_noIOM_VLC2

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC2$M_CCO2_VLC2[1] <- 0
ModelDFSL_VLC2$Accum_CCO2_VLC2[1] <- 0

#Balance validation
ModelDFSL_VLC2$Balance_VLC2 <- ModelDFSL_VLC2$AccumInput_VLC2 - ModelDFSL_VLC2$Accum_CCO2_VLC2 - (ModelDFSL_VLC2$DPM_VLC2 + ModelDFSL_VLC2$RPM_VLC2 + ModelDFSL_VLC2$BIO_VLC2 + ModelDFSL_VLC2$HUM_VLC2)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC2$M_CO2_VLC2 <- ModelDFSL_VLC2$M_CCO2_VLC2 * 44/12
ModelDFSL_VLC2$Accum_CO2_VLC2 <- ModelDFSL_VLC2$Accum_CCO2_VLC2 * 44/12

#This model will be called VLC2C because implies a continuous input of C
ModelDFSL_VLC2C <- ModelDFSL_VLC2

#Export the dataframe
write_xlsx(ModelDFSL_VLC2C,"VXC_Models\\ModelDFSL_R_VLC2C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC2 <- ModelDFSL_VLC2 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC2.1 <- rbind(c(0:0), ModelDFSLt0_VLC2)
ModelDFSLt1_VLC2.2 <- rbind(c(0:0), ModelDFSLt1_VLC2.1)
ModelDFSLt1_VLC2.3 <- rbind(c(0:0), ModelDFSLt1_VLC2.2)
ModelDFSLt1_VLC2.4 <- rbind(c(0:0), ModelDFSLt1_VLC2.3)
ModelDFSLt1_VLC2.5 <- rbind(c(0:0), ModelDFSLt1_VLC2.4)
ModelDFSLt1_VLC2.6 <- rbind(c(0:0), ModelDFSLt1_VLC2.5)
ModelDFSLt1_VLC2.7 <- rbind(c(0:0), ModelDFSLt1_VLC2.6)
ModelDFSLt1_VLC2.8 <- rbind(c(0:0), ModelDFSLt1_VLC2.7)
ModelDFSLt1_VLC2.9 <- rbind(c(0:0), ModelDFSLt1_VLC2.8)
ModelDFSLt1_VLC2.10 <- rbind(c(0:0), ModelDFSLt1_VLC2.9)
ModelDFSLt1_VLC2.11 <- rbind(c(0:0), ModelDFSLt1_VLC2.10)
ModelDFSLt1_VLC2.12 <- rbind(c(0:0), ModelDFSLt1_VLC2.11)
ModelDFSLt1_VLC2.13 <- ModelDFSLt1_VLC2.12[-nrow(ModelDFSLt1_VLC2.12),]
ModelDFSLt1_VLC2.14 <- ModelDFSLt1_VLC2.13[-nrow(ModelDFSLt1_VLC2.13),]
ModelDFSLt1_VLC2.15 <- ModelDFSLt1_VLC2.14[-nrow(ModelDFSLt1_VLC2.14),]
ModelDFSLt1_VLC2.16 <- ModelDFSLt1_VLC2.15[-nrow(ModelDFSLt1_VLC2.15),]
ModelDFSLt1_VLC2.17 <- ModelDFSLt1_VLC2.16[-nrow(ModelDFSLt1_VLC2.16),]
ModelDFSLt1_VLC2.18 <- ModelDFSLt1_VLC2.17[-nrow(ModelDFSLt1_VLC2.17),]
ModelDFSLt1_VLC2.19 <- ModelDFSLt1_VLC2.18[-nrow(ModelDFSLt1_VLC2.18),]
ModelDFSLt1_VLC2.20 <- ModelDFSLt1_VLC2.19[-nrow(ModelDFSLt1_VLC2.19),]
ModelDFSLt1_VLC2.21 <- ModelDFSLt1_VLC2.20[-nrow(ModelDFSLt1_VLC2.20),]
ModelDFSLt1_VLC2.22 <- ModelDFSLt1_VLC2.21[-nrow(ModelDFSLt1_VLC2.21),]
ModelDFSLt1_VLC2.23 <- ModelDFSLt1_VLC2.22[-nrow(ModelDFSLt1_VLC2.22),]
ModelDFSLt1_VLC2.24 <- ModelDFSLt1_VLC2.23[-nrow(ModelDFSLt1_VLC2.23),]

ModelDFSLt1_VLC2 <- ModelDFSLt1_VLC2.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC2 <- ModelDFSLt0_VLC2 - ModelDFSLt1_VLC2

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC2$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC2P because implies a one-off input of C
ModelDFSL_VLC2P <- ModelDFSL1y_VLC2

#Export the dataframe
write_xlsx(ModelDFSL_VLC2P,"VXP_Models\\ModelDFSL_R_VLC2P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC2 <- ggplot(ModelDFSL_VLC2P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC2, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC2, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC2, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC2, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC2, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC2, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC2, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC2
ggplotly(P_CFluxI1y_VLC2)


#Plot 3: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC2P_YCO2 <- ModelDFSL_VLC2P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC2P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC2")

PA_CO21y_VLC2 <- ggplot(ModelDFSL_VLC2P_YCO2, aes(x = Year, y = AnnualCO2_VLC2)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC2
ggplotly(PA_CO21y_VLC2)

#Plot 3: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC2P_YCO2D <- merge(ModelDFSL_VLC2P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC2P_YCO2D$AnnualCO2D_VLC2 <- ModelDFSL_VLC2P_YCO2D$AnnualCO2_VLC2 * ModelDFSL_VLC2P_YCO2D$GWP100

PA_CO21yD_VLC2 <- ggplot(ModelDFSL_VLC2P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC2)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC2
ggplotly(PA_CO21yD_VLC2)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC2P_YC <- ModelDFSL_VLC2P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC2)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC2P_YC) <- c("Year", "Months", "AnnualCTail_VLC2")

PA_C1y_VLC2 <- ggplot(ModelDFSL_VLC2P_YC, aes(x = Year, y = AnnualCTail_VLC2)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC2
ggplotly(PA_C1y_VLC2)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC2P_YCT <- ModelDFSL_VLC2P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC2)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC2P_YCT) <- c("Year", "Months", "AnnualCTail_VLC2")

PA_CT1y_VLC2 <- ggplot(ModelDFSL_VLC2P_YCT, aes(x = Year, y = AnnualCTail_VLC2)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC2
ggplotly(PA_CT1y_VLC2)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC2P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC2P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC2P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC2P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC2P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC2P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC2P_YCT,"CTails_P\\ModelDFSL_R_C_VLC2P.xlsx") #Yearly C emissions



#### 7.4 - VLC3) Litter; 30%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC3=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC3=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
               S.Thick = soil.thick, pClay = clay100*0.3,
               pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC3=data.frame(years,rep(fT_VLC3*fW_VLC3,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC3=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay100*0.3, #Percent clay in mineral soil
  xi=xi.frame_VLC3) #Loads the model

Ct_VLC3=getC(Model_VLC3) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC3, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC3 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC3 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC3 <- as.data.frame(VEC_Lit_VLC3) #Converting the Litter vector to a data frame
VEC_LitDF_VLC3$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC3 <- as.data.frame(VEC_Man_VLC3) #Converting the Manure vector to a data frame
VEC_ManDF_VLC3$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC3, class) #Check that class is numeric
sapply(VEC_ManDF_VLC3, class) #Check that class is numeric
LitterCinputs_VLC3=VEC_LitDF_VLC3   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC3=VEC_ManDF_VLC3 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC3 <- merge(LitterCinputs_VLC3, ManureCinputs_VLC3, by = "MNumber")
MCinputs_VLC3$MInput_VLC3 <- MCinputs_VLC3$VEC_Lit_VLC3 + MCinputs_VLC3$VEC_Man_VLC3

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC3)[which(names(MCinputs_VLC3) == "VEC_Lit_VLC3")] <- "LitterC_VLC3"
colnames(MCinputs_VLC3)[which(names(MCinputs_VLC3) == "VEC_Man_VLC3")] <- "ManureC_VLC3"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC3 <- as.data.frame(Ct_VLC3)
colnames(ModelDF_VLC3) <- c('DPM_VLC3','RPM_VLC3','BIO_VLC3', 'HUM_VLC3', 'IOM_VLC3')
ModelDFS_VLC3 <- ModelDF_VLC3[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC3$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC3$AllPools_VLC3 <- ModelDFS_VLC3$DPM_VLC3 + ModelDFS_VLC3$RPM_VLC3 + ModelDFS_VLC3$BIO_VLC3 + ModelDFS_VLC3$HUM_VLC3 + ModelDFS_VLC3$IOM_VLC3

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC3$AllPools_noIOM_VLC3 <- ModelDFS_VLC3$DPM_VLC3 + ModelDFS_VLC3$RPM_VLC3 + ModelDFS_VLC3$BIO_VLC3 + ModelDFS_VLC3$HUM_VLC3

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC3 <- merge(ModelDFS_VLC3, MCinputs_VLC3, by = "MNumber")

ModelDFSL_VLC3$MInput_VLC3 <- ModelDFSL_VLC3$LitterC_VLC3 + ModelDFSL_VLC3$ManureC_VLC3
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC3$CTails_VLC3 <- ModelDFSL_VLC3$AllPools_noIOM_VLC3 + ModelDFSL_VLC3$MInput_VLC3

#Create Monthly Accumulated input of C
ModelDFSL_VLC3$AccumInput_VLC3 = ModelDFSL_VLC3$AccumInput_VLC3=cumsum(ModelDFSL_VLC3$MInput_VLC3)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC3$MGrowth_DPM_VLC3 <- ave(ModelDFSL_VLC3$DPM_VLC3, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC3$MGrowth_RPM_VLC3 <- ave(ModelDFSL_VLC3$RPM_VLC3, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC3$MGrowth_BIO_VLC3 <- ave(ModelDFSL_VLC3$BIO_VLC3, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC3$MGrowth_HUM_VLC3 <- ave(ModelDFSL_VLC3$HUM_VLC3, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC3$MGrowth_IOM_VLC3 <- ave(ModelDFSL_VLC3$IOM_VLC3, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC3$M_CCO2_VLC3 <- ModelDFSL_VLC3$MInput_VLC3 - ModelDFSL_VLC3$MGrowth_DPM_VLC3 - ModelDFSL_VLC3$MGrowth_RPM_VLC3 - ModelDFSL_VLC3$MGrowth_BIO_VLC3 - ModelDFSL_VLC3$MGrowth_HUM_VLC3
ModelDFSL_VLC3$Accum_CCO2_VLC3 <- ModelDFSL_VLC3$AccumInput_VLC3 - ModelDFSL_VLC3$AllPools_noIOM_VLC3

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC3$M_CCO2_VLC3[1] <- 0
ModelDFSL_VLC3$Accum_CCO2_VLC3[1] <- 0

#Balance validation
ModelDFSL_VLC3$Balance_VLC3 <- ModelDFSL_VLC3$AccumInput_VLC3 - ModelDFSL_VLC3$Accum_CCO2_VLC3 - (ModelDFSL_VLC3$DPM_VLC3 + ModelDFSL_VLC3$RPM_VLC3 + ModelDFSL_VLC3$BIO_VLC3 + ModelDFSL_VLC3$HUM_VLC3)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC3$M_CO2_VLC3 <- ModelDFSL_VLC3$M_CCO2_VLC3 * 44/12
ModelDFSL_VLC3$Accum_CO2_VLC3 <- ModelDFSL_VLC3$Accum_CCO2_VLC3 * 44/12

#This model will be called VLC3C because implies a continuous input of C
ModelDFSL_VLC3C <- ModelDFSL_VLC3

#Export the dataframe
write_xlsx(ModelDFSL_VLC3C,"VXC_Models\\ModelDFSL_R_VLC3C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC3 <- ModelDFSL_VLC3 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC3.1 <- rbind(c(0:0), ModelDFSLt0_VLC3)
ModelDFSLt1_VLC3.2 <- rbind(c(0:0), ModelDFSLt1_VLC3.1)
ModelDFSLt1_VLC3.3 <- rbind(c(0:0), ModelDFSLt1_VLC3.2)
ModelDFSLt1_VLC3.4 <- rbind(c(0:0), ModelDFSLt1_VLC3.3)
ModelDFSLt1_VLC3.5 <- rbind(c(0:0), ModelDFSLt1_VLC3.4)
ModelDFSLt1_VLC3.6 <- rbind(c(0:0), ModelDFSLt1_VLC3.5)
ModelDFSLt1_VLC3.7 <- rbind(c(0:0), ModelDFSLt1_VLC3.6)
ModelDFSLt1_VLC3.8 <- rbind(c(0:0), ModelDFSLt1_VLC3.7)
ModelDFSLt1_VLC3.9 <- rbind(c(0:0), ModelDFSLt1_VLC3.8)
ModelDFSLt1_VLC3.10 <- rbind(c(0:0), ModelDFSLt1_VLC3.9)
ModelDFSLt1_VLC3.11 <- rbind(c(0:0), ModelDFSLt1_VLC3.10)
ModelDFSLt1_VLC3.12 <- rbind(c(0:0), ModelDFSLt1_VLC3.11)
ModelDFSLt1_VLC3.13 <- ModelDFSLt1_VLC3.12[-nrow(ModelDFSLt1_VLC3.12),]
ModelDFSLt1_VLC3.14 <- ModelDFSLt1_VLC3.13[-nrow(ModelDFSLt1_VLC3.13),]
ModelDFSLt1_VLC3.15 <- ModelDFSLt1_VLC3.14[-nrow(ModelDFSLt1_VLC3.14),]
ModelDFSLt1_VLC3.16 <- ModelDFSLt1_VLC3.15[-nrow(ModelDFSLt1_VLC3.15),]
ModelDFSLt1_VLC3.17 <- ModelDFSLt1_VLC3.16[-nrow(ModelDFSLt1_VLC3.16),]
ModelDFSLt1_VLC3.18 <- ModelDFSLt1_VLC3.17[-nrow(ModelDFSLt1_VLC3.17),]
ModelDFSLt1_VLC3.19 <- ModelDFSLt1_VLC3.18[-nrow(ModelDFSLt1_VLC3.18),]
ModelDFSLt1_VLC3.20 <- ModelDFSLt1_VLC3.19[-nrow(ModelDFSLt1_VLC3.19),]
ModelDFSLt1_VLC3.21 <- ModelDFSLt1_VLC3.20[-nrow(ModelDFSLt1_VLC3.20),]
ModelDFSLt1_VLC3.22 <- ModelDFSLt1_VLC3.21[-nrow(ModelDFSLt1_VLC3.21),]
ModelDFSLt1_VLC3.23 <- ModelDFSLt1_VLC3.22[-nrow(ModelDFSLt1_VLC3.22),]
ModelDFSLt1_VLC3.24 <- ModelDFSLt1_VLC3.23[-nrow(ModelDFSLt1_VLC3.23),]

ModelDFSLt1_VLC3 <- ModelDFSLt1_VLC3.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC3 <- ModelDFSLt0_VLC3 - ModelDFSLt1_VLC3

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC3$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC3P because implies a one-off input of C
ModelDFSL_VLC3P <- ModelDFSL1y_VLC3

#Export the dataframe
write_xlsx(ModelDFSL_VLC3P,"VXP_Models\\ModelDFSL_R_VLC3P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC3 <- ggplot(ModelDFSL_VLC3P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC3, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC3, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC3, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC3, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC3, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC3, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC3, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC3
ggplotly(P_CFluxI1y_VLC3)


#Plot 3: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC3P_YCO2 <- ModelDFSL_VLC3P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC3P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC3")

PA_CO21y_VLC3 <- ggplot(ModelDFSL_VLC3P_YCO2, aes(x = Year, y = AnnualCO2_VLC3)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC3
ggplotly(PA_CO21y_VLC3)

#Plot 3: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC3P_YCO2D <- merge(ModelDFSL_VLC3P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC3P_YCO2D$AnnualCO2D_VLC3 <- ModelDFSL_VLC3P_YCO2D$AnnualCO2_VLC3 * ModelDFSL_VLC3P_YCO2D$GWP100

PA_CO21yD_VLC3 <- ggplot(ModelDFSL_VLC3P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC3)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC3
ggplotly(PA_CO21yD_VLC3)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC3P_YC <- ModelDFSL_VLC3P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC3)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC3P_YC) <- c("Year", "Months", "AnnualCTail_VLC3")

PA_C1y_VLC3 <- ggplot(ModelDFSL_VLC3P_YC, aes(x = Year, y = AnnualCTail_VLC3)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC3
ggplotly(PA_C1y_VLC3)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC3P_YCT <- ModelDFSL_VLC3P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC3)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC3P_YCT) <- c("Year", "Months", "AnnualCTail_VLC3")

PA_CT1y_VLC3 <- ggplot(ModelDFSL_VLC3P_YCT, aes(x = Year, y = AnnualCTail_VLC3)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC3
ggplotly(PA_CT1y_VLC3)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC3P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC3P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC3P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC3P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC3P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC3P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC3P_YCT,"CTails_P\\ModelDFSL_R_C_VLC3P.xlsx") #Yearly C emissions



#### 7.5 - VLC4) Litter; 40%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC4=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC4=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
               S.Thick = soil.thick, pClay = clay100*0.4,
               pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC4=data.frame(years,rep(fT_VLC4*fW_VLC4,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC4=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay100*0.4, #Percent clay in mineral soil
  xi=xi.frame_VLC4) #Loads the model

Ct_VLC4=getC(Model_VLC4) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC4, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC4 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC4 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC4 <- as.data.frame(VEC_Lit_VLC4) #Converting the Litter vector to a data frame
VEC_LitDF_VLC4$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC4 <- as.data.frame(VEC_Man_VLC4) #Converting the Manure vector to a data frame
VEC_ManDF_VLC4$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC4, class) #Check that class is numeric
sapply(VEC_ManDF_VLC4, class) #Check that class is numeric
LitterCinputs_VLC4=VEC_LitDF_VLC4   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC4=VEC_ManDF_VLC4 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC4 <- merge(LitterCinputs_VLC4, ManureCinputs_VLC4, by = "MNumber")
MCinputs_VLC4$MInput_VLC4 <- MCinputs_VLC4$VEC_Lit_VLC4 + MCinputs_VLC4$VEC_Man_VLC4

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC4)[which(names(MCinputs_VLC4) == "VEC_Lit_VLC4")] <- "LitterC_VLC4"
colnames(MCinputs_VLC4)[which(names(MCinputs_VLC4) == "VEC_Man_VLC4")] <- "ManureC_VLC4"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC4 <- as.data.frame(Ct_VLC4)
colnames(ModelDF_VLC4) <- c('DPM_VLC4','RPM_VLC4','BIO_VLC4', 'HUM_VLC4', 'IOM_VLC4')
ModelDFS_VLC4 <- ModelDF_VLC4[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC4$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC4$AllPools_VLC4 <- ModelDFS_VLC4$DPM_VLC4 + ModelDFS_VLC4$RPM_VLC4 + ModelDFS_VLC4$BIO_VLC4 + ModelDFS_VLC4$HUM_VLC4 + ModelDFS_VLC4$IOM_VLC4

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC4$AllPools_noIOM_VLC4 <- ModelDFS_VLC4$DPM_VLC4 + ModelDFS_VLC4$RPM_VLC4 + ModelDFS_VLC4$BIO_VLC4 + ModelDFS_VLC4$HUM_VLC4

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC4 <- merge(ModelDFS_VLC4, MCinputs_VLC4, by = "MNumber")

ModelDFSL_VLC4$MInput_VLC4 <- ModelDFSL_VLC4$LitterC_VLC4 + ModelDFSL_VLC4$ManureC_VLC4
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC4$CTails_VLC4 <- ModelDFSL_VLC4$AllPools_noIOM_VLC4 + ModelDFSL_VLC4$MInput_VLC4

#Create Monthly Accumulated input of C
ModelDFSL_VLC4$AccumInput_VLC4 = ModelDFSL_VLC4$AccumInput_VLC4=cumsum(ModelDFSL_VLC4$MInput_VLC4)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC4$MGrowth_DPM_VLC4 <- ave(ModelDFSL_VLC4$DPM_VLC4, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC4$MGrowth_RPM_VLC4 <- ave(ModelDFSL_VLC4$RPM_VLC4, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC4$MGrowth_BIO_VLC4 <- ave(ModelDFSL_VLC4$BIO_VLC4, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC4$MGrowth_HUM_VLC4 <- ave(ModelDFSL_VLC4$HUM_VLC4, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC4$MGrowth_IOM_VLC4 <- ave(ModelDFSL_VLC4$IOM_VLC4, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC4$M_CCO2_VLC4 <- ModelDFSL_VLC4$MInput_VLC4 - ModelDFSL_VLC4$MGrowth_DPM_VLC4 - ModelDFSL_VLC4$MGrowth_RPM_VLC4 - ModelDFSL_VLC4$MGrowth_BIO_VLC4 - ModelDFSL_VLC4$MGrowth_HUM_VLC4
ModelDFSL_VLC4$Accum_CCO2_VLC4 <- ModelDFSL_VLC4$AccumInput_VLC4 - ModelDFSL_VLC4$AllPools_noIOM_VLC4

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC4$M_CCO2_VLC4[1] <- 0
ModelDFSL_VLC4$Accum_CCO2_VLC4[1] <- 0

#Balance validation
ModelDFSL_VLC4$Balance_VLC4 <- ModelDFSL_VLC4$AccumInput_VLC4 - ModelDFSL_VLC4$Accum_CCO2_VLC4 - (ModelDFSL_VLC4$DPM_VLC4 + ModelDFSL_VLC4$RPM_VLC4 + ModelDFSL_VLC4$BIO_VLC4 + ModelDFSL_VLC4$HUM_VLC4)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC4$M_CO2_VLC4 <- ModelDFSL_VLC4$M_CCO2_VLC4 * 44/12
ModelDFSL_VLC4$Accum_CO2_VLC4 <- ModelDFSL_VLC4$Accum_CCO2_VLC4 * 44/12

#This model will be called VLC4C because implies a continuous input of C
ModelDFSL_VLC4C <- ModelDFSL_VLC4

#Export the dataframe
write_xlsx(ModelDFSL_VLC4C,"VXC_Models\\ModelDFSL_R_VLC4C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC4 <- ModelDFSL_VLC4 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC4.1 <- rbind(c(0:0), ModelDFSLt0_VLC4)
ModelDFSLt1_VLC4.2 <- rbind(c(0:0), ModelDFSLt1_VLC4.1)
ModelDFSLt1_VLC4.3 <- rbind(c(0:0), ModelDFSLt1_VLC4.2)
ModelDFSLt1_VLC4.4 <- rbind(c(0:0), ModelDFSLt1_VLC4.3)
ModelDFSLt1_VLC4.5 <- rbind(c(0:0), ModelDFSLt1_VLC4.4)
ModelDFSLt1_VLC4.6 <- rbind(c(0:0), ModelDFSLt1_VLC4.5)
ModelDFSLt1_VLC4.7 <- rbind(c(0:0), ModelDFSLt1_VLC4.6)
ModelDFSLt1_VLC4.8 <- rbind(c(0:0), ModelDFSLt1_VLC4.7)
ModelDFSLt1_VLC4.9 <- rbind(c(0:0), ModelDFSLt1_VLC4.8)
ModelDFSLt1_VLC4.10 <- rbind(c(0:0), ModelDFSLt1_VLC4.9)
ModelDFSLt1_VLC4.11 <- rbind(c(0:0), ModelDFSLt1_VLC4.10)
ModelDFSLt1_VLC4.12 <- rbind(c(0:0), ModelDFSLt1_VLC4.11)
ModelDFSLt1_VLC4.13 <- ModelDFSLt1_VLC4.12[-nrow(ModelDFSLt1_VLC4.12),]
ModelDFSLt1_VLC4.14 <- ModelDFSLt1_VLC4.13[-nrow(ModelDFSLt1_VLC4.13),]
ModelDFSLt1_VLC4.15 <- ModelDFSLt1_VLC4.14[-nrow(ModelDFSLt1_VLC4.14),]
ModelDFSLt1_VLC4.16 <- ModelDFSLt1_VLC4.15[-nrow(ModelDFSLt1_VLC4.15),]
ModelDFSLt1_VLC4.17 <- ModelDFSLt1_VLC4.16[-nrow(ModelDFSLt1_VLC4.16),]
ModelDFSLt1_VLC4.18 <- ModelDFSLt1_VLC4.17[-nrow(ModelDFSLt1_VLC4.17),]
ModelDFSLt1_VLC4.19 <- ModelDFSLt1_VLC4.18[-nrow(ModelDFSLt1_VLC4.18),]
ModelDFSLt1_VLC4.20 <- ModelDFSLt1_VLC4.19[-nrow(ModelDFSLt1_VLC4.19),]
ModelDFSLt1_VLC4.21 <- ModelDFSLt1_VLC4.20[-nrow(ModelDFSLt1_VLC4.20),]
ModelDFSLt1_VLC4.22 <- ModelDFSLt1_VLC4.21[-nrow(ModelDFSLt1_VLC4.21),]
ModelDFSLt1_VLC4.23 <- ModelDFSLt1_VLC4.22[-nrow(ModelDFSLt1_VLC4.22),]
ModelDFSLt1_VLC4.24 <- ModelDFSLt1_VLC4.23[-nrow(ModelDFSLt1_VLC4.23),]

ModelDFSLt1_VLC4 <- ModelDFSLt1_VLC4.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC4 <- ModelDFSLt0_VLC4 - ModelDFSLt1_VLC4

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC4$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC4P because implies a one-off input of C
ModelDFSL_VLC4P <- ModelDFSL1y_VLC4

#Export the dataframe
write_xlsx(ModelDFSL_VLC4P,"VXP_Models\\ModelDFSL_R_VLC4P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC4 <- ggplot(ModelDFSL_VLC4P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC4, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC4, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC4, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC4, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC4, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC4, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC4, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC4
ggplotly(P_CFluxI1y_VLC4)


#Plot 3: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC4P_YCO2 <- ModelDFSL_VLC4P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC4P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC4")

PA_CO21y_VLC4 <- ggplot(ModelDFSL_VLC4P_YCO2, aes(x = Year, y = AnnualCO2_VLC4)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC4
ggplotly(PA_CO21y_VLC4)

#Plot 3: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC4P_YCO2D <- merge(ModelDFSL_VLC4P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC4P_YCO2D$AnnualCO2D_VLC4 <- ModelDFSL_VLC4P_YCO2D$AnnualCO2_VLC4 * ModelDFSL_VLC4P_YCO2D$GWP100

PA_CO21yD_VLC4 <- ggplot(ModelDFSL_VLC4P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC4)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC4
ggplotly(PA_CO21yD_VLC4)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC4P_YC <- ModelDFSL_VLC4P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC4)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC4P_YC) <- c("Year", "Months", "AnnualCTail_VLC4")

PA_C1y_VLC4 <- ggplot(ModelDFSL_VLC4P_YC, aes(x = Year, y = AnnualCTail_VLC4)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC4
ggplotly(PA_C1y_VLC4)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC4P_YCT <- ModelDFSL_VLC4P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC4)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC4P_YCT) <- c("Year", "Months", "AnnualCTail_VLC4")

PA_CT1y_VLC4 <- ggplot(ModelDFSL_VLC4P_YCT, aes(x = Year, y = AnnualCTail_VLC4)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC4
ggplotly(PA_CT1y_VLC4)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC4P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC4P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC4P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC4P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC4P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC4P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC4P_YCT,"CTails_P\\ModelDFSL_R_C_VLC4P.xlsx") #Yearly C emissions



#### 7.6 - VLC5) Litter; 50%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC5=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC5=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
               S.Thick = soil.thick, pClay = clay100*0.5,
               pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC5=data.frame(years,rep(fT_VLC5*fW_VLC5,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC5=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay100*0.5, #Percent clay in mineral soil
  xi=xi.frame_VLC5) #Loads the model

Ct_VLC5=getC(Model_VLC5) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC5, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC5 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC5 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC5 <- as.data.frame(VEC_Lit_VLC5) #Converting the Litter vector to a data frame
VEC_LitDF_VLC5$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC5 <- as.data.frame(VEC_Man_VLC5) #Converting the Manure vector to a data frame
VEC_ManDF_VLC5$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC5, class) #Check that class is numeric
sapply(VEC_ManDF_VLC5, class) #Check that class is numeric
LitterCinputs_VLC5=VEC_LitDF_VLC5   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC5=VEC_ManDF_VLC5 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC5 <- merge(LitterCinputs_VLC5, ManureCinputs_VLC5, by = "MNumber")
MCinputs_VLC5$MInput_VLC5 <- MCinputs_VLC5$VEC_Lit_VLC5 + MCinputs_VLC5$VEC_Man_VLC5

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC5)[which(names(MCinputs_VLC5) == "VEC_Lit_VLC5")] <- "LitterC_VLC5"
colnames(MCinputs_VLC5)[which(names(MCinputs_VLC5) == "VEC_Man_VLC5")] <- "ManureC_VLC5"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC5 <- as.data.frame(Ct_VLC5)
colnames(ModelDF_VLC5) <- c('DPM_VLC5','RPM_VLC5','BIO_VLC5', 'HUM_VLC5', 'IOM_VLC5')
ModelDFS_VLC5 <- ModelDF_VLC5[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC5$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC5$AllPools_VLC5 <- ModelDFS_VLC5$DPM_VLC5 + ModelDFS_VLC5$RPM_VLC5 + ModelDFS_VLC5$BIO_VLC5 + ModelDFS_VLC5$HUM_VLC5 + ModelDFS_VLC5$IOM_VLC5

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC5$AllPools_noIOM_VLC5 <- ModelDFS_VLC5$DPM_VLC5 + ModelDFS_VLC5$RPM_VLC5 + ModelDFS_VLC5$BIO_VLC5 + ModelDFS_VLC5$HUM_VLC5

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC5 <- merge(ModelDFS_VLC5, MCinputs_VLC5, by = "MNumber")

ModelDFSL_VLC5$MInput_VLC5 <- ModelDFSL_VLC5$LitterC_VLC5 + ModelDFSL_VLC5$ManureC_VLC5
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC5$CTails_VLC5 <- ModelDFSL_VLC5$AllPools_noIOM_VLC5 + ModelDFSL_VLC5$MInput_VLC5

#Create Monthly Accumulated input of C
ModelDFSL_VLC5$AccumInput_VLC5 = ModelDFSL_VLC5$AccumInput_VLC5=cumsum(ModelDFSL_VLC5$MInput_VLC5)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC5$MGrowth_DPM_VLC5 <- ave(ModelDFSL_VLC5$DPM_VLC5, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC5$MGrowth_RPM_VLC5 <- ave(ModelDFSL_VLC5$RPM_VLC5, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC5$MGrowth_BIO_VLC5 <- ave(ModelDFSL_VLC5$BIO_VLC5, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC5$MGrowth_HUM_VLC5 <- ave(ModelDFSL_VLC5$HUM_VLC5, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC5$MGrowth_IOM_VLC5 <- ave(ModelDFSL_VLC5$IOM_VLC5, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC5$M_CCO2_VLC5 <- ModelDFSL_VLC5$MInput_VLC5 - ModelDFSL_VLC5$MGrowth_DPM_VLC5 - ModelDFSL_VLC5$MGrowth_RPM_VLC5 - ModelDFSL_VLC5$MGrowth_BIO_VLC5 - ModelDFSL_VLC5$MGrowth_HUM_VLC5
ModelDFSL_VLC5$Accum_CCO2_VLC5 <- ModelDFSL_VLC5$AccumInput_VLC5 - ModelDFSL_VLC5$AllPools_noIOM_VLC5

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC5$M_CCO2_VLC5[1] <- 0
ModelDFSL_VLC5$Accum_CCO2_VLC5[1] <- 0

#Balance validation
ModelDFSL_VLC5$Balance_VLC5 <- ModelDFSL_VLC5$AccumInput_VLC5 - ModelDFSL_VLC5$Accum_CCO2_VLC5 - (ModelDFSL_VLC5$DPM_VLC5 + ModelDFSL_VLC5$RPM_VLC5 + ModelDFSL_VLC5$BIO_VLC5 + ModelDFSL_VLC5$HUM_VLC5)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC5$M_CO2_VLC5 <- ModelDFSL_VLC5$M_CCO2_VLC5 * 44/12
ModelDFSL_VLC5$Accum_CO2_VLC5 <- ModelDFSL_VLC5$Accum_CCO2_VLC5 * 44/12

#This model will be called VLC5C because implies a continuous input of C
ModelDFSL_VLC5C <- ModelDFSL_VLC5

#Export the dataframe
write_xlsx(ModelDFSL_VLC5C,"VXC_Models\\ModelDFSL_R_VLC5C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC5 <- ModelDFSL_VLC5 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC5.1 <- rbind(c(0:0), ModelDFSLt0_VLC5)
ModelDFSLt1_VLC5.2 <- rbind(c(0:0), ModelDFSLt1_VLC5.1)
ModelDFSLt1_VLC5.3 <- rbind(c(0:0), ModelDFSLt1_VLC5.2)
ModelDFSLt1_VLC5.4 <- rbind(c(0:0), ModelDFSLt1_VLC5.3)
ModelDFSLt1_VLC5.5 <- rbind(c(0:0), ModelDFSLt1_VLC5.4)
ModelDFSLt1_VLC5.6 <- rbind(c(0:0), ModelDFSLt1_VLC5.5)
ModelDFSLt1_VLC5.7 <- rbind(c(0:0), ModelDFSLt1_VLC5.6)
ModelDFSLt1_VLC5.8 <- rbind(c(0:0), ModelDFSLt1_VLC5.7)
ModelDFSLt1_VLC5.9 <- rbind(c(0:0), ModelDFSLt1_VLC5.8)
ModelDFSLt1_VLC5.10 <- rbind(c(0:0), ModelDFSLt1_VLC5.9)
ModelDFSLt1_VLC5.11 <- rbind(c(0:0), ModelDFSLt1_VLC5.10)
ModelDFSLt1_VLC5.12 <- rbind(c(0:0), ModelDFSLt1_VLC5.11)
ModelDFSLt1_VLC5.13 <- ModelDFSLt1_VLC5.12[-nrow(ModelDFSLt1_VLC5.12),]
ModelDFSLt1_VLC5.14 <- ModelDFSLt1_VLC5.13[-nrow(ModelDFSLt1_VLC5.13),]
ModelDFSLt1_VLC5.15 <- ModelDFSLt1_VLC5.14[-nrow(ModelDFSLt1_VLC5.14),]
ModelDFSLt1_VLC5.16 <- ModelDFSLt1_VLC5.15[-nrow(ModelDFSLt1_VLC5.15),]
ModelDFSLt1_VLC5.17 <- ModelDFSLt1_VLC5.16[-nrow(ModelDFSLt1_VLC5.16),]
ModelDFSLt1_VLC5.18 <- ModelDFSLt1_VLC5.17[-nrow(ModelDFSLt1_VLC5.17),]
ModelDFSLt1_VLC5.19 <- ModelDFSLt1_VLC5.18[-nrow(ModelDFSLt1_VLC5.18),]
ModelDFSLt1_VLC5.20 <- ModelDFSLt1_VLC5.19[-nrow(ModelDFSLt1_VLC5.19),]
ModelDFSLt1_VLC5.21 <- ModelDFSLt1_VLC5.20[-nrow(ModelDFSLt1_VLC5.20),]
ModelDFSLt1_VLC5.22 <- ModelDFSLt1_VLC5.21[-nrow(ModelDFSLt1_VLC5.21),]
ModelDFSLt1_VLC5.23 <- ModelDFSLt1_VLC5.22[-nrow(ModelDFSLt1_VLC5.22),]
ModelDFSLt1_VLC5.24 <- ModelDFSLt1_VLC5.23[-nrow(ModelDFSLt1_VLC5.23),]

ModelDFSLt1_VLC5 <- ModelDFSLt1_VLC5.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC5 <- ModelDFSLt0_VLC5 - ModelDFSLt1_VLC5

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC5$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC5P because implies a one-off input of C
ModelDFSL_VLC5P <- ModelDFSL1y_VLC5

#Export the dataframe
write_xlsx(ModelDFSL_VLC5P,"VXP_Models\\ModelDFSL_R_VLC5P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC5 <- ggplot(ModelDFSL_VLC5P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC5, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC5, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC5, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC5, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC5, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC5, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC5, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC5
ggplotly(P_CFluxI1y_VLC5)


#Plot 3: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC5P_YCO2 <- ModelDFSL_VLC5P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC5P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC5")

PA_CO21y_VLC5 <- ggplot(ModelDFSL_VLC5P_YCO2, aes(x = Year, y = AnnualCO2_VLC5)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC5
ggplotly(PA_CO21y_VLC5)

#Plot 3: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC5P_YCO2D <- merge(ModelDFSL_VLC5P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC5P_YCO2D$AnnualCO2D_VLC5 <- ModelDFSL_VLC5P_YCO2D$AnnualCO2_VLC5 * ModelDFSL_VLC5P_YCO2D$GWP100

PA_CO21yD_VLC5 <- ggplot(ModelDFSL_VLC5P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC5)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC5
ggplotly(PA_CO21yD_VLC5)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC5P_YC <- ModelDFSL_VLC5P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC5)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC5P_YC) <- c("Year", "Months", "AnnualCTail_VLC5")

PA_C1y_VLC5 <- ggplot(ModelDFSL_VLC5P_YC, aes(x = Year, y = AnnualCTail_VLC5)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC5
ggplotly(PA_C1y_VLC5)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC5P_YCT <- ModelDFSL_VLC5P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC5)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC5P_YCT) <- c("Year", "Months", "AnnualCTail_VLC5")

PA_CT1y_VLC5 <- ggplot(ModelDFSL_VLC5P_YCT, aes(x = Year, y = AnnualCTail_VLC5)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC5
ggplotly(PA_CT1y_VLC5)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC5P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC5P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC5P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC5P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC5P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC5P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC5P_YCT,"CTails_P\\ModelDFSL_R_C_VLC5P.xlsx") #Yearly C emissions



#### 7.7 - VLC6) Litter; 60%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC6=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC6=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
               S.Thick = soil.thick, pClay = clay100*0.6,
               pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC6=data.frame(years,rep(fT_VLC6*fW_VLC6,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC6=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay100*0.6, #Percent clay in mineral soil
  xi=xi.frame_VLC6) #Loads the model

Ct_VLC6=getC(Model_VLC6) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC6, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC6 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC6 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC6 <- as.data.frame(VEC_Lit_VLC6) #Converting the Litter vector to a data frame
VEC_LitDF_VLC6$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC6 <- as.data.frame(VEC_Man_VLC6) #Converting the Manure vector to a data frame
VEC_ManDF_VLC6$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC6, class) #Check that class is numeric
sapply(VEC_ManDF_VLC6, class) #Check that class is numeric
LitterCinputs_VLC6=VEC_LitDF_VLC6   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC6=VEC_ManDF_VLC6 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC6 <- merge(LitterCinputs_VLC6, ManureCinputs_VLC6, by = "MNumber")
MCinputs_VLC6$MInput_VLC6 <- MCinputs_VLC6$VEC_Lit_VLC6 + MCinputs_VLC6$VEC_Man_VLC6

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC6)[which(names(MCinputs_VLC6) == "VEC_Lit_VLC6")] <- "LitterC_VLC6"
colnames(MCinputs_VLC6)[which(names(MCinputs_VLC6) == "VEC_Man_VLC6")] <- "ManureC_VLC6"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC6 <- as.data.frame(Ct_VLC6)
colnames(ModelDF_VLC6) <- c('DPM_VLC6','RPM_VLC6','BIO_VLC6', 'HUM_VLC6', 'IOM_VLC6')
ModelDFS_VLC6 <- ModelDF_VLC6[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC6$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC6$AllPools_VLC6 <- ModelDFS_VLC6$DPM_VLC6 + ModelDFS_VLC6$RPM_VLC6 + ModelDFS_VLC6$BIO_VLC6 + ModelDFS_VLC6$HUM_VLC6 + ModelDFS_VLC6$IOM_VLC6

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC6$AllPools_noIOM_VLC6 <- ModelDFS_VLC6$DPM_VLC6 + ModelDFS_VLC6$RPM_VLC6 + ModelDFS_VLC6$BIO_VLC6 + ModelDFS_VLC6$HUM_VLC6

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC6 <- merge(ModelDFS_VLC6, MCinputs_VLC6, by = "MNumber")

ModelDFSL_VLC6$MInput_VLC6 <- ModelDFSL_VLC6$LitterC_VLC6 + ModelDFSL_VLC6$ManureC_VLC6
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC6$CTails_VLC6 <- ModelDFSL_VLC6$AllPools_noIOM_VLC6 + ModelDFSL_VLC6$MInput_VLC6

#Create Monthly Accumulated input of C
ModelDFSL_VLC6$AccumInput_VLC6 = ModelDFSL_VLC6$AccumInput_VLC6=cumsum(ModelDFSL_VLC6$MInput_VLC6)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC6$MGrowth_DPM_VLC6 <- ave(ModelDFSL_VLC6$DPM_VLC6, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC6$MGrowth_RPM_VLC6 <- ave(ModelDFSL_VLC6$RPM_VLC6, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC6$MGrowth_BIO_VLC6 <- ave(ModelDFSL_VLC6$BIO_VLC6, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC6$MGrowth_HUM_VLC6 <- ave(ModelDFSL_VLC6$HUM_VLC6, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC6$MGrowth_IOM_VLC6 <- ave(ModelDFSL_VLC6$IOM_VLC6, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC6$M_CCO2_VLC6 <- ModelDFSL_VLC6$MInput_VLC6 - ModelDFSL_VLC6$MGrowth_DPM_VLC6 - ModelDFSL_VLC6$MGrowth_RPM_VLC6 - ModelDFSL_VLC6$MGrowth_BIO_VLC6 - ModelDFSL_VLC6$MGrowth_HUM_VLC6
ModelDFSL_VLC6$Accum_CCO2_VLC6 <- ModelDFSL_VLC6$AccumInput_VLC6 - ModelDFSL_VLC6$AllPools_noIOM_VLC6

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC6$M_CCO2_VLC6[1] <- 0
ModelDFSL_VLC6$Accum_CCO2_VLC6[1] <- 0

#Balance validation
ModelDFSL_VLC6$Balance_VLC6 <- ModelDFSL_VLC6$AccumInput_VLC6 - ModelDFSL_VLC6$Accum_CCO2_VLC6 - (ModelDFSL_VLC6$DPM_VLC6 + ModelDFSL_VLC6$RPM_VLC6 + ModelDFSL_VLC6$BIO_VLC6 + ModelDFSL_VLC6$HUM_VLC6)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC6$M_CO2_VLC6 <- ModelDFSL_VLC6$M_CCO2_VLC6 * 44/12
ModelDFSL_VLC6$Accum_CO2_VLC6 <- ModelDFSL_VLC6$Accum_CCO2_VLC6 * 44/12

#This model will be called VLC6C because implies a continuous input of C
ModelDFSL_VLC6C <- ModelDFSL_VLC6

#Export the dataframe
write_xlsx(ModelDFSL_VLC6C,"VXC_Models\\ModelDFSL_R_VLC6C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC6 <- ModelDFSL_VLC6 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC6.1 <- rbind(c(0:0), ModelDFSLt0_VLC6)
ModelDFSLt1_VLC6.2 <- rbind(c(0:0), ModelDFSLt1_VLC6.1)
ModelDFSLt1_VLC6.3 <- rbind(c(0:0), ModelDFSLt1_VLC6.2)
ModelDFSLt1_VLC6.4 <- rbind(c(0:0), ModelDFSLt1_VLC6.3)
ModelDFSLt1_VLC6.5 <- rbind(c(0:0), ModelDFSLt1_VLC6.4)
ModelDFSLt1_VLC6.6 <- rbind(c(0:0), ModelDFSLt1_VLC6.5)
ModelDFSLt1_VLC6.7 <- rbind(c(0:0), ModelDFSLt1_VLC6.6)
ModelDFSLt1_VLC6.8 <- rbind(c(0:0), ModelDFSLt1_VLC6.7)
ModelDFSLt1_VLC6.9 <- rbind(c(0:0), ModelDFSLt1_VLC6.8)
ModelDFSLt1_VLC6.10 <- rbind(c(0:0), ModelDFSLt1_VLC6.9)
ModelDFSLt1_VLC6.11 <- rbind(c(0:0), ModelDFSLt1_VLC6.10)
ModelDFSLt1_VLC6.12 <- rbind(c(0:0), ModelDFSLt1_VLC6.11)
ModelDFSLt1_VLC6.13 <- ModelDFSLt1_VLC6.12[-nrow(ModelDFSLt1_VLC6.12),]
ModelDFSLt1_VLC6.14 <- ModelDFSLt1_VLC6.13[-nrow(ModelDFSLt1_VLC6.13),]
ModelDFSLt1_VLC6.15 <- ModelDFSLt1_VLC6.14[-nrow(ModelDFSLt1_VLC6.14),]
ModelDFSLt1_VLC6.16 <- ModelDFSLt1_VLC6.15[-nrow(ModelDFSLt1_VLC6.15),]
ModelDFSLt1_VLC6.17 <- ModelDFSLt1_VLC6.16[-nrow(ModelDFSLt1_VLC6.16),]
ModelDFSLt1_VLC6.18 <- ModelDFSLt1_VLC6.17[-nrow(ModelDFSLt1_VLC6.17),]
ModelDFSLt1_VLC6.19 <- ModelDFSLt1_VLC6.18[-nrow(ModelDFSLt1_VLC6.18),]
ModelDFSLt1_VLC6.20 <- ModelDFSLt1_VLC6.19[-nrow(ModelDFSLt1_VLC6.19),]
ModelDFSLt1_VLC6.21 <- ModelDFSLt1_VLC6.20[-nrow(ModelDFSLt1_VLC6.20),]
ModelDFSLt1_VLC6.22 <- ModelDFSLt1_VLC6.21[-nrow(ModelDFSLt1_VLC6.21),]
ModelDFSLt1_VLC6.23 <- ModelDFSLt1_VLC6.22[-nrow(ModelDFSLt1_VLC6.22),]
ModelDFSLt1_VLC6.24 <- ModelDFSLt1_VLC6.23[-nrow(ModelDFSLt1_VLC6.23),]

ModelDFSLt1_VLC6 <- ModelDFSLt1_VLC6.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC6 <- ModelDFSLt0_VLC6 - ModelDFSLt1_VLC6

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC6$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC6P because implies a one-off input of C
ModelDFSL_VLC6P <- ModelDFSL1y_VLC6

#Export the dataframe
write_xlsx(ModelDFSL_VLC6P,"VXP_Models\\ModelDFSL_R_VLC6P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC6 <- ggplot(ModelDFSL_VLC6P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC6, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC6, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC6, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC6, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC6, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC6, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC6, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC6
ggplotly(P_CFluxI1y_VLC6)


#Plot 3: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC6P_YCO2 <- ModelDFSL_VLC6P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC6P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC6")

PA_CO21y_VLC6 <- ggplot(ModelDFSL_VLC6P_YCO2, aes(x = Year, y = AnnualCO2_VLC6)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC6
ggplotly(PA_CO21y_VLC6)

#Plot 3: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC6P_YCO2D <- merge(ModelDFSL_VLC6P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC6P_YCO2D$AnnualCO2D_VLC6 <- ModelDFSL_VLC6P_YCO2D$AnnualCO2_VLC6 * ModelDFSL_VLC6P_YCO2D$GWP100

PA_CO21yD_VLC6 <- ggplot(ModelDFSL_VLC6P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC6)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC6
ggplotly(PA_CO21yD_VLC6)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC6P_YC <- ModelDFSL_VLC6P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC6)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC6P_YC) <- c("Year", "Months", "AnnualCTail_VLC6")

PA_C1y_VLC6 <- ggplot(ModelDFSL_VLC6P_YC, aes(x = Year, y = AnnualCTail_VLC6)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC6
ggplotly(PA_C1y_VLC6)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC6P_YCT <- ModelDFSL_VLC6P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC6)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC6P_YCT) <- c("Year", "Months", "AnnualCTail_VLC6")

PA_CT1y_VLC6 <- ggplot(ModelDFSL_VLC6P_YCT, aes(x = Year, y = AnnualCTail_VLC6)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC6
ggplotly(PA_CT1y_VLC6)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC6P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC6P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC6P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC6P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC6P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC6P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC6P_YCT,"CTails_P\\ModelDFSL_R_C_VLC6P.xlsx") #Yearly C emissions



#### 7.8 - VLC7) Litter; 70%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC7=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC7=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
               S.Thick = soil.thick, pClay = clay100*0.7,
               pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC7=data.frame(years,rep(fT_VLC7*fW_VLC7,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC7=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay100*0.7, #Percent clay in mineral soil
  xi=xi.frame_VLC7) #Loads the model

Ct_VLC7=getC(Model_VLC7) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC7, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC7 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC7 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC7 <- as.data.frame(VEC_Lit_VLC7) #Converting the Litter vector to a data frame
VEC_LitDF_VLC7$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC7 <- as.data.frame(VEC_Man_VLC7) #Converting the Manure vector to a data frame
VEC_ManDF_VLC7$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC7, class) #Check that class is numeric
sapply(VEC_ManDF_VLC7, class) #Check that class is numeric
LitterCinputs_VLC7=VEC_LitDF_VLC7   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC7=VEC_ManDF_VLC7 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC7 <- merge(LitterCinputs_VLC7, ManureCinputs_VLC7, by = "MNumber")
MCinputs_VLC7$MInput_VLC7 <- MCinputs_VLC7$VEC_Lit_VLC7 + MCinputs_VLC7$VEC_Man_VLC7

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC7)[which(names(MCinputs_VLC7) == "VEC_Lit_VLC7")] <- "LitterC_VLC7"
colnames(MCinputs_VLC7)[which(names(MCinputs_VLC7) == "VEC_Man_VLC7")] <- "ManureC_VLC7"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC7 <- as.data.frame(Ct_VLC7)
colnames(ModelDF_VLC7) <- c('DPM_VLC7','RPM_VLC7','BIO_VLC7', 'HUM_VLC7', 'IOM_VLC7')
ModelDFS_VLC7 <- ModelDF_VLC7[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC7$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC7$AllPools_VLC7 <- ModelDFS_VLC7$DPM_VLC7 + ModelDFS_VLC7$RPM_VLC7 + ModelDFS_VLC7$BIO_VLC7 + ModelDFS_VLC7$HUM_VLC7 + ModelDFS_VLC7$IOM_VLC7

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC7$AllPools_noIOM_VLC7 <- ModelDFS_VLC7$DPM_VLC7 + ModelDFS_VLC7$RPM_VLC7 + ModelDFS_VLC7$BIO_VLC7 + ModelDFS_VLC7$HUM_VLC7

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC7 <- merge(ModelDFS_VLC7, MCinputs_VLC7, by = "MNumber")

ModelDFSL_VLC7$MInput_VLC7 <- ModelDFSL_VLC7$LitterC_VLC7 + ModelDFSL_VLC7$ManureC_VLC7
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC7$CTails_VLC7 <- ModelDFSL_VLC7$AllPools_noIOM_VLC7 + ModelDFSL_VLC7$MInput_VLC7

#Create Monthly Accumulated input of C
ModelDFSL_VLC7$AccumInput_VLC7 = ModelDFSL_VLC7$AccumInput_VLC7=cumsum(ModelDFSL_VLC7$MInput_VLC7)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC7$MGrowth_DPM_VLC7 <- ave(ModelDFSL_VLC7$DPM_VLC7, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC7$MGrowth_RPM_VLC7 <- ave(ModelDFSL_VLC7$RPM_VLC7, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC7$MGrowth_BIO_VLC7 <- ave(ModelDFSL_VLC7$BIO_VLC7, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC7$MGrowth_HUM_VLC7 <- ave(ModelDFSL_VLC7$HUM_VLC7, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC7$MGrowth_IOM_VLC7 <- ave(ModelDFSL_VLC7$IOM_VLC7, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC7$M_CCO2_VLC7 <- ModelDFSL_VLC7$MInput_VLC7 - ModelDFSL_VLC7$MGrowth_DPM_VLC7 - ModelDFSL_VLC7$MGrowth_RPM_VLC7 - ModelDFSL_VLC7$MGrowth_BIO_VLC7 - ModelDFSL_VLC7$MGrowth_HUM_VLC7
ModelDFSL_VLC7$Accum_CCO2_VLC7 <- ModelDFSL_VLC7$AccumInput_VLC7 - ModelDFSL_VLC7$AllPools_noIOM_VLC7

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC7$M_CCO2_VLC7[1] <- 0
ModelDFSL_VLC7$Accum_CCO2_VLC7[1] <- 0

#Balance validation
ModelDFSL_VLC7$Balance_VLC7 <- ModelDFSL_VLC7$AccumInput_VLC7 - ModelDFSL_VLC7$Accum_CCO2_VLC7 - (ModelDFSL_VLC7$DPM_VLC7 + ModelDFSL_VLC7$RPM_VLC7 + ModelDFSL_VLC7$BIO_VLC7 + ModelDFSL_VLC7$HUM_VLC7)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC7$M_CO2_VLC7 <- ModelDFSL_VLC7$M_CCO2_VLC7 * 44/12
ModelDFSL_VLC7$Accum_CO2_VLC7 <- ModelDFSL_VLC7$Accum_CCO2_VLC7 * 44/12

#This model will be called VLC7C because implies a continuous input of C
ModelDFSL_VLC7C <- ModelDFSL_VLC7

#Export the dataframe
write_xlsx(ModelDFSL_VLC7C,"VXC_Models\\ModelDFSL_R_VLC7C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC7 <- ModelDFSL_VLC7 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC7.1 <- rbind(c(0:0), ModelDFSLt0_VLC7)
ModelDFSLt1_VLC7.2 <- rbind(c(0:0), ModelDFSLt1_VLC7.1)
ModelDFSLt1_VLC7.3 <- rbind(c(0:0), ModelDFSLt1_VLC7.2)
ModelDFSLt1_VLC7.4 <- rbind(c(0:0), ModelDFSLt1_VLC7.3)
ModelDFSLt1_VLC7.5 <- rbind(c(0:0), ModelDFSLt1_VLC7.4)
ModelDFSLt1_VLC7.6 <- rbind(c(0:0), ModelDFSLt1_VLC7.5)
ModelDFSLt1_VLC7.7 <- rbind(c(0:0), ModelDFSLt1_VLC7.6)
ModelDFSLt1_VLC7.8 <- rbind(c(0:0), ModelDFSLt1_VLC7.7)
ModelDFSLt1_VLC7.9 <- rbind(c(0:0), ModelDFSLt1_VLC7.8)
ModelDFSLt1_VLC7.10 <- rbind(c(0:0), ModelDFSLt1_VLC7.9)
ModelDFSLt1_VLC7.11 <- rbind(c(0:0), ModelDFSLt1_VLC7.10)
ModelDFSLt1_VLC7.12 <- rbind(c(0:0), ModelDFSLt1_VLC7.11)
ModelDFSLt1_VLC7.13 <- ModelDFSLt1_VLC7.12[-nrow(ModelDFSLt1_VLC7.12),]
ModelDFSLt1_VLC7.14 <- ModelDFSLt1_VLC7.13[-nrow(ModelDFSLt1_VLC7.13),]
ModelDFSLt1_VLC7.15 <- ModelDFSLt1_VLC7.14[-nrow(ModelDFSLt1_VLC7.14),]
ModelDFSLt1_VLC7.16 <- ModelDFSLt1_VLC7.15[-nrow(ModelDFSLt1_VLC7.15),]
ModelDFSLt1_VLC7.17 <- ModelDFSLt1_VLC7.16[-nrow(ModelDFSLt1_VLC7.16),]
ModelDFSLt1_VLC7.18 <- ModelDFSLt1_VLC7.17[-nrow(ModelDFSLt1_VLC7.17),]
ModelDFSLt1_VLC7.19 <- ModelDFSLt1_VLC7.18[-nrow(ModelDFSLt1_VLC7.18),]
ModelDFSLt1_VLC7.20 <- ModelDFSLt1_VLC7.19[-nrow(ModelDFSLt1_VLC7.19),]
ModelDFSLt1_VLC7.21 <- ModelDFSLt1_VLC7.20[-nrow(ModelDFSLt1_VLC7.20),]
ModelDFSLt1_VLC7.22 <- ModelDFSLt1_VLC7.21[-nrow(ModelDFSLt1_VLC7.21),]
ModelDFSLt1_VLC7.23 <- ModelDFSLt1_VLC7.22[-nrow(ModelDFSLt1_VLC7.22),]
ModelDFSLt1_VLC7.24 <- ModelDFSLt1_VLC7.23[-nrow(ModelDFSLt1_VLC7.23),]

ModelDFSLt1_VLC7 <- ModelDFSLt1_VLC7.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC7 <- ModelDFSLt0_VLC7 - ModelDFSLt1_VLC7

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC7$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC7P because implies a one-off input of C
ModelDFSL_VLC7P <- ModelDFSL1y_VLC7

#Export the dataframe
write_xlsx(ModelDFSL_VLC7P,"VXP_Models\\ModelDFSL_R_VLC7P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC7 <- ggplot(ModelDFSL_VLC7P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC7, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC7, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC7, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC7, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC7, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC7, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC7, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC7
ggplotly(P_CFluxI1y_VLC7)


#Plot 3: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC7P_YCO2 <- ModelDFSL_VLC7P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC7P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC7")

PA_CO21y_VLC7 <- ggplot(ModelDFSL_VLC7P_YCO2, aes(x = Year, y = AnnualCO2_VLC7)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC7
ggplotly(PA_CO21y_VLC7)

#Plot 3: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC7P_YCO2D <- merge(ModelDFSL_VLC7P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC7P_YCO2D$AnnualCO2D_VLC7 <- ModelDFSL_VLC7P_YCO2D$AnnualCO2_VLC7 * ModelDFSL_VLC7P_YCO2D$GWP100

PA_CO21yD_VLC7 <- ggplot(ModelDFSL_VLC7P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC7)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC7
ggplotly(PA_CO21yD_VLC7)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC7P_YC <- ModelDFSL_VLC7P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC7)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC7P_YC) <- c("Year", "Months", "AnnualCTail_VLC7")

PA_C1y_VLC7 <- ggplot(ModelDFSL_VLC7P_YC, aes(x = Year, y = AnnualCTail_VLC7)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC7
ggplotly(PA_C1y_VLC7)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC7P_YCT <- ModelDFSL_VLC7P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC7)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC7P_YCT) <- c("Year", "Months", "AnnualCTail_VLC7")

PA_CT1y_VLC7 <- ggplot(ModelDFSL_VLC7P_YCT, aes(x = Year, y = AnnualCTail_VLC7)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC7
ggplotly(PA_CT1y_VLC7)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC7P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC7P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC7P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC7P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC7P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC7P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC7P_YCT,"CTails_P\\ModelDFSL_R_C_VLC7P.xlsx") #Yearly C emissions



#### 7.9 - VLC8) Litter; 80%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC8=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC8=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
               S.Thick = soil.thick, pClay = clay100*0.8,
               pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC8=data.frame(years,rep(fT_VLC8*fW_VLC8,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC8=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay100*0.8, #Percent clay in mineral soil
  xi=xi.frame_VLC8) #Loads the model

Ct_VLC8=getC(Model_VLC8) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC8, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC8 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC8 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC8 <- as.data.frame(VEC_Lit_VLC8) #Converting the Litter vector to a data frame
VEC_LitDF_VLC8$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC8 <- as.data.frame(VEC_Man_VLC8) #Converting the Manure vector to a data frame
VEC_ManDF_VLC8$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC8, class) #Check that class is numeric
sapply(VEC_ManDF_VLC8, class) #Check that class is numeric
LitterCinputs_VLC8=VEC_LitDF_VLC8   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC8=VEC_ManDF_VLC8 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC8 <- merge(LitterCinputs_VLC8, ManureCinputs_VLC8, by = "MNumber")
MCinputs_VLC8$MInput_VLC8 <- MCinputs_VLC8$VEC_Lit_VLC8 + MCinputs_VLC8$VEC_Man_VLC8

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC8)[which(names(MCinputs_VLC8) == "VEC_Lit_VLC8")] <- "LitterC_VLC8"
colnames(MCinputs_VLC8)[which(names(MCinputs_VLC8) == "VEC_Man_VLC8")] <- "ManureC_VLC8"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC8 <- as.data.frame(Ct_VLC8)
colnames(ModelDF_VLC8) <- c('DPM_VLC8','RPM_VLC8','BIO_VLC8', 'HUM_VLC8', 'IOM_VLC8')
ModelDFS_VLC8 <- ModelDF_VLC8[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC8$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC8$AllPools_VLC8 <- ModelDFS_VLC8$DPM_VLC8 + ModelDFS_VLC8$RPM_VLC8 + ModelDFS_VLC8$BIO_VLC8 + ModelDFS_VLC8$HUM_VLC8 + ModelDFS_VLC8$IOM_VLC8

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC8$AllPools_noIOM_VLC8 <- ModelDFS_VLC8$DPM_VLC8 + ModelDFS_VLC8$RPM_VLC8 + ModelDFS_VLC8$BIO_VLC8 + ModelDFS_VLC8$HUM_VLC8

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC8 <- merge(ModelDFS_VLC8, MCinputs_VLC8, by = "MNumber")

ModelDFSL_VLC8$MInput_VLC8 <- ModelDFSL_VLC8$LitterC_VLC8 + ModelDFSL_VLC8$ManureC_VLC8
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC8$CTails_VLC8 <- ModelDFSL_VLC8$AllPools_noIOM_VLC8 + ModelDFSL_VLC8$MInput_VLC8

#Create Monthly Accumulated input of C
ModelDFSL_VLC8$AccumInput_VLC8 = ModelDFSL_VLC8$AccumInput_VLC8=cumsum(ModelDFSL_VLC8$MInput_VLC8)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC8$MGrowth_DPM_VLC8 <- ave(ModelDFSL_VLC8$DPM_VLC8, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC8$MGrowth_RPM_VLC8 <- ave(ModelDFSL_VLC8$RPM_VLC8, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC8$MGrowth_BIO_VLC8 <- ave(ModelDFSL_VLC8$BIO_VLC8, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC8$MGrowth_HUM_VLC8 <- ave(ModelDFSL_VLC8$HUM_VLC8, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC8$MGrowth_IOM_VLC8 <- ave(ModelDFSL_VLC8$IOM_VLC8, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC8$M_CCO2_VLC8 <- ModelDFSL_VLC8$MInput_VLC8 - ModelDFSL_VLC8$MGrowth_DPM_VLC8 - ModelDFSL_VLC8$MGrowth_RPM_VLC8 - ModelDFSL_VLC8$MGrowth_BIO_VLC8 - ModelDFSL_VLC8$MGrowth_HUM_VLC8
ModelDFSL_VLC8$Accum_CCO2_VLC8 <- ModelDFSL_VLC8$AccumInput_VLC8 - ModelDFSL_VLC8$AllPools_noIOM_VLC8

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC8$M_CCO2_VLC8[1] <- 0
ModelDFSL_VLC8$Accum_CCO2_VLC8[1] <- 0

#Balance validation
ModelDFSL_VLC8$Balance_VLC8 <- ModelDFSL_VLC8$AccumInput_VLC8 - ModelDFSL_VLC8$Accum_CCO2_VLC8 - (ModelDFSL_VLC8$DPM_VLC8 + ModelDFSL_VLC8$RPM_VLC8 + ModelDFSL_VLC8$BIO_VLC8 + ModelDFSL_VLC8$HUM_VLC8)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC8$M_CO2_VLC8 <- ModelDFSL_VLC8$M_CCO2_VLC8 * 44/12
ModelDFSL_VLC8$Accum_CO2_VLC8 <- ModelDFSL_VLC8$Accum_CCO2_VLC8 * 44/12

#This model will be called VLC8C because implies a continuous input of C
ModelDFSL_VLC8C <- ModelDFSL_VLC8

#Export the dataframe
write_xlsx(ModelDFSL_VLC8C,"VXC_Models\\ModelDFSL_R_VLC8C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC8 <- ModelDFSL_VLC8 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC8.1 <- rbind(c(0:0), ModelDFSLt0_VLC8)
ModelDFSLt1_VLC8.2 <- rbind(c(0:0), ModelDFSLt1_VLC8.1)
ModelDFSLt1_VLC8.3 <- rbind(c(0:0), ModelDFSLt1_VLC8.2)
ModelDFSLt1_VLC8.4 <- rbind(c(0:0), ModelDFSLt1_VLC8.3)
ModelDFSLt1_VLC8.5 <- rbind(c(0:0), ModelDFSLt1_VLC8.4)
ModelDFSLt1_VLC8.6 <- rbind(c(0:0), ModelDFSLt1_VLC8.5)
ModelDFSLt1_VLC8.7 <- rbind(c(0:0), ModelDFSLt1_VLC8.6)
ModelDFSLt1_VLC8.8 <- rbind(c(0:0), ModelDFSLt1_VLC8.7)
ModelDFSLt1_VLC8.9 <- rbind(c(0:0), ModelDFSLt1_VLC8.8)
ModelDFSLt1_VLC8.10 <- rbind(c(0:0), ModelDFSLt1_VLC8.9)
ModelDFSLt1_VLC8.11 <- rbind(c(0:0), ModelDFSLt1_VLC8.10)
ModelDFSLt1_VLC8.12 <- rbind(c(0:0), ModelDFSLt1_VLC8.11)
ModelDFSLt1_VLC8.13 <- ModelDFSLt1_VLC8.12[-nrow(ModelDFSLt1_VLC8.12),]
ModelDFSLt1_VLC8.14 <- ModelDFSLt1_VLC8.13[-nrow(ModelDFSLt1_VLC8.13),]
ModelDFSLt1_VLC8.15 <- ModelDFSLt1_VLC8.14[-nrow(ModelDFSLt1_VLC8.14),]
ModelDFSLt1_VLC8.16 <- ModelDFSLt1_VLC8.15[-nrow(ModelDFSLt1_VLC8.15),]
ModelDFSLt1_VLC8.17 <- ModelDFSLt1_VLC8.16[-nrow(ModelDFSLt1_VLC8.16),]
ModelDFSLt1_VLC8.18 <- ModelDFSLt1_VLC8.17[-nrow(ModelDFSLt1_VLC8.17),]
ModelDFSLt1_VLC8.19 <- ModelDFSLt1_VLC8.18[-nrow(ModelDFSLt1_VLC8.18),]
ModelDFSLt1_VLC8.20 <- ModelDFSLt1_VLC8.19[-nrow(ModelDFSLt1_VLC8.19),]
ModelDFSLt1_VLC8.21 <- ModelDFSLt1_VLC8.20[-nrow(ModelDFSLt1_VLC8.20),]
ModelDFSLt1_VLC8.22 <- ModelDFSLt1_VLC8.21[-nrow(ModelDFSLt1_VLC8.21),]
ModelDFSLt1_VLC8.23 <- ModelDFSLt1_VLC8.22[-nrow(ModelDFSLt1_VLC8.22),]
ModelDFSLt1_VLC8.24 <- ModelDFSLt1_VLC8.23[-nrow(ModelDFSLt1_VLC8.23),]

ModelDFSLt1_VLC8 <- ModelDFSLt1_VLC8.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC8 <- ModelDFSLt0_VLC8 - ModelDFSLt1_VLC8

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC8$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC8P because implies a one-off input of C
ModelDFSL_VLC8P <- ModelDFSL1y_VLC8

#Export the dataframe
write_xlsx(ModelDFSL_VLC8P,"VXP_Models\\ModelDFSL_R_VLC8P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC8 <- ggplot(ModelDFSL_VLC8P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC8, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC8, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC8, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC8, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC8, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC8, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC8, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC8
ggplotly(P_CFluxI1y_VLC8)


#Plot 3: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC8P_YCO2 <- ModelDFSL_VLC8P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC8P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC8")

PA_CO21y_VLC8 <- ggplot(ModelDFSL_VLC8P_YCO2, aes(x = Year, y = AnnualCO2_VLC8)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC8
ggplotly(PA_CO21y_VLC8)

#Plot 3: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC8P_YCO2D <- merge(ModelDFSL_VLC8P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC8P_YCO2D$AnnualCO2D_VLC8 <- ModelDFSL_VLC8P_YCO2D$AnnualCO2_VLC8 * ModelDFSL_VLC8P_YCO2D$GWP100

PA_CO21yD_VLC8 <- ggplot(ModelDFSL_VLC8P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC8)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC8
ggplotly(PA_CO21yD_VLC8)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC8P_YC <- ModelDFSL_VLC8P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC8)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC8P_YC) <- c("Year", "Months", "AnnualCTail_VLC8")

PA_C1y_VLC8 <- ggplot(ModelDFSL_VLC8P_YC, aes(x = Year, y = AnnualCTail_VLC8)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC8
ggplotly(PA_C1y_VLC8)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC8P_YCT <- ModelDFSL_VLC8P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC8)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC8P_YCT) <- c("Year", "Months", "AnnualCTail_VLC8")

PA_CT1y_VLC8 <- ggplot(ModelDFSL_VLC8P_YCT, aes(x = Year, y = AnnualCTail_VLC8)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC8
ggplotly(PA_CT1y_VLC8)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC8P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC8P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC8P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC8P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC8P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC8P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC8P_YCT,"CTails_P\\ModelDFSL_R_C_VLC8P.xlsx") #Yearly C emissions



#### 7.10 - VLC9) Litter; 90%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC9=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC9=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
               S.Thick = soil.thick, pClay = clay100*0.9,
               pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC9=data.frame(years,rep(fT_VLC9*fW_VLC9,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC9=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay100*0.9, #Percent clay in mineral soil
  xi=xi.frame_VLC9) #Loads the model

Ct_VLC9=getC(Model_VLC9) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC9, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC9 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC9 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC9 <- as.data.frame(VEC_Lit_VLC9) #Converting the Litter vector to a data frame
VEC_LitDF_VLC9$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC9 <- as.data.frame(VEC_Man_VLC9) #Converting the Manure vector to a data frame
VEC_ManDF_VLC9$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC9, class) #Check that class is numeric
sapply(VEC_ManDF_VLC9, class) #Check that class is numeric
LitterCinputs_VLC9=VEC_LitDF_VLC9   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC9=VEC_ManDF_VLC9 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC9 <- merge(LitterCinputs_VLC9, ManureCinputs_VLC9, by = "MNumber")
MCinputs_VLC9$MInput_VLC9 <- MCinputs_VLC9$VEC_Lit_VLC9 + MCinputs_VLC9$VEC_Man_VLC9

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC9)[which(names(MCinputs_VLC9) == "VEC_Lit_VLC9")] <- "LitterC_VLC9"
colnames(MCinputs_VLC9)[which(names(MCinputs_VLC9) == "VEC_Man_VLC9")] <- "ManureC_VLC9"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC9 <- as.data.frame(Ct_VLC9)
colnames(ModelDF_VLC9) <- c('DPM_VLC9','RPM_VLC9','BIO_VLC9', 'HUM_VLC9', 'IOM_VLC9')
ModelDFS_VLC9 <- ModelDF_VLC9[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC9$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC9$AllPools_VLC9 <- ModelDFS_VLC9$DPM_VLC9 + ModelDFS_VLC9$RPM_VLC9 + ModelDFS_VLC9$BIO_VLC9 + ModelDFS_VLC9$HUM_VLC9 + ModelDFS_VLC9$IOM_VLC9

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC9$AllPools_noIOM_VLC9 <- ModelDFS_VLC9$DPM_VLC9 + ModelDFS_VLC9$RPM_VLC9 + ModelDFS_VLC9$BIO_VLC9 + ModelDFS_VLC9$HUM_VLC9

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC9 <- merge(ModelDFS_VLC9, MCinputs_VLC9, by = "MNumber")

ModelDFSL_VLC9$MInput_VLC9 <- ModelDFSL_VLC9$LitterC_VLC9 + ModelDFSL_VLC9$ManureC_VLC9
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC9$CTails_VLC9 <- ModelDFSL_VLC9$AllPools_noIOM_VLC9 + ModelDFSL_VLC9$MInput_VLC9

#Create Monthly Accumulated input of C
ModelDFSL_VLC9$AccumInput_VLC9 = ModelDFSL_VLC9$AccumInput_VLC9=cumsum(ModelDFSL_VLC9$MInput_VLC9)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC9$MGrowth_DPM_VLC9 <- ave(ModelDFSL_VLC9$DPM_VLC9, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC9$MGrowth_RPM_VLC9 <- ave(ModelDFSL_VLC9$RPM_VLC9, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC9$MGrowth_BIO_VLC9 <- ave(ModelDFSL_VLC9$BIO_VLC9, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC9$MGrowth_HUM_VLC9 <- ave(ModelDFSL_VLC9$HUM_VLC9, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC9$MGrowth_IOM_VLC9 <- ave(ModelDFSL_VLC9$IOM_VLC9, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC9$M_CCO2_VLC9 <- ModelDFSL_VLC9$MInput_VLC9 - ModelDFSL_VLC9$MGrowth_DPM_VLC9 - ModelDFSL_VLC9$MGrowth_RPM_VLC9 - ModelDFSL_VLC9$MGrowth_BIO_VLC9 - ModelDFSL_VLC9$MGrowth_HUM_VLC9
ModelDFSL_VLC9$Accum_CCO2_VLC9 <- ModelDFSL_VLC9$AccumInput_VLC9 - ModelDFSL_VLC9$AllPools_noIOM_VLC9

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC9$M_CCO2_VLC9[1] <- 0
ModelDFSL_VLC9$Accum_CCO2_VLC9[1] <- 0

#Balance validation
ModelDFSL_VLC9$Balance_VLC9 <- ModelDFSL_VLC9$AccumInput_VLC9 - ModelDFSL_VLC9$Accum_CCO2_VLC9 - (ModelDFSL_VLC9$DPM_VLC9 + ModelDFSL_VLC9$RPM_VLC9 + ModelDFSL_VLC9$BIO_VLC9 + ModelDFSL_VLC9$HUM_VLC9)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC9$M_CO2_VLC9 <- ModelDFSL_VLC9$M_CCO2_VLC9 * 44/12
ModelDFSL_VLC9$Accum_CO2_VLC9 <- ModelDFSL_VLC9$Accum_CCO2_VLC9 * 44/12

#This model will be called VLC9C because implies a continuous input of C
ModelDFSL_VLC9C <- ModelDFSL_VLC9

#Export the dataframe
write_xlsx(ModelDFSL_VLC9C,"VXC_Models\\ModelDFSL_R_VLC9C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC9 <- ModelDFSL_VLC9 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC9.1 <- rbind(c(0:0), ModelDFSLt0_VLC9)
ModelDFSLt1_VLC9.2 <- rbind(c(0:0), ModelDFSLt1_VLC9.1)
ModelDFSLt1_VLC9.3 <- rbind(c(0:0), ModelDFSLt1_VLC9.2)
ModelDFSLt1_VLC9.4 <- rbind(c(0:0), ModelDFSLt1_VLC9.3)
ModelDFSLt1_VLC9.5 <- rbind(c(0:0), ModelDFSLt1_VLC9.4)
ModelDFSLt1_VLC9.6 <- rbind(c(0:0), ModelDFSLt1_VLC9.5)
ModelDFSLt1_VLC9.7 <- rbind(c(0:0), ModelDFSLt1_VLC9.6)
ModelDFSLt1_VLC9.8 <- rbind(c(0:0), ModelDFSLt1_VLC9.7)
ModelDFSLt1_VLC9.9 <- rbind(c(0:0), ModelDFSLt1_VLC9.8)
ModelDFSLt1_VLC9.10 <- rbind(c(0:0), ModelDFSLt1_VLC9.9)
ModelDFSLt1_VLC9.11 <- rbind(c(0:0), ModelDFSLt1_VLC9.10)
ModelDFSLt1_VLC9.12 <- rbind(c(0:0), ModelDFSLt1_VLC9.11)
ModelDFSLt1_VLC9.13 <- ModelDFSLt1_VLC9.12[-nrow(ModelDFSLt1_VLC9.12),]
ModelDFSLt1_VLC9.14 <- ModelDFSLt1_VLC9.13[-nrow(ModelDFSLt1_VLC9.13),]
ModelDFSLt1_VLC9.15 <- ModelDFSLt1_VLC9.14[-nrow(ModelDFSLt1_VLC9.14),]
ModelDFSLt1_VLC9.16 <- ModelDFSLt1_VLC9.15[-nrow(ModelDFSLt1_VLC9.15),]
ModelDFSLt1_VLC9.17 <- ModelDFSLt1_VLC9.16[-nrow(ModelDFSLt1_VLC9.16),]
ModelDFSLt1_VLC9.18 <- ModelDFSLt1_VLC9.17[-nrow(ModelDFSLt1_VLC9.17),]
ModelDFSLt1_VLC9.19 <- ModelDFSLt1_VLC9.18[-nrow(ModelDFSLt1_VLC9.18),]
ModelDFSLt1_VLC9.20 <- ModelDFSLt1_VLC9.19[-nrow(ModelDFSLt1_VLC9.19),]
ModelDFSLt1_VLC9.21 <- ModelDFSLt1_VLC9.20[-nrow(ModelDFSLt1_VLC9.20),]
ModelDFSLt1_VLC9.22 <- ModelDFSLt1_VLC9.21[-nrow(ModelDFSLt1_VLC9.21),]
ModelDFSLt1_VLC9.23 <- ModelDFSLt1_VLC9.22[-nrow(ModelDFSLt1_VLC9.22),]
ModelDFSLt1_VLC9.24 <- ModelDFSLt1_VLC9.23[-nrow(ModelDFSLt1_VLC9.23),]

ModelDFSLt1_VLC9 <- ModelDFSLt1_VLC9.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC9 <- ModelDFSLt0_VLC9 - ModelDFSLt1_VLC9

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC9$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC9P because implies a one-off input of C
ModelDFSL_VLC9P <- ModelDFSL1y_VLC9

#Export the dataframe
write_xlsx(ModelDFSL_VLC9P,"VXP_Models\\ModelDFSL_R_VLC9P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC9 <- ggplot(ModelDFSL_VLC9P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC9, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC9, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC9, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC9, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC9, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC9, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC9, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC9
ggplotly(P_CFluxI1y_VLC9)


#Plot 3: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC9P_YCO2 <- ModelDFSL_VLC9P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC9P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC9")

PA_CO21y_VLC9 <- ggplot(ModelDFSL_VLC9P_YCO2, aes(x = Year, y = AnnualCO2_VLC9)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC9
ggplotly(PA_CO21y_VLC9)

#Plot 3: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC9P_YCO2D <- merge(ModelDFSL_VLC9P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC9P_YCO2D$AnnualCO2D_VLC9 <- ModelDFSL_VLC9P_YCO2D$AnnualCO2_VLC9 * ModelDFSL_VLC9P_YCO2D$GWP100

PA_CO21yD_VLC9 <- ggplot(ModelDFSL_VLC9P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC9)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC9
ggplotly(PA_CO21yD_VLC9)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC9P_YC <- ModelDFSL_VLC9P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC9)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC9P_YC) <- c("Year", "Months", "AnnualCTail_VLC9")

PA_C1y_VLC9 <- ggplot(ModelDFSL_VLC9P_YC, aes(x = Year, y = AnnualCTail_VLC9)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC9
ggplotly(PA_C1y_VLC9)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC9P_YCT <- ModelDFSL_VLC9P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC9)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC9P_YCT) <- c("Year", "Months", "AnnualCTail_VLC9")

PA_CT1y_VLC9 <- ggplot(ModelDFSL_VLC9P_YCT, aes(x = Year, y = AnnualCTail_VLC9)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC9
ggplotly(PA_CT1y_VLC9)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC9P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC9P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC9P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC9P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC9P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC9P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC9P_YCT,"CTails_P\\ModelDFSL_R_C_VLC9P.xlsx") #Yearly C emissions



#### 7.11 - VLC10) Litter; 100%clay; Conventional tillage ####
#The effects of climate on decomposition can be calculated now with the functions SoilR::fT.RothC and SoilR::fW.RothC as
fT_VLC10=SoilR::fT.RothC(Temp[,2]) #Temperature effects per month
fW_VLC10=SoilR::fW.RothC(P=(Precip[,2]), E=(Evp[,2]),
               S.Thick = soil.thick, pClay = clay100,
               pE = 1.0, bare = FALSE)$b #Moisture effects per month
xi.frame_VLC10=data.frame(years,rep(fT_VLC10*fW_VLC10,length.out=length(years)))

#To run the model, we use the function SoilR::RothCModel to initialize the model and create a SoilR Model object to which we will solve to calculate C stocks for each pool as
Model_VLC10=SoilR::RothCModel(
  t=years, #Simulation Length
  ks=c(k.DPM = 10*CTRM, k.RPM = 0.3*CTRM, k.BIO = 0.66*CTRM, k.HUM = 0.02*CTRM, k.IOM = 0*CTRM), #Decomposition rates for the different pools
  C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=0), #Initial amount of carbon for the 5 pools
  In=Scalar_LitterCinputs*1, #Amount of litter inputs by time
  FYM = Scalar_ManureCinputs*0, #Amount of Farm Yard Manure by time
  clay=clay100, #Percent clay in mineral soil
  xi=xi.frame_VLC10) #Loads the model

Ct_VLC10=getC(Model_VLC10) #Calculates stocks for each pool per month

#The results can be plotted with the commands
matplot(years, Ct_VLC10, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

VEC_Lit_VLC10 <- rep(M_Scalar_LitterCinputs*1, SimulationLength_months) #Creating a vector for Litter Inputs of Length equal of Simulation Length (in months)
VEC_Man_VLC10 <- rep(M_Scalar_ManureCinputs*0, SimulationLength_months) #Creating a vector for Manure Inputs of Length equal of Simulation Length (in months)

VEC_LitDF_VLC10 <- as.data.frame(VEC_Lit_VLC10) #Converting the Litter vector to a data frame
VEC_LitDF_VLC10$MNumber <- seq(from = 1, to = SimulationLength_months)
VEC_ManDF_VLC10 <- as.data.frame(VEC_Man_VLC10) #Converting the Manure vector to a data frame
VEC_ManDF_VLC10$MNumber <- seq(from = 1, to = SimulationLength_months)

sapply(VEC_LitDF_VLC10, class) #Check that class is numeric
sapply(VEC_ManDF_VLC10, class) #Check that class is numeric
LitterCinputs_VLC10=VEC_LitDF_VLC10   #Litter MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
ManureCinputs_VLC10=VEC_ManDF_VLC10 #Manure MONTHLY C inputs to soil in Mg/ha/yr - please note that Mg is megagrams, not milligrams
MCinputs_VLC10 <- merge(LitterCinputs_VLC10, ManureCinputs_VLC10, by = "MNumber")
MCinputs_VLC10$MInput_VLC10 <- MCinputs_VLC10$VEC_Lit_VLC10 + MCinputs_VLC10$VEC_Man_VLC10

#Change names of litter and manure columns for more common ones
colnames(MCinputs_VLC10)[which(names(MCinputs_VLC10) == "VEC_Lit_VLC10")] <- "LitterC_VLC10"
colnames(MCinputs_VLC10)[which(names(MCinputs_VLC10) == "VEC_Man_VLC10")] <- "ManureC_VLC10"

#Create a dataframe with the data considering the simulation length
ModelDF_VLC10 <- as.data.frame(Ct_VLC10)
colnames(ModelDF_VLC10) <- c('DPM_VLC10','RPM_VLC10','BIO_VLC10', 'HUM_VLC10', 'IOM_VLC10')
ModelDFS_VLC10 <- ModelDF_VLC10[1:SimulationLength_months, ]

#Create a column with the number of month of analysis
ModelDFS_VLC10$MNumber <- seq(from = 1, to = SimulationLength_months)

#Create a column that sums all pools
ModelDFS_VLC10$AllPools_VLC10 <- ModelDFS_VLC10$DPM_VLC10 + ModelDFS_VLC10$RPM_VLC10 + ModelDFS_VLC10$BIO_VLC10 + ModelDFS_VLC10$HUM_VLC10 + ModelDFS_VLC10$IOM_VLC10

#Create a column that sums all pools except IOM (static)
ModelDFS_VLC10$AllPools_noIOM_VLC10 <- ModelDFS_VLC10$DPM_VLC10 + ModelDFS_VLC10$RPM_VLC10 + ModelDFS_VLC10$BIO_VLC10 + ModelDFS_VLC10$HUM_VLC10

#Create a column that add the monthly input of C (manure and litter separately)
ModelDFSL_VLC10 <- merge(ModelDFS_VLC10, MCinputs_VLC10, by = "MNumber")

ModelDFSL_VLC10$MInput_VLC10 <- ModelDFSL_VLC10$LitterC_VLC10 + ModelDFSL_VLC10$ManureC_VLC10
#ModelDF_SL$MInput[1] <- 0

#Create a column that add the monthly C tail (carbon in pools (no IOM) + Monthly input of C)
ModelDFSL_VLC10$CTails_VLC10 <- ModelDFSL_VLC10$AllPools_noIOM_VLC10 + ModelDFSL_VLC10$MInput_VLC10

#Create Monthly Accumulated input of C
ModelDFSL_VLC10$AccumInput_VLC10 = ModelDFSL_VLC10$AccumInput_VLC10=cumsum(ModelDFSL_VLC10$MInput_VLC10)

#Create Monthly Growths for each carbon pool
ModelDFSL_VLC10$MGrowth_DPM_VLC10 <- ave(ModelDFSL_VLC10$DPM_VLC10, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC10$MGrowth_RPM_VLC10 <- ave(ModelDFSL_VLC10$RPM_VLC10, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC10$MGrowth_BIO_VLC10 <- ave(ModelDFSL_VLC10$BIO_VLC10, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC10$MGrowth_HUM_VLC10 <- ave(ModelDFSL_VLC10$HUM_VLC10, FUN = function(x) c(0, diff(x)))
ModelDFSL_VLC10$MGrowth_IOM_VLC10 <- ave(ModelDFSL_VLC10$IOM_VLC10, FUN = function(x) c(0, diff(x)))

#Create column for Monthly and Accumulated C-CO2 emissions
ModelDFSL_VLC10$M_CCO2_VLC10 <- ModelDFSL_VLC10$MInput_VLC10 - ModelDFSL_VLC10$MGrowth_DPM_VLC10 - ModelDFSL_VLC10$MGrowth_RPM_VLC10 - ModelDFSL_VLC10$MGrowth_BIO_VLC10 - ModelDFSL_VLC10$MGrowth_HUM_VLC10
ModelDFSL_VLC10$Accum_CCO2_VLC10 <- ModelDFSL_VLC10$AccumInput_VLC10 - ModelDFSL_VLC10$AllPools_noIOM_VLC10

#Set M_CCO2 and Accum_CCO2 first row value to 0 since otherwise it's an error since these parameters are calculated through balance
ModelDFSL_VLC10$M_CCO2_VLC10[1] <- 0
ModelDFSL_VLC10$Accum_CCO2_VLC10[1] <- 0

#Balance validation
ModelDFSL_VLC10$Balance_VLC10 <- ModelDFSL_VLC10$AccumInput_VLC10 - ModelDFSL_VLC10$Accum_CCO2_VLC10 - (ModelDFSL_VLC10$DPM_VLC10 + ModelDFSL_VLC10$RPM_VLC10 + ModelDFSL_VLC10$BIO_VLC10 + ModelDFSL_VLC10$HUM_VLC10)

#Convert C-CO2 emissions into CO2 emissions
ModelDFSL_VLC10$M_CO2_VLC10 <- ModelDFSL_VLC10$M_CCO2_VLC10 * 44/12
ModelDFSL_VLC10$Accum_CO2_VLC10 <- ModelDFSL_VLC10$Accum_CCO2_VLC10 * 44/12

#This model will be called VLC10C because implies a continuous input of C
ModelDFSL_VLC10C <- ModelDFSL_VLC10

#Export the dataframe
write_xlsx(ModelDFSL_VLC10C,"VXC_Models\\ModelDFSL_R_VLC10C.xlsx")

#Subtracting two models (t=x)-(t=x+1) to obtain the effect of a single year
#Create two equal models by using the above general script
#Create model t=x
ModelDFSLt0_VLC10 <- ModelDFSL_VLC10 #Create model t=x

#Create model t=x+1 by adding 12 rows full of zeros at the beginning and deleting the last 12 rows
ModelDFSLt1_VLC10.1 <- rbind(c(0:0), ModelDFSLt0_VLC10)
ModelDFSLt1_VLC10.2 <- rbind(c(0:0), ModelDFSLt1_VLC10.1)
ModelDFSLt1_VLC10.3 <- rbind(c(0:0), ModelDFSLt1_VLC10.2)
ModelDFSLt1_VLC10.4 <- rbind(c(0:0), ModelDFSLt1_VLC10.3)
ModelDFSLt1_VLC10.5 <- rbind(c(0:0), ModelDFSLt1_VLC10.4)
ModelDFSLt1_VLC10.6 <- rbind(c(0:0), ModelDFSLt1_VLC10.5)
ModelDFSLt1_VLC10.7 <- rbind(c(0:0), ModelDFSLt1_VLC10.6)
ModelDFSLt1_VLC10.8 <- rbind(c(0:0), ModelDFSLt1_VLC10.7)
ModelDFSLt1_VLC10.9 <- rbind(c(0:0), ModelDFSLt1_VLC10.8)
ModelDFSLt1_VLC10.10 <- rbind(c(0:0), ModelDFSLt1_VLC10.9)
ModelDFSLt1_VLC10.11 <- rbind(c(0:0), ModelDFSLt1_VLC10.10)
ModelDFSLt1_VLC10.12 <- rbind(c(0:0), ModelDFSLt1_VLC10.11)
ModelDFSLt1_VLC10.13 <- ModelDFSLt1_VLC10.12[-nrow(ModelDFSLt1_VLC10.12),]
ModelDFSLt1_VLC10.14 <- ModelDFSLt1_VLC10.13[-nrow(ModelDFSLt1_VLC10.13),]
ModelDFSLt1_VLC10.15 <- ModelDFSLt1_VLC10.14[-nrow(ModelDFSLt1_VLC10.14),]
ModelDFSLt1_VLC10.16 <- ModelDFSLt1_VLC10.15[-nrow(ModelDFSLt1_VLC10.15),]
ModelDFSLt1_VLC10.17 <- ModelDFSLt1_VLC10.16[-nrow(ModelDFSLt1_VLC10.16),]
ModelDFSLt1_VLC10.18 <- ModelDFSLt1_VLC10.17[-nrow(ModelDFSLt1_VLC10.17),]
ModelDFSLt1_VLC10.19 <- ModelDFSLt1_VLC10.18[-nrow(ModelDFSLt1_VLC10.18),]
ModelDFSLt1_VLC10.20 <- ModelDFSLt1_VLC10.19[-nrow(ModelDFSLt1_VLC10.19),]
ModelDFSLt1_VLC10.21 <- ModelDFSLt1_VLC10.20[-nrow(ModelDFSLt1_VLC10.20),]
ModelDFSLt1_VLC10.22 <- ModelDFSLt1_VLC10.21[-nrow(ModelDFSLt1_VLC10.21),]
ModelDFSLt1_VLC10.23 <- ModelDFSLt1_VLC10.22[-nrow(ModelDFSLt1_VLC10.22),]
ModelDFSLt1_VLC10.24 <- ModelDFSLt1_VLC10.23[-nrow(ModelDFSLt1_VLC10.23),]

ModelDFSLt1_VLC10 <- ModelDFSLt1_VLC10.24 #Create model t=x+1

#Subtract model (t=x) - (t=x+1)
ModelDFSL1y_VLC10 <- ModelDFSLt0_VLC10 - ModelDFSLt1_VLC10

#Re-do Month Number column because the substraction was also applied to it
ModelDFSL1y_VLC10$MNumber <- seq(from = 1, to = SimulationLength_months)

#This model will be called VLC10P because implies a one-off input of C
ModelDFSL_VLC10P <- ModelDFSL1y_VLC10

#Export the dataframe
write_xlsx(ModelDFSL_VLC10P,"VXP_Models\\ModelDFSL_R_VLC10P.xlsx")

#Plot 2: Geom_line with interactive plotly to represent carbon flows
P_CFluxI1y_VLC10 <- ggplot(ModelDFSL_VLC10P, aes(x = MNumber)) +
  geom_line(aes(y = DPM_VLC10, color = "DPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = RPM_VLC10, color="RPM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = BIO_VLC10, color = "BIO", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = HUM_VLC10, color="HUM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = IOM_VLC10, color = "IOM", linetype = "Output"), size = 0.5) +
  geom_line(aes(y = AccumInput_VLC10, color="AccumInput", linetype="Input"), size = 1) +
  geom_line(aes(y = Accum_CCO2_VLC10, color = "Accum_CCO2", linetype="Output"), size = 1) +
  xlab("Month number") + ylab("Accumulated Carbon Flows") +
  guides(color=guide_legend(title="Flow")) +
  guides(linetype=guide_legend(title="Input/Output"))
#facet_zoom(ylim = c(0, 25))

P_CFluxI1y_VLC10
ggplotly(P_CFluxI1y_VLC10)


#Plot 3: Interactive Yearly CO2 emissions #
MY <- 12
ModelDFSL_VLC10P_YCO2 <- ModelDFSL_VLC10P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CO2_VLC10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC10P_YCO2) <- c("Year", "Months", "AnnualCO2_VLC10")

PA_CO21y_VLC10 <- ggplot(ModelDFSL_VLC10P_YCO2, aes(x = Year, y = AnnualCO2_VLC10)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21y_VLC10
ggplotly(PA_CO21y_VLC10)

#Plot 3: Interactive Yearly CO2 emissions considering a delay
GWP100delay <- data.frame(   Year = 1:100,   GWP100 = c(     0.9923, 0.9846, 0.9768, 0.9690, 0.9612,     0.9534, 0.9456, 0.9378, 0.9299, 0.9220,     0.9141, 0.9062, 0.8982, 0.8902, 0.8822,     0.8742, 0.8662, 0.8581, 0.8500, 0.8419,     0.8338, 0.8256, 0.8174, 0.8092, 0.8010,     0.7927, 0.7844, 0.7761, 0.7677, 0.7594,     0.7509, 0.7425, 0.7340, 0.7255, 0.7170,     0.7084, 0.6998, 0.6912, 0.6825, 0.6738,     0.6650, 0.6562, 0.6474, 0.6385, 0.6296,     0.6207, 0.6117, 0.6027, 0.5936, 0.5845,     0.5753, 0.5661, 0.5568, 0.5475, 0.5381,     0.5287, 0.5193, 0.5097, 0.5002, 0.4905,     0.4808, 0.4711, 0.4613, 0.4514, 0.4415,     0.4315, 0.4214, 0.4113, 0.4011, 0.3908,     0.3804, 0.3700, 0.3595, 0.3489, 0.3382,     0.3275, 0.3166, 0.3057, 0.2947, 0.2835,     0.2723, 0.2609, 0.2495, 0.2379, 0.2262,     0.2143, 0.2023, 0.1902, 0.1778, 0.1653,     0.1525, 0.1395, 0.1262, 0.1125, 0.0985,     0.0839, 0.0688, 0.0531, 0.0365, 0.0188   ) )
ModelDFSL_VLC10P_YCO2D <- merge(ModelDFSL_VLC10P_YCO2, GWP100delay, by = "Year")
ModelDFSL_VLC10P_YCO2D$AnnualCO2D_VLC10 <- ModelDFSL_VLC10P_YCO2D$AnnualCO2_VLC10 * ModelDFSL_VLC10P_YCO2D$GWP100

PA_CO21yD_VLC10 <- ggplot(ModelDFSL_VLC10P_YCO2D, aes(x = Year, y = AnnualCO2D_VLC10)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly CO2 emissions")

PA_CO21yD_VLC10
ggplotly(PA_CO21yD_VLC10)

#Plot 4: Interactive Yearly C emissions #
MY <- 12
ModelDFSL_VLC10P_YC <- ModelDFSL_VLC10P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), sum = sum(M_CCO2_VLC10)) #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC10P_YC) <- c("Year", "Months", "AnnualCTail_VLC10")

PA_C1y_VLC10 <- ggplot(ModelDFSL_VLC10P_YC, aes(x = Year, y = AnnualCTail_VLC10)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C emissions")

PA_C1y_VLC10
ggplotly(PA_C1y_VLC10)

#Plot 5: Interactive Yearly C tails (remaining C in Soil) #
MY <- 12
ModelDFSL_VLC10P_YCT <- ModelDFSL_VLC10P %>%
  group_by(grp = as.integer(gl(n(), MY, n()))) %>%
  summarise(range = str_c(first(MNumber), last(MNumber), sep=" -- "), last = last(AllPools_noIOM_VLC10)) #Here is last because AllPools_nIOM is the size of the C pools #https://stackoverflow.com/questions/59505473/sum-every-3-rows
colnames(ModelDFSL_VLC10P_YCT) <- c("Year", "Months", "AnnualCTail_VLC10")

PA_CT1y_VLC10 <- ggplot(ModelDFSL_VLC10P_YCT, aes(x = Year, y = AnnualCTail_VLC10)) +
  geom_bar(aes(), stat = "identity", position = "stack") + ylab("Yearly C Tail (t/ha)")

PA_CT1y_VLC10
ggplotly(PA_CT1y_VLC10)

#Save Dataframes for Plots 3.1, 3.2, 4 and 5
write_xlsx(ModelDFSL_VLC10P_YCO2,"CO2Emissions_P_NoDelay\\ModelDFSL_R_CO2_VLC10P.xlsx") #Yearly CO2 emissions (no delay)
write_xlsx(ModelDFSL_VLC10P_YCO2D,"CO2Emissions_P_Delay\\ModelDFSL_R_CO2D_VLC10P.xlsx") #Yearly CO2 emissions (delay)
write_xlsx(ModelDFSL_VLC10P_YC,"CEmissions_P\\ModelDFSL_R_C_VLC10P.xlsx") #Yearly C emissions
write_xlsx(ModelDFSL_VLC10P_YCT,"CTails_P\\ModelDFSL_R_C_VLC10P.xlsx") #Yearly C emissions





}
