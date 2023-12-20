library(here)
library(tidyverse)
library(deSolve)
library(nsga2R)  
library(readxl)


#Step = 1 Day

START <- 0
FINISH <- 365*nyears
STEP <- 1
mg_to_kg <- 1000000
set.seed(NULL)

simTime <- seq(START, FINISH, by = STEP)

#### Select Species of Interest

SPECIES = "Macrocystis"

## Extract Name of Min/Max Columns
nm_op_min = paste0("Opti_Min_",SPECIES)
nm_op_max = paste0("Opti_Max_",SPECIES)

##Define aux/parameter/min/max/stock vectors

input_data <- read_xls(here("Input/auxs.xls")) %>% as_tibble() %>% 
  select(var_type, var, all_of(SPECIES), nm_op_min, nm_op_max) 

auxs <- input_data %>%
  as_tibble() %>% 
  drop_na(SPECIES) %>% 
  filter(var_type == "auxs") 

auxs <- structure(as.numeric(pull(auxs,get(SPECIES))),names=as.character(auxs$var))

env <- input_data %>% 
  as_tibble() %>% 
  drop_na(SPECIES) %>% 
  filter(var_type == "env")

env <- structure(as.numeric(pull(env, get(SPECIES))), names = as.character(env$var))

stocks_o <- input_data %>% 
  as_tibble() %>% 
  drop_na(SPECIES) %>% 
  filter(var_type == "stocks")

stocks_o <- structure(as.numeric(pull(stocks_o,get(SPECIES))),names=as.character(stocks_o$var))

aHarvest_0 = 0

parms <- input_data %>% 
  as_tibble() %>% 
  drop_na(SPECIES) %>% 
  filter(var_type == "parms")

parms <- structure(as.numeric(pull(parms,get(SPECIES))),names=as.character(parms$var))

nharvest <- auxs["aHarvest_Freq"]*nyears

harvest_min <- structure(rep(0,nharvest), names = paste("aHarvest",seq(1:nharvest),sep = "_"))

harvest_max <- harvest_min + 1

parms <- c(parms,harvest_min)

parms_nh <- parms

# Boundary parameter values
opti_min <- input_data %>% 
  as_tibble() %>% 
  filter(var_type == "parms") %>% 
  select(var,nm_op_min)

opti_min <- c(structure(pull(opti_min,nm_op_min),names=opti_min$var),harvest_min)

opti_max <- input_data %>% 
  as_tibble() %>% 
  filter(var_type == "parms") %>% 
  select(var,nm_op_max) 

opti_max <- c(structure(pull(opti_max,nm_op_max),names=opti_max$var), harvest_max)

# Initialize stocks
sDry_Biomass <-c(auxs["aSeed_Size"],rep(0,(FINISH-1)))

stocks_D <- structure(as.numeric(sDry_Biomass), names = paste("D_",seq(1:FINISH), sep ="")) 

stocks <- c(stocks_D, stocks_o)

# Set Nitrogen concentrations according to Hadley 2015
stocks["sNH4"] = as.list(env)$aNO3_Mean + as.list(env)$aNO3_SD*sin(2*pi/365 + as.list(env)$aTime_to_Peak_N) 

stocks["sNO3"] = as.list(env)$aNH4_Mean + as.list(env)$aNH4_SD*sin(2*pi/365 + as.list(env)$aTime_to_Peak_N)

stocks["sN_Stored"] = sum(sDry_Biomass)*auxs["aQ_Min"]#/(auxs["aCult_Depth"]*auxs["aArea_Farm"])

# Set Objectives
objectives = c("sFood", "sBiodiversity_Benefit", "sCO2e_Mitigation","sProfit")

source("scenarios.R")

# Consolidate inputs
vars <- c(auxs,parms,sens,env)

### Set Initial Conditions
stocks["sProfit"] <- stocks["sProfit"] - vars["aInitial_Cost"]*vars["aArea_Farm"]

stocks["sCO2e_Mitigation"] <- stocks["sCO2e_Mitigation"] - vars["aInitial_Carbon"]*vars["aArea_Farm"]





