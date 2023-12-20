conv_leak <- function(x,y,z) {
  x = lag(x)*(1-z)
  x[1] = y
  return(x)
}

nzmean <- function(y) {
  x = y
  x[x== 0] <- NA
  sum(x*seq(1:length(x)), na.rm = TRUE)/sum(x, na.rm = TRUE)
  
}

##
squarewave <- function (t,z) {  
  
  ifelse(((t %% z) < 1),1,0)
}
##
squarewave2 <- function (t,z,y) {
  
  ifelse(((t %% z) < z/y),0,1)
}
#test <- seq(1:120)
#squarewave2(test-1,600,600/6)
##120
mysamp <- function(n, m, s, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, m, s)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  }  
  stop(simpleError("Not enough values to sample from. Try increasing nnorm."))
}

nzero_stock <- function(x,y) {ifelse(x + y < 0, -y, x)}

f.harvest <- function(x,y) {
  paste("aHarvest",ceiling(y*x/365), sep = "_")
}

aCf_Scale = auxs["aCf_Scale"]
aCf_Shape = auxs["aCf_Shape"]

aProbability_Loss =  ((aCf_Shape/aCf_Scale)*(c(1:FINISH)/aCf_Scale)^(aCf_Shape - 1))*exp((c(1:FINISH)/aCf_Scale)^aCf_Shape) ### Rodriguez et al. 2013, probability of frond loss at age, t (in days)

aProbability_Loss[aProbability_Loss>1] <- 1


f.irr <- Vectorize(function(r)
{ (2.087/(sqrt(2*pi)*aSigma))*
    exp(-0.5*((log(r)-aMu)/aSigma)^2)
})




####### CHANGED RATIOS OF QMIN need to adjust things. ALSO DROPBOX LINK
model <- function(time, stocks, auxs, parms, sens, env = parent.frame()){
  with(as.list(c(stocks, auxs,env)),
       { 
         ## External Conditions 
         
         aTime_Month = time*12/365
         
         aVolume_Farm = aCult_Depth*aArea_Farm
         
         aTemperature = aTemp_Mean + aTemp_SD*sin(2*pi*time/365 + aTime_to_Peak)
         
         aIrradiance = aIrr_Mean + aIrr_SD*sin(2*pi*time/365 + aTime_to_Peak) 
         
         #  aSigma =  unname(sqrt(log((aIrr_Ran/aIrr_Sat)^2 + 1))) 
         
          # aMu =  unname(log(aIrr_Sat) - .5*(aSigma^2))
         
         # aDetritus_Ref = aVolume_Farm*
         
         aDry_Biomass = sum(stocks[1:FINISH])
         
         aN_Fixed = aDry_Biomass*aQ_Min
         
         aHeight = ((aCf_Height*(aN_Fixed)/aNum_Fronds)^1.047)
         
         aVolume_MA = aHeight*aArea_Farm
         
         aNO3_Ref = (aNO3_Mean + aNO3_SD*sin(2*pi*time/365 + aTime_to_Peak_N))#*aVolume_MA
         
         aNH4_Ref = aNH4_Mean + aNH4_SD*sin(2*pi*time/365 + aTime_to_Peak_N)#*aVolume_MA
         
         aSelf_Shading = (aN_Fixed)*aShade_Nf*max(aHeight/aCult_Depth,1)*min(aHeight, aCult_Depth)^(-1) # from Hadley 2015; 
         
         aShading = aSelf_Shading + aIrr_Att
         
         aIrr_Canopy = aIrradiance*exp(-aShading*aCult_Depth) # Irradiance at top of macroalgal canopy
         
         #  aLim_Temp1 = #max(.2,
         #    exp(-2*3*((aTemperature-aTemp_Opt)/aTemp_Ran)^2)
         #  #  ) ## Temperature limitation multiplier; Martins&Marques2002; ***Other one is better with flipped order.
         
         aLim_Irr = (exp(1)/(aHeight*aShading))*(exp(-aIrr_Canopy*exp(-aShading*aHeight)/aIrr_Sat)-exp(-aIrr_Canopy/aIrr_Sat))
         
         
         aLim_Temp = 1/(exp(-(aTemp_Opt - aTemperature)/aTemp_Ran)) ## Temperature limitation multiplier; Hadley 2015; seems not to make sense. ### FLIPPED aTemp_Opt and aTemperature
         
          # aLim_Irr1 = 4.121*(1/(sqrt(2*pi)*aSigma))* ### Not used in model
          #   exp(-0.5*((log(aIrr_Canopy)-aMu)/aSigma)^2)
         
         
         aInternal_N_Quota = aQ_Min*(1+sN_Stored/aN_Fixed) ## kg N per kg DW 
         
         aLim_Nutrient = (aInternal_N_Quota - aQ_Min)/(aInternal_N_Quota - aHalf_Growth)
         
         aViability = aLim_Temp*aLim_Irr*aLim_Nutrient
         
         ## Growth Parameters
         
         aUptake_NH4 = aMax_Uptake_NH4*(sNH4/(aHalf_Sat_NH4 + sNH4))*((aQ_Max-aInternal_N_Quota)/(aQ_Max - aQ_Min))
         
         aUptake_NO3 = 
           aMax_Uptake_NO3*(sNO3/(aHalf_Sat_NO3 + sNO3))*((aQ_Max-aInternal_N_Quota)/(aQ_Max - aQ_Min))
         
         
         aSurface_Area1 = aDry_Biomass*(1/aRatio_Weight_Area) #Broch and Slagstad
         
         aSurface_Area = 5.13*aDry_Biomass^0.78 ### m2/kg; Using Model from Starko & Martone 2016
         
         aOld_Biomass = sum(stocks[aOld_Age:FINISH])
         
         aProduction_Rate = aMax_Production*aLim_Nutrient*aLim_Irr*aLim_Temp*((aDry_Biomass - aOld_Biomass)/aDry_Biomass)
         
         aProduction  = aProduction_Rate*aDry_Biomass# g to kg conversion ### N_stored is in grams ## 1 - 2 cm per day ## 196 days, 2 mm -> 45 cm; max 1 meter is 2kg wet, 4 fronded; 1 line per meter.
         
         ## Farming Module
         
         aHarvest_Period = round(365/aHarvest_Freq) ## Parameter of interest
         
         aSeed = ifelse(aDry_Biomass<=1,aSeed_Size*squarewave(time + 1,aHarvest_Period),0)
         
         ## 1 if harvest that month
         
         aHarvest_Proportion = round(squarewave(time + 1,aHarvest_Period)*get(f.harvest((time) ,aHarvest_Freq)),1)
         
         aHarvest = aDry_Biomass*aHarvest_Proportion*aArea_Farm/1000 # to convert g to kg
         
         aGrazing = aGrazing_Factor*(1-aHerb_Exclusion)*ifelse(!first,sBiodiversity,0)
         
         aLoss = min(aGrazing + aHarvest_Proportion,1)
         
         aBmass_1_Day = conv_leak(stocks[1:FINISH], aProduction, aLoss) 
         #####               
         aLoss_Erosion = sum(aBmass_1_Day*aProbability_Loss)
         
         aTrips_Harvest = ceiling(aHarvest/aHarvest_Capacity) ## How many trips are needed to harvest total?
         
         aTrips_Seed = ifelse(aSeed != 0, 1, 0)
         
         aTrips_Maintenance = squarewave(time + 14, round(365/aTrips_Maintenance_freq))
         
         aTrips = aTrips_Harvest + aTrips_Maintenance + aTrips_Seed
         
         ## Eco Module
         
         aAge_Mean = nzmean(stocks[1:FINISH]*c(1:FINISH))
         
         if(first){
           aPredicted_Biodiversity = 0
         } else {
         aPredicted_Biodiversity =  aCf_Age*aAge_Mean + aCf_SA*aSurface_Area # + aConnectivity ### Need to get Cf values from somewhere ALSO Does connectivity fit in? Or time since disturbance? Mick Keough, UniMelb
         #*Michael look at fouling literature OR just estimate exported production
         }
        
         aRefresh_Rate = aRefresh_Rate_Ref*(1-aRefresh_Reduction*(aDry_Biomass/aCarrying_Capacity))
         
          ## Market Module
         
       #  aFeed_Production = aFeed_Proportion*aHarvest
         
         aFood_Production = aHarvest*round(aFood_Proportion,1)
         
         #aFuel_Production = aFuel_Proportion*aFuel_Processing_Co*aHarvest
         
        # aFeed_Revenue = aFeed_Price*aFeed_Production
         
         aFood_Revenue = aFood_Price*aFood_Production
         
         # aFuel_Revenue = aFuel_Price*aFuel_Production
         
       ## Carbon Mitigation Module
         
        # aFeed_Consumption = sFeed_Biomass/aHarvest_Period
         
       #  aFood_From_Feed = aFeed_Consumption*aLivestock_Conversion
         
       #  aAvoided_Methane = aFeed_Consumption*aCH4_Mitigation_Asp*aCH4_CO2e_Conversion*aIs_Asp
         
         aExported_Carbon = aRefresh_Rate*((sDetritus-aDetritus_Ref)/aQ_Min)*aArea_Farm*aDry_toCarbon_Ratio
         
         aSequestered_Erosion = aProportion_Sequestration*aExported_Carbon*aCarbon_CO2_conversion
         
         aMaintenance_Carbon = (aInitial_Carbon*aArea_Farm/(aTrips_Maintenance_freq*aFarm_Life))*aTrips_Maintenance
         
         
         aCarbonSeq_Proportion = (1 - (aFood_Proportion # + aFeed_Proportion #+ aFuel_Proportion
         ))
         
         aManual_Sequestration = aCarbonSeq_Proportion*aHarvest
         
        # aCarbon_Pile_Out = ifelse(sCarbon_Pile>=aHarvest_Capacity,aHarvest_Capacity,0)
         
         aCarbon_Processing = aManual_Sequestration*aSink_Processing
 
         aTrip_Seq = ceiling(aManual_Sequestration/aHarvest_Capacity)
         
         aCarbon_Manual_Seq = aManual_Sequestration*aDry_toCarbon_Ratio*aCarbon_CO2_conversion
         
         aSinking_Distance = 2*aTrip_Seq*aDistance_Sink
         
         aCarbon_Benefit = aSequestered_Erosion + aCarbon_Manual_Seq #+ aAvoided_Methane 
         
         aNitrogen_Removed = aHarvest*aQ_Min/1000 ## convert harvest which is kg to grams of Nitrogen and then to kg of nitrogen
         + (sN_Stored*aHarvest_Proportion*aArea_Farm)/1000000 ## Convert mg/m3 to kg
         ## N g
         
         aNitrogen_Revenue = aNitrogen_Price*(aNitrogen_Removed)
         
         aHarvesting_Distance = 2*aDistance_Port*aTrips
         
         aFuel_Consumed = (aHarvesting_Distance + aSinking_Distance)*aFuel_Efficiency
         
         aFuel_Cost = aFuel_Consumed*aFuel_Price
         
         aLabour_Cost = (aTrips+aTrip_Seq)*aWages*aPersonnel*(1-aAutomation)
         
         aMaintenance_Cost = aMaterials_Cost*aTrips_Maintenance*aArea_Farm
         
         aCarbon_Loss = aFuel_Consumed*aCO2e_Fuel_Burned + aMaintenance_Carbon + aCarbon_Processing
         
         aCarbon_Revenue = aCarbon_Price*(aCarbon_Benefit - aCarbon_Loss)
         
         aCost = aFuel_Cost + aMaintenance_Cost + aLabour_Cost
         
         aRevenue = aFood_Revenue + aCarbon_Revenue + aNitrogen_Revenue #aFeed_Revenue + # + aFuel_Revenue 
         
         ######## ###### Stock Dx/dts   
         
         dDry_Biomass = aBmass_1_Day*(1-aProbability_Loss) - stocks[1:FINISH]
         
         dBB_dt = sBiodiversity - (aInitial_Biodiversity/FINISH)
         dB_dt = (1/aBiodiversity_Lag)*(aPredicted_Biodiversity - sBiodiversity) - aHarvest_Proportion*sBiodiversity
         dF_dt = aFood_Production #+ aFood_From_Feed 
         dC_dt = aCarbon_Benefit - aCarbon_Loss
        # dFe_dt = 0 # aFeed_Production - aFeed_Consumption
         dP_dt = aRevenue - aCost#/1000 # converts to 1000 AUD
         
         
         aRemineralized_NH4 = aRemineralization_Rate*sDetritus
         aUptaken_NH4 = aUptake_NH4*min(aHeight/aCult_Depth,1)*(aN_Fixed/aQ_Min)
         aNitrified_NH4 = aNitrification_Rate*sNH4
         aRefresh_NH4 = aRefresh_Rate*(aNH4_Ref - (sNH4 + aRemineralized_NH4 + aN_IMTA - 
                                         (aUptaken_NH4 + aNitrified_NH4)))
         
         dNH4_dt = max(-sNH4,aN_IMTA +
           aRefresh_NH4 + aRemineralized_NH4 - 
                         (aUptaken_NH4 + aNitrified_NH4) ) ## g N per m3 per day
        
         aUptaken_NO3 = aUptake_NO3*min(aHeight/aCult_Depth,1)*(aN_Fixed/aQ_Min)
         aRefresh_NO3 = aRefresh_Rate*(aNO3_Ref-sNO3 + aNitrified_NH4 - aUptaken_NO3)
         
         dNO3_dt = max(-sNO3,
           aRefresh_NO3 + aNitrified_NH4 - aUptaken_NO3) ## g N per m3 per day
         
         dN_Stored_dt = (aUptaken_NH4 + aUptaken_NO3) - sN_Stored*(min( aProduction_Rate + # fixing
                                                                          sum(aProbability_Loss*stocks[1:FINISH])/sum(stocks[1:FINISH]) # erosion 
                                                                        + aLoss,1)) # harvest and grazing ## g N per m2 per day
            ## N g/m3
         dDetritus_dt = aRefresh_Rate*(aDetritus_Ref - sDetritus) + aLoss_Erosion - aRemineralization_Rate*sDetritus
            ## N g/m3
         dN_Removed = aNitrogen_Removed
            ## N kg
        # dCarbon_Pile = aCarbon_Pile_In - aCarbon_Pile_Out
         return(list(c(
           dDry_Biomass,
           dBB_dt,
           dB_dt,
           dF_dt, 
           dC_dt,
         #  dFe_dt,
           dP_dt,
           dNH4_dt,
           dNO3_dt,
           dN_Stored_dt,
           dDetritus_dt,
           dN_Removed
         #  dCarbon_Pile
         ),
         #          Seed = aSeed,
         #         Profit = sProfit,
         #       Delta_Biodiversity = aDelta_Biodiversity, 
         Biodiversity = sBiodiversity, 
         Dry_Biomass = aDry_Biomass, 
         Production = aProduction,
         Grazing = aGrazing,
         Viability = aViability,
         Revenue = aRevenue,
         #  Nf_New = aNf_New,
         #  Nf_Foul = aNf_Foul,
         #  Nf_Sen = aNf_Sen,
         Loss = aLoss,
         Production_Rate = aProduction_Rate,
         Age_Mean = aAge_Mean,
         Surface_Area = aSurface_Area,
         Mitigation = aCarbon_Benefit,
         Refresh_NH4 = aRefresh_NH4,
         Refresh_NO3 = aRefresh_NO3,
         #Self_Shading = aSelf_Shading,
         Harvest = aHarvest,
         Harvest_Proportion = aHarvest_Proportion,
         Sequestered_Erosion = aSequestered_Erosion,
         Carbon_Manual_Seq = aCarbon_Manual_Seq,
         Carbon_Benefit = aCarbon_Benefit,
         Nitrogen_Revenue = aNitrogen_Revenue,
         #Nitrogen_Fixed = aN_Fixed,
         #Nitrogen_Stored = sN_Stored,
         NH4_Ref = aNH4_Ref,
         NO3_Ref = aNO3_Ref,
       #  Carbon_Pile = sCarbon_Pile,
         N_Removed = sN_Removed,
         Detritus = sDetritus,
         dNH4_dt = dNH4_dt,
         dNO3_dt = dNO3_dt,
         Loss_Erosion = aLoss_Erosion,
         Self_Shading = aSelf_Shading,
         Irr_Canopy = aIrr_Canopy,
         Food_Production = aFood_Production,
        # Feed_Production = aFeed_Production,
         Maintenance_Carbon = aMaintenance_Carbon,
         Carbon_Loss = aCarbon_Loss,
        Sinking_Distance = aSinking_Distance,
         Height = aHeight,
         Uptake_NH4 = aUptake_NH4,
         Uptake_NO3 = aUptake_NO3,
         dN_Stored_dt = dN_Stored_dt,
         Volume_MA = aVolume_MA,
         N_Fixed = aN_Fixed,
         Cost = aCost,
         N_Stored = sN_Stored,
         Refresh_Rate = aRefresh_Rate,
         Trips_Maintenance = aTrips_Maintenance,
         #        CO2e_Mitigation = sCO2e_Mitigation, 
         #        Food = sFood, 
         #      Carbon_Capture = dC_dt, 
         #     Food_Supply = dF_dt, 
         #    Irradiance = aIrradiance,
         Lim_Irr = aLim_Irr,
         # Lim_Irr1 = aLim_Irr1,
         #  Temperature = aTemperature,
         Lim_Temp = aLim_Temp,
         #Lim_Temp1 = aLim_Temp1,
         Lim_Nutrient = aLim_Nutrient,
         Temperature = aTemperature,
         Irradiance = aIrradiance,
         Trips = aTrips,
         Remineralized_NH4 = aRemineralized_NH4,
         Uptaken_NH4 = aUptaken_NH4,
         Uptaken_NO3 = aUptaken_NO3,
         Nitrified_NH4 = aNitrified_NH4,
         Refresh_Rate = aRefresh_Rate,
         Internal_N_Quota = aInternal_N_Quota,
         Predicted_Biodiversity = aPredicted_Biodiversity,
         Shading = aShading,
         Fuel_Cost = aFuel_Cost,
        Refresh_NH4 = aRefresh_NH4,
         Maintenance_Cost = aMaintenance_Cost,
         Labour_Cost = aLabour_Cost
         #  Sequestered_Erosion = aSequestered_Erosion
         ))
       })
}


#to_plot = c("Lim_Nutrient","dNH4_dt","aInternal_N_Quota","dN_Stored_dt")
plot.model <- function(parms,to_plot = "Dry_Biomass"){
  
  o <- as.data.frame(data.frame(ode(y = stocks, times = simTime, func = model, parms = c(parms,auxs,sens,env), method = "euler")))
  nam <- names(o %>% dplyr::select(!starts_with("D_"),-time))
  data_long <- o %>% 
    gather(variable, value,nam) %>% 
    select(time, variable, value)
  
  data_long$variable <- factor(data_long$variable, levels = nam)
  
  
  #(SGR = 100*log((o %>% select(Dry_Biomass) %>% slice_tail()) - auxs["aSeed_Size"])/FINISH)
  
  ggplot(data = data_long %>% filter(variable %in% to_plot)) +
    geom_line(aes(x = time, y = value)) +
    facet_wrap(~variable, scales = "free") +
    theme_classic()+
    xlab("Days") +
    ylab("Kelp Biomass (Dry Weight, ")
}
