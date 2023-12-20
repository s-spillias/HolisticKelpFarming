
sens <- input_data %>%
  as_tibble() %>% 
  drop_na(SPECIES) %>% 
  filter(var_type == "sens") 

sens <- structure(as.numeric(pull(sens,get(SPECIES))),names=as.character(sens$var))

sens_mins <- t(matrix(sens,nrow = length(sens),ncol = length(sens)))

sens_maxs <- sens_mins

sens_bounds <- input_data %>% filter(var_type == "sens") %>% select(starts_with("Opti"))

diag(sens_mins) <- sens_bounds$Opti_Min_Macrocystis

diag(sens_maxs) <- sens_bounds$Opti_Max_Macrocystis

sens_opts <- as.data.frame(rbind(sens,sens_mins#,sens_maxs
))

names(sens_opts) <- names(sens)
row.names(sens_opts) <- c("BASE",names(sens))


## ADD in Double Carbon Scenario 
sens_opts <- sens_opts %>% 
  bind_rows(sens_opts["aCarbon_Price",]) %>%  
  `rownames<-`( c(row.names(sens_opts),"aCarbon_Price_x2")) 
sens_opts["aCarbon_Price_x2","aCarbon_Price"] <- 2*sens_opts["aCarbon_Price","aCarbon_Price"]
###

## ADD in Half Sequestration Scenario 
sens_opts <- sens_opts %>% 
  bind_rows(sens_opts["aProportion_Sequestration",]) %>%  
  `rownames<-`( c(row.names(sens_opts),"aProportion_Sequestration_x0.5")) 
sens_opts["aProportion_Sequestration_x0.5","aProportion_Sequestration"] <- 0.5*sens_opts["BASE","aProportion_Sequestration"]
###

## ADD in X10 Carbon Scenario 
sens_opts <- sens_opts %>% 
  bind_rows(sens_opts["aCarbon_Price",]) %>%  
  `rownames<-`( c(row.names(sens_opts),"aCarbon_Price_x10")) 
sens_opts["aCarbon_Price_x10","aCarbon_Price"] <- 40*sens_opts["aCarbon_Price","aCarbon_Price"]
###

## ADD in X20 Carbon Scenario 
sens_opts <- sens_opts %>% 
  bind_rows(sens_opts["aCarbon_Price",]) %>%  
  `rownames<-`( c(row.names(sens_opts),"aCarbon_Price_x20")) 
sens_opts["aCarbon_Price_x20","aCarbon_Price"] <- 20*sens_opts["aCarbon_Price","aCarbon_Price"]
###

## ADD in Distance `Zero1km Scenario
sens_opts <- sens_opts %>% 
  bind_rows(sens_opts["aDistance_Sink",]) %>%  
  `rownames<-`( c(row.names(sens_opts),"aDistance_Sink_1")) 
sens_opts["aDistance_Sink_1","aDistance_Sink"] <- 1
###

## ADD in Really Good Fuel Scenario
sens_opts <- sens_opts %>% 
  bind_rows(sens_opts["aFuel_Efficiency",]) %>%  
  `rownames<-`( c(row.names(sens_opts),"aFuel_Efficiency_x10")) 
sens_opts["aFuel_Efficiency_x10","aFuel_Efficiency"] <- 0.1*sens_opts["aFuel_Efficiency_x10","aFuel_Efficiency"]
###
## ADD in Close Sink and Carbon Price Scenario
sens_opts <- sens_opts %>% 
  bind_rows(sens_opts["aDistance_Sink",]) %>%  
  `rownames<-`( c(row.names(sens_opts),"CloseSink_CarbonPrice")) 
sens_opts["CloseSink_CarbonPrice","aCarbon_Price"] <- sens_opts["aCarbon_Price","aCarbon_Price"]
###
## ADD in NO Sequestration Scenario
sens_opts <- sens_opts %>% 
  bind_rows(sens_opts["aProportion_Sequestration",]) %>%  
  `rownames<-`( c(row.names(sens_opts),"No_Sequestration")) 
sens_opts["No_Sequestration","aProportion_Sequestration"] <- 0
###

## ADD in NO Sequestration Scenario
sens_opts <- sens_opts %>% 
  bind_rows(sens_opts["aInitial_Biodiversity",]) %>%  
  `rownames<-`( c(row.names(sens_opts),"Habitat_Improvement")) 
sens_opts["Habitat_Improvement","aInitial_Biodiversity"] <- 0.2
###

sens_opts <- sens_opts[c(1,17,11,7,3,14,2,15,4,12,9,5,6,18,8,10,13,16),]#4:8,10:13,9,3,2,14,15,16,17),]
sens_names = c("Baseline",
               "No Sequestration",
               "Low Sequestration",
               "High Sequestration",
               "Closer Sink",
               "Farm at Sink",
               "Better Fuel Efficiency",
               "Best Fuel Efficiency",
               "Carbon Price",
               "High Carbon Price",
               "Nitrogen Price",
               "Nitrogen Input",
               "Habitat Replacement",
               "Habitat Improvement",
               "Dep. Connected",
               "Dep. Medium Carbon Price",
               "Dep. Carbon Price x20",
               "Dep. CloseSink CarbonPrice"
)

#sens_plot_names <- sens_names[c(1,2,9,11,3,5,8,4,12,14,13,15)] %>% rev


price_sens <- c(1,9,10,11)
sink_dist_sens <- c(1,5,6)
sequ_sens <- c(1:4)
fuel_sens <- c(1,7,8)
others <- c(1,12,13,14)
deprecated <- c(15,16,17,18)
#base_nitr <- c(1,3)

all_sens <- c(sequ_sens, sink_dist_sens, fuel_sens, price_sens, others) %>% unique

all_naive <- all_sens

to_sens <- lst(all_sens,
               price_sens,
               sink_dist_sens,
               fuel_sens,
               others,
               all_naive,
               # base_nitr,
               sequ_sens)
