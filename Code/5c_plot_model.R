


pubnames <- c("Food (t)",
              "Habitat Quality (1)",
              "Carbon (1 kt CO2e)",
              "Income (million AUD)",
              "Standing Biomass (t)",
              "Erosion (kg/d)",
              "Carbon Harvest (t)",
              "Feed Harvest (t)",
              "Food Produced (kt)",
              "Cost (1000 AUD)",
              "Revenue (1000 AUD)",
              "Emissions (kg CO2e)")

var_names <- c("Food_Production",
               "sBiodiversity_Benefit",
               "sCO2e_Mitigation",
               "sProfit",
               "Dry_Biomass",
               "Loss_Erosion",
               "Carbon_Manual_Seq",
               "Feed_Production",
               "sFood",
               "Cost",
               "Revenue",
               "Carbon_Loss")
unit_mod <- c(1000,
              1,
              1000000,
              1000000,
              1000,
              1,
              1000,
              1000,
              1000000,
              1000,
              1000,
              1 ) %>% setNames(var_names)

#to_plot = c("Lim_Nutrient","dNH4_dt","aInternal_N_Quota","dN_Stored_dt")
plot.model <- function(parms,to_plot = "Dry_Biomass"){
 
o <<- as.data.frame(data.frame(ode(y = stocks, times = simTime, func = model, parms = c(parms,auxs,sens,env), method = "euler")))
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

#plot.model(parms)

extract.parms <- function(sens,index){
  df_result %>% dplyr::select(starts_with("a")) %>% filter(rowname == index)
}

plot.parms <- function(parms){
  parms %>% as_tibble() %>% bind_cols(names(parms)) %>% setNames(c("value","parameter")) %>% relocate(parameter) %>% 
    mutate(parameter = factor(parameter, levels = str_sort(.$parameter, numeric = TRUE)),
      type = ifelse(str_detect(parameter,"aHarvest"),"Harvest","Other"))  %>% 
    ggplot(aes(x = parameter,y=value, fill = type)) +
    geom_col()+
    theme_classic()+
    ylim(0,1)+
    ylab("Proportion") +
    theme(axis.text = element_text(angle = 45,
                                   hjust = 1))
}

#plot.parms(parms)
