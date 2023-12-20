## Run Before HPC for loop.

## Assumed Production:
## 25 kg(ww) per m2 per year (i.e. two harvests at 12.5kg ww per m2) 
### FROM http://marineagronomy.org/sites/default/files/D6-4%20Life%20Cycle%20Assessment%20of%20biofuels%20from%20seaweed.pdf

source(here("Code/3_objectives.R"))
source(here("Code/3a_nsga2R_vol.R"))

## Define Biodiversity Parameters
first = TRUE
p = structure(rep(0,length(parms)), names=names(parms))
out <-  as.data.frame(data.frame(
  ode(y = stocks, times = simTime, func = model, parms = c(p,auxs,sens,env),
      method = "euler")) )
o <- out %>% 
  select(Age_Mean,Surface_Area) %>% 
  apply(2,max)

aCf_Age = 0.5/(o["Age_Mean"]*FINISH)
aCf_SA = 0.5/(o["Surface_Area"]*FINISH)   

#FINISH =  aCf_Age*aAge_Mean + aCf_SA*aSurface_Area

t_max <- out %>% slice_max(Dry_Biomass)
t_0 <- out %>% slice_head(n=1)

rm(o)
first = FALSE

out <-  as.data.frame(data.frame(
  ode(y = stocks, times = simTime, func = model, parms = c(p,auxs,sens,env),
      method = "euler")) )

SGR = 100*log((t_max$Dry_Biomass - t_0$Dry_Biomass))/t_max$time
carr_cap <- ((out %>% tail(FINISH*(1/3)))$Dry_Biomass %>% 
               mean())/1000 %>% #### convert g to kg / m2
  setNames("Carrying_Capacity")

rm(p)

#####


