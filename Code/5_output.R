library(here)

nyears = 1
remove_food <- TRUE

source(here("Code/1_inputs.R"))
source(here("Code/2_SnF_model.R"))
source(here("Code/3_objectives.R"))
library(cluster)
# library(factoextra)
library(NbClust)
library(beyonce)
library(patchwork)

obj <- c("Food","Income","Habitat","Carbon")

source(here("Code/5c_plot_model.R"))

sens_opts %>% 
  mutate(sens = row_number())

rds_files <- list.files(here("Output/"), full.names = TRUE, pattern = "results")
rds_files <- rds_files[!str_detect(rds_files,"_naive")]
sens_run <- list.files(here("Output/"), full.names = FALSE, pattern = "results") %>% lapply(function(x) str_extract_all(x,"\\(?[0-9,.]+\\)?")[[1]][1]) %>% unlist()
rds_obj <- lapply(rds_files, function(x) {readRDS(x)})
df_result <- lapply(1:length(rds_files), function(x) {
  readRDS(rds_files[x])[["df_result"]] %>% # bind_rows(readRDS(x)["naive_run"]) %>%  
    mutate(optimized = "Optimized")  %>% 
    mutate(Profit_05 = ifelse(sProfit >= -1*(max(sProfit)-min(sProfit))/20, "Income", ""), #### Identify Solutions with less than 5 % loss in one objective.
           Biodiversity_05 = ifelse(sBiodiversity_Benefit >= -1*(max(sBiodiversity_Benefit)-min(sBiodiversity_Benefit))/20, "Habitat", ""),
           Climate_05 = ifelse(sCO2e_Mitigation >= -1*(max(sCO2e_Mitigation)-min(sCO2e_Mitigation))/20, "Carbon", ""),
           Food_05 = ifelse(sFood >= -1*(max(sFood)-min(sFood))/20, "Food", "")) %>% 
    mutate(missing = ifelse(sProfit >= 0, 1, 0) + ifelse(sBiodiversity_Benefit >= 0, 10,0) +
             ifelse(sCO2e_Mitigation >= 0, 100,0) + 
             ifelse(sFood >=0, 1000, 0)) %>% 
    #  mutate(missing = obj[!obj %in% (str_split(Benefits," ") %>% unlist() )] ) %>% 
       {if (x %in% 1:nrow(sens_opts)){
         bind_rows(.,
                   readRDS(here(paste0("Output/results_",x,"_naive.RDS"))) %>% 
                   mutate(optimized = "Naive") %>% 
                     slice_head(n=50))
         } else {.}} %>% 
    mutate(sens = str_remove(rds_files[x],here("Output/results_")) %>% str_remove(".RDS")  %>% str_remove("_"),
           sens_name = sens_names[as.numeric(sens)]
           ) %>% 
  #}) %>% 
 # bind_rows() %>% 
  relocate(sens, optimized, sens_name) %>% 
  mutate(across(starts_with("a"),.fns = function(x) {round(x,1)}),
         aSeq_Proportion = 1 #- aFeed_Proportion
         - aFood_Proportion) %>% 
  mutate(Profit = ifelse(sProfit > 0, "Income", ""),
         Biodiversity = ifelse(sBiodiversity_Benefit > 0, "Habitat", ""),
         Climate = ifelse(sCO2e_Mitigation > 0, "Carbon", ""),
         Food = ifelse(sFood > 0, "Food", "")) %>% 
  mutate(Benefits = paste(Profit, Biodiversity, Climate, Food, sep = " ") %>% trimws(),
         Benefits = ifelse(Benefits == "","None",Benefits),
         Benefits = ifelse(Benefits == "Income Habitat Carbon Food", "ALL", Benefits )) %>% 
   # group_by(optimized) %>% 
    mutate(Benefits_05 = paste(Profit_05, Biodiversity_05, Climate_05, Food_05, sep = " ") %>% trimws(),
           Benefits_05 = paste0(Benefits,Benefits_05),
           Benefits_05 = ifelse(missing %>% as.character() %>% str_count("0") == 1,
                                1*Benefits_05 %>% str_detect(obj[1]) +
                                  10*Benefits_05 %>% str_detect(obj[2]) +
                                  100*Benefits_05 %>% str_detect(obj[3]) +
                                  1000*Benefits_05 %>% str_detect(obj[4]), 
                                ""),
           Benefits_05 = ifelse(Benefits_05 == 1111, "ALL", "" )) %>% 
    dplyr::select(-missing) %>% 
  mutate(Trades = case_when(optimized == "naive" ~ "Naive",
                               Benefits == "ALL" ~ "Holistic",
                                Benefits_05 == "ALL" ~ "Quasi-Holistic",
                                TRUE ~ "Non-Holistic")) %>% 
      rownames_to_column()
  }) %>% bind_rows() %>% 
  mutate(sFood = sFood/unit_mod["sFood"],   ## Change Units for Publication
         sBiodiversity_Benefit = sBiodiversity_Benefit/unit_mod["sBiodiversity_Benefit"],
         sCO2e_Mitigation = sCO2e_Mitigation/unit_mod["sCO2e_Mitigation"],
         sProfit = sProfit/unit_mod["sProfit"]) %>% 
  left_join(by = "sens",
            sens_opts %>% 
              mutate(sens = row_number() %>% as.character()))  

brks <- sort(unique(df_result$Benefits))[c(1,2,7,3,4,6,5)]



source(here("Code/5b_test_corr.R"))

