

sens_index = as.double(Sys.getenv("PBS_ARRAY_INDEX"))
is.HPC = !is.na(sens_index)
sens_index = ifelse(is.HPC,sens_index,1)


#source(here("Code/1_inputs.R"))


nsga_fun <- nsga2R_doc

sens <- sens_opts[ifelse(is.na(sens_index),1,sens_index),]

print(sens)

source(here("Code/4a_set-Biodiversity.R"))


n_gen = ifelse(is.HPC,150,5)
n_pop = ifelse(is.HPC,50,5)

for (k in c(""#,"_all"
            )) {
  
fun = if(k == "") {f.object} else {f.object_all}

### Set Sensitivity Parameters to Run

#sens_opts <- sens_opts %>% add_row(aCarbon_Price = 0,aRefresh_Rate_Ref = (1:2)/2) 


#set.seed(NULL)
set.seed(42)

weight = rbind(rep(1,length(objectives)),diag(length(objectives)))
weight = weight[1,]
# results <- list()
# HV <- list()
#for (j in nrow(weight) ){ ONLY USEFUL FOR EDGE CASES
  f.object_min <- function(x) {
    -fun(x)*weight#[j,]
  }

  if(exists("init")){ 
    initial = init } else {initial = tibble()}
    if (nrow(initial) == 0 ) {initial = rbind(opti_min,opti_max,(opti_max+opti_min)/4)} #else {initial = rbind(opti_min,opti_max,(opti_max+opti_min)/4)}
  
 results <- nsga_fun(fn=f.object_min,
                       varNo=length(parms),
                objDim=length(objectives),
                lowerBounds=opti_min,
                upperBounds=opti_max,
                popSize= n_pop,
                tourSize=2,
                generations= n_gen,
                cprob=0.5,
                XoverDistIdx=20,
                mprob=0.1,
                MuDistIdx=3,
                get_naive = TRUE
                )
 naive_save <- results$naive %>% 
   mutate(across(.cols = contains("obj"), function(x) x*(-1))) %>% 
   setNames(c(objectives,names(parms)))
# HV <- results$hypervolume

 # if(j!=nrow(weight)) { 
 #   parm <- results[[j]]$parameters %>% as_tibble() %>%
 #  setNames(names(parms)) %>% 
 #  slice_head(n = 1)
# 
#    results[[j]] <- c(f.object(parm),
#                          results[[j]]$parameters %>% as_tibble() %>% slice_head(n = 1)
#                          )  %>% 
#       setNames(c(objectives,names(parms))) %>% as_tibble()
#  } else {
  results <- bind_cols(results$objectives %>%  `*`(-1) %>% as_tibble(),
             results$parameters %>% as_tibble()) %>%
     setNames(c(objectives,names(parms)))
   # }
#}

if (k == "") {init = results %>% 
  filter(across(all_of(objectives), .fns = function(x) (x > 0))) %>% 
  dplyr::select(names(parms))
}

results_list <- list(bind_rows(results),#naive_run,
                     SGR,carr_cap) %>% setNames(c("df_result",#"naive_run",
                                                                        "SGR","Carrying_Capacity"))
if(is.HPC){
message("Saving...")
message(here(paste0("Output/results_",sens_index,"_.RDS")))
if(!only_naive){
saveRDS(results_list, here(paste0("Output/results_",sens_index,"_.RDS")) )
}
saveRDS(naive_save,here(paste0("Output/results_",sens_index,"_naive.RDS")))
} 
 }

