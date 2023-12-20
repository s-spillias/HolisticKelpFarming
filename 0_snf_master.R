library(here)

nyears = 10
# set-up initial conditions
source(here("Code/1_inputs.R"))

# set-up model
source(here("Code/2_SnF_model.R"))

# create objective functions
source(here("Code/3_objectives.R"))

library(beyonce)
library(cluster)

only_naive = FALSE


# set-up scenarios
source(here("Code/scenarios.R"))
# import modified nsga2R function that documents progress
source(here("Code/3b_nsga2R_doc.R"))

#source(here("Code/4a_preHPC.R"))
source(here("Code/4_SnF_HPC.R"))

# Plot Figures
source(here("Code/5_output.R"))
source(here("Code/5a_reporting_prep.R"))
source(here("Code/5b_test_corr.R"))
source(here("Code/6_figure_prep.R"))
source(here("Code/6a_plot_paretos.R"))
source(here("Code/6b_plot_par.R"))
#source(here("Code/6f_plot_noharvest.R"))
source(here("Code/6g_plot_scen.R"))
