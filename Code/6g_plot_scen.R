#Scenario Performance

# pal_pareto <- c(beyonce_palette(21,n = 18, type = 'continuous')[16],"grey75",
#                 "grey45",beyonce_palette(8)[5])
library(ggrepel)
library(here)
library(tidyverse)
library(patchwork)
source(here("Code/6_figure_prep.R"))

# remove_food = FALSE
# if(remove_food){
# obj <- obj[obj!="Food"]
# }
# 

#for (i in 1:length(sens_do)){
   df_scen <- df_result %>% 
     filter(sens %in% all_sens) %>% 
     filter(optimized != "Naive")

(scen_number <- df_scen %>% 
    mutate(Trades = factor(Trades, levels = c("Holistic","Quasi-Holistic",
                                            "Non-Holistic", "Single-Objective") %>% rev )) %>% 
  group_by(sens,Trades) %>%
  summarise(value = n()) %>% 
  mutate(Scenario = sens_names[as.numeric(sens)] %>% factor(levels = sens_names[all_sens] %>% rev)) %>% 
    {scen_table <<- .} %>% 
  # summarise(Benefits_All = sum(Benefits == "ALL"),
  #           Benefits_All05 = sum(Benefits_05 == "ALL"),
  #           Unsustainable = sum(B)) %>% 
 # pivot_longer(names_to = "variable", cols = -1) %>% 
  ggplot() +
  geom_col(aes(x = Scenario, y = value, fill = Trades)) +
  theme_classic() +
  xlab(NULL) +
  ylab("Number of Stategies on \n Pareto Surface") +
  #  scale_x_discrete(position = "top") +
    ggtitle("A. Number of Strategies \n     on Pareto Surface ") +
      theme(axis.text.y.right = element_text(#angle = 45,
                                  # vjust = 0,
                                   hjust = 0.5),
            axis.title = element_blank(),
           axis.ticks.length.y.right = unit(.5, "cm"),
           axis.ticks.y.right = element_blank(),
           legend.position = "bottom",
           title = element_text(size = 10)) +
  coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) +
 # scale_x_discrete(labels = sens_names) +
  scale_fill_manual(values = pal_pareto[3:1],
                    name = "",
                    drop = TRUE
                    ) )

  scen_table %>% 
    mutate(value = ifelse(is.na(value),0,value)) %>% 
    pivot_wider(names_from = Trades, values_from = value) %>% 
    mutate( total = sum(`Non-Holistic`, `Quasi-Holistic`, Holistic, na.rm = TRUE),
      holistic_proportion = Holistic/total ) %>% 
    pull(holistic_proportion) %>% hist()
  
  
 scen_trade_off <-  df_scen %>%
    mutate(Trades = ifelse(Trades=="Holistic","Holistic","Not")) %>% 
    group_by(sens, Trades) %>% 
    summarise(across(objectives, .fns = max)) %>% 
    mutate(level = "max") %>% 
    bind_rows(
      df_scen %>% 
        mutate(Trades = ifelse(Trades=="Holistic","Holistic","Not")) %>% 
        group_by(sens, Trades) %>% 
        summarise(across(objectives, .fns = min)) %>% 
        mutate(level = "min")) %>% 
    pivot_longer(all_of(objectives), names_to = "objective",values_to = "values") %>% 
    pivot_wider(names_from = c(Trades,level), values_from = values) %>% 
    mutate(sens = sens_names[sens %>% as.numeric]%>% factor(levels = sens_names[all_sens] %>% rev)) %>% 
    mutate(objective = case_when(objective == "sProfit" ~ "Income",# \n (1000 AUD)",
                                  objective == "sBiodiversity_Benefit" ~ "Habitat",# \n (1)",
                                  objective == "sCO2e_Mitigation" ~ "Carbon",# \n (T CO2e)",
                                  objective == "sFood" ~ "Food"# \n (T)"
                                  ),
           objective = factor(objective, levels = obj)) %>% 
   rowwise() %>% 
   mutate(range_max = max(Not_max,Holistic_max),
          range_min = min(Not_min,Holistic_min)) %>% 
   ungroup() %>% 
  # {if(remove_food){filter(.,objective!="Food")}else{.}} %>% 
   mutate(Trade_Off = range_max/(range_max-range_min) - Holistic_max/(range_max-range_min)) %>% 
   ungroup() %>% 
   mutate(obj_num = match(objective, obj)) %>% 
   {df_hist <<- .} %>% 
    ggplot() +
   geom_tile(aes(x = objective,
             y = sens,
             fill = Trade_Off),
              height = 0.9,
             width = 0.95) +
   # geom_point(aes(y = sens, x = 0.4 + obj_num ),
   #            col = "grey25",
   #            size = 2.2) +
   # geom_errorbar(aes(xmin = -0.4 + obj_num #+ (Holistic_min/
   #                   # (Not_max-Not_min))
   #                   ,
   #                  xmax = 0.4 + obj_num # + (Holistic_max/
   #                   # (Not_max-Not_min))
   #                  ,
   #                  # y = 1,
   #                  y = sens),
   #              col = "grey25",
   #              #lineend = "square",
   #              size = 1.2,
   #              width = 0) +
   # geom_segment(aes(x = obj_num + 0.8*(((Holistic_min-range_min)/
   #                    (range_max-range_min))- 0.5)     ,
   #                   xend = obj_num  + 0.8*(((Holistic_max-range_min)/
   #                   (range_max-range_min)) - 0.5)  ,
   #                   # y = 1,
   #                   y = sens,
   #                  yend = sens),
   #               col = pal_pareto[1],
   #               lineend = "round",
   #               size = 2.2,
   #               width = 0) +
    theme_classic() +
   ggtitle("B. Cost of Holistic Management") +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          title = element_text(size = 10)
          #axis.line.y = element_blank(),
         # strip.background = element_blank(),
          # strip.text.y.left = element_text(angle = 0,
          #                                  hjust = 0),
          #strip.text.y.left = element_blank(),
         # strip.placement = "outside"
    ) +
   scale_y_discrete(drop = TRUE) +
   scale_fill_gradient(low = "white",
                       high = pal_pareto[4],
                       labels = function(x) 100*x,
                       name = "Cost of Holistic Management (%)",
                       na.value = "grey75"
                       ) #+
    # scale_x_discrete(#n.breaks = 3,
                       # name = "Trade-Offs between Holistic and Single-Objective Strategies") 
    ggsave(scen_trade_off,file=here(paste0("Plots/scen_compare.png")), 
            device = "png", width = 15, height = 10, units = "cm")
  set.seed(40)
 (cost_plot <- df_hist %>% 
  mutate(objective = factor(objective, levels = obj %>% rev)) %>% 
     mutate(base = sens == "Baseline") %>% 
  #  mutate(objective = as.character(objective)) %>% 
   ggplot() +
   geom_boxplot(aes(x = objective, y = 100*Trade_Off),
                outlier.shape = NA) +
   geom_jitter(aes(x = objective, y = 100*Trade_Off, col = base),
               alpha = .5,
              # size = 1,
              # col = "red",
               width = 0.088) +
   # geom_text_repel(aes(x = objective, y = 100*Trade_Off,
   #           label = ifelse(100*Trade_Off>40 ,as.character(sens),NA))) +
   #facet_wrap(~objective) +
   theme_classic() +
   coord_flip() +
   ggtitle("B. Cost of Holistic Management") +
    ylab("% of Single-Yield Maximum") +
   xlab(NULL) +
   theme(axis.text.y = element_text(vjust = 2.5,
                                    hjust = 0.5,
                                    size = 10)) +
     scale_color_manual(values = c("grey75","red"),
                       guide = "none"
                       #breaks = c(FALSE,TRUE),
                       #limits = c(TRUE)
                       )
   )# +
   # scale_color_gradient(low = pal_pareto[1],
   #                      high = pal_pareto[3],
   #                      guide = "none"))
 #ggsave(file=here("Plots/cost_holistic.png"), device = "png",dpi = 300, width = 14, height = 11, units = "cm")
   x_shift_min = 0.015
   x_shift_max = 0.115
   y_shift_min = 0.85
   y_shift_max = 0.95
   
   y_offset = 0.235
 
   (p_work <- scen_number + cost_plot +
   inset_element(p = icons[["food"]], ## Dugong
                 left = x_shift_min -0.002,
                 bottom =y_shift_min,
                 right = x_shift_max,
                 ignore_tag = TRUE,
                 top = y_shift_max,
                 align_to = "plot",
                 on_top = TRUE,
                 clip = FALSE) +
 inset_element(p = icons[["prof"]], ## Dugong
               left = x_shift_min + 0.01,
               bottom = y_shift_min - y_offset ,
               right = x_shift_max,
               top = y_shift_max - y_offset,
               ignore_tag = TRUE,
               align_to = "plot",
               on_top = FALSE,
               clip = FALSE) +
 inset_element(p = icons[["hab"]], ## Dugong
               left = x_shift_min,
               bottom =y_shift_min - y_offset*1.95,
               right = x_shift_max,
               top = y_shift_max - y_offset*1.95,
               align_to = "plot",
               ignore_tag = TRUE,
               on_top = FALSE,
               clip = FALSE) +
 inset_element(p = icons[["carb"]], ## Dugong
               left = x_shift_min - 0.03,
               bottom = y_shift_min - y_offset*2.85,
               right = x_shift_max + 0.03,
               top = y_shift_max - y_offset*2.85,
               ignore_tag = TRUE,
               align_to = "plot",
               on_top = FALSE,
               clip = FALSE))
 
 (scen_analysis <-  p_work + 
    # plot_annotation(tag_levels = "A") +
   plot_layout(#guides = "collect",
               widths = c(0.48,0.52)
               ) & theme(legend.position = 'bottom') )
  
   ggsave(scen_analysis, file=here(paste0("Plots/scen_analysis_main.png")), device = "png", width = 20, height = 12, units = "cm")
#}
 

  