### PLOTS SINGLE OPTIMIZATION FOR BASE SCENARIO
## set scenario
library(grid)
library(patchwork)
library(png)

x_shift_min = 0.01
x_shift_max = 0.11
y_shift_min = 0.152
y_shift_max = 0.252

(obj_plot <- df_result %>% #filter(sens %in% set) %>% 
    group_by(sens) %>% 
    # dplyr::select(-c(objective, value)) %>% 
    # distinct() %>% 
    relocate(aSeq_Proportion) %>% 
    pivot_longer(cols = all_of(c("aSeq_Proportion",names(parms))), names_to = "variable", values_to = "value") %>% 
    dplyr::select(c(all_of(objectives),variable, value, Benefits,sens#,MCU
                    )) %>% 
    mutate(holistic = (sFood>0)*(sBiodiversity_Benefit>0)*(sCO2e_Mitigation>0)*(sProfit>0)) %>% 
    mutate(across(all_of(objectives), .fns = function(x) x==max(x), .names = "{.col}.max" 
    )) %>% 
   # filter(holistic == 1 | sFood.max | sBiodiversity_Benefit.max | sCO2e_Mitigation.max | sProfit.max ) %>% 
    #ungroup() %>% 
    dplyr::select(-value, -variable, -Benefits) %>% 
    #distinct() %>% 
    pivot_longer(all_of(objectives), names_to = "objective",values_to = "value") %>% 
   # mutate(objective = ifelse(!(holistic == 1 | sFood.max | sBiodiversity_Benefit.max | sCO2e_Mitigation.max | sProfit.max),"Other",objective)) %>% 
    mutate(sens = factor(str_replace(sens_names[sens %>% as.numeric]," ","\n"),levels = (sens_names %>% str_replace(" ","\n"))[all_sens]),
           objective = case_when(objective == "sProfit" ~ "Income",#pubnames[4] %>% str_replace("\\(","\n \\("),
                                 objective == "sBiodiversity_Benefit" ~ "Habitat",#pubnames[2] %>% str_replace("\\(","\n \\("),
                                 objective == "sCO2e_Mitigation" ~ "Carbon",# pubnames[3] %>% str_replace("\\(","\n \\("),
                                 objective == "sFood" ~ "Food", #pubnames[1] %>% str_replace("\\(","\n \\("),
                               # objective == "Other" ~ "Other",
                                 TRUE ~ "Holistic"),
           
           objective = factor(objective, 
                              levels = c("Holistic",obj#, "Other"#,
                                         # pubnames[1] %>% str_replace("\\(","\n \\("),
                                         # pubnames[3] %>% str_replace("\\(","\n \\("),
                                         # pubnames[2] %>% str_replace("\\(","\n \\("),
                                         # pubnames[4] %>% str_replace("\\(","\n \\("),
                                         # "Holistic"
                              ))) %>% 
    mutate(Management = case_when(holistic == 1 ~ "Holistic",
                                  sFood.max ~ "Food",
                                  sBiodiversity_Benefit.max ~ "Habitat",
                                  sCO2e_Mitigation.max ~ "Carbon",
                                  sProfit.max ~ "Income",
                                  TRUE ~ "Other") %>% 
             factor(levels = c("Holistic",obj,"Other"))) %>% 
    mutate(objective_axis = case_when(objective == "Food" ~    pubnames[9],#%>% str_replace("\\(","\n \\("),
                                 objective == "Income" ~  pubnames[4],#%>% str_replace("\\(","\n \\("),
                                 objective == "Habitat" ~ pubnames[2],#%>% str_replace("\\(","\n \\("),
                                 objective == "Carbon" ~  pubnames[3]#,#%>% str_replace("\\(","\n \\(")
                                # TRUE ~ "Other"
    ),
    objective_axis = factor(objective_axis,
                       levels = c(
                         pubnames[9] ,#%>% str_replace("\\(","\n \\("),
                         pubnames[4] ,#%>% str_replace("\\(","\n \\("),
                         pubnames[2] ,#%>% str_replace("\\(","\n \\("),
                         pubnames[3]#, #%>% str_replace("\\(","\n \\(")
                        # "Other"
                       ))) %>% 
    dplyr::select(sens,Management, value, objective, objective_axis#,MCU
                  ) %>% 
    mutate(type = ifelse(Management == "Holistic","Holistic","Maximization"),
           type = ifelse(Management == "Other","Other",type)) %>% 
    distinct() %>% 
    {spider_df <<- .} %>% 
    filter(sens == "Baseline") %>% 
    filter(Management != "Other") %>% 
    {ggplot(.) + 
        geom_point(aes(x = objective, y = value, 
                       shape = Management,
                       col = type
        ),
        size = 3) +
        xlab(NULL) +
        ylab(NULL) +
        # facet_grid(rows = vars(sens)#, switch = "y" 
        #    ) +
        # geom_hline(yintercept=0, col = "grey75") +
        # geom_vline(xintercept = (1:4)-0.5, col = "grey75")+
        ylim(min(.$value),max(.$value)) +
        scale_color_manual(values = pal_pareto[c(1,4,3)],
                           #  name = "Management",
                           # limits = c("Holistic","Maximization"),
                           # labels = c("Holistic",""),
                           # breaks = c("Holistic","Maximization")
        ) +
        scale_shape_manual(name = "Priority",
                    values = c(16,15,17,16),
                    limits = c("Holistic","Food","Habitat","Other"),
                    labels = c("Holistic","Food / Income","Carbon / Habitat","Other"),
                    breaks = c("Holistic","Food","Habitat","Other")) +
        # coord_polar() +
        coord_flip() +
        theme_classic() +
        facet_wrap(~objective_axis, scales = "free", ncol = 1, strip.position = "bottom") +
        theme(strip.text.y = element_blank(),#element_text(angle = 0),
              strip.background = element_blank(),
              legend.position = "right",#c(0.8,0.25),
              plot.title = element_text(hjust = 0.4),
              strip.placement = "outside",
              #legend.background = element_rect(fill = NA, colour = "grey45"),
              # panel.border = element_blank(),#element_rect(fill = NA),
              # panel.background = element_blank(),
              #   legend.key = element_rect(fill = NA),
              axis.text.y = element_text(vjust = 2.5,
                                         hjust = 0.5,
                                         size = 10)
        ) +
        ggtitle("Cumulative 10-Year Yields") + 
        guides(col = "none",
               shape = guide_legend(override.aes = list(col = pal_pareto[c(1,4,4,3)]))) +
      inset_element(p = icons[["carb"]], ## Dugong
                    left = x_shift_min + 0.001,
                    bottom =y_shift_min,
                    right = x_shift_max + 0.001,
                    ignore_tag = TRUE,
                    top = y_shift_max,
                    align_to = "plot",
                    on_top = TRUE,
                    clip = FALSE) +
      inset_element(p = icons[["hab"]], ## Dugong
                    left = x_shift_min,
                    bottom =y_shift_min + 0.25,
                    right = x_shift_max,
                    top = y_shift_max + 0.25,
                    ignore_tag = TRUE,
                    align_to = "plot",
                    on_top = TRUE,
                    clip = FALSE) +
      inset_element(p = icons[["prof"]], ## Dugong
                    left = x_shift_min + 0.005,
                    bottom =y_shift_min + 0.5,
                    right = x_shift_max + 0.005,
                    top = y_shift_max + 0.5,
                    align_to = "plot",
                    ignore_tag = TRUE,
                    on_top = TRUE,
                    clip = FALSE) +
      inset_element(p = icons[["food"]], ## Dugong
                    left = x_shift_min - 0.015,
                    bottom =y_shift_min + 0.75,
                    right = x_shift_max + 0.045,
                    top = y_shift_max + 0.75,
                    ignore_tag = TRUE,
                    align_to = "plot",
                    on_top = TRUE,
                    clip = FALSE)
    })
ggsave(file=here(paste0("Plots/Base_max.png")), device = "png", dpi = 300, width = 14, height = 11, units = "cm")



remove_food = TRUE

sens_names_ <- sens_names %>% str_replace_all(" ","_")



harv_num = names(parms) %>% tail(1) %>% str_remove("aHarvest_") %>% as.numeric

## Remove Food
object_plus <- c(objectives,"all")[ !(c(objectives,"all") %in% c("sBiodiversity_Benefit","sFood"))]

sens_do <- list()
sens_do[[1]] <- all_sens
sens_do[[2]] <- c(1)
names(sens_do) <- c("main","base")
#sens_do[["main"]] <- all_sens[-c(sens_do[[2]])]

for(i in 1:length(sens_do)){
 
   row_height = ifelse(i == 2,6,-7)
  x_shift_min = 0.1
  x_shift_max = 0.165
  y_shift_min = 0.85
  y_shift_max = 0.95
  
  set=sens_do[[i]]
  
  (dec_plot <-  df_result %>% 
      filter(sens %in% set) %>% 
      filter(optimized != "Naive") %>% 
      group_by(sens) %>% 
      # dplyr::select(-c(objective, value)) %>% 
      # distinct() %>% 
      # relocate(aSeq_Proportion) %>% 
      pivot_longer(cols = all_of(c(#"aSeq_Proportion",
        contains("aHarvest"))), names_to = "variable", values_to = "value") %>% 
      dplyr::select(c(all_of(objectives),variable, value, sens, aFood_Proportion)) %>% 
      mutate(holistic = as.logical((sFood>0)*(sBiodiversity_Benefit>0)*(sCO2e_Mitigation>0)*(sProfit>0))) %>% 
      mutate(across(all_of(objectives), .fns = function(x) x==max(x)#, .names = "{.col}.max" 
      )) %>% 
      
      mutate(variable = ifelse(variable %>% str_detect("aHarvest_"), "Harvest\nProportion",variable),
             # variable = case_when(variable == "aFood_Proportion" ~ "F:C",
             #                      variable == "aSeq_Proportion" ~ "Carbon",
             #                      TRUE ~ variable),
             variable = factor(variable, levels = c("Harvest\nProportion", "F:C", "Carbon")),
             # facet = ifelse(variable %in% "Harvest",1,2)
      ) %>%
      # rowwise() %>% 
      # mutate(max = any(sFood,sBiodiversity_Benefit,sCO2e_Mitigation,sProfit)) %>% 
      # ungroup() %>% 
      # pivot_longer(cols = all_of(objectives), names_to = "objective", values_to = "maxes") %>% 
      #  mutate(objective = ifelse(holistic == 1, "Holistic",objective)) %>% 
      # filter(!(maxes == FALSE & objective != "Holistic")) %>% 
      mutate(sens = factor(str_replace_all(sens_names[sens %>% as.numeric]," ","\n"),
                           levels = (sens_names %>% str_replace_all(" ","\n"))[all_sens])) %>% 
      mutate(priority = case_when(sProfit & sFood ~ "Food/Income",
                                  sBiodiversity_Benefit & sCO2e_Mitigation ~ "Carbon/Habitat",
                                  sProfit & sCO2e_Mitigation ~ "Income/Carbon",
                                  sProfit ~ "Income",
                                  sBiodiversity_Benefit ~ "Habitat",
                                  sCO2e_Mitigation ~ "Carbon",
                                  sFood ~ "Food",
                                  holistic ~ "Holistic",
                                  TRUE ~ "Other")) %>% ungroup() %>% 
      { if(i==1){ mutate(.,priority_combined = case_when(str_detect(priority,"Income") | str_detect(priority,"Food") ~ "Food/Income",
                                                         str_detect(priority,"Carbon") | str_detect(priority,"Habitat") ~ "Carbon/Habitat",
                                                         TRUE ~ priority))
      } else {
        mutate(.,priority_combined = priority)
      }} %>% 
      arrange(priority) %>% 
      group_by(sens) %>% mutate(type = paste0(unique(priority),collapse = ",")) %>% 
      mutate(priority = factor(priority, 
                               levels = c("Holistic","Food/Income","Food","Income","Carbon/Habitat","Carbon","Habitat","Income/Carbon","Other"
                               ))) %>% 
      mutate(priority_combined = factor(priority_combined, 
                                        levels = c("Holistic","Food/Income","Food","Income","Carbon/Habitat","Carbon","Habitat","Income/Carbon","Other"
                                        ))) %>% 
      
      mutate(holistic = priority == "Holistic") %>% 
      # {if(remove_food){filter(.,objective!="Food")}else{.}} %>% 
      # filter(!is.na(objective)) %>% 
      # filter(variable != "Carbon") %>% 
      # mutate(holistic = as.character(holistic)) %>% 
      group_by(variable, sens,holistic, priority_combined) %>% 
      # summarise(.groups = "keep",
      #   mean_value = mean(value),
      #           error_value = sd(value),
      #           mean_use = mean(aFood_Proportion),
      #           error_use = sd(aFood_Proportion)/n()
      #           ) %>% 
      mutate(mean_use = mean(aFood_Proportion),
             mean_value = mean(value)) %>% 
      filter(priority_combined != "Other") %>% 
      mutate(mean_use = 1-ifelse(mean_value<=0.02,0.5,mean_use)) %>% 
      {mgmt_df <<- .} %>% 
      ggplot() +
      # geom_point() + 
      # geom_line() +
      geom_density(aes(x =  ..scaled..,
                       y = value, fill = mean_use, 
                       #  col = factor(Benefits)
      ),
      
      # adjust = 2.5, 
      bw = 0.05,
      col = NA,
      #alpha = 0.65,
      # kernel = "gaussian"#outlier.shape = NA
      ) +
      #{if(j == 4){
      ylab("Monthly Proportion of Standing Biomass Harvested") + #}else{ylab(NULL)}} +
      xlab("Frequency") +
      labs(colour = "Net Benefits") +
      facet_grid(sens~priority_combined, scales = "free_y",
                 space="fixed", #switch = "y"
                 ) +
      geom_rect(data = data.frame(priority_combined = mgmt_df %>% arrange(priority_combined) %>% pull(priority_combined) %>% unique %>% droplevels()),
                aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
                fill = NA,
                col = c(
                  rep(c(pal_pareto[1],rep(pal_pareto[4],length(mgmt_df$priority_combined %>% unique %>% droplevels())-1)),mgmt_df %>% pull(sens) %>% unique() %>% length())
                ),
                size = 1) +
      geom_hline(aes(yintercept = mean_value),
                 linetype = 2) + 
      # scale_x_discrete(sec_axis(~))+
      coord_flip(clip = "off") +
      # ggtitle(paste0(c(#"A: Maximize ",
      #   "B: Maximize ","C: Maximize ",#"D: Maximize ",
      #   "A: ")[i],c(obj,"Holistic")[i])) +
      theme_classic()+
      # {if(j == 1){ 
      ggtitle("Management Priority") +
      #  }else{ ggtitle(NULL)}} +
      # ylim(0,1)+
      scale_y_continuous(breaks = c(0:2)/2
      ) +
      #   scale_x_discrete(sec.axis = sec_axis(~,name = "Management Priority"))+
      theme(legend.title = element_text(vjust = 0.9),
            # plot.subtitle = element_text(hjust= 0),
            legend.position = "bottom",
            # plot.margin = unit(c(1,1,1,1), "cm"),
            plot.title = element_text(size = 10,
                                      hjust = 0.5, 
                                      vjust = 1),
            strip.background = element_blank(),
            axis.line = element_blank(),
            strip.background.x = element_rect(fill = NA, colour = NA), 
            strip.text.y = if(i != 3){element_text(angle = 0,
                                             hjust = 0,
                                             size = 8)
              }else{
                element_blank()
              },
            axis.title.x = element_text(vjust = -0.5),
            #panel.border = element_rect(size = 1, colour = "grey50", fill = NA),
            axis.text.y = element_blank(),
            strip.text.x = element_text(margin = margin(b=10,t=25),
                                        vjust = 15#, size = 15
            ),
            #  axis.title.x.bottom = element_text("Scenario"),
            axis.ticks.y = element_blank(),
            # axis.text.x = #if(i %in% c())){
            #   element_text(#angle = 45,
            #     vjust = .95,
            #     size = 6
            #     #  hjust = 1
            #   )
            # } else {
            #  element_blank()
            #  }
            #,
            #  legend.position = "none"
      )  +
      scale_colour_gradientn(#midpoint = 0.5, 
        colors = beyonce_palette(33) %>% rev
        #  low = beyonce_palette(33)[3],
        #  mid = beyonce_palette(107)[4],
        # high = beyonce_palette(33)[5]
      ) +
      scale_fill_gradientn(#midpoint = 0.5,
        colors = (beyonce_palette(33)[-1] %>% rev)#[-c(2,4)]
        ,
        name = "Proportion of Harvest Use",
        breaks = c(0,1),
        limits = c(0,1),
        guide = "colourbar",
        labels = c("Food","Sequestration") 
      ) +
      guides(colour = "none",
             #   fill = guide_legend(override.aes = list(alpha = .65, guide = "colourbar"))
      ) #+ annotate("text",x = 0, y = 0.9, label = "Scenario")
  )
  # on_top = FALSE
  # if(i == 1){
  #  ( dec_plot <- dec_plot +
  #     inset_element(p = icons[["food"]], ## Dugong
  #                   left = x_shift_min,
  #                   bottom =y_shift_min,
  #                   right = x_shift_max,
  #                   top = y_shift_max,
  #                   align_to = "plot",
  #                   on_top = on_top,
  #                   clip = FALSE) +
  #     inset_element(p = icons[["prof"]], ## Dugong
  #                   left = x_shift_min + 0.04,
  #                   bottom =y_shift_min ,
  #                   right = x_shift_max  + 0.04,
  #                   top = y_shift_max,
  #                   align_to = "plot",
  #                   on_top = on_top,
  #                   clip = FALSE) +
  #     inset_element(p = icons[["hab"]], ## Dugong
  #                   left = x_shift_min+ 0.1,
  #                   bottom =y_shift_min ,
  #                   right = x_shift_max + 0.1,
  #                   top = y_shift_max,
  #                   align_to = "plot",
  #                   on_top = on_top,
  #                   clip = FALSE) +
  #     annotation_custom(grob = icons[["carb"]], ## Dugong
  #                   xmin = 0,#x_shift_min + 0.07,
  #                   ymin =0,#y_shift_min + 0,
  #                   xmax = x_shift_max + 0.07,
  #                   ymax = y_shift_max + 0#,
  #                  # align_to = "plot",
  #                  # on_top = on_top,
  #                   #clip = FALSE
  #                  ) )
  # }
  ggsave(file=here(paste0("Plots/",names(sens_do)[i],".png")), dpi = 300,
         device = "png", width = 12+1.1*mgmt_df$priority %>% unique %>% length, height = 2.5*length(set) + row_height, units = "cm")
}


png(here(paste0("Plots/comb_plot.png")), 
    width = 18, height = 18, units = "cm", res = 450)
grid.newpage()
print(dec_plot,  vp = viewport(0.5, 0.2, width = 1, height = 0.4))
print(obj_plot,  vp = viewport(0.5, 0.7, width = 1, height = 0.55))

dev.off()

# design = '
# AAAAA
# #BBBB'
# 
# 
# dec_plot + obj_plot +   plot_layout(nrow = 2, heights = c(0.2,0.8)) +
#   plot_annotation(tag_levels = "A")
# 
# ggsave(file=here(paste0("Plots/comb_plot.png")), dpi = 300,
#        device = "png", width = 16, height = 16 + 2.5, units = "cm")

#  
# facets <- mgmt_df$type %>% unique %>% as_tibble() %>% setNames("type") %>% mutate(hol_pres = str_detect(type,"Holistic")) %>% 
#   arrange(desc(hol_pres)) %>% pull(type)
# panels <- list()
# 
#    
#    for(j in 1:length(facets)){
#    panels[[j]] <- mgmt_df %>%# filter(type == facets[j]) %>% 
#     ggplot() +
#     # geom_point() + 
#     # geom_line() +
#     geom_density(aes(x =  ..scaled..,
#       y = value, fill = mean_use, 
#                     #  col = factor(Benefits)
#     ),
#    
#    # adjust = 2.5, 
#    bw = 0.05,
#     col = NA,
#     alpha = 0.65,
#    # kernel = "gaussian"#outlier.shape = NA
#       ) +
#       # geom_segment(data = summarise(mgmt_df,
#       #                                mean_value = mean(value),
#       #                                error_min = mean_value - sd(value),
#       #                               error_max = mean_value + sd(value)),
#       #               aes(x = variable,
#       #                   xend = variable,
#       #                   y = max(0,error_min),yend = error_max)
#       #              ) +
#       # geom_hline(data = summarise(mgmt_df,
#       #                             mean_use = mean_use,
#       #                             mean_value = mean(value),
#       #                             error_value = sd(value)/sqrt(n())),
#       #            aes(#x = variable, 
#       #                yintercept = mean_value,
#       #                col = mean_use)
#       #            ) +
#       # geom_rect(data=data.frame(objective = c("Holistic",obj) %>% factor(levels = c("Holistic",obj) )),
#       #           aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
#       #           fill = NA,
#                 # col=c(rep(c(pal_pareto[1],rep(pal_pareto[2],length(obj))),mgmt_df %>% pull(sens) %>% unique() %>% length())), size = 2) +
#       {if(j == 4){ylab("Harvest Proportion")}else{ylab(NULL)}} +
#     xlab(NULL) +
#     labs(colour = "Net Benefits") +
#     facet_grid(sens~priority, scales = "free_y",
#                space="fixed", switch = "y") +
#     #  scale_x_discrete(sec_axis(~))+ 
#      coord_flip() +
#     # ggtitle(paste0(c(#"A: Maximize ",
#     #   "B: Maximize ","C: Maximize ",#"D: Maximize ",
#     #   "A: ")[i],c(obj,"Holistic")[i])) +
#     theme_classic()+
#      {if(j == 1){ ggtitle("Management Priority")}else{ ggtitle(NULL)}} +
#         # ylim(0,1)+
#       scale_y_continuous(breaks = c(0:2)/2
#                          ) +
#   #   scale_x_discrete(sec.axis = sec_axis(~,name = "Management Priority"))+
#     theme(legend.title = element_text(vjust = 0.9),
#           legend.position = "bottom",
#       plot.title = element_text(size = 10,
#                            hjust = 0.5),
#       strip.background = element_blank(),
#       axis.line = element_blank(),
#       strip.text.y.left = element_text(angle = 0,
#                                   hjust = 1),
#       axis.title.x = element_text(vjust = -0.5),
#       #panel.border = element_rect(size = 1, colour = "grey50", fill = NA),
#       axis.text.y = element_blank(),
#       axis.ticks.y = element_blank(),
#       axis.text.x = #if(i %in% c())){
#         element_text(#angle = 45,
#           vjust = .95,
#           size = 6
#           #  hjust = 1
#         )
#       # } else {
#       #  element_blank()
#       #  }
#       ,
#     #  legend.position = "none"
#     )  +
#      scale_colour_gradientn(#midpoint = 0.5, 
#                           colors = beyonce_palette(33) %>% rev
#                          #  low = beyonce_palette(33)[3],
#                          #  mid = beyonce_palette(107)[4],
#                          # high = beyonce_palette(33)[5]
#                          ) +
#       scale_fill_gradientn(#midpoint = 0.5,
#                             colors = beyonce_palette(33) %>% rev,
#                             name = "Proportion of Harvest Use",
#                             breaks = c(0,1),
#                             limits = c(0,1),
#                             guide = "colourbar",
#                             labels = c("Food","Sequestration") %>% rev
#                             ) +
#       guides(colour = "none",
#           #   fill = guide_legend(override.aes = list(alpha = .65, guide = "colourbar"))
#       )
#   
#     # }
#   # }
#    }
#    
# panel_heights <- mgmt_df %>% ungroup() %>%  dplyr::select(type,sens) %>% distinct() %>% group_by(type) %>% summarise(n=n()) %>% 
#   mutate(type = factor(type, levels = facets)) %>% arrange(type) %>% pull(n) 
# if(length(panels)>3){
#    panels[[1]] + panels[[2]] + panels[[3]] + panels[[4]] +
#      plot_layout(nrow = length(panels),#widths = c(5,4,5,3),
#                  heights = panel_heights, guides = "collect") &
#      theme(legend.position='bottom')
# } else {
#   panels[[1]] + panels[[2]] + panels[[3]] + #panels[[4]] 
#   plot_layout(nrow = length(panels),#widths = c(5,4,5,3),
#               heights = panel_heights, guides = "collect") &
#     theme(legend.position='bottom')
# }
#    




#     
#   ######################
# para_plots <- list()
# para_dec <- list()
# para_obj <- list()
# for(i in 1:length(unique(object_plus))){
#   message("Plotting ", unique(object_plus)[i])
#   df_para <- df_result %>% #filter(sens %in% sens_do) %>%
#     group_by(sens) %>%
#   #  {maxes<<-.} %>%
#    mutate(across(.cols = objectives, .fns = function(x) {normalizeFront(as_tibble(x))}, .names = "norm_{.col}")) %>%
#   dplyr::select(rowname,sens,optimized,sens_name,contains("norm")) %>% 
#   ungroup() %>% 
#   rowwise() %>% 
#   mutate(MCU = sum(norm_sFood,norm_sBiodiversity_Benefit,norm_sCO2e_Mitigation,norm_sProfit)/4) %>% 
#   dplyr::select(-norm_sFood,-norm_sBiodiversity_Benefit,-norm_sCO2e_Mitigation,-norm_sProfit)
#     {if(i ==length(unique(object_plus))) {
#       filter(., Benefits == "ALL")
#     }else{
#       slice_max(.,order_by = !!as.symbol(unique(object_plus)[i]))}
#       } %>%
#    # filter(Benefits =="ALL") %>%
#     pivot_longer(cols = all_of(objectives), names_to = "objective", values_to = "value") %>%
#     mutate(objective = case_when(objective == "sProfit" ~ pubnames[4] %>% str_replace("\\(","\n \\("),
#                                  objective == "sBiodiversity_Benefit" ~ pubnames[2] %>% str_replace("\\(","\n \\("),
#                                  objective == "sCO2e_Mitigation" ~ pubnames[3] %>% str_replace("\\(","\n \\("),
#                                  objective == "sFood" ~ pubnames[1] %>% str_replace("\\(","\n \\(")),
#            objective = factor(objective,
#                               levels = c(
#                               pubnames[1] %>% str_replace("\\(","\n \\("),
#                               pubnames[3] %>% str_replace("\\(","\n \\("),
#                               pubnames[2] %>% str_replace("\\(","\n \\("),
#                               pubnames[4] %>% str_replace("\\(","\n \\(")
#                               )),
#            polarity = value < 0
#           )
# #   # mins <- maxes %>% dplyr::select(sens,all_of(objectives)) %>% group_by(sens) %>% summarise(across(everything(), min))
# #   #  maxes <- maxes %>% dplyr::select(sens,all_of(objectives)) %>% group_by(sens) %>% summarise(across(everything(), max))
# #   # 
#   #  extremes <- mins %>% pivot_longer(-sens,names_to = "objective", values_to = "min") %>% 
#   #    left_join(maxes %>% pivot_longer(-sens,names_to = "objective", values_to = "max"), by = c("sens","objective"))  %>% 
#   #    # rename("max" = 3) %>% 
#   #    mutate(objective = case_when(objective == "sProfit" ~ pubnames[4] %>% str_replace("\\(","\n \\("),
#   #                                 objective == "sBiodiversity_Benefit" ~ pubnames[2] %>% str_replace("\\(","\n \\("),
#   #                                 objective == "sCO2e_Mitigation" ~ pubnames[3] %>% str_replace("\\(","\n \\("),
#   #                                 objective == "sFood" ~ pubnames[1] %>% str_replace("\\(","\n \\(")),
#   #           objective = factor(objective, 
#   #                              levels = c(pubnames[1] %>% str_replace("\\(","\n \\("),
#   #                              pubnames[3] %>% str_replace("\\(","\n \\("),
#   #                              pubnames[2] %>% str_replace("\\(","\n \\("),
#   #                              pubnames[4] %>% str_replace("\\(","\n \\("))))
#   #  
#   
#   if(nrow(df_para) == 0){
#     message(unique(objectives)[i], " Failed...")
#     next
#   }
#   
#   # para_obj[[i]] <- ggplot(data = df_para) +
#   #  { if(i ==length(unique(object_plus))) { 
#   #    geom_boxplot(aes(x = objective, y = value, col = sens, 
#   #                             #col = factor(Benefits)
#   #    ))
#   #   }else{
#   #    geom_col(position = "dodge",aes(x = objective, y = value, fill = polarity, group = sens,
#   #                         #col = factor(Benefits)
#   #    ))
#   #   } } + 
#   #   geom_hline(yintercept = 0) +
#   #   {
#   #     if(i ==length(unique(object_plus))){
#   #       geom_boxplot(data = extremes,
#   #                  aes(x = objective,y = min, col = sens),
#   #                  linetype = "dotted")
#   #     }
#   #   } +
#   #   {
#   #     if(i ==length(unique(object_plus))){
#   #       geom_boxplot(data = extremes,
#   #                    aes(x = objective,y = max, col = sens),
#   #                    linetype = "dotted")
#   #     }
#   #   } +
#   #  # geom_point(data = zeroes, aes(x = objective, y = value), size = 1.5, shape = 3, stroke = 1) +
#   #   #geom_boxplot() +
#   #   ylab(NULL) +
#   #   xlab(NULL)+
#   #   labs(colour = "Net Benefits") +
#   #   facet_grid(~objective, scales = "free", space = "free") +
#   #   # coord_flip() +
#   #   theme_classic()+
#   #      ylim(mins %>% dplyr::select(-sens) %>%  min ,maxes %>% dplyr::select(-sens) %>% max) +
#   #   theme(
#   #     strip.background = element_blank(),
#   #     strip.text.x = element_blank(),
#   #     axis.line.x = element_blank(),
#   #     axis.text.x = #if(i>=length(unique(objectives))){
#   #       element_text(#angle = 45,
#   #                    vjust = .95,
#   #                   # hjust = 1
#   #                   )
#   #    # } else {
#   #    #   element_blank()
#   #  #   }
#   #  ) +
#   #   scale_color_manual(values = pal, 
#   #                      breaks = brks
#   #   ) +
#   #   scale_fill_manual(values = c("grey65","red"),
#   #                     guide = "none")
#   # 
#   para_dec[[i]] <- df_para %>% dplyr::select(-c(objective, value)) %>% 
#     distinct() %>% 
#     relocate(aSeq_Proportion) %>% 
#     pivot_longer(cols = all_of(c("aSeq_Proportion",names(parms))), names_to = "variable", values_to = "value") %>% 
#     
#     mutate(variable = ifelse(variable %>% str_detect("aHarvest_"), "Harvest",variable),
#            variable = case_when(variable == "aFood_Proportion" ~ "Food",
#                                 variable == "aSeq_Proportion" ~ "Carbon",
#                                 TRUE ~ variable),
#              factor(variable, levels = c("Harvest", "Food", "Carbon")),
#            facet = ifelse(variable %in% "Harvest",1,2)) %>% 
#     ggplot(aes(x = variable, y = value, col = sens, 
#              #  col = factor(Benefits)
#                )) +
#    # geom_point() + 
#    # geom_line() +
#     geom_boxplot() +
#     ylab("Proportion") +
#     xlab(NULL)+
#     labs(colour = "Net Benefits") +
#     facet_grid(~facet, scales = "free_x",
#                space="free_x") +
#     # coord_flip() +
#     ggtitle(paste0(c(#"A: Maximize ",
#       "B: Maximize ","C: Maximize ",#"D: Maximize ",
#                      "A: ")[i],c(obj,"Holistic")[i])) +
#     theme_classic()+
#     ylim(0,1)+
#     theme(
#       title = element_text(size = 8),
#       strip.background = element_blank(),
#       strip.text.x = element_blank(),
#       
#         axis.text.x = #if(i %in% c())){
#                             element_text(#angle = 45,
#                                  vjust = .95,
#                                #  hjust = 1
#                                )
#        # } else {
#         #  element_blank()
#       #  }
#       ,
#       legend.position = "none"
#     ) +
#     scale_color_manual(values = pal, 
#                        breaks = brks
#     )
#   
#   
#          para_plots <-  para_dec[[i]] +  para_obj[[i]] + plot_layout(ncol = 2, widths = c(1,2), guides = "collect")
#   
# #  ggsave(para_plots,file=here(paste0("Plots/para_plot_maxes",obj[i],".png")), device = "png", width = 20, height = 10, units = "cm") 
#   #assign(paste0("para_plot_",obj[i]), para_plot)
# }
# 
# if(df_para %>% filter(Benefits == "ALL") %>% nrow() == 0){ 
#   layout <- '
# AAABBBBCCCDDDD
# EEEGGGG#######
# '
#   wrap_plots(A = para_dec[[1]], B = para_obj[[1]], C = para_dec[[2]], D = para_obj[[2]], 
#            #  E = para_dec[[3]], G = para_obj[[3]], #H = para_dec[[4]], I = para_obj[[4]], 
#              design = layout)
#   
#   ggsave(file=here(paste0("Plots/","main",".png")), device = "png", width = 25, height = 15, units = "cm")
# 
#   } else {
#   if (!(sens_index %in% c(4))){
#     layout <- '
# ACH
# '
#     wrap_plots(A = para_dec[[3]], 
#              #  B = para_obj[[3]], 
#                C = para_dec[[1]], 
#              #  D = para_obj[[1]], 
#               # E = para_dec[[2]], 
#               # G = para_obj[[2]], 
#                H = para_dec[[2]], 
#              #  I = para_obj[[2]], 
#                # J =  para_dec[[5]],
#                #K = para_obj[[5]],
#                design = layout)
#     
#     ggsave(file=here(paste0("Plots/","main",".png")), device = "png", width = 20, height = 7, units = "cm")
#     
# } else {
#  # message("no")
#   layout <- '
# AAABBBB
# CCCDDDD
# EEEGGGG
# HHHIIII
# '
#   wrap_plots(A = para_dec[[4]], 
#              B = para_obj[[4]], 
#              C = para_dec[[1]], 
#              D = para_obj[[1]], 
#              E = para_dec[[2]], 
#              G = para_obj[[2]], 
#              H = para_dec[[3]], 
#              I = para_obj[[3]], 
#             # J =  para_dec[[5]],
#             #K = para_obj[[5]],
#              design = layout)
#  
#   ggsave(file=here(paste0("Plots/",sens_names_[sens_index],".png")), device = "png", width = 25, height = 15, units = "cm")
# }
#   }
# 
