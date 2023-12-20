
plot_combos <- t(combn(1:length(objectives),2))

bars <- list()

for (sens_index in all_sens){
  df <- df_result %>% filter(sens == sens_index) %>% 
    {if(sens_index != 1){filter(., optimized == "Optimized")}else{.}
    } %>% 
    filter(sFood > 0) 
  maxes <- df %>% 
    filter(optimized == "Optimized") %>% 
    dplyr::select(all_of(objectives)) %>% summarise(across(everything(), max))
  df <- df  %>% 
    mutate(extreme = sFood == maxes$sFood | 
             sBiodiversity_Benefit == maxes$sBiodiversity_Benefit |
             sCO2e_Mitigation == maxes$sCO2e_Mitigation |
             sProfit == maxes$sProfit,
           SO = ifelse(extreme, "Single-Objective",Trades)) %>% 
    mutate( Trades = factor(Trades, levels = fact)) %>% 
    relocate(all_of(objectives))
  
  figs <- list()
  fig_bar <- list()
  

 spacer = -0.1
 icons_par <- icons[c(2,3,1,4)]
  for (i in 1:nrow(plot_combos)) {
    
    obj1 = names(df)[plot_combos[i,1]]
    obj2 = names(df)[plot_combos[i,2]]
    df_plot = df %>% 
      dplyr::select(!!as.symbol(obj1),!!as.symbol(obj2),Trades,optimized,SO)
    
    df_opti <- df_plot %>% filter(optimized == "Optimized")
 
    icon1 = icons_par[[plot_combos[i,1]]]
    icon2 = icons_par[[plot_combos[i,2]]]
    
    tit1 = pubnames[which(var_names==obj1,useNames = TRUE)]
    #mod1 = unit_mod[which(var_names==obj1,useNames = TRUE)]
    
    tit2 = pubnames[which(var_names==obj2,useNames = TRUE)]
  #  mod2 = unit_mod[which(var_names==obj2,useNames = TRUE)]
    
  #bar_offset[i] <- 0.05*(max(!!as.symbol(obj1))-min(!!as.symbol(obj1)))  
    point_size = 1.5
    
    figs[[i]] <- df_opti %>% {ggplot() +
        geom_point(data = filter(df_plot,SO == "Single-Objective"),
                   size = 3*point_size, aes(x = !!as.symbol(names(.)[1]),
                                 y = !!as.symbol(names(.)[2]),
                                 # fill = Benefits
                                 shape = optimized),
                   col = pal_pareto[4],
        ) +
        geom_point(data = filter(df_plot,optimized == "Naive"),
                   size = .6*point_size, aes(x = !!as.symbol(names(.)[1]),
                                             y = !!as.symbol(names(.)[2]),
                                             # fill = Benefits,
                                             
                                             shape = optimized),
                   col = "grey10",
                   stroke = .8,
                   alpha = 0.2
        ) +
      geom_point(data = filter(df_plot,optimized != "Naive"),
        size = 1.5*point_size, 
                 aes(x = !!as.symbol(names(.)[1]),
                                 y = !!as.symbol(names(.)[2]),
                                 # fill = Benefits,
                                 col = Trades,
                                 shape = optimized)
                                 #,
      
                 #stroke = 2
      )  + 
      geom_segment(data = .,aes(x = spacer*(max(!!as.symbol(names(.)[1]))-min(!!as.symbol(names(.)[1])))  + min(!!as.symbol(names(.)[1])),
                       xend = spacer*(max(!!as.symbol(names(.)[1]))-min(!!as.symbol(names(.)[1])))  + min(!!as.symbol(names(.)[1])), 
                       y = min(!!as.symbol(names(.)[2])), 
                       yend = max(!!as.symbol(names(.)[2]))),
                   col = pal_pareto[3], size =1*point_size, lineend = 'round')  +
        geom_segment(data = .,aes(y = spacer*(max(!!as.symbol(names(.)[2]))-min(!!as.symbol(names(.)[2])))  + min(!!as.symbol(names(.)[2])),
                         yend = spacer*(max(!!as.symbol(names(.)[2]))-min(!!as.symbol(names(.)[2])))  + min(!!as.symbol(names(.)[2])), 
                         x = min(!!as.symbol(names(.)[1])), 
                         xend = max(!!as.symbol(names(.)[1]))),
                     col = pal_pareto[3], size =1*point_size, lineend = 'round')  +
        geom_point(data = .,aes(x = spacer*(max(!!as.symbol(names(.)[1]))-min(!!as.symbol(names(.)[1])))  + min(!!as.symbol(names(.)[1])),
                       y = max(!!as.symbol(names(.)[2]))),
                   col = pal_pareto[3], size =2*point_size) +
        geom_point(data = .,aes(y = spacer*(max(!!as.symbol(names(.)[2]))-min(!!as.symbol(names(.)[2])))  + min(!!as.symbol(names(.)[2])),
                       x = max(!!as.symbol(names(.)[1]))),
                   col = pal_pareto[3], size =2*point_size) +
      geom_segment(data = .,#data = filter(.,optimized != "Naive"),
                   aes(x = spacer*(max(!!as.symbol(names(.)[1]))-min(!!as.symbol(names(.)[1])))  + min(!!as.symbol(names(.)[1])),
                       xend = spacer*(max(!!as.symbol(names(.)[1]))-min(!!as.symbol(names(.)[1])))  + min(!!as.symbol(names(.)[1])), 
                       y = max(filter(.,Trades == "Holistic") %>% pull(!!as.symbol(names(.)[2]))), 
                       yend = min(filter(.,Trades == "Holistic") %>% pull(!!as.symbol(names(.)[2])))),
                   col = ifelse(nrow(df %>% filter(Trades =="Holistic"))==0,pal_pareto[2],pal_pareto[1]), size = ifelse(nrow(df %>% filter(Trades =="Holistic"))==0,0,2)*point_size, lineend = 'round') +
        geom_segment(data = .,#data = filter(.,optimized != "Naive"),
                     aes(y = spacer*(max(!!as.symbol(names(.)[2]))-min(!!as.symbol(names(.)[2])))  + min(!!as.symbol(names(.)[2])),
                         yend = spacer*(max(!!as.symbol(names(.)[2]))-min(!!as.symbol(names(.)[2])))  + min(!!as.symbol(names(.)[2])), 
                         x = max(filter(.,Trades == "Holistic") %>% pull(!!as.symbol(names(.)[1]))), 
                         xend = min(filter(.,Trades == "Holistic") %>% pull(!!as.symbol(names(.)[1])))),
                     col = ifelse(nrow(df %>% filter(Trades =="Holistic"))==0,pal_pareto[2],pal_pareto[1]), size =ifelse(nrow(df %>% filter(Trades =="Holistic"))==0,0,2)*point_size, lineend = 'round')  +
           theme_classic() +
        xlim(spacer*(max(df_opti %>% pull(!!as.symbol(names(.)[1])))-min(df_opti %>% pull(!!as.symbol(names(.)[1]))))  + min(df_opti %>% pull(!!as.symbol(names(.)[1]))),max(df_opti %>% pull(!!as.symbol(names(.)[1])))) +
        ylim(spacer*(max(df_opti %>% pull(!!as.symbol(names(.)[2])))-min(df_opti %>% pull(!!as.symbol(names(.)[2]))))  + min(df_opti %>% pull(!!as.symbol(names(.)[2]))),max(df_opti %>% pull(!!as.symbol(names(.)[2])))) +
        
                theme(#legend.position = c(0.85,0.75),
              axis.title.x = element_text(hjust = 0.45),
              axis.title.y = element_text(hjust = 0.45)) +
      # scale_x_continuous(obj1) +
      # scale_y_continuous(obj2) +
      labs(color = "Management"#,
           # shape = "Scenario"
      ) +
        guides(color = guide_legend(override.aes = list(shape = c(16,16,16,16,1),
                                                        size = c(3,3,3,3,2))),
               shape = "none") +
        xlab(label = tit1) +
      ylab(label = tit2) +
      scale_colour_manual(values = pal_pareto,
                          breaks = fact,
                          limits = fact,
                          labels = fact
                          ) +
       scale_shape_manual(values = c(1,16),
                          name = NULL,
                         # labels = c("Naive",element_blank()),
                          breaks = c("Naive","Optimized")
                         )
      
  
      # %>% 
     #   ggdraw(insert_xaxis_grob(plot = ., grob = icon1, position = "top", clip = "off")) %>% 
     #   ggdraw(insert_yaxis_grob(plot = ., grob = icon2, position = "right", clip = "off"))
            # inset_element(p = icon1,
            #               left = 0.85,
            #               right = 0.95,
            #               bottom = 0.1,
            #               top = 0.15,
            #               align_to = "plot",
            #               ignore_tag = TRUE,
            #               on_top = TRUE,
            #               clip = FALSE) +
            # inset_element(p = icon2,
            #               left = 0.1,
            #               right = 0.15,
            #               bottom = 0.85,
            #               top = 0.95,
            #               align_to = "plot",
            #               ignore_tag = TRUE,
            #               on_top = TRUE,
            #               clip = FALSE)
    # scale_color_manual(values = c(pal[1],pal[1],1)
    #  ggsave(file=here(paste0("Plots/Pair_Plots/pair_",j,obj1,"_",obj2,".png")), device = "png", width = 14, height = 10, units = "cm") 
    }
  #  leg <- get_legend(figs[[i]])
#      figs[[i]] <-  ggdraw(plot = figs[[i]])
#     figs[[i]] <-  ggdraw(insert_xaxis_grob(plot = figs[[i]], grob = icon1, position = "bottom", clip = "off", height = grid::unit(0.1, "null")))
# (          figs[[i]] <- ggdraw(insert_yaxis_grob(plot = figs[[i]], grob = icon2, position = "left", clip = "off", width = grid::unit(0.05, "null"))))
#     
#       
 fig_bar[[i]] <- df_opti %>% {ggplot(.) +
      
      geom_segment(aes(y = spacer*(max(!!as.symbol(names(.)[2]))-min(!!as.symbol(names(.)[2])))  + min(!!as.symbol(names(.)[2])),
                       yend = spacer*(max(!!as.symbol(names(.)[2]))-min(!!as.symbol(names(.)[2])))  + min(!!as.symbol(names(.)[2])), 
                       x = min(!!as.symbol(names(.)[1])), 
                       xend = max(!!as.symbol(names(.)[1]))),
                   col = pal_pareto[3], size =2, lineend = 'round')  +
        geom_segment(aes(y = spacer*(max(!!as.symbol(names(.)[2]))-min(!!as.symbol(names(.)[2])))  + min(!!as.symbol(names(.)[2])),
                         yend = spacer*(max(!!as.symbol(names(.)[2]))-min(!!as.symbol(names(.)[2])))  + min(!!as.symbol(names(.)[2])), 
                         x = max(filter(.,Trades == "Holistic") %>% pull(!!as.symbol(names(.)[1]))), 
                         xend = min(filter(.,Trades == "Holistic") %>% pull(!!as.symbol(names(.)[1])))),
                     col = pal_pareto[1], size =2) +
        
        theme_classic() +
     theme(axis.line.y = element_blank(),
           axis.ticks.y = element_blank(),
           axis.text.y = element_blank())+
        # scale_x_continuous(obj1) +
        # scale_y_continuous(obj2) +
        labs(color = "Management"#,
             # shape = "Scenario"
        ) +
        # guides(shape = FALSE)+
        xlab(tit1) +
        ylab(NULL) +
        scale_colour_manual(values = pal_pareto,
                            breaks = fact,
        ) +
        scale_shape_manual(values = c(17,1,19),
                           )
 }
  }
 
 
 df_corr_sen <- df_corr %>% filter(sens == sens_names[sens_index %>% as.numeric])
 corr_tresh = 0.9
 include_food <- case_when(df_corr_sen$F_P > corr_tresh ~ FALSE,
                           TRUE ~ TRUE
             )
 include_hab <- case_when(df_corr_sen$C_H > corr_tresh ~ FALSE,
                          TRUE ~ TRUE)
 
if(sens_index!=1#include_food & include_hab
  ){  
             figures <- figs[[1]] + figs[[2]] + figs[[3]] + figs[[4]] + figs[[5]] + figs[[6]] + 
             plot_layout(ncol = 3, guides = "collect")  +
             plot_annotation(tag_levels = "A") 
           height = 12
           width = 20
} else {
  # # if(include_food){
  # #   figures <- figs[[1]] + figs[[2]] + figs[[3]] + 
  # #     plot_layout(ncol = 3, guides = "collect")  +
  # #     plot_annotation(tag_levels = "A") 
  # #   height = 10
  # #   width = 25
  # # } else {
  # #   if(include_hab){
  #   figures <-figs[[4]] + figs[[5]] + figs[[6]] + 
  #     plot_layout(ncol = 3, guides = "collect")  +
  #     plot_annotation(tag_levels = "A") 
  #   height = 10
  #   width = 34
  #   } else {
      design = '
      1112
      3344'
      figures <- figs[[6]] + guide_area() + figs[[3]]+figs[[4]]  +
        plot_annotation(tag_levels = "A")  + 
        plot_layout(design = design, guides = "collect", heights = c(2,1)) &
        theme(legend.position="right")
      height = 20
      width = 16
#}
 # }
}

#  
#   if (sens_index %in% c(14)){
#     figures <- figs[[1]] + figs[[2]] + figs[[3]] + figs[[4]] + figs[[5]] + figs[[6]] + 
#       plot_layout(ncol = 3, guides = "collect")  +
#       plot_annotation(tag_levels = "A") 
#     height = 20
#  #   bars[[sens_index]] <- fig_bar
#   } else {
#     figures <- figs[[4]] + figs[[5]] + figs[[6]] + 
#       plot_layout(ncol = 3, guides = "collect")  +
#       plot_annotation(tag_levels = "A") 
#     height = 10
# }
#  message("Saving ",sens_index)
  ggsave(figures #+ plot_annotation(title = sens_names[sens_index %>% as.numeric])
         ,
         file=here(paste0("Plots/Pareto_",sens_index,".png")), device = "png", width = width, height = height, units = "cm") 

  caption <- paste0("Scenario: ",sens_names[sens_index %>% as.double])
  
  message("\\begin{figure}[!htb]")
  message("\\centering")
  message("\\includegraphics[width=1\\textwidth]{Pareto_",sens_index,".png}")
  message("\\caption[",caption,"]{",caption,"}")
  message("\\label{fig:Pareto_",sens_names[sens_index %>% as.double],"}")
  message("\\end{figure}")
  

  
  }


