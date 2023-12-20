df_corr <- df_result %>% 
  group_by(sens) %>% 
  summarise(F_P = cor(sProfit,sFood, method = "kendall" ),
            C_P = cor(sProfit,sCO2e_Mitigation, method = "kendall"),
            H_P = cor(sProfit,sBiodiversity_Benefit, method = "kendall"),
            F_C = cor(sCO2e_Mitigation,sFood, method = "kendall"),
            F_H = cor(sBiodiversity_Benefit,sFood, method = "kendall"),
            C_H = cor(sBiodiversity_Benefit,sCO2e_Mitigation, method = "kendall"),
  ) %>% 
  mutate(sens = sens_names[sens %>% as.numeric],
         sens = sens %>% factor(levels = sens_names %>% rev)) %>% 
  filter(!is.na(sens)) 

corr_plots <- list()
for (i in 2:ncol(df_corr)){
  to_plot <- names(df_corr)[i]
  name1 <- case_when(to_plot %>% str_detect("F")~"Food",
                     to_plot %>% str_detect("P")~"Profit",
                     to_plot %>% str_detect("C")~"Carbon",
                     to_plot %>% str_detect("H")~"Habitat",
                     )
  name2 <- case_when(to_plot %>% str_detect("H")~"Habitat",
                     to_plot %>% str_detect("C")~"Carbon",
                     to_plot %>% str_detect("P")~"Profit",
                     to_plot %>% str_detect("F")~"Food"
  )
  
 corr_plots[[i]] <- df_corr %>% 
    ggplot() +
    geom_col(aes(x = sens, y = !!as.symbol(to_plot))) +
    theme_classic() +
    #coord_cartesian() +
    coord_flip(#ylim=c(0.9, 1)
      ) +
    xlab(NULL) +
    ylab("Kendall Coefficient") +
    ggtitle(paste0("Between ",name1," and ",name2))
 
 
}
corr_plots[[5]] + corr_plots[[2]] + corr_plots[[3]] + corr_plots[[4]] + corr_plots[[6]] + corr_plots[[7]] +
  plot_layout(ncol = 2, guides = "collect")
  ggsave(here("Plots/coef.png"), device = "png", width = 30, height = 30, units = "cm")
  



# Food vs. Profit
df_result %>% 
  group_by(sens) %>% 
  summarise(co = cor(sProfit,sFood, method = "kendall")) %>% 
  mutate(sens = sens_names[sens %>% as.numeric],
         sens = sens %>% factor(levels = sens_names[all_sens] %>% rev)) %>% 
  filter(!is.na(sens)) %>% 
 { df_fp <<- . } %>%  
   ggplot() +
  geom_col(aes(x = sens, y = co)) +
  theme_classic() +
  #coord_cartesian() +
  coord_flip(ylim=c(min(df_fp$co), 1)) +
  xlab(NULL) +
  ylab("Pearson Coefficient") +
  ggtitle("Pearson Coefficient of Correlation \nBetween Food and Profit") 
ggsave(here("Plots/food_profit_coef.png"), device = "png", width = 18, height = 14, units = "cm")

# Carbon vs. Habitat
df_result %>% 
  group_by(sens) %>% 
  summarise(co = cor(sCO2e_Mitigation,sBiodiversity_Benefit, method = "kendall")) %>% 
  mutate(sens = sens_names[sens %>% as.numeric],
         sens = sens %>% factor(levels = sens_names[all_sens] %>% rev)) %>% 
  filter(!is.na(sens)) %>% 
  { df_ch <<- . } %>%  
    ggplot() +
  geom_col(aes(x = sens, y = co)) +
  theme_classic() +
  #coord_cartesian() +
  coord_flip(ylim=c(min(df_ch$co), 1)) +
  xlab(NULL) +
  ylab("Pearson Coefficient") +
  ggtitle("Pearson Coefficient of Correlation \nBetween Carbon and Habitat") 
ggsave(here("Plots/carbon_habitat_coef.png"), device = "png", width = 18, height = 14, units = "cm")
