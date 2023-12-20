## Figure Prep
library(beyonce)
library(png)
library(grid)
source("colors.R")





icons <- list.files(path = here("Plots/"), pattern = "icon_", full.names = TRUE) %>% 
  {icon_names <<-.} %>% 
  lapply(readPNG) %>% 
  lapply(function(x) rasterGrob(image = x, interpolate = TRUE))  
icon_names <- icon_names %>% str_remove(here("Plots/icon_")) %>% str_remove(".png")

names(icons) <- icon_names





(pal_pareto <- c(beyonce_palette(64,n = 18, type = 'continuous')[c(17,11)],#"grey65",
                "grey75", beyonce_palette(64,n = 36, type = 'continuous')[15],"grey10") %>% structure(class = "palette"))

pal <- rev(c(beyonce_palette(40)[c(2,3,4,6,7,8)],"grey45"#beyonce_palette(76)[7]
))






source("5_output.R")

main <- all_sens[-c(15:18)]
basics <- c(1,2,4,6,13,14,12,7,9#,10
            )
sens_do = lst(main,basics)



fact <- c("Holistic","Quasi-Holistic",
          "Non-Holistic", "Single-Objective","Naive")

