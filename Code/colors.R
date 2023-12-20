

colors_from <- function(file, n=8, cs="scRGB",name = "",type = c("discrete", "continuous"), sort = FALSE){
  #print(cs) 
  require(beyonce, quietly = TRUE)
  require(tidyverse, quietly = TRUE)
  im <- magick::image_read(file)
  type <- match.arg(type)
  tmp <- im %>% magick::image_resize("100") %>% 
    magick::image_quantize(max = 2*n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
    imager::magick2cimg() %>%  ## I'm converting, becauase I want to use as.data.frame function in imager package.
    imager::RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
    mutate(hex=hsv(scales::rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=T) %>% 
    mutate(colorspace = cs)
  
pal <-   tmp %>% dplyr::select(colorspace,hex,hue,sat,value,n) %>% 
    pull(hex)

if(sort){
pal_rgb <- col2rgb(pal)
tsp <- TSP::as.TSP(dist(t(pal_rgb)))
sol <- TSP::solve_TSP(tsp, control = list(repetitions = 1e3))
pal <- pal[sol]
}
  
  out <- switch(type, continuous = colorRampPalette(pal)(n), 
                discrete = pal[1:n])
  structure(out, class = "palette", number = name
            )
  
 # return(out) ## I want data frame as a result.
}


