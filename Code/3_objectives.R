
f.object <- function(x) { ## x = the paramter vector 'parms'
  p = structure(x, names=names(parms))
  temp = 1 - sum(p[names(parms)[which(names(parms) %>% str_detect("aF"))]])
  if(temp < 0) {return(rep(-999999999,length(objectives)))
    }  else { 
      o <-  as.data.frame(data.frame(ode(y = stocks, times = simTime, func = model, parms = c(p,auxs,sens,env), method = "euler")) %>% 
                               select(all_of(objectives)) %>% 
                               slice_tail())
      }
  if(is.na(sum(o))) {
   o = rep(-999999999,length(objectives))
  } 
  return(#ifelse(is.na(o),rep(-999999999,length(objectives)),
    as.numeric(o))
  #)
}

f.object_w <- function(x,y) { ## where x == parms as above and y == weight vector
  
  sum(y*f.object(x))}


f.object_min <- function(x) {
  -f.object(x)
}

f.object_all <- function(x) {
  temp_0 <- f.object(x)
  if((temp_0 < 0) %>% sum() > 0 ) {rep(-999999999,length(objectives))} else { temp_0 }
}

