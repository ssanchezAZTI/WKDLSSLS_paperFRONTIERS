
library(dplyr)

res.dir <- "./output"

files <- list.files(file.path(res.dir,"output_scenarios"))
sl <- unique(sapply(gsub('.{6}$', '', files),function(x){strsplit(x,"_")[[1]][2]}))

# SCENARIOS: fopt, 1o2_(0.8,0.8) & 1o2_(0.5,1.5)
sl <- c( "sc000062", "sc000063", "sc000064", # STK1 + 1o2_(0.5,1.5)
         "sc000074", "sc000075", "sc000076", # STK1 + 1o2_(0.8,0.8)
         "sc000641", "sc000642", "sc000643", # STK2 + 1o2_(0.5,1.5)
         "sc000653", "sc000654", "sc000655") # STK1 + 1o2_(0.8,0.8)

# # SCENARIOS: fhig, 1o2_(0.8,0.8) & 1o2_(0.5,1.5)
# sl <- c( "sc000448", "sc000449", "sc000450", # STK1 + 1o2_(0.5,1.5)
#          "sc000460", "sc000461", "sc000462", # STK1 + 1o2_(0.8,0.8)
#          "sc001027", "sc001028", "sc001029", # STK2 + 1o2_(0.5,1.5)
#          "sc001039", "sc001040", "sc001041") # STK1 + 1o2_(0.8,0.8)

it <- 2

out <- NULL

for (scenario in sl){
  
  print(paste("~~~ Working on scenario", scenario))
  print(Sys.time())
  
  file.RData <- file.path(res.dir, "output_scenarios", paste0("out_",scenario,".RData"))
  
  require(dplyr)
  
  # load results ("biosem","bio","adv","pars","idx","refpts") 
  
  load(file.RData)  
  obj.bio <- get( paste(scenario,"bio",sep="_") )
  obj.biosem <- get( paste(scenario,"biosem",sep="_") )
  obj.adv <- get( paste(scenario,"adv",sep="_") )
  obj.pars <- get( paste(scenario,"pars",sep="_") )
  obj.idx <- get( paste(scenario,"idx",sep="_") )
  obj.refpts <- get( paste(scenario,"refpts",sep="_") )
  
  # make compatible outputs
  obj.bio    <- obj.bio %>% ungroup() %>% 
    mutate(year = as.character(year), iter = as.character(iter))
  obj.biosem <- obj.biosem %>%  ungroup() %>%
    mutate(year = as.character(year), season = as.character(season), iter = as.character(iter))
  obj.adv    <- obj.adv %>% ungroup() %>% 
    mutate(year = as.character(year), iter = as.character(iter))
  
  # add index
  
  if (!is.null(obj.idx)){
    obj.idx <- obj.idx %>% select(year, iter, data)%>%
      mutate(year=as.character(year),
             iter=as.character(iter)) %>%
      rename(index=data)
    
    obj.bio <- left_join(obj.bio, obj.idx, by=c("year","iter")) 
  }else{
    obj.bio$index <- NA
  } 
  
  # compute hr
  
  obj.bio <- obj.bio %>% mutate(hr=catch/index)
  
  # seasonal values 
  
  tmp <- obj.biosem %>% group_by(stock, scenario, iter, year) %>%
    summarize(biomass.sem1 = biomass[season==1], 
              biomass.sem2 = biomass[season==2], 
              catch.sem1=catch[season==1], 
              catch.sem2=catch[season==2], 
              f.sem1=f[season==1], 
              f.sem2=f[season==2])
  
  obj.bio <- left_join(obj.bio, tmp, by=c("stock", "scenario", "iter", "year"))
  
  # compute catch from the mp calendar
  
  if(obj.pars$ADVT=="iny"){
    obj.bio <- obj.bio %>% 
      group_by(stock, scenario, iter) %>% 
      arrange(as.numeric(year)) %>%
      mutate(catch.mp=catch.sem2 + lead(catch.sem1))  
  }else{
    obj.bio <- obj.bio %>% mutate(catch.mp=catch)
  }
  
  # add tac 
  
  obj.bio <-  left_join(obj.bio, select(obj.adv, stock, scenario, iter, year, tac), by=c("stock", "scenario", "iter", "year")) 
  
  # compute quotaUp based on management calendar
  
  obj.bio <- obj.bio %>% mutate(quotaUpt=catch.mp/tac)
  
  
  # select iteration = it
  
  obj.it <- obj.bio %>% ungroup() %>% 
    filter(iter == it) %>% select(-iter) %>% 
    select(scenario, year, rec, biomass.sem1, biomass.sem2, ssb, f.sem1, f.sem2, catch.sem1, catch.sem2, catch.mp, tac, index)
  
  # Join all
  
  out <- rbind(out, obj.it)
  
}

# Save the data
write.table(out, file=file.path(res.dir,"traces_fopt_allCalendars.csv"), sep=";", row.names=F, append=F)
# write.table(out, file=file.path(res.dir,"traces_fhigh_allCalendars.csv"), sep=";", row.names=F, append=F)

