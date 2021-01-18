
library(dplyr)

res.dir <- "./output"

files <- list.files(file.path(res.dir,"output_scenarios"))
sl <- unique(sapply(gsub('.{6}$', '', files),function(x){strsplit(x,"_")[[1]][2]}))


# SCENARIOS with "problems" in calendar comparison

# STK2_fhigh
# HCRT         UC      int      iny      fpa
#  1o2  (0.2,0.2) sc000981 sc000979 sc000980
#  1o2 (0.2,0.25) sc000993 sc000991 sc000992
#  1o3 (0.2,0.25) sc000996 sc000994 sc000995
#  1o5 (0.2,0.25) sc001002 sc001000 sc001001
#  2o3  (0.2,0.2) sc000987 sc000985 sc000986
#  2o3 (0.2,0.25) sc000999 sc000997 sc000998

sl <- c( "sc000979", "sc000980", "sc000981", # STK2 + fhigh + 1o2_(0.2,0.2)
         "sc000985", "sc000986", "sc000987", # STK2 + fhigh + 2o3_(0.2,0.2)
         "sc000991", "sc000992", "sc000993", # STK2 + fhigh + 1o2_(0.2,0.25)
         "sc000994", "sc000995", "sc000996", # STK2 + fhigh + 1o3_(0.2,0.25) 
         "sc000997", "sc000998", "sc000999", # STK2 + fhigh + 2o3_(0.2,0.25) 
         "sc001000", "sc001001", "sc001002") # STK2 + fhigh + 1o5_(0.2,0.25)
    

it <- 464

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
      dplyr::rename(index=data)
    
    obj.bio <- left_join(obj.bio, obj.idx, by=c("year","iter")) 
  }else{
    obj.bio$index <- NA
  } 
  
  # compute hr
  
  obj.bio <- obj.bio %>% mutate(hr=catch/index)
  
  # seasonal values 
  
  tmp <- obj.biosem %>% group_by(stock, scenario, iter, year) %>%
    dplyr::summarize(biomass.sem1 = biomass[season==1], 
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
  
  obj.bio <- left_join(obj.bio, select(obj.adv, stock, scenario, iter, year, tac), by=c("stock", "scenario", "iter", "year")) 
  
  # compute quotaUp based on management calendar
  
  obj.bio <- obj.bio %>% mutate(quotaUpt=catch.mp/tac)
  
  
  # select iteration = it
  
  obj.it <- obj.bio %>% ungroup() %>% 
    filter(iter == it) %>% select(-iter) %>% 
    select(scenario, year, rec, biomass.sem1, biomass.sem2, ssb, f.sem1, f.sem2, catch.sem1, catch.sem2, catch.mp, tac, index)
  
  # Join all
  
  out <- rbind(out, obj.it)
  
}

# Add information on scenario

# add the other variables for scenario description
 
sc.dat <- read.table(file.path("input","list_scenarios.csv"), header=T, sep=",") %>% 
  dplyr::rename(scenario=SCENARIO)

out <- out %>% left_join(sc.dat, by="scenario") %>% 
  select(scenario, OM:BSAFE, year:index)


# Save the data
write.table(out, file=file.path(res.dir,"traces_STK2fhigh_allCalendars_HCRselection.csv"), sep=";", row.names=F, append=F)

