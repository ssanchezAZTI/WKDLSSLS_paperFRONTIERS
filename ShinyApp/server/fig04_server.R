
dat04 <- dat_bioQ %>% filter(indicator %in% c("ssb","catch") &
                               LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny" &
                               HCRT=="2o3" & UCPL==0.2 & UCPU==0.2 & BSAFE=="none")

refpts <- list()
for (lh in unique(dat04$LHnam))
  refpts[[lh]] <- loadToEnv(file.path("data",paste0(lh,"_dataLH.RData")))[["ref.pts"]]

ref04 <- data.frame(STKN = rep(unique(dat04$STKN),each=3),
                    indicator = rep(c(rep("ssb",2),"catch"),length(unique(dat04$STKN))),
                    Bref = unlist(lapply(refpts, function(x) c(Blim=x[["Blim"]], Bcollapse=0.1*x[["B0"]],
                                                               MSY=x[["MSY"]]))))


#! MISSING ITERATIONS LINES
# # all iterations (wide format)
# 
# dd.its <- NULL
# 
# sc0 <- unique(dat_bio$scenario)[1]
# nit.sample <- 2
# it.sel <- sample( loadToEnv(file.path("data",paste0("out_",sc0,".RData")))[[paste(sc0,"bio",sep="_")]] %>%
#                     .$iter %>% unique(), nit.sample)
# #   6 466
# 
# for (sc in unique(dat04$scenario)) {
# 
#   # specific scenario
# 
#   sc.its <- loadToEnv(file.path("data",paste0("out_",sc,".RData")))[[paste(sc,"bio",sep="_")]]
# 
#   # specific iteration
# 
#   sc.its <- sc.its %>% filter(iter %in% it.sel) %>%
#     select(scenario,stock,year,iter,one_of(c("ssb","catch"))) %>%
#     mutate(iter = as.factor(iter))
# 
#   dd.its <- rbind(dd.its, sc.its)
# 
#   rm(sc.its)
# 
# }
# 
# # add information on scenario
# 
# dd.its <- dd.its %>% rename(SCENARIO = scenario) %>%
#   left_join(sc.dat, by="SCENARIO") %>% select( names(sc.dat), names(dd.its[-1])) %>%
#   mutate(FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")))
# 
# # reshape to the long format for ggplot
# 
# dat04b <- reshape( dd.its, direction="long", varying=names(dd.its)[-c(1:20)], v.names=c("value"),
#                  idvar=names(dd.its)[c(1:20)], timevar="indicator", times=names(dd.its)[-c(1:20)]) %>%
#   mutate(fhist = factor(FHIST, levels=c("flow","fopt","fhigh")))

output$plot04 <- renderPlot({

  dat04 <- dat04 %>%
    filter(STKN %in% input$stkn04 & FHIST %in% input$fhist04 &
             indicator %in% input$id04)
  
  ref04 <- ref04 %>% 
    filter(STKN %in% input$stkn04 & indicator %in% input$id04)
  
  nom04 <- length(input$stkn04)*length(input$fhist04)
  
  if( !"catch" %in% input$id04) {
    col04 <- rep(c("orange", "red"),nom04)
  } else if (!"ssb" %in% input$id04) {
    col04 <- rep(c("green"),nom04)
  } else 
    col04 <- rep(c("orange", "red", "green"),nom04)
    
  
  dat04 %>% 
    ggplot(aes(x = year, y = q50)) +
    geom_line() +
    geom_ribbon(aes(x = year, ymin = q05, ymax = q95), alpha = 0.35) +
    # geom_line(data=dat04b, aes(year, y=value, group=iter, col=iter))+
    facet_grid(FHIST ~ STKN + indicator) +
    expand_limits(y=0) +
    geom_vline(xintercept = proj.yr - 0.5, linetype = "longdash") +
    geom_hline(lty = 2, data = ref04,
               aes(yintercept = Bref), color = col04) +
    theme_bw() +
    theme(text = element_text(size = 18),
          title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 18),
          legend.position = "none") +
    ylab("tonnes") +
    theme(plot.title = element_text(hjust = 0.5))

})


output$text04 <- renderUI({

  tagList(tags$b("Figure 4 (interactive version)."),
          "Trajectories of catch and SSB in tonnes along years (x-axis) for the 2-over-3 rule 
          with a 20% uncertainty cap and under an in-year advice for different life histories: 
          stock-types in columns and historical exploitation levels in rows. 
          The solid line represents the median and the shaded area the 90% confidence intervals 
          computed from the 5th and 95th percentiles and coloured lines represent specific iterations. 
          The dashed vertical line is located before year 31, which is the first year of the projection. 
          The dashed horizontal lines represent the different reference points: 
          the green line in catch plots correspond to MSY and orange and 
          red lines in SSB plots to Blim (20% B0) and Bcollapse (10% B0), respectively.")

})
