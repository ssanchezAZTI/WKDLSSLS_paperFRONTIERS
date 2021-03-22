
dat09 <- df_bc %>% 
  filter(indicator %in% perfnms & 
           LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny") %>% 
  select(term, STKN, FHIST, OMnam, HCRT, BSAFE, HCR, UC, indicator, value) %>% 
  tidyr::spread(indicator, value)

# Restrict to BSAFE cases

rls  <- grep( "_I", unique(dat09$HCR), value = TRUE) %>% substr(1,3) %>% unique()
ucps <- dat09 %>% filter(grepl( "_I", HCR)) %>% .$UC %>% unique()

dat09 <- dat09 %>% filter(HCRT %in% rls & UC %in% ucps)


output$plot09 <- renderPlot({
  
  dat09 <- dat09 %>%
    filter(STKN %in% input$stkn09 & FHIST %in% input$fhist09 & term %in% input$term09 & 
             HCRT %in% input$hcrt09 & UC %in% input$uc09)
  
  p09 <- dat09 %>%  
    ggplot(aes(x=catch.MSY, y=Risk3.Blim, shape=HCR, colour=UC))+
    geom_point(size=3)+
    geom_hline(yintercept = 0.05, linetype = "longdash")+
    scale_colour_manual(values = ucp.col3)+
    theme(text = element_text(size = 18), 
          title = element_text(size = 14, face = "bold"), 
          strip.text = element_text(size = 16))
  
  if (input$idpos09 == "STKN_FHIST in cols") {
    p09 + facet_grid(term ~ OMnam, scales="free_y")
  } else if (input$idpos09 == "STKN_FHIST in rows") {
    p09 + facet_grid(OMnam ~ term, scales="free_y")
  } else if (input$idpos09 == "STKN in cols & FHIST in rows") {
    p09 + facet_grid(FHIST ~ STKN + term, scales="free_y")
  } else if (input$idpos09 == "STKN in rows & FHIST in cols") {
    p09 + facet_grid(STKN + term ~ FHIST, scales="free_y")
  }
  
})


output$text09 <- renderUI({
  
  if (input$idpos09 == "STKN_FHIST in cols") {
    cols09 <- "the different operating models (as combination of the stock-type and historical exploitation)"
    # rows09 <- "the temporal scales: the short-term (first 5 projection years), medium-term (next 5 projection years) and the long-term (last 10 projection years)"
    rows09 <- paste0( "the temporal scales: ", paste(paste0(input$term09, "-term (", TERMdef[input$term09],")"), collapse = ", "))
  } else if (input$idpos09 == "STKN_FHIST in rows") {
    rows09 <- "the different operating models (as combination of the stock-type and historical exploitation)"
    cols09 <- paste0( "the temporal scales: ", paste(paste0(input$term09, "-term (", TERMdef[input$term09],")"), collapse = ", "))
  } else if (input$idpos09 == "STKN in cols & FHIST in rows") {
    cols09 <- paste0( "the different stock-type and temporal scales: ", paste(paste0(input$term09, "-term (", TERMdef[input$term09],")"), collapse = ", "))
    rows09 <- "the historical exploitation level"
  } else if (input$idpos09 == "STKN in rows & FHIST in cols") {
    rows09 <- paste0( "the different stock-type and temporal scales: ", paste(paste0(input$term09, "-term (", TERMdef[input$term09],")"), collapse = ", "))
    cols09 <- "the historical exploitation level"
  }
  
  tagList(tags$b("Figure 9 (interactive version)."),
          "Biological risks (Risk3.Blim: maximum probability of falling below Blim) 
          versus the relative yields (catches/MSY) (x-axis) by rule type (symbols) for the ", 
          paste0(substr(input$hcrt09,1,1),"-over-",substr(input$hcrt09,3,3)), "rule", 
          "by uncertainty cap lower and upper limits (colours)and biomass safeguards (symbols). 
          The columns correspond to ", cols09,
          "and the rows to ", rows09,
          ". The horizontal black dashed line represents the 0.05 biological risk. 
          See Table 1 in Sanchez et al. (2021) for definitions of abbreviations.")
  
})
