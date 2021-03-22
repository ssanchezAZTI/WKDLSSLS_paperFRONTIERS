
dat08 <- df_bc %>% 
  filter(indicator %in% perfnms & 
           LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny") %>% 
  select(term, STKN, FHIST, OMnam, HCRT, BSAFE, UC, indicator, value) %>% 
  tidyr::spread(indicator, value) %>% 
  filter( BSAFE == "none")


output$plot08 <- renderPlot({
  
  dat08 <- dat08 %>%
    filter(STKN %in% input$stkn08 & FHIST %in% input$fhist08 & term %in% input$term08 & 
             HCRT %in% input$hcrt08 & UC %in% input$uc08)
  
  p08 <- dat08 %>%  
    ggplot(aes(x=catch.MSY, y=Risk3.Blim, shape=HCRT, colour=UC))+
    geom_point(size=3)+
    geom_hline(yintercept = 0.05, linetype = "longdash")+
    scale_colour_manual(values = ucp.col2)+
    theme(text = element_text(size = 18), 
          title = element_text(size = 14, face = "bold"), 
          strip.text = element_text(size = 16))
  
  if (input$idpos08 == "STKN_FHIST in cols") {
    p08 + facet_grid(term ~ OMnam, scales="free_y")
  } else if (input$idpos08 == "STKN_FHIST in rows") {
    p08 + facet_grid(OMnam ~ term, scales="free_x")
  } else if (input$idpos08 == "STKN in cols & FHIST in rows") {
    p08 + facet_grid(FHIST ~ STKN + term)
  } else if (input$idpos08 == "STKN in rows & FHIST in cols") {
    p08 + facet_grid(STKN + term ~ FHIST)
  }
  
})


output$text08 <- renderUI({
  
  if (input$idpos08 == "STKN_FHIST in cols") {
    cols08 <- "the different operating models (as combination of the stock-type and historical exploitation)"
    # rows08 <- "the temporal scales: the short-term (first 5 projection years), medium-term (next 5 projection years) and the long-term (last 10 projection years)"
    rows08 <- paste0( "the temporal scales: ", paste(paste0(input$term08, "-term (", TERMdef[input$term08],")"), collapse = ", "))
  } else if (input$idpos08 == "STKN_FHIST in rows") {
    rows08 <- "the different operating models (as combination of the stock-type and historical exploitation)"
    cols08 <- paste0( "the temporal scales: ", paste(paste0(input$term08, "-term (", TERMdef[input$term08],")"), collapse = ", "))
  } else if (input$idpos08 == "STKN in cols & FHIST in rows") {
    cols08 <- paste0( "the different stock-type and temporal scales: ", paste(paste0(input$term08, "-term (", TERMdef[input$term08],")"), collapse = ", "))
    rows08 <- "the historical exploitation level"
  } else if (input$idpos08 == "STKN in rows & FHIST in cols") {
    rows08 <- paste0( "the different stock-type and temporal scales: ", paste(paste0(input$term08, "-term (", TERMdef[input$term08],")"), collapse = ", "))
    cols08 <- "the historical exploitation level"
  }
  
  tagList(tags$b("Figure 8 (interactive version)."),
          "Biological risks (Risk3.Blim: maximum probability of falling below Blim) 
          versus the relative yields (catches/MSY) (x-axis) by rule type (symbols) 
          and uncertainty cap lower and upper limits (colours). 
          The columns correspond to ", cols08,
          "and the rows to ", rows08,
          ". The horizontal black dashed line represents the 0.05 biological risk. 
          See Table 1 in Sanchez et al. (2021) for definitions of abbreviations.")
  
})
