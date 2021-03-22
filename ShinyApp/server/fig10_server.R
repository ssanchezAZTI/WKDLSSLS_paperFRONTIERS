
dat10 <- df_cvid %>% 
  filter(indicator %in% perfnms & 
           LHSC == "bc" & SIGR == 0.75) %>% 
  mutate(rule = paste(HCRT,UC,sep="_"))

output$plot10 <- renderPlot({
  
  dat10 <- dat10 %>%
    filter(STKN %in% input$stkn10 & FHIST %in% input$fhist10 & term %in% input$term10 & 
             HCRT %in% input$hcrt10 & UC %in% input$uc10 &
             indicator %in% input$id10)
  
  p10 <- dat10 %>%  
    ggplot(aes(x=CVIndex, y=value, colour=HCRT))+
    geom_line(aes(linetype=UC), size = 1.75)+
    geom_point(aes(shape=HCRT), size = 2, col="black", stroke = 2)+
    scale_fill_brewer(palette="Paired", type = "qual")+
    ylab("")+
    theme(text = element_text(size = 18), 
          title = element_text(size = 14, face = "bold"), 
          strip.text = element_text(size = 16), 
          legend.key.size = grid::unit(2.5,"lines"))
    
  if ("Risk3.Blim" %in% input$id10) 
    p10 <- p10 + 
      geom_hline(lty = 2, data = data.frame(indicator = "Risk3.Blim", ref = 0.05), aes(yintercept = ref))
      # geom_hline(aes(yintercept = 0.05), data = subset(dat10, indicator == "Risk3.Blim"), linetype="dashed")
  
  if (input$idpos10 == "STKN_FHIST in cols") {
    p10 + facet_grid(indicator + term ~ OMnam, scales="free")
  } else if (input$idpos10 == "STKN_FHIST in rows") {
    p10 + facet_grid(OMnam ~ indicator + term, scales="free")
  } else if (input$idpos10 == "STKN in cols & FHIST in rows") {
    p10 + facet_grid(FHIST + indicator ~ STKN + term, scales="free")
  } else if (input$idpos10 == "STKN in rows & FHIST in cols") {
    p10 + facet_grid(STKN + term ~ FHIST + indicator, scales="free")
  }
  
})


output$text10 <- renderUI({
  
  if (length(input$id10)==2) {
    figvar10 <- "Relative yields (catch/MSY) and
                 biological risks (Risk3.Blim: maximum probability of falling below Blim) "
    riskl10 <- "The horizontal dashed line corresponds to the 0.05 risk."
  } else if (input$id10=="Risk3.Blim") {
    figvar10 <- "Biological risks (Risk3.Blim: maximum probability of falling below Blim) "
    riskl10 <- "The horizontal dashed line corresponds to the 0.05 risk."
  } else {
    figvar10 <- "Relative yields (catch/MSY) "
    riskl10 <- ""
  }
  
  if (input$idpos10 == "STKN_FHIST in cols") {
    tpos10  <- "(in rows)"
    ompos10 <- "The columns correspond to the different operating models (as combination of the stock-type and historical exploitation level)."
  } else if (input$idpos10 == "STKN_FHIST in rows") {
    tpos10  <- "(in columns)"
    ompos10 <- "The rows correspond to the different operating models (as combination of the stock-type and historical exploitation level)."
  } else if (input$idpos10 == "STKN in cols & FHIST in rows") {
    tpos10  <- "(in columns)"
    ompos10 <- "The columns correspond to the different stock-type (and temporal scales) and the rows to the historical exploitation level."
  } else if (input$idpos10 == "STKN in rows & FHIST in cols") {
    tpos10  <- "(in rows)"
    ompos10 <- "The columns correspond to the historical exploitation level and the rows to the different stock-type (and temporal scales)."
  }
  
  tagList(tags$b("Figure 10 (interactive version)."), 
          figvar10, "in the ", paste(paste0(input$term10,"-"), collapse = " and "), "term", tpos10,
          "under an in-year advice for the different CV values of the index (x-axis).",
          ompos10, 
          "The harvest control rule types are represented by the coloured lines and 
          the uncertainty cap lower and upper limits combinations are represented by the line types
          (solid line: 80% symmetric uncertainty cap; and dashed line: 20% symmetric uncertainty cap).",
          riskl10, 
          "See Table 1 in Sanchez et al. (2021) for definitions of abbreviations.")
  
})

