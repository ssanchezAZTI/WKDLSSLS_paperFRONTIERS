
dat03 <- df_bc %>% 
  filter(BSAFE == "none" & indicator %in% perfnms)


output$plot03 <- renderPlot({
  
  dat03 <- dat03 %>%
    filter(STKN %in% input$stkn03 & FHIST %in% input$fhist03 & term %in% input$term03 & 
             HCRT %in% input$hcrt03 & UC %in% input$uc03 &
             indicator %in% input$id03)
  
  p03 <- dat03 %>% 
    ggplot(aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=UC, lty=HCRT))+
    geom_line()+
    # geom_hline(aes(yintercept = 0.05), data = subset(dat03, indicator == "Risk3.Blim"), linetype="dashed") +
    # facet_grid(indicator + term ~ OMnam, scales="free")+
    scale_colour_manual(values = ucp.col)+
    ylab("")+
    theme(text = element_text(size = 18),
          title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 18))
  
  if ("Risk3.Blim" %in% input$id03) 
    p03 <- p03 + 
    geom_hline(aes(yintercept = 0.05), data = subset(dat03, indicator == "Risk3.Blim"), linetype="dashed")
  
  if (input$idpos03 == "STKN_FHIST in cols") {
    p03 + facet_grid(indicator + term ~ OMnam, scales="free")
  } else if (input$idpos03 == "STKN_FHIST in rows") {
    p03 + facet_grid(OMnam ~ indicator + term, scales="free")
  } else if (input$idpos03 == "STKN in cols & FHIST in rows") {
    p03 + facet_grid(FHIST + term ~ STKN + indicator, scales="free")
  } else if (input$idpos03 == "STKN in rows & FHIST in cols") {
    p03 + facet_grid(STKN + indicator ~ FHIST + term, scales="free")
  }
  
})


output$text03 <- renderUI({
  
  # input$id03
  if (length(input$id03)==2) {
    figvar03 <- "Relative yields (catch/MSY) and
                 biological risks (Risk3.Blim: maximum probability of falling below Blim) "
  } else if (input$id03=="Risk3.Blim") {
    figvar03 <- "Biological risks (Risk3.Blim: maximum probability of falling below Blim) "
  } else
    figvar03 <- "Relative yields (catch/MSY) "
  
  if (input$idpos03 == "STKN_FHIST in cols") {
    ompos03 <- "(in columns)"
  } else if (input$idpos03 == "STKN_FHIST in rows") {
    ompos03 <- "(in rows)"
  } else if (input$idpos03 == "STKN in cols & FHIST in rows") {
    ompos03 <- "(stock-types -STKN- in columns and historical exploitation level -fhist- in rows)"
  } else if (input$idpos03 == "STKN in rows & FHIST in cols")
    ompos03 <- "(stock-types -STKN- in rows and historical exploitation level -fhist- in columns)"
  
  tagList(tags$b("Figure 3 (interactive version)."), 
          figvar03,
          "by calendar type (ADVT, x-axis) and alternative operating models", ompos03,
          "The harvest control rules (HCRT) are represented by line types and uncertainty caps limits (UC)
          by line colours. See Table 1 in Sanchez et al. (2021) for definitions of abbreviations.
          The horizontal black dashed line represents the 0.05 biological risk.")
  
})
