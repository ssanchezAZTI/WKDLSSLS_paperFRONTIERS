

dat <- df_bc %>% 
  filter(BSAFE == "none" & indicator %in% perfnms)


output$plot03b <- renderPlot({
  
  dat <- dat %>%
    filter(STKN %in% input$stkn03b & FHIST %in% input$fhist03b & term %in% input$term03b & 
             HCRT %in% input$hcrt03b & UC %in% input$uc03b &
             indicator %in% input$id03b)
  
  p <- dat %>% 
    ggplot(aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=UC, lty=HCRT))+
    geom_line()+
    geom_hline(aes(yintercept = 0.05), data = subset(dat, indicator == "Risk3.Blim"), linetype="dashed") +
    # facet_grid(indicator + term ~ OMnam, scales="free")+
    scale_colour_manual(values = ucp.col)+
    ylab("")+
    theme(text = element_text(size = 18),
          title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 18))
  
  if (input$idpos03b == "STKN_fhist in cols") {
    p + facet_grid(indicator + term ~ OMnam, scales="free")
  } else if (input$idpos03b == "STKN_fhist in rows") {
    p + facet_grid(OMnam ~ indicator + term, scales="free")
  } else if (input$idpos03b == "STKN in cols & fhist in rows") {
    p + facet_grid(FHIST + term ~ STKN + indicator, scales="free")
  } else if (input$idpos03b == "STKN in rows & fhist in cols") {
    p + facet_grid(STKN + indicator ~ FHIST + term, scales="free")
  }
  
})


output$text03b <- renderText({
  
  paste("<b> Figure 3 (interactive version). </b>")
  
})
# output$text03b <- renderText({
#   
#   figvar03 <- ifelse(length(input$id03)==2, 
#                      "Relative yields (catch/MSY) and biological risks 
#                      (Risk3.Blim: maximum probability of falling below Blim) ",
#                      ifelse(input$id03=="Risk3.Blim", "Biological risks 
#                      (Risk3.Blim: maximum probability of falling below Blim) ",
#                             "Relative yields (catch/MSY) ")) 
#   
#   paste("<b> Figure 3 (interactive version). </b>", figvar03,
#         "by calendar type (ADVT, x-axis) and alternative operating models (in columns). 
#         The harvest control rules (HCRT) are represented by line types and uncertainty caps limits (UC) 
#         by line colours. See Sanchez et al. (2021) Table 1 for definitions of abbreviations. 
#         The horizontal black dashed line represents the 0.05 biological risk")
# })
