
id07 <- "catch.MSY"

dat07 <- df_bc %>% filter(indicator == id07 & term %in% c("short", "long"))

dat07_st <- dat07 %>% subset(HCRT != "ft0" & BSAFE == "none" & ADVT == "iny" & term == "short")
dat07_lt <- dat07 %>% subset(HCRT != "ft0" & BSAFE == "none" & ADVT == "iny" & term == "long")

output$plot07 <- renderPlot({
  
  dat07_st <- dat07_st %>%
    filter(STKN %in% input$stkn07 & FHIST %in% input$fhist07 & 
             HCRT %in% input$hcrt07 & UC %in% input$uc07)
  
  dat07_lt <- dat07_lt %>%
    filter(STKN %in% input$stkn07 & FHIST %in% input$fhist07 & 
             HCRT %in% input$hcrt07 & UC %in% input$uc07)
  
  dat07_lt %>% 
    ggplot(aes(x=UC, y=value, fill=UC))+
    geom_bar(stat="identity")+
    facet_grid(OMnam ~ HCRT)+
    ylab(plabs[id07])+
    geom_hline(yintercept = 1.00, linetype = "longdash")+
    scale_fill_manual(values = ucp.col)+
    geom_point(aes(x=UC, y=value), data = dat07_st, colour = "black")+
    theme(axis.text.x=element_blank(), 
          text = element_text(size = 18), 
          title = element_text(size = 14, face = "bold"), 
          strip.text = element_text(size = 15))
  
})


output$text07 <- renderUI({
  
  tagList(tags$b("Figure 7 (interactive version)."), 
          "Relative yields (catches/MSY) for the different configurations of the rule in an in-year advice. 
          The rows correspond to the operating models (combination of stock type and the historical exploitation level) 
          and the columns to the harvest control rule type. 
          The uncertainty cap lower and upper limits combinations are represented on the x-axis 
          for alternative timeframes: the black dots represent the risks in the short-term (first 5 projection years) 
          and the colour bars represent the risk in the long-term (last 10 projection years). 
          The horizontal dashed line corresponds to yield at MSY levels. 
          See Table 1 in Sanchez et al. (2021) for definitions of abbreviations.")
  
})
