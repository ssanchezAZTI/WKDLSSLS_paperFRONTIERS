
id05 <- "Risk3.Blim"

dat05 <- df_bc %>% filter(indicator == id05 & term %in% c("short", "long"))

dat05_st <- dat05 %>% subset(HCRT != "ft0" & BSAFE == "none" & ADVT == "iny" & term == "short")
dat05_lt <- dat05 %>% subset(HCRT != "ft0" & BSAFE == "none" & ADVT == "iny" & term == "long")

output$plot05 <- renderPlot({
  
  dat05_st <- dat05_st %>%
    filter(STKN %in% input$stkn05 & FHIST %in% input$fhist05 & 
             HCRT %in% input$hcrt05 & UC %in% input$uc05)
  
  dat05_lt <- dat05_lt %>%
    filter(STKN %in% input$stkn05 & FHIST %in% input$fhist05 & 
             HCRT %in% input$hcrt05 & UC %in% input$uc05)
  
  dat05_lt %>% 
    ggplot(aes(x=UC, y=value, fill=UC))+
    geom_bar(stat="identity")+
    facet_grid(OMnam ~ HCRT)+
    ylab(plabs[id05])+
    geom_hline(yintercept = 0.05, linetype = "longdash")+
    ylim(c(0,1))+
    scale_fill_manual(values = ucp.col)+
    geom_point(aes(x=UC, y=value), data = dat05_st, colour = "black")+
    theme(axis.text.x=element_blank(), 
          text = element_text(size = 18), 
          title = element_text(size = 14, face = "bold"), 
          strip.text = element_text(size = 15))
  
})


output$text05 <- renderUI({
  
  tagList(tags$b("Figure 5 (interactive version)."), 
          "Biological risks (Risk3.Blim: maximum probability of falling below Blim) for the 
          different configurations of the rule in an in-year advice. 
          The rows correspond to the operating models (combination of stock type and the historical exploitation level) 
          and the columns to the harvest control rule type. 
          The uncertainty cap lower and upper limits combinations are represented on the x-axis 
          for alternative timeframes: the black dots represent the risks in the short-term (first 5 projection years) 
          and the colour bars represent the risk in the long-term (last 10 projection years). 
          The horizontal dashed line corresponds to the 0.05 risk. 
          See Table 1 in Sanchez et al. (2021) for definitions of abbreviations.")
  
})
