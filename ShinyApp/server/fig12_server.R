
id12 <- "Risk3.Blim"

dat12 <- df_om %>% filter(indicator == id12 & CVID == "low") %>% 
  mutate(rule = paste(HCRT,UC,sep="_"))


output$plot12 <- renderPlot({
  
  dat12 <- dat12 %>%
    filter(STKN %in% input$stkn12 & FHIST %in% input$fhist12 & term == input$term12 & 
             HCRT %in% input$hcrt12 & UC %in% input$uc12)
  
  if (input$idpos12 == "SIGR in x-axis") {
    p12 <- dat12 %>% 
      ggplot(aes(x=SIGR, y=value, fill=LHSC))+
      facet_grid(rule ~ OMnam)
  } else if (input$idpos12 == "LHSC in x-axis") {
    p12 <- dat12 %>% 
      ggplot(aes(x=LHSC, y=value, fill=SIGR))+
      facet_grid(rule ~ OMnam)
  }
  
  p12 +
    geom_bar(stat="identity", position="dodge")+
    geom_hline(yintercept = 0.05, linetype = "longdash")+
    ylab(plabs[[id12]])+
    scale_fill_brewer(palette="Paired", type = "qual")+
    theme(text = element_text(size = 18), 
          title = element_text(size = 14, face = "bold"), 
          strip.text = element_text(size = 18))
  
})


output$text12 <- renderUI({
  
  if (input$idpos12 == "SIGR in x-axis") {
    xaxis12 <- "standard deviation of recruitment"
    cols12  <- "stock productivity"
  } else if (input$idpos12 == "LHSC in x-axis") {
    cols12  <- "standard deviation of recruitment"
    xaxis12 <- "stock productivity"
  }
  
  tagList(tags$b("Figure 12 (interactive version)."), 
          "Biological risks (Risk3.Blim: maximum probability of falling below Blim) in the ", 
          paste0(input$term12,"-term (", TERMdef[input$term12],")"), 
          "versus the", xaxis12, " (x-axis) and ", cols12, " (colours) under an in-year advice. 
          The columns correspond to the alternative OMs (as combination of the stock-type and historical exploitation) 
          and the rows to the configurations of the rule (harvest control rule type and uncertainty cap lower and upper limits). 
          Dashed line corresponds to the 0.05 risk. See Table 1 in Sanchez et al. (2021) for definitions of abbreviations.")
  
})
