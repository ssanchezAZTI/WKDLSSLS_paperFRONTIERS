
id13 <- "catch.MSY"

dat13 <- df_om %>% filter(indicator == id13 & CVID == "low") %>% 
  mutate(rule = paste(HCRT,UC,sep="_"))


output$plot13 <- renderPlot({
  
  dat13 <- dat13 %>%
    filter(STKN %in% input$stkn13 & FHIST %in% input$fhist13 & term == input$term13 & 
             HCRT %in% input$hcrt13 & UC %in% input$uc13)
  
  if (input$idpos13 == "SIGR in x-axis") {
    p13 <- dat13 %>% 
      ggplot(aes(x=SIGR, y=value, fill=LHSC))+
      facet_grid(rule ~ OMnam)
  } else if (input$idpos13 == "LHSC in x-axis") {
    p13 <- dat13 %>% 
      ggplot(aes(x=LHSC, y=value, fill=SIGR))+
      facet_grid(rule ~ OMnam)
  }
  
  p13 +
    geom_bar(stat="identity", position="dodge")+
    geom_hline(yintercept = 1.00, linetype = "longdash")+
    ylab(plabs[[id13]])+
    scale_fill_brewer(palette="Paired", type = "qual")+
    theme(text = element_text(size = 18), 
          title = element_text(size = 14, face = "bold"), 
          strip.text = element_text(size = 18))
  
})


output$text13 <- renderUI({
  
  if (input$idpos13 == "SIGR in x-axis") {
    xaxis13 <- "standard deviation of recruitment"
    cols13  <- "stock productivity"
  } else if (input$idpos13 == "LHSC in x-axis") {
    cols13  <- "standard deviation of recruitment"
    xaxis13 <- "stock productivity"
  }
  
  tagList(tags$b("Figure 13 (interactive version)."), 
          "Relative yields (catches/MSY) in the ", 
          paste0(input$term13,"-term (", TERMdef[input$term13],")"), 
          "versus the", xaxis13, " (x-axis) and ", cols13, " (colours) under an in-year advice. 
          The columns correspond to the alternative OMs (as combination of the stock-type and historical exploitation) 
          and the rows to the configurations of the rule (harvest control rule type and uncertainty cap lower and upper limits). 
          Dashed line corresponds to yield at MSY levels. See Table 1 in Sanchez et al. (2021) for definitions of abbreviations.")
  
})
