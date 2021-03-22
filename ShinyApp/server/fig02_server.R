
output$plot02 <- renderPlot({

  dat02 <- dhist %>%
    filter(STKN %in% input$stkn02 & LHSC %in% input$lhsc02 &
             SIGR %in% input$sigr02 & FHIST %in% input$fhist02)

  if (input$idpos02 == "SIGR") {
    p02 <- dat02 %>%
      ggplot(aes(x=SIGR, y=IAVhist, fill=FHIST))+
      facet_grid(LHSC ~ STKN)
  } else if (input$idpos02 == "FHIST") {
    p02 <- dat02 %>%
      ggplot(aes(x=FHIST, y=IAVhist, fill=SIGR))+
      facet_grid(LHSC ~ STKN)
  }

  p02 +
    geom_bar(stat="identity", position="dodge")+
    facet_grid(LHSC ~ STKN)+
    theme(text = element_text(size = 18),
          title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 18))

})


output$text02 <- renderUI({
  
  if (input$idpos02 == "SIGR") {
    xaxis02 <- "standard deviation for the recruitment log-normal error (SIGR, x-axis)"
    cols02  <- "exploitation level (FHIST): zero catch (f0), under exploitation (flow), 
                fully exploited (fopt) and overexploitation (fhigh)"
  } else if (input$idpos02 == "FHIST") {
    cols02  <- "standard deviation for the recruitment log-normal error (SIGR)"
    xaxis02 <- "exploitation level (FHIST, x-axis): zero catch (f0), under exploitation (flow), 
                fully exploited (fopt) and overexploitation (fhigh)"
  }
  
  tagList(tags$b("Figure 2 (interactive version)."), 
          "Interannual variation in the historic period (IAVhist) by ",
          xaxis02, 
          "as a function of the stock type: anchovy-like (STK1) and sprat/ sardine-like (STK2); 
          the stock productivity: low (lowprod), medium (bc) and high (highprod); 
          and the ", cols02, ".")
  
})
