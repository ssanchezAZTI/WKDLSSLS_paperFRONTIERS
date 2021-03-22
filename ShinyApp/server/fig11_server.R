
dat11 <- df_cvid %>%
  filter(indicator %in% perfnms & UCPL == 0 & UCPU == 0 &
           LHSC == "bc" & SIGR == 0.75)

output$plot11 <- renderPlot({

  dat11 <- dat11 %>%
    filter(STKN %in% input$stkn11 & FHIST %in% input$fhist11 & term %in% input$term11 &
             HCRT %in% input$hcrt11 & CVID %in% input$cvid11 &
             indicator %in% input$id11)

  p11 <- dat11 %>%
    ggplot(aes(x=IAVproj, y=value, col=OMnam, shape=HCRT))+
    geom_point(size=3)+
    xlab("IAVproj")+
    ylab(plabs[[input$id11]])+
    theme(text = element_text(size = 18),
          title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 16))
  
  if (input$idpos11 == "CVID in cols") { 
    p11 + facet_grid(term ~ CVID)
  } else if (input$idpos11 == "CVID in rows")
    p11 + facet_grid(CVID ~ term)
  

})


output$text11 <- renderUI({

  if (input$id11=="Risk3.Blim") {
    figvar11 <- "Biological risks (Risk3.Blim: maximum probability of falling below Blim) "
  } else {
    figvar11 <- "Relative yields (catch/MSY) "
  }
  
  if (input$idpos11 == "CVID in cols") { 
    cvidpos11 <- "columns"
    timepos11 <- "rows"
  } else if (input$idpos11 == "CVID in rows") { 
    cvidpos11 <- "rows"
    timepos11 <- "columns"
  }

  tagList(tags$b("Figure 11 (interactive version)."),
          figvar11, "versus the interannual variability (x-axis) under an in-year advice.",
          "The ", cvidpos11, " correspond to the CVs of the index and the ", timepos11, " to the timeframes: ",
          paste(paste0(input$term08, "-term (", TERMdef[input$term08],")"), collapse = ", "),
          ". The harvest control rule types without any uncertainty cap are represented by the dot types and
          the operating models (combination of stock-type and historical exploitation level) are represented by the dot colours. ",
          "See Table 1 in Sanchez et al. (2021) for definitions of abbreviations.")

})

