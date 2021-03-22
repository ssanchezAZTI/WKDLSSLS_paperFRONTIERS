
dat06 <- dat_bio %>% filter(LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny" & 
                              HCRT != "ft0" & BSAFE == "none" & HCRI == "nin") %>% 
  select(-LHSC, -SIGR, -CVID, -ADVT, -HCRI)

# output$text06test <- renderText({
#   paste("STKN: ", paste(input$stkn06, collapse = ", "), 
#         "FHIST: ", paste(input$fhist06, collapse = ", "), 
#         "HCRT: ", paste(input$hcrt06, collapse = ", "), 
#         "UC: ", paste(input$uc06, collapse = ", "), 
#         "indicator: ", input$id06)
# })
# 
# output$table06 <- renderTable({
#   dat06 <- dat06 %>%
#     filter(STKN %in% input$stkn06 & FHIST %in% input$fhist06 & 
#              HCRT %in% input$hcrt06 & UC %in% input$uc06)
# })

output$plot06 <- renderPlot({
  
  dat06 <- dat06 %>%
    filter(STKN %in% input$stkn06 & FHIST %in% input$fhist06 & 
             HCRT %in% input$hcrt06 & UC %in% input$uc06)
  
  if (input$id06 == "Risk3.Blim") {
    p06 <- dat06 %>%
      ggplot(aes(x=year, y=Risk.Blim, col=UC)) +
      geom_hline(yintercept = 0.05, linetype = "longdash")
  } else
    p06 <- dat06 %>%
      ggplot(aes(x=year, y=catch.MSY, col=UC)) +
      geom_hline(yintercept = 1.00, linetype = "longdash")
      
  p06 <- p06 +
    geom_line() +
    scale_colour_manual(values = ucp.col)+
    theme_bw() +
    theme(text = element_text(size = 18),
          title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 16))
  
  if (input$idpos06 == "STKN_FHIST in rows") {
    p06 + facet_grid(OMnam ~ HCRT, scales="free")
  } else if (input$idpos06 == "STKN_FHIST in cols") {
    p06 + facet_grid(HCRT ~ OMnam, scales="free")
  } else if (input$idpos06 == "STKN in cols & FHIST in rows") {
    p06 + facet_grid(FHIST ~ STKN + HCRT, scales="free")
  } else if (input$idpos06 == "STKN in rows & FHIST in cols") {
    p06 + facet_grid(STKN + HCRT  ~ FHIST, scales="free")
  }
  
})


output$text06 <- renderUI({
  
  if (input$id06=="Risk3.Blim") {
    figvar06 <- "biological risks (Risk3.Blim: maximum probability of falling below Blim) "
    hl06 <- "The horizontal dashed line corresponds to the 0.05 risk."
  } else {
    figvar06 <- "relative yields (catch/MSY) "
    hl06 <- "The horizontal dashed line corresponds to yield at MSY levels."
  }
  
  if (input$idpos06 == "STKN_FHIST in rows") {
    rows06 <- "the operating models (combination of stock type and the historical exploitation level)"
    cols06 <- "the harvest control rule type"
  } else if (input$idpos06 == "STKN_FHIST in cols") {
    rows06 <- "the harvest control rule type"
    cols06 <- "the operating models (combination of stock type and the historical exploitation level)"
  } else if (input$idpos06 == "STKN in cols & FHIST in rows") {
    rows06 <- "the stock and harvest control rule types"
    cols06 <- "the historical exploitation level"
  } else if (input$idpos06 == "STKN in rows & FHIST in cols") {
    rows06 <- "the historical exploitation level"
    cols06 <- "the stock and harvest control rule types"
  }
  
  tagList(tags$b("Figure 6 (interactive version)."), 
          "Trajectories of ", figvar06,
          "along years (x-axis) under an in-year advice. The rows correspond to ", rows06,
          "and the columns to ", cols06, ".",  
          "The uncertainty cap lower and upper limits combinations are represented by different coloured lines.",  
          hl06, "See Table 1 in Sanchez et al. (2021) for definitions of abbreviations.")
  
})
