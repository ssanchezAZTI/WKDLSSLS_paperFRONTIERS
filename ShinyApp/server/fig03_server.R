
dat <- df_bc %>% 
  filter(BSAFE == "none" & indicator %in% perfnms)


# stkn_sel  <- reactive(input$stkn)
# fhist_sel <- reactive(input$fhist)
# term_sel  <- reactive(input$term)
# hcrt_sel  <- reactive(input$hcrt)
# uc_sel    <- reactive(input$uc)


output$text03 <- renderText({
  paste(input$id03, collapse = ", ")
})
# output$text03a <- renderText({ print(paste(input$stkn03, collapse = ", ")) })
# output$text03b <- renderText({ print(paste(input$fhist03, collapse = ", ")) })
# output$text03c <- renderText({ print(paste(input$term03, collapse = ", ")) })
# output$text03d <- renderText({ print(paste(input$hcrt03, collapse = ", ")) })
# output$text03e <- renderText({ print(paste(input$uc03, collapse = ", ")) })

output$plot03 <- renderPlot({
  
  # dat <- dat %>%
  #     filter(STKN %in% stkn_sel() & FHIST %in% fhist_sel() & term %in% term_sel() & 
  #              HCRT %in% hcrt_sel() & UC %in% uc_sel())
  
  dat <- dat %>%
    filter(STKN %in% input$stkn03 & FHIST %in% input$fhist03 & term %in% input$term03 & 
             HCRT %in% input$hcrt03 & UC %in% input$uc03 &
             indicator %in% input$id03)
  
  dat %>% 
    # # Use your inputs to filter the data
    # ggplot(mapping=aes(x=ADVT, y=catch)) +
    # geom_point(aes_string(col=input$Col))
    ggplot(aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=UC, lty=HCRT))+
    geom_line()+
    geom_hline(aes(yintercept = 0.05), data = subset(dat, indicator == "Risk3.Blim"), linetype="dashed") +
    facet_grid(indicator + term ~ OMnam, scales="free")+
    scale_colour_manual(values = ucp.col)+
    ylab("")+
    theme(text = element_text(size = 18),
          title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 18))

})

