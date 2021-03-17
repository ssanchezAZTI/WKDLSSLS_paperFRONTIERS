

dat <- full_join(perf.dat, sc.dat, by=c("scenario"="SCENARIO"))

output$plot1 <- renderPlot({
  dat %>% 
    # Use your inputs to filter the data
    ggplot(mapping=aes(x=ssb, y=catch)) +
    geom_point(aes_string(col=input$Col))
})