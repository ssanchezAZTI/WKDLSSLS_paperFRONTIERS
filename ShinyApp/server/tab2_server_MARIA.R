# -----------------------------------
# GT4 Results
# -----------------------------------

output$mapgt4 <- renderLeaflet({
  leaflet() %>% addTiles() %>%
    setView(lng = -5,lat =  43, zoom = 5)
})

# -----------------------------------
# Reactive data to map gt4
# -----------------------------------
# this is the dataset for polygons

dat.mapgt4  <- eventReactive(input$updategt4, {
  dat <-switch(input$levelgt4,
               #"all" = country,
               "country" = country,
               "zone" = zone,
               "emu" = emu,
               "basin" = basin,
               "sudoe" = sudoe_gerem)
  # if (input$levelgt4 == "all") {
  #   datmerged <- inner_join(
  #     dat %>% select(-one_of("R","ID")),
  #     eda_data %>% filter(geo_level==input$levelgt4 & var == input$indicatorgt4) %>%
  #       mutate(ID = recode(geo_label,'FR'="France",'SP'="Spain",'PT'='Portugal'))%>%
  #       select(ID, year, value) %>%
  #       mutate(year = as.integer(year)),
  #     by=c("year"))
  # } else {
    datmerged <- inner_join(
      dat %>% select(-one_of("R")),
      eda_data %>% filter(geo_level==input$levelgt4 & var == input$indicatorgt4) %>%
        mutate(ID=recode(geo_label,'FR'="France",'SP'="Spain",'PT'='Portugal'))%>%
        select(ID,year,value) %>%
        mutate(year=as.integer(year)),
      by=c("year","ID"))
  #}
  #return(datmerged)
  datmerged <- datmerged[datmerged$year == input$yeargt4, , drop=FALSE]
  datmerged
}, ignoreNULL = FALSE)


# for each polygon time series plotting
dfgt4  <- eventReactive(input$updategt4, {
  dat <- switch(input$levelgt4,
                #"all" = country,
                "country" = country,
                "zone" = zone,
                "emu" = emu,
                "basin" = basin)
  # select in the dataset only the values corresponding to selected indicator (e.g. nsilver)
  # and level (e.g. country) and merge them back in the original geodata
  # if (input$levelgt4 == "all") {
  #   datmerged <- inner_join(
  #     dat %>% select(-one_of("R","ID")),
  #     eda_data %>% filter(geo_level==input$levelgt4, var == input$indicatorgt4) %>%
  #       mutate(ID = recode(geo_label,'FR'="France",'SP'="Spain",'PT'='Portugal'))%>%
  #       select(ID, year, value, geo_label) %>%
  #       mutate(year = as.integer(year)),
  #     by=c("year"))
  # } else {
    datmerged <- inner_join(
      dat %>% select(-one_of("R")),
      eda_data %>% filter(geo_level==input$levelgt4, var == input$indicatorgt4) %>%
        mutate(ID=recode(geo_label,'FR'="France",'SP'="Spain",'PT'='Portugal'))%>%
        select(ID,year,value, geo_label) %>%
        mutate(year=as.integer(year)),
      by=c("year","ID"))
    
    #CI
    dataCI <- eda_data %>%
      filter(geo_level==input$levelgt4,
             var %in% c("density", "density025", "density0975"))%>%
      pivot_wider(names_from = "var", values_from = value) %>%
      mutate(CI_inf_ratio=density025/density, CI_sup_ratio= density0975/density) %>%
      select(all_of(c("year","geo_label","CI_inf_ratio", "CI_sup_ratio")))
    
    #join
    datatoplot <- datmerged %>% 
      inner_join(dataCI %>% mutate(year=as.integer(year)), by=c("year", "geo_label")) %>%
      mutate("q2.5"= value * CI_inf_ratio, "q97.5"= value * CI_sup_ratio)
  #}
  #return(datmerged)
  datatoplot
  # columns ID, year, value, geometry
}, ignoreNULL = FALSE)



# -----------------------------------
# Observer for polygons
# -----------------------------------

draw_maps_gt4 <- function() {
  pal <- colorNumeric ("viridis", domain = as.numeric(dfgt4()$value)[!is.na(dfgt4()$value)])
  
  mymapgt4 <- leafletProxy("mapgt4", data=dat.mapgt4())%>%
    clearShapes()%>%
    clearControls()%>%
    addPolygons(
      weight = ifelse(dat.mapgt4()$ID %in% rvgt4$myDf$id,1,0),
      smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0.8,
      fillColor = ~pal(value),
      color = ifelse(dat.mapgt4()$ID %in% rvgt4$myDf$id,"red",NA),
      layerId = dat.mapgt4()$ID,
      highlightOptions = highlightOptions(color = "white", weight = 2,
                                          bringToFront = TRUE))
  if (input$level != "all") mymapgt4 <- mymapgt4 %>%
    addLegend( "topleft",
               pal=pal,
               values=dat.mapgt4()$value,
               title = case_when(
                 input$indicatorgt4 == "nsilver" ~ "Number of Silver eel",
                 input$indicatorgt4 == "bsilver" ~ "Biomass of silver eel in kg",
                 input$indicatorgt4 == "nmale" ~ "Number of males",
                 input$indicatorgt4 == "nfemale" ~ "Number of females",
                 input$indicatorgt4 == "nyellow" ~ "Number of eel (yellow + silver)",
                 input$indicatorgt4 == "byellow" ~ "Number of eel (yellow + silver) kg",
                 input$indicatorgt4 == "sex_ratio" ~ "Sex Ratio (Nmale / Nfemales)",
                 TRUE ~ "Error unknown category"
               ),
               opacity = 0.8)
  mymapgt4
}

observeEvent(input$updategt4,{
  draw_maps_gt4()
})

# -----------------------------------
# Observer for polygon click and plot display
# -----------------------------------


## use reactive values to store the data you generate from observing the shape click
rvgt4 <- reactiveValues()

#Create 0 row data.frame
rvgt4$myDf <- data.frame(lat = numeric(), lon = numeric(), id = numeric())

selected_eda_data <- reactiveValues()


selected_eda_data$df <- data.frame()


observeEvent(input$updategt4, {
  
  rvgt4$myDf <- data.frame(lat = numeric(), lon = numeric(), id = numeric())
  
})

observeEvent(input$mapgt4_shape_click, {
  
  event <- input$mapgt4_shape_click
  print(str(event))
  
  input$updategt4
  # clicks the update button
  isolate({
    if (event$id %in% unique(rvgt4$myDf$id)) {
      rvgt4$myDf <- rvgt4$myDf[-which(rvgt4$myDf$id == event$id),]
    } else {
      rvgt4$myDf[nrow(rvgt4$myDf) + 1,] <- c(event$lat, event$lng, event$id)
    }
    print(rvgt4$myDf)
    draw_maps_gt4()
    update_activeset_eda()
  })
  
})

update_activeset_eda <- function(){
  req(eda_data)
  pols <- unique(rvgt4$myDf$id)
  years <- unique(dfgt4()$year)
  new_df <- matrix(NA, nrow=length(years),	ncol=length(pols))
  storage.mode(new_df) <- "numeric"
  new_df <- as.data.frame(new_df)
  if (input$levelgt4 =="basin") {
    pols <- ifelse(!is.na(basins_names$name_basin[match(pols,basins_names$seaidsegment)]),
                   as.character(basins_names$name_basin[match(pols,basins_names$seaidsegment)]),
                   pols)
  }
  names(new_df) <- pols
  rownames(new_df) <- years
  old_df <- isolate(selected_eda_data$df)
  if (nrow(old_df) > 0 & !all(is.na(match(names(new_df),names(old_df))))){
    new_df[,names(new_df) %in% names(old_df)] <- old_df[,na.omit(match(names(new_df), names(old_df)))]
  }
  selected_eda_data$df <- new_df
}


# -----------------------------------
# Display plotly
# -----------------------------------

output$plotlygt4 <- renderPlotly({
  req(input$updategt4)
  req(input$mapgt4_shape_click)
  
  if (nrow(rvgt4$myDf)== 0) {return (NULL)} 

  if (input$scalegt4 == "Absolute" & input$graphgt4 == "Lines"){
     
      ggplot(subset(dfgt4(), ID%in%c(unique(rvgt4$myDf$id))),aes(x=year,y=value))+
        geom_line(aes(color=ID))+
        geom_ribbon(aes(ymin=q2.5, ymax=q97.5,fill = ID), alpha=0.3)+
        ylab(unique(dfgt4()$var))+
    		theme_bw()}
  else{
    if(input$scalegt4 == "Relative" & input$graphgt4 == "Lines"){
      
      ggplot(subset(dfgt4(), ID%in%c(unique(rvgt4$myDf$id))),aes(x=year,y=value))+
        geom_line(aes(color=ID))+
         geom_ribbon(aes(ymin=q2.5, ymax=q97.5,fill = ID), alpha=0.3)+
         scale_y_continuous(trans="log")+
        ylab(unique(dfgt4()$var))+
         theme_bw()}
    else{
      if (input$scalegt4 == "Absolute" & input$graphgt4 == "Bars"){

      ggplot(subset(dfgt4(), ID%in%c(unique(rvgt4$myDf$id))),aes(x=year,y=value))+
        geom_bar(aes(fill=ID),
                 stat="identity",
                 position=position_dodge(),
                 alpha=.3) +
        geom_errorbar(aes(ymin=q2.5,ymax=q97.5,col=geo_label),
         		position=position_dodge(.9))+
         ylab(unique(dfgt4()$var))+
        theme_bw()}
      
      else{
        if(input$scalegt4 == "Relative"  & input$graphgt4 == "Bars"){
          
          ggplot(subset(dfgt4(), ID%in%c(unique(rvgt4$myDf$id))),aes(x=year,y=value))+
            geom_bar(aes(fill=ID),
                     stat="identity",
                     position=position_dodge(),
                     alpha=.3) +
            scale_y_continuous(trans="log")+
            geom_errorbar(aes(ymin=q2.5,ymax=q97.5,col=geo_label),
            		position=position_dodge(.9))+
            ylab(unique(dfgt4()$var))+
            theme_bw()}
      }
    }
  }

   })

# -----------------------------------
# debug
# -----------------------------------

# output$myDf_output <- renderTable({
#   nrow(rvgt4$myDf)
#   #names(dfgt4())
# })
