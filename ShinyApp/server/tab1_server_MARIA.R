# -----------------------------------
# GT3 Results
# -----------------------------------

# -----------------------------------
# The map
# -----------------------------------

output$map <- renderLeaflet({
  leaflet() %>% addTiles() %>%
    setView(lng = -5,lat =  43, zoom = 5)
})


# -----------------------------------
# Reactive data to map
# -----------------------------------

dat.map  <- eventReactive(input$updategt3, {
  df <-switch(input$level,
              "country" = country,
              "zone" = zone,
              "emu" = emu,
              "basin" = basin,
              "sudoe" = sudoe_gerem)
  df <- df[df$year == input$year,]
  df
}, ignoreNULL = FALSE)

# for each polygone time series ploting
map.df  <- eventReactive(input$updategt3, {
  dat <- switch(input$level,
                "country" = country,
                "zone" = zone,
                "emu" = emu,
                "basin" = basin)
  dat
}, ignoreNULL = FALSE)




# -----------------------------------
# Observer for polygons
# -----------------------------------


update_matrix_catch <- function(){
  req(mydata)
  pols <- unique(rvgt3$myDf$id)
  years <-rownames(mydata$logUObs)
  new_df <- matrix(NA,nrow=length(years),
                   ncol=length(pols))
  storage.mode(new_df) <- "numeric"
  new_df <- as.data.frame(new_df)
  if (input$level =="basin") {
    pols <- ifelse(!is.na(basins_names$name_basin[match(pols,basins_names$seaidsegment)]),
                   as.character(basins_names$name_basin[match(pols,basins_names$seaidsegment)]),
                   pols)
  }
  names(new_df) <- pols
  rownames(new_df) <- years
  old_df <- isolate(catch_data$catch_df)
  if (nrow(old_df) > 0 & !all(is.na(match(names(new_df),names(old_df))))){
    new_df[,names(new_df) %in% names(old_df)] <- old_df[,na.omit(match(names(new_df),names(old_df)))]
  }
  catch_data$catch_df <- new_df
}

draw_maps <- function() {
  pal<-colorNumeric ("viridis", domain = as.numeric(dat.map()$R))
  
  mymap <- leafletProxy("map", data=dat.map())%>%
    clearShapes()%>%
    clearControls()%>%
    addPolygons(
      weight = ifelse(dat.map()$ID %in% rvgt3$myDf$id,1,0),
      smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0.8,
      fillColor = ~pal(R),
      color = ifelse(dat.map()$ID %in% rvgt3$myDf$id,"red",NA),
      layerId = dat.map()$ID,
      highlightOptions = highlightOptions(color = "white", weight = 2,
                                          bringToFront = TRUE))
  if (input$level != "sudoe") mymap <- mymap %>%
    addLegend( "topleft", pal=pal, values=dat.map()$R, title = "Recruitment (Kg)",opacity = 0.8)
  mymap
}

observeEvent(input$updategt3,{
  draw_maps()
})

# -----------------------------------
# Observer for polygon click and plot display
# -----------------------------------


## use reactive values to store the data you generate from observing the shape click
rvgt3 <- reactiveValues()
catch_data <- reactiveValues()

#Create 0 row data.frame
rvgt3$myDf <- data.frame(lat = numeric(), lon = numeric(), id = numeric())
catch_data$catch_df=data.frame()

observeEvent(input$updategt3, {
  
  rvgt3$myDf <- data.frame(lat = numeric(), lon = numeric(), id = numeric())
  
})

observeEvent(input$level, {
  if (input$level == "basin") {
    showNotification("GEREM does not take into account the local specificities of river basins, so estimates correspond estimates of average recruitment for a basin of a given surface in a given zone.",
                     duration = NULL, closeButton = TRUE,id="notif_basin")
  } else {
    removeNotification(id="notif_basin")
  }               
})

observeEvent(
  input$catchdata$changes$changes, # observe if any changes to the cells of the rhandontable
  {
    catch_data$catch_df <- hot_to_r(input$catchdata)
  }
)

observeEvent(input$map_shape_click, {
  
  event <- input$map_shape_click
  print(str(event))
  
  input$updategt3
  isolate({
    if (event$id %in% unique(rvgt3$myDf$id)) {
      rvgt3$myDf <- rvgt3$myDf[-which(rvgt3$myDf$id == event$id),]
    } else {
      rvgt3$myDf[nrow(rvgt3$myDf) + 1,] <- c(event$lat, event$lng, event$id)
    }
    print(rvgt3$myDf)
    draw_maps()
    update_matrix_catch()
  })
  
})

# -----------------------------------
# Display plotly
# -----------------------------------

output$plotR2 <- renderPlotly({
  req(input$updategt3)
  req(input$map_shape_click)
  
  isdensity <- input$indicator == "Density"
  logscale <- input$logscale == "TRUE"
  
  name_var <- switch(input$level,
                     emu  = "emu_name_short",
                     country = "country",
                     basin = "seaidsegment",
                     zone = "gerem_zone_4",
                     sudoe = "sudoe_gerem")
  if (sum(unique(rvgt3$myDf$id) %in% maps_data[,name_var])>=1){
    
    #isolate({
    if (input$scale == "Absolute" & input$graph == "Lines"){
      ggTimeSeries(c(unique(rvgt3$myDf$id)), mydata,gerem_res,maps_data,input$level,isdensity,logscale=logscale,cr=FALSE)}
    else{
      if(input$scale == "Relative" & input$graph == "Lines"){
        ggTimeSeries(c(unique(rvgt3$myDf$id)), mydata,gerem_res,maps_data,input$level,isdensity, absolute = F,logscale=logscale,cr=FALSE)}
      else{
        if (input$scale == "Absolute" & input$graph == "Bars"){
          ggTimeSeries(c(unique(rvgt3$myDf$id)), mydata,gerem_res,maps_data,input$level,isdensity, bars= T,logscale=logscale,cr=FALSE)}
        else{
          if(input$scale == "Relative"  & input$graph == "Bars"){
            ggTimeSeries(c(unique(rvgt3$myDf$id)), mydata,gerem_res,maps_data,input$level,isdensity, absolute = F, bars = T,logscale=logscale,cr=FALSE)}
        }
      }
    }
    #})
    
  }})

output$catchdata <- renderRHandsontable({
  DF <- catch_data$catch_df
  if (nrow(DF) > 0){
    rhandsontable(DF, stretchH = "all")
  }
})

output$plotCatchRate <- renderPlotly({
  #req(input$updategt4)
  #req(input$map_shape_click)
  catch_matrix <- catch_data$catch_df
  
  if (nrow(catch_matrix)>0 & !all(is.na(catch_matrix))) {
    isdensity <- FALSE
    logscale <- input$logscale == "TRUE"
    
    
    name_var <- switch(input$level,
                       emu  = "emu_name_short",
                       country = "country",
                       basin = "seaidsegment",
                       zone = "gerem_zone_4",
                       sudoe = "sudoe_gerem")
    if (sum(unique(rvgt3$myDf$id) %in% maps_data[,name_var])>=1){
      
      #isolate({
      if (input$scale == "Absolute" & input$graph == "Lines"){
        ggTimeSeries(c(unique(rvgt3$myDf$id)), mydata,gerem_res,maps_data,input$level,isdensity,logscale=logscale,cr=TRUE,
                     catch_matrix=catch_matrix[,unique(rvgt3$myDf$id),drop=FALSE])}
      else{
        if(input$scale == "Relative" & input$graph == "Lines"){
          ggTimeSeries(c(unique(rvgt3$myDf$id)), mydata,gerem_res,maps_data,input$level,isdensity, absolute = F,logscale=logscale,cr=TRUE,
                       catch_matrix=catch_matrix[,unique(rvgt3$myDf$id),drop=FALSE])}
        else{
          if (input$scale == "Absolute" & input$graph == "Bars"){
            ggTimeSeries(c(unique(rvgt3$myDf$id)), mydata,gerem_res,maps_data,input$level,isdensity, bars= T,logscale=logscale,cr=TRUE,
                         catch_matrix=catch_matrix[,unique(rvgt3$myDf$id),drop=FALSE])}
          else{
            if(input$scale == "Relative"  & input$graph == "Bars"){
              ggTimeSeries(c(unique(rvgt3$myDf$id)), mydata,gerem_res,maps_data,input$level,isdensity, absolute = F, bars = T,logscale=logscale,cr=TRUE,
                           catch_matrix=catch_matrix[,unique(rvgt3$myDf$id),drop=FALSE])}
          }
        }
      }
      #})
    }
  }})	