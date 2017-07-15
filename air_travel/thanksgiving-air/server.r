library(data.table)
library(ggplot2)
library(KernSmooth)
library(sp)

shinyServer(function(input, output){
  
  year<-reactive({
    year.filter<-filter(tday.03.16, Year>=input$year[1] & Year<=input$year[2])
  })
  
  day<-reactive({
    day.filter<-filter(year(), WEEKDAY==input$weekday)
  })
  
  airline<-reactive({
    air.filter<-if (input$airline=="All") day() else filter(day(), Description==input$airline)
  })
  
  output$heat<-renderLeaflet({

    leaflet() %>% addTiles() %>% setView(lng=-99, lat=39, zoom=3) 
    
  })

  output$airport<-renderLeaflet({
    
    airport1<-tday.03.16 %>% filter(WEEKDAY %in% c("Tuesday", "Wednesday", "Thursday")) %>% group_by(DEST_AIRPORT, DEST_LONG, DEST_LAT) %>% summarize(Count=sum(Pass.Flight))
    airport2<-tday.03.16 %>% filter(WEEKDAY %in% c("Friday", "Saturday", "Sunday")) %>% group_by(DEST_AIRPORT, DEST_LONG, DEST_LAT) %>% summarize(Count=sum(Pass.Flight))
    print(nrow(airport1))
    print(nrow(airport2))
    
    airport.comp<-full_join(airport1, airport2, by=c("DEST_AIRPORT", "DEST_LONG", "DEST_LAT")) %>% filter(Count.y/nrow(airline()) > .1)  %>% mutate(Difference = (Count.x-Count.y)/Count.y)
    print(nrow(airport.comp))
    airport.comp$Adjusted<-(airport.comp$Difference-.023)*100
    airport.comp$SR<-abs(airport.comp$Adjusted)^0.5*ifelse(airport.comp$Adjusted*100>=0, 1, -1)
    pal <- colorNumeric(palette = "RdYlGn", domain = airport.comp$SR)
    airport.comp$Color<-pal(airport.comp$SR)
    print(head(airport.comp$Color))
    
    leaflet() %>% addTiles() %>% setView(lng=-99, lat=39, zoom=3) %>%
      addCircleMarkers(data=airport.comp, lng=~DEST_LONG, lat=~DEST_LAT, opacity=1, color =~Color , radius= 3,
       popup= paste(sep="<br/>", airport.comp$DEST_AIRPORT, paste(round(airport.comp$Count.x, 0), "came for Thanksgiving,", round(airport.comp$Count.y, 0), "returned here"), paste(signif(airport.comp$Difference*100, 2), "% Change"))) %>%
      addLegend("bottomleft", colors = c("#B91927","#FFA500", "#FFFFBE", "#C3E57D", "#008000"), labels = c(-10, -3, 0, 3, 10), na.label= 'Not Reported', title = "% Change, Visit vs. Return")
    
  })
  
  output$table<-DT::renderDataTable({
    datatable(airline() %>% select(Description, FL_NUM, FL_DATE, CRS_DEP_TIME, CRS_ARR_TIME, ORIGIN_AIRPORT, DEST_AIRPORT))
  })
  

  observe({
  
    all.pass<-data.table(airline())[,list(x=rep(DEST_LONG, Pass.Flight),y=rep(DEST_LAT,Pass.Flight))]
    print(nrow(all.pass))
    kde <- bkde2D(all.pass[ , list(x,y)],
                  bandwidth=c(1, .6), gridsize = c(1000,1000))
    CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
    
    LEVS <- as.factor(sapply(CL, `[[`, "level"))
    NLEV <- length(levels(LEVS))
    print(NLEV)
    pgons <- lapply(1:length(CL), function(i) Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
    spgons = SpatialPolygons(pgons)
    print(class(spgons))
    proxy<-leafletProxy("heat", data=spgons)
      
    proxy %>% clearShapes() %>% addPolygons(color = heat.colors(NLEV, NULL)[LEVS])

  })

})