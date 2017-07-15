#VISUALIZE THE MAP
library(ggmap)
t<-ggplot(tday.combined[1:100000,], aes(x=DEST_LONG, y=DEST_LAT))
t + geom_density2d(stat="identity")

usa<-get_map("united states", maptype="satellite", zoom=3)
ggmap(usa, extent = "device") +  geom_density_2d(data = rep.with.dt[1:100000,], aes(x, y), size = 0.3) +
  stat_density_2d(data = rep.with.dt[1:100000,], aes(x, y, fill=..level.., alpha=..level..), size=0.01, bins=16, geom="polygon") +
  scale_fill_gradient(low = "green", high = "red")

tday.02.16<-as.data.frame(tday.02.16)

ggplot(tday.02.16[1:10000,], aes(x=DEST_LONG,y=DEST_LAT, fill = Pass.Flight)) + 
  geom_raster() +
  scale_fill_gradient(low="blue", high="red")


#
library(data.table)
rep.with.dt<-data.table(tday.02.16[1:00000,])[,list(x=rep(DEST_LONG, Pass.Flight),y=rep(DEST_LAT,Pass.Flight))]


kde <- bkde2D(rep.with.dt[ , list(x,y)],
              bandwidth=c(0.2, 0.2), gridsize = c(2000,2000))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])


library(leaflet)

airport1<-tday.03.16 %>% filter(WEEKDAY %in% c("Tuesday", "Wednesday", "Thursday")) %>% group_by(DEST_AIRPORT, DEST_LONG, DEST_LAT) %>% summarize(Count=sum(Pass.Flight))
total.pre<-sum(airport1$Count)
airport2<-tday.03.16 %>% filter(WEEKDAY %in% c("Friday", "Saturday", "Sunday")) %>% group_by(DEST_AIRPORT, DEST_LONG, DEST_LAT) %>% summarize(Count=sum(Pass.Flight))
total.post<-sum(airport2$Count)

airport.comp<-full_join(airport1, airport2, by=c("DEST_AIRPORT", "DEST_LONG", "DEST_LAT")) %>% filter(Count.y > 10000)  %>% mutate(Difference = (Count.x-Count.y)/Count.y)


airport.comp$Adjusted<-(airport.comp$Difference-.023)*100
airport.comp$SR<-abs(airport.comp$Adjusted)^0.5*ifelse(airport.comp$Adjusted*100>=0, 1, -1)

pal <- colorNumeric(palette = "RdYlGn", domain = airport.comp$SR)
airport.comp$Color<-pal(airport.comp$SR)


leaflet() %>% setView(lng=-99, lat=39, zoom=3) %>% addTiles() %>% addCircleMarkers(data=airport.comp, lng=~DEST_LONG, lat=~DEST_LAT, opacity=1, color =~Color , radius= 3,
            popup= paste(sep="<br/>", airport.comp$DEST_AIRPORT, paste(round(airport.comp$Count.x, 0), "came for Thanksgiving,", round(airport.comp$Count.y, 0), "returned here"), paste(signif(airport.comp$Difference*100, 2), "% Change"))) %>%
            addLegend("topright", colors = c("#B91927","#FFA500", "#FFFFBE", "#C3E57D", "#008000"), labels = c(-10, -3, 0, 3, 10), na.label= 'Not Reported', title = "% Change, Visting Versus Returning")

