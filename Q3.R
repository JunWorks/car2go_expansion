library(jsonlite)
library(ggplot2)
library(ggmap)
library(dplyr)
library(grid)
library(broom)
source('decodeLine.R')
source('init.R')
# for loop to scrape car2go location data
for (day in 1:7){
  car.df.time <- data.frame()
  
  for (i in (1:288)){
    car.df <- data.frame()
    city = "austin"
    car2goURL <- paste('https://www.car2go.com/api/v2.1/vehicles?loc=', city, '&oauth_consumer_key=car2gowebsite&format=json', sep = '' )
    #import from JSON
    car2goData <- fromJSON(txt = car2goURL)
    
    #unlist and save
    coord <- data.frame(matrix(unlist(car2goData[[1]][2]), ncol=3, byrow = T))
    car.df.raw <- data.frame(matrix(unlist(car2goData[[1]]), ncol=11, byrow = F), City = city)[c(-2, -3, -4)]
    car.df.raw <- cbind(car.df.raw[,1], charging = NA, car.df.raw[,2:9], coord)
    colnames(car.df.raw) <- c(colnames(car2goData[[1]])[1], 'charging', colnames(car2goData[[1]])[3:9], 'City', 'Longitude', 'Latitude', 'Altitude')
    #add timestamp
    car.df <- data.frame(rbind(car.df, car.df.raw), Time = Sys.time())
  }
car.df.time <- rbind(car.df.time, car.df)
print(Sys.time())
Sys.sleep(299)
}

car.df.time$fuel <- as.numeric(as.character(car.df.time$fuel))
write.csv(car.df.time, file = paste('data/', day, 'Timedcar2go_week.csv', sep = ''))
time.df <- read.csv('data/1Timedcar2go_week.csv', header = T)

# identify car movement
car.first.app <- subset(time.df, !duplicated(time.df[, c(-1,-15)], fromLast = F))
car.first.app <- car.first.app[duplicated(car.first.app[, 8]) | duplicated(car.first.app[, 8], fromLast = T),]

car.last.app <- subset(time.df, !duplicated(time.df[, c(-1,-15)], fromLast = T))
car.last.app <- car.last.app[duplicated(car.last.app[, 8]) | duplicated(car.last.app[, 8], fromLast = T),]

car.name <- as.character(unique(car.first.app$name))
car.route <- data.frame()
trip.info <- data.frame()
# for loop to construct trip stats and routes
for (name in car.name){
  
  car.single <- car.first.app[car.first.app$name == name,]
  car.single.last <- car.last.app[car.last.app$name == name,]
  
  car.route.single <- data.frame()
  trip.single <- data.frame()
  
  for (trip.ct in 1:(nrow(car.single)-1)){  
    
    car.route.all <- route(from = c(car.single[trip.ct, 12], car.single[trip.ct, 13]) , 
                           to = c(car.single[trip.ct+1, 12], car.single[trip.ct+1, 13]), structure = 'route', mode = 'driving',
                           output = 'all')
    
    car.route.temp <- decodeLine( car.route.all$routes[[1]]$overview_polyline$points )
    car.route.single <- rbind(car.route.single, data.frame(car.route.temp, startTime = car.single.last$Time....Sys.time..[trip.ct]))
    trip.single.temp <- data.frame(name = name, Dist = car.route.all$routes[[1]]$legs[[1]]$distance$value, 
                                   Fuel = car.single$fuel[trip.ct]-car.single$fuel[trip.ct+1], 
                                   Time = as.numeric(difftime(car.single$Time....Sys.time..[trip.ct+1], car.single.last$Time....Sys.time..[trip.ct], units = "mins")),
                                   startTime = car.single.last$Time....Sys.time..[trip.ct]
    )
    trip.single <- rbind(trip.single.temp, trip.single)
  }
  car.route <- rbind(car.route, data.frame(car.route.single, name = name))
  trip.info <- rbind(trip.single, trip.info)
}
#output files
write.csv(car.route, file = 'data/Mondayroutetime.csv')
write.csv(trip.info, file = 'data/Mondaytrip.csv')

#visualize routes
map <- get_map(location = 'austin', zoom = 12)
trip <- ggmap(map) + geom_path(aes(x = lon, y = lat, group = name), size = 1, data = car.route.sub, lineend = 'round', alpha = 0.1, color = '#D55E00') + 
  #geom_point(aes(x = Longitude, y = Latitude, color = name), data = car.app.sub, size =3)+ 
  theme_tws_map() + theme(legend.position="none",axis.title = element_blank())

png('plots/trip.png', width=1080, height=1080)
print(trip)
dev.off()

# for loop to extract zip code if available in address, this is to save google map api calls
i = 1
for (address in car.last.app$address){
  add <- unlist(strsplit(toString(address), split = ' '))
  zip <- add[pmatch('787', add)]
  car.last.app$zip[i] <- zip
  i = i+1
}

i = 1
for (address in car.first.app$address){
  add <- unlist(strsplit(toString(address), split = ' '))
  zip <- add[pmatch('787', add)]
  car.first.app$zip[i] <- zip
  i = i+1
}

# If zip code is not available in address, using google map api to look up based on lon and lat
i=1
for (zip in car.last.app$zip){
  if (is.na(zip)){
    car.last.app$zip[i] <- toString(revgeocode(c(car.last.app$Longitude[i], car.last.app$Latitude[i]), output="more")$postal_code)
  }
  i=i+1
}

i=1
for (zip in car.first.app$zip){
  if (is.na(zip)){
    car.first.app$zip[i] <- toString(revgeocode(c(car.first.app$Longitude[i], car.first.app$Latitude[i]), output="more")$postal_code)
  }
  i=i+1
}

# save and combine data
write.csv(car.first.app[,-1], file = 'data/firstappearance.csv')
write.csv(car.last.app[,-1], file = 'data/lastappearance.csv')
car.start.end <- rbind(car.last.app[, -1], car.first.app[, -1])

# aggregate trip stats per zip
trip.zip <- car.start.end %>%
  group_by(zip) %>%
  summarise(count = n()*0.5)%>%
  arrange(desc(count))

trip.zip$zip[trip.zip$count<30] <- "Other"

trip.zip <- trip.zip %>%
  group_by(zip) %>%
  summarise(count = sum(count))%>%
  arrange(desc(count))

# plot trip count
plot.zip.1 <- ggplot(data=filter(trip.zip, count > 30), aes(x=factor(zip), y=count)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits=unique(filter(trip.zip, count > 30)$zip)) +
  title_with_subtitle("Trip count for each Zip in Austin, TX", 'A trip between two Zips counts as 0.5 for each Zip') +
  labs(x="Zipcode", y="Count") +
  theme_tws(base_size = 20) + theme(legend.position=c(0.9, 0.5))

plot.zip.2 <- ggplot(data=filter(trip.zip, count < 30), aes(x=factor(zip), y=count)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits=unique(filter(trip.zip, count < 30)$zip)) +
  theme_tws(base_size = 13) + theme(legend.position='none', 
                                    axis.title = element_blank(),
                                    axis.text.x  = element_text(angle=45, vjust=0.5))

plot_inset <- function(name, p1, p2){
  png(name, width=640, height=480)
  grid.newpage()
  v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
  v2<-viewport(width = 0.4, height = 0.4, x = 0.74, y = 0.62) #plot area for the inset map
  print(p1,vp=v1) 
  print(p2,vp=v2)
  dev.off()  
}

plot_inset('plots/zip2.png', plot.zip.1, plot.zip.2)
