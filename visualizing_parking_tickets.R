library(dplyr)
library(tidyr)

pysakointivirheet <- read.csv("Pysakointivirheet.csv", header = TRUE, sep = ",", 
                              colClasses = c("factor","character","factor","factor",
                                             "factor","factor","numeric","numeric",
                                             "factor","factor","factor","factor",
                                             "character"))


#Pysakointivirheet analyysi

str(pysakointivirheet)
summary(pysakointivirheet)
summary(pysakointivirheet$Postinumero)
glimpse(pysakointivirheet)
head <-head(pysakointivirheet)
write.csv(head,"head_pysakointivirheet.csv")
min(pysakointivirheet$Virheen.tekovuosi)
max(pysakointivirheet$Virheen.tekovuosi)

#Grouping the data by postal codes and making a count variable summarising
#the amount of parking tickets
library(dplyr)
p <- pysakointivirheet %>%
  group_by(Postinumero) %>%
  summarise(count = n()) %>%
  arrange(count)

#let's delete the identified NA row 
p <- p[-89,]
#barplot(p$Postinumero,p$count)
#as.character(p$Postinumero)
#p2 <- data.frame(as.character(p$Postinumero),p$count)
#Let's create another variable that that has the percentages of parking tickets  
percentages <- p$count / sum(p$count)
p2 <- data.frame(p$Postinumero,p$count,percentages)

#Let's download .shp map and plot the tickets on it! 
library(rgeos)
library(maptools)
np_dist <- readShapeSpatial("PKS_postinumeroalueet_2017_shp.shp")
#The map currently has also Espoo and Vantaa so let's filter it so that only Helsinki remains
helsinki_postal_codes <- c("00100","00120","00130","00140","00150","00160","00170",
                           "00180","00200","00210","00220","00230","00240","00250",
                           "00260","00270","00280","00290","00300","00310","00320",
                           "00330","00340","00350","00360","00370","00380","00390",
                           "00400","00410","00420","00430","00440","00500","00510",
                           "00520","00530","00540","00550","00560","00570","00580",
                           "00590","00600","00610","00620","00630","00640","00670",
                           "00680","00690","00700","00710","00720","00730","00740",
                           "00750","00760","00770","00780","00790","00800","00810",
                           "00820","00830","00840","00850","00860","00870","00880",
                           "00890","00900","00910","00920","00930","00940","00950",
                           "00960","00970","00980","00990","00450","00460","00470",
                           "00480","00490")
np_dist <- np_dist[np_dist$Posno %in% helsinki_postal_codes,]
length(np_dist$Posno)

#Let's fortify the map, so that it can be drawn, postal numbers as id's!
np_dist <- fortify(np_dist, region = "Posno")


library(ggplot2)

#This dataframe is used to group the map data by postal code and getting
#means of both long and lat for all postal codes for labeling the areas
distcenters <- np_dist %>%
                group_by(id) %>%
                summarise(clat = mean(lat), clong = mean(long))

#Let's merge the dataframes 
merged <- merge(p,distcenters,by.x="Postinumero", by.y="id", all.x = TRUE, all.y = TRUE)
merged <- merged[merged$Postinumero!="002230" & merged$Postinumero!="00501" & merged$Postinumero!="00631" & merged$Postinumero!="00632", ]
merged_sorted <- merged[order(merged$Postinumero),]
mean(merged_sorted$count, na.rm = TRUE)
median(merged_sorted$count, na.rm = TRUE)
merged_top <- head(merged_sorted,1)
merged_top

#Now we can actually plot the Chloropleth map!!
ggplot() + geom_map(data = merged, aes(map_id = Postinumero, fill = count),
                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat) + scale_fill_gradient2(low = "white", 
                    midpoint = 6000, high = "red", limits = c(0, 89561)) +
                    ggtitle("Counts of parking tickets in Helsinki region postal codes 2014-2017") + 
                    geom_text(data = merged_top, aes(x = clong, y = clat, label = Postinumero, size = 3), size = 3, col = "darkgrey")

#below adds labels to the map, however, it is a mess...
#+ geom_text(data = merged_top, aes(x = clong, y = clat, label = Postinumero, size = 3), size = 2)

#Let's now take a deep dive into the area, where most parking tickets are granted
np_dist <- readShapeSpatial("PKS_postinumeroalueet_2017_shp.shp")
helsinki_postal_codes <- c("00100","00120","00130","00140","00150","00160","00170",
                           "00180","00200","00210","00220","00230","00240","00250",
                           "00260","00270","00280","00290","00300")
np_dist <- np_dist[np_dist$Posno %in% helsinki_postal_codes,]
np_dist <- fortify(np_dist, region = "Posno")
distcenters <- np_dist %>%
  group_by(id) %>%
  summarise(clat = mean(lat), clong = mean(long))

merged <- merge(p,distcenters,by.x="Postinumero", by.y="id", all.x = FALSE, all.y = TRUE)
merged_sorted <- merged[order(merged$Postinumero),]
mean(merged_sorted$count, na.rm = TRUE)
median(merged_sorted$count, na.rm = TRUE)
merged_top <- head(merged_sorted,19)
merged_top

ggplot() + geom_map(data = merged, aes(map_id = Postinumero, fill = count),
                    map = np_dist) +
                    #geom_polygon(data=np_dist, aes(x=lat, y=long), col='black') + 
                    expand_limits(x = np_dist$long, y = np_dist$lat) + 
                    scale_fill_gradient2(low = "white", 
                    midpoint = 10000, high = "red", limits = c(0, 89561)) +
                    ggtitle("Counts of parking tickets in Helsinki region postal codes 2014-2017") + 
                    geom_text(data = merged_top, aes(x = clong, y = clat, label = Postinumero, size = 3), size = 3, col = "darkgrey")

#After we know where to (not) park, we should also know when to (not) park
library(dplyr)
t1 <- pysakointivirheet %>%
  group_by(Virheen.tekovuosi) %>%
  summarise(count = n())


t2 <- pysakointivirheet %>%
  group_by(Virheen.tekokuukausi,Virheen.tekovuosi) %>%
  summarise(count = n())

t2 <- t2[order(t2$count),]
sd(t2$count)

plot(t1)
barplot(counts)
plot(t2)
summary(t1)
summary(t2)

barplot(t2$count, legend=t2$Virheen.tekokuukausi)

p<-ggplot(data=t2, aes(x=t2$Virheen.tekokuukausi, y=t2$count)) +
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Blues")
  #scale_x_discrete(limits=c("Elokuu", "Hein‰kuu", "Helmikuu"))
  #theme_minimal()

p

#Let's look at the ticket types 
library(dplyr)
library(tidyr)
t4 <- pysakointivirheet %>%
  group_by(Virheen.kirjaaja) %>%
  summarise(count = n())
pie(t4$count, labels = t4$Virheen.kirjaaja)

t4 <- pysakointivirheet %>%
  separate(Virheen.p‰‰luokka...p‰‰syy,c("Ticket type","description"))
  
t4 <- t4 %>%
    group_by(`Ticket type`) %>%
    summarise(count = n())



#Making a map of Helsinki with scatter dots of parking tickets 
np_dist <- readShapeSpatial("PKS_postinumeroalueet_2017_shp.shp")
helsinki_postal_codes <- c("00100","00120","00130","00140","00150","00160","00170",
                           "00180","00200","00210","00220","00230","00240","00250",
                           "00260","00270","00280","00290","00300")
#Let's filter the data and the map to only include Helsinki "center" area 
np_dist <- np_dist[np_dist$Posno %in% helsinki_postal_codes,]
np_dist <- fortify(np_dist, region = "Posno")
sakot_helsinki <- pysakointivirheet[pysakointivirheet$Postinumero %in% helsinki_postal_codes,]
summary(sakot_helsinki$y)
summary(sakot_helsinki$x)
#For some reason both the y and x coordinates include zeros let's get rid of them
sakot_helsinki <- sakot_helsinki[sakot_helsinki$y != 0, ]
sakot_helsinki <- sakot_helsinki[sakot_helsinki$x != 0, ]
summary(sakot_helsinki$y)
summary(sakot_helsinki$x)

sakot_helsinki_top <- head(sakot_helsinki,20000)
#plot(np_dist)
library(ggplot2)
library(ggmap)

#Plotting
ggplot() + geom_polygon(data = np_dist, aes(x=long, y = lat, fill = "posno", group = group),color="white",fill="gray") +
  geom_point(data = sakot_helsinki, aes(x = x, y = y), color = "red", size = 0.1) + 
  guides(fill=TRUE) + 
  ggtitle("Parking tickets in Helsinki 2014-2017")

#Zooming in this image you can actually start to see the streets of Helsinki :)

#Moving to create visualizations solely about the postal code area 00100
np_dist <- readShapeSpatial("PKS_postinumeroalueet_2017_shp.shp")
postal_code <- "00100"
np_dist <- np_dist[np_dist$Posno %in% postal_code,]
#np_dist <- fortify(np_dist, region = "Posno")
sakot_postinumero <- pysakointivirheet[pysakointivirheet$Postinumero %in% postal_code,]
#We have to feature engineer the coordinates
sakot_postinumero <- sakot_postinumero[sakot_postinumero$y != 0, ]
sakot_postinumero <- sakot_postinumero[sakot_postinumero$x != 0, ]
sakot_postinumero$x_new <- sakot_postinumero$x / 1000000
sakot_postinumero$y_new <- sakot_postinumero$y / 100000
sakot_postinumero_top <- head(sakot_postinumero,80000)

#Plotting the scatter plot
ggplot() + geom_polygon(data = np_dist, aes(x=long, y = lat, group = group),color="white",fill="gray") +
  geom_point(data = sakot_postinumero_top, aes(x = x, y = y), color = "red", size = 0.1) +
  #guides(fill=TRUE) + 
  ggtitle("Parking tickets in 00100 2014-2017")

#Let's make a heatmap of 00100 parking tickets
ggplot() + geom_polygon(data = np_dist, aes(x=long, y = lat, group = group),color="white",fill="gray") +
  geom_density2d(data = sakot_postinumero_top, aes(x = x, y = y), size = 0.3) +
  stat_density2d(data = sakot_postinumero_top, 
    aes(x = x, y = y, fill = ..level.., alpha = ..level..), size = 0.01, 
    bins = 20, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.7), guide = FALSE) +
  ggtitle("Parking tickets in 00100 2014-2017")

#Which streets are the most common
kadut <- sakot_postinumero %>%
  group_by(Osoite) %>%
  summarise(count = n()) %>%
  
kadut <- kadut[order(kadut$count, decreasing=TRUE), ]
head(kadut,10)
write.csv(kadut,"parkkisakot_kadut.csv")


#Now let's try to make a satellite image picture
library(ggmap)
sbbox <- make_bbox(lon = sakot_postinumero$x_new, lat = sakot_postinumero$y_new, f=.1)
sbbox
coords <- c(mean(sakot_postinumero$x_new),mean(sakot_postinumero$y_new))
coords
coords2 <- c(24.93837910000002,60.16985569999999)
sq_map <- get_map(location = coords2, maptype = "roadmap", source = "google",zoom=14)
ggmap(sq_map)

setwd("~/Information visualization/Pyoramaarat/Helsinki_liikennevaylat_avoin_data/Shape")
katukartta <- readShapeSpatial("~/Information visualization/Pyoramaarat/Helsinki_liikennevaylat_avoin_data/Shape/Hki_liikennevaylat.shp")
