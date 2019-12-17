# Research questions:
# The relationship between crime occured and location
# Does the number of occurence increasing by year
pacman::p_load(tidyverse,readr,knitr,ggplot2,magrittr,dplyr,plotly,leaflet)

# Read the data 
crime <- read.csv("Crime Incident.csv")
# Rename the columns
colnames(crime) <- c("Incident_Number","Offence_Code","Offence_Code_Group","Description","District","Area",
                     "Shooting","Occurred_Date","Year","Month","Day_of_Week","Hour","UCR_Part","Street","Latitude",
                     "Longtitude","Location")
# Remove the first 7 rows because of the data missing
crime <- crime[-c(1:7),]
# have a general idea about the data
summary(crime)
# remove missing values
crime %<>% na.omit()
# create new column called "City"
crime %<>% mutate(City = District)
crime$City <- as.character(crime$City)
crime$City[crime$City == "E18"] <- "Hyde Park"
crime$City[crime$City == "D14"] <- "Brighton"
crime$City[crime$City == "D4"] <- "South End"
crime$City[crime$City == "A15"] <- "Charlestown"
crime$City[crime$City == "B3"] <- "Mattapan"
crime$City[crime$City == "A7"] <- "East Boston"
crime$City[crime$City == "A1"] <- "Downtown"
crime$City[crime$City == "E5"] <- "West Roxbury"
crime$City[crime$City == "B2"] <- "Roxbury"
crime$City[crime$City == "C11"] <- "Dorchester"
crime$City[crime$City == "C6"] <- "South Boston"
crime$City[crime$City == "E13"] <- "Jamaica Plain"
crime$City[crime$City == ""] <- NA
crime %<>% na.omit()

# Clean the data further more
crime %<>% filter(Offence_Code_Group != "Aircraft")

# Take a look at "Motor Vehicle Accident Response"
motor <- crime %>% select(Incident_Number,Offence_Code_Group,Latitude,Longtitude,City,Year) %>% 
  filter(Offence_Code_Group == "Motor Vehicle Accident Response")
write.csv(motor,"Motor_Incident.csv")
larceny <- crime %>% select(Incident_Number,Offence_Code_Group,Latitude,Longtitude,City,Year) %>% 
  filter(Offence_Code_Group == "Larceny")
write.csv(larceny,"Larceny_Incident.csv")
assult <- crime %>% select(Incident_Number,Offence_Code_Group,Latitude,Longtitude,City,Year) %>% 
  filter(Offence_Code_Group == "Simple Assault")
write.csv(assult,"Simple_Assault.csv")

#ggplot(motor)+
  #geom_bar(aes(x=City,fill = factor(Year)),stat = "count",position = "dodge") + coord_flip()

# Visually see the data
#ggplot(crime)+
  #geom_bar(aes(x = Offence_Code_Group,y = ..prop..,group = 1),show.legend = FALSE)

plotly_build(ggplot(crime)+
  geom_bar(aes(x = City,fill = factor(Year)),stat = "count",position = "dodge")+
  coord_flip())

plotly_build(ggplot(crime)+
               geom_bar(aes(x = Year,fill = factor(City)),stat = "count",position = "dodge")+
               coord_flip())

crime$Day_of_Week <- factor(crime$Day_of_Week,
                            levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# The plot shows that there are no clear differences in the number of crimes vary from weeks.

# face_wrap by Year to see whether there is a different
crime$Year <- factor(crime$Year,levels = c("2015","2016","2017","2018","2019"))
y <- ggplot(crime) + 
  geom_bar(aes(x = City, y = ..prop..,group=1)) + 
  facet_wrap(~ Year, nrow = 2) +
  theme(axis.text.x=element_blank())
y$data <- y$data %>% group_by(Year) %>% filter(Year == "2015")
y

# face_wrap by City to see whehter there is a different
p<-ggplot(crime) + 
  geom_bar(aes(x = Year,y = ..prop..,group = 1)) + 
  facet_wrap(~ City, nrow = 4)
p$data <- p$data %>% group_by(City) %>% filter(City == "Brighton")
p


## Prepare groups for mapping
crime %<>% mutate(group = as.numeric(as.factor(crime$City)))
cities <- unique(crime$City)
m <- leaflet(crime) %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12) 
pal <- colorFactor(c("#999999", "#E69F00", "#56B4E9","#B2182B",
                     "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0",
                     "#92C5DE", "#4393C3", "#2166AC","#FFCC33"), domain = cities)
map1 <- m %>% addTiles() %>% addCircleMarkers(data = crime,lng = ~Longtitude,lat = ~Latitude,weight = 1,fillOpacity = 0.5,
                                     radius = 0.5,color = ~pal(City)) %>% 
  addLegend(position = "bottomright",pal = pal,values = cities,opacity = 1) 
map1

map2 <- m %>% addTiles() %>%
  addAwesomeMarkers(data = crime,lng = ~Longtitude,lat = ~Latitude,clusterOptions = T) %>% 
  addMarkers(lng =  -71.099688,lat = 42.349634,icon = bookIcon)
map2
## It acctually can be seen form the cluster map that there were many incidents happened in the Boston Univeristy Area.

# Mark Boston University
bookIcon <- makeIcon(iconUrl = "http://icons.iconarchive.com/icons/icons8/windows-8/512/Science-School-icon.png",
                     iconWidth = 38, iconHeight = 38)
# Map the first 30 outcomes for motor incidents
mo <- m %>% addTiles() %>% addMarkers(lng =  -71.099688,lat = 42.349634,icon = bookIcon) %>% 
  addMarkers(data = motor[1:30,],lng = ~Longtitude,lat = ~Latitude)
mo

# Map the first 30 outcomes for larceny incidents
lar <- m %>% addTiles() %>% addMarkers(lng =  -71.099688,lat = 42.349634,icon = bookIcon) %>% 
  addMarkers(data = larceny[1:30,],lng = ~Longtitude,lat = ~Latitude)
lar

# Map the first 30 outcomes for simple assult
a <- m %>% addTiles() %>% addMarkers(lng =  -71.099688,lat = 42.349634,icon = bookIcon) %>% 
  addMarkers(data = assult[1:30,],lng = ~Longtitude,lat = ~Latitude)
a

# text mining
library(tidytext)
crime$Description <- tolower(crime$Description)
test <- tibble(line = 1:length(crime$Description),text = crime$Description)
word <- test %>% unnest_tokens(word,text)
library(wordcloud)
word %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

## Prepare data for Shiny app
crime_shiny <- crime %>% select(Incident_Number,Offence_Code_Group,City,group,Year,Latitude,Longtitude,Description)
write.csv(crime_shiny,"crime_shiny.csv")

