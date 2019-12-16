# Research questions:
# The relationship between crime occured and location
# Does the number of occurence increasing by year
pacman::p_load(tidyverse,readr,knitr,ggplot2,magrittr,dplyr)

# Read the data 
crime <- read.csv("Crime Incident.csv")
liquor <- read.csv("liquor-licenses.csv")
liquor$Address <- tolower(liquor$Address)
liquor %<>% rename(Street = Address)
# Rename the columns
colnames(crime) <- c("Incident_Number","Offence_Code","Offence_Code_Group","Description","District","Area",
                     "Shooting","Occurred_Date","Year","Month","Day_of_Week","Hour","UCR_Part","Street","Latitude",
                     "Longtitude","Location")
# Remove the first 7 rows because of the data missing
crime <- crime[-c(1:7),]
# have a general idea about the data
summary(crime)
# change street names into lower case
crime$Street <- tolower(crime$Street)
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
motor <- crime %>% filter(Offence_Code_Group == "Motor Vehicle Accident Response")
ggplot(motor)+
  geom_bar(aes(x=City,fill = factor(Year)),stat = "count")

# Visually see the data
ggplot(crime)+
  geom_bar(aes(x = Offence_Code_Group,y = ..prop..,group = 1),show.legend = FALSE)

ggplot(crime)+
  geom_bar(aes(x = City),stat = "count",show.legend = FALSE)+
  coord_flip()

crime$Day_of_Week <- factor(crime$Day_of_Week,
                            levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
ggplot(crime)+
  geom_bar(aes(x=Day_of_Week,y=..prop..,group = 1))
ggplot(crime)+
  geom_bar(aes(x=Day_of_Week,fill = District),stat = "count",position = "fill")
# The plot shows that there are no clear differences in the number of crimes vary from weeks.

# face_wrap by Year to see whether there is a different
y <- ggplot(crime) + 
  geom_bar(aes(x = City, y = ..prop..,group=1)) + 
  facet_wrap(~ Year, nrow = 2)
y$data <- y$data %>% group_by(Year) %>% filter(Year == "2015")
y

# face_wrap by City to see whehter there is a different
p<-ggplot(crime) + 
  geom_bar(aes(x = Year,y = ..prop..,fill=Year)) + 
  facet_wrap(~ City, nrow = 4)
p$data <- p$data %>% group_by(City) %>% filter(City == "Brighton")
p

ggplot(crime)+
  geom_bar(aes(x=Brighton,y=..prop..,group=1))

## Prepare groups for mapping
crime %<>% mutate(group = as.numeric(as.factor(crime$City)))

## Prepare data for Shiny app
crime_shiny <- crime %>% select(Incident_Number,Offence_Code_Group,City,group,Year,Latitude,Longtitude)
write.csv(crime_shiny,"crime_shiny.csv")

