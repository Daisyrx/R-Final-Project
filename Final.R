# Research questions:
# The relationship between crime occured and location
# Does the number of occurence increasing by year
pacman::p_load(tidyverse,readr,knitr,ggplot2)

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

# Visually see the data

ggplot(crime)+
  geom_bar(aes(x = Offence_Code_Group,y = ..prop..,group = 1),show.legend = FALSE)

crime$Day_of_Week <- factor(crime$Day_of_Week,
                            levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
ggplot(crime)+
  geom_bar(aes(x=Day_of_Week,y=..prop..,group = 1))
ggplot(crime)+
  geom_bar(aes(x=Day_of_Week,fill = District),stat = "count",position = "fill")
# The plot shows that there are no clear differences in the number of crimes vary from weeks.



