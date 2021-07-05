### Loading Data from file

# install.packages("DT")
# install.packages("ggthemes")
# install.packages('esquisse')
# 
# update.packages(ask = FALSE,checkBuilt = TRUE)
library(psych)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
# library(esquisse)
FLT_DATA_19_20 <- read_excel("D:/DSINSAID/DataSet/Flight Data/FLT DATA 19-20.xlsx")

View(head(FLT_DATA_19_20,10)) #default is 6


# statistics analysis of data
glimpse(FLT_DATA_19_20) # gibe
summary(FLT_DATA_19_20)
# View(describe(FLT_DATA_19_20))

#NOTE::::
# Pipe operator %>% (Ctrl + Shift + M)
# For Chart we require 
  # (1) Dataset 
  # (2) Aesthetics (Variable to plots) 
  # (3) Chart Type (Bar, scatter, pie, hist etc )
  # (4) Coordinates (X and Y)
  # (5) Themes Colors
  # (6) faceting -- breaking the charts

GetDate <- function(msg)
{
  n <-  readline(prompt = msg )
  return(as.Date(n))
}


FromDate <- GetDate("Enter From date (yyyy-mm-dd):")

ToDate <- GetDate("Enter To date (yyyy-mm-dd):")

print(c(FromDate,ToDate))

### How many operational type of aircraft?
### How many fleets per aircraft?
View(distinct(FLT_DATA_19_20,"AircraftType" = AcName, "Aircraft" = AcRegNo)
%>% arrange(AircraftType,Aircraft) )

DF <- group_by(FLT_DATA_19_20,"AircraftType" = AcName, "Aircraft" = AcRegNo) %>% summarise(Flights = n())
View(DF)


# # Create data for the graph.# Barplot & pie plot
ggplot(DF, aes(x="", y=factor(Flights), fill=Aircraft))+
  geom_bar(width = 1, stat = "identity",position = "dodge")+labs(title = "title")

ggplot(DF, aes(x="", y=factor(Flights), fill=Aircraft))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)

plot(x=FLT_DATA_19_20$FlightStatusID, y = FLT_DATA_19_20$P1EmpID)

### How many operational airfields?
### How many flying sectors and country wise departures?
DATA_AIRFIELD <- FLT_DATA_19_20 %>% group_by("Country" = FromCountryName, "Airfield" = FromCity)  %>% 
  summarise(Flights = n()) %>% arrange(desc(Flights)) 
View(DATA_AIRFIELD)

ggplot(DATA_AIRFIELD, aes(x="", y=factor(Flights), fill=Country))+
  geom_bar(width = 1, stat = "identity",position = "dodge") 


ggplot(DATA_AIRFIELD, aes(x="", y=factor(Flights), fill=Country))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)

# flying sectors
View(DATA_AIRFIELD %>% group_by(Country)  %>% summarise(Airfields = n()) 
%>% arrange(desc(Airfields)))

# ------Plot Country wise departure count
ggplot(FLT_DATA_19_20,aes(x= FromCountryCode)) + geom_bar(color="red", fill="green")


### Airfield wise flights count on selected date range?
DF1 <- FLT_DATA_19_20 %>%  filter(FlightDate >= as.Date("2020-01-05") & FlightDate <= as.Date("2020-01-06") )
DF2 <- DF1 %>% group_by("Country" = FromCountryName,"Airfield" = FromCity) %>% summarise(Flights = n())
View(DF2)

# bar chart for showing data
ggplot(DF2, aes(x=factor(Airfield), y=Flights, fill=Airfield)) +   
  geom_bar(width = 1, stat = "identity", position = "dodge") +
  labs(title = "title")
#stack,fill, dodge
# 
# dt_car = mtcars
# esquisser()

# bar chart for showing data country wise
ggplot(DF2, aes(x="", y=Flights, fill=Country)) +   
  geom_bar(width = 1, stat = "identity", position = "dodge")
#stack,fill, dodge

# piechart for showing data
ggplot(DF2, aes(x="", y=Flights, fill=Country))+
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) 
  # geom_text(aes(x=factor(1), y=Flights, label=Flights, ymax=Flights), 
  #           position=position_fill(width=1))


# ggplot(DF2, aes(x = "", y = Flights, 
#                fill = as.factor(Country))) +
#   geom_bar(width = 1, stat = "identity", color = "white") +
#   coord_polar("y", start = 0) +  
#   scale_fill_manual(values =c("#0073C2FF", "#EFC000FF", "#868686FF"))+
#   theme_void()
# +
#   geom_text(aes(y = as.factor(Flights), 
#                 label = as.factor(Flights)), color = "white")
# # +
# #   


# 
# ggplot(DF2, 
#        aes(x = factor(1),fill = factor(Country))) + 
#   geom_bar(stat = "count") + 
#   scale_y_continuous(breaks = seq(0,12,3), labels = c("0", "25%", "50%", "75%", "100%")) +
#   coord_polar(theta='y') +
#   theme(axis.text.y = element_blank(), 
#         axis.title.y = element_blank(), 
#         axis.ticks.y = element_blank(),
#         axis.title.x = element_blank()) +
#   labs(fill = "Country")

# 
# ggplot(FLT_DATA_19_20,aes(x= FromCountryCode)) + geom_bar(color="red", fill="green")
# 
# 
# plot(FLD_AIRCRAFT$`FLT_DATA_19_20$AcName`, FLD_AIRCRAFT$n) + hist(FLD_AIRCRAFT$`FLT_DATA_19_20$AcName`,FLD_AIRCRAFT$n)

# Adding BlockTime and IsDelay
FLT_DATA_19_20$IsDelay = ifelse(FLT_DATA_19_20$DelayCode == 'NULL' | is.null(FLT_DATA_19_20$DelayCode),0,1)
FLT_DATA_19_20$BlockTime = difftime(FLT_DATA_19_20$Arrival, FLT_DATA_19_20$Departure, minute()) 
FLT_DATA_19_20$TotalCrewCount = FLT_DATA_19_20$AircrewCount + FLT_DATA_19_20$CCCount + FLT_DATA_19_20$DHCount 

FLT_DATA_19_20 %>%  head() %>% View()

### Which is longest flight till now?
FLT_DATA_19_20 %>% filter( FlightStatusID != 1 & FromCityID != ToCityID) %>%  arrange(desc(BlockTime)) %>% mutate(DTL='Longest Flight') %>% 
  select( DTL,FlightID,FlightDate,FlightNo,FromCity,ToCity,BlockTime) %>%  head(1) %>% View()


### Which is shortest flight till now?
FLT_DATA_19_20 %>% filter(FlightStatusID != 1 & FromCityID != ToCityID) %>%  arrange(BlockTime) %>% mutate(DTL='Shortest Flight') %>% 
  select( DTL,FlightID,FlightDate,FlightNo,FromCity,ToCity,BlockTime) %>%  head(1) %>% View()



### Delay count fleet wise and aircraft type wise on selected date range?

# FromDate <- GetDate("Enter From date:")
# ToDate <- GetDate("Enter To date:")

FromDate <- '2019-01-01'
ToDate <- '2022-01-01'

FLT_DATA_19_20 %>% filter(FlightStatusID == 0 & IsDelay == 1 & FlightDate >= as.Date(FromDate) & FlightDate <= as.Date(ToDate) ) %>% 
  group_by(AcName,AcRegNo) %>% summarise( Cnt = n()) %>% arrange(desc(Cnt)) %>% View()


### Showcase most delay code in specific date range?
### Cancelled flight report grouping with fleet and aircraft type wise?
FLT_DATA_CANCEL <- FLT_DATA_19_20 %>% filter(FlightStatusID == 1 & FlightDate >= as.Date(FromDate) & FlightDate <= as.Date(ToDate) ) %>% 
  group_by(AcName,AcRegNo) %>% summarise( Cnt = n()) %>% arrange(desc(Cnt)) 
View(FLT_DATA_CANCEL)


# piechart for showing data
ggplot(FLT_DATA_CANCEL, aes(x="", y=Cnt, fill=AcRegNo)) +   
  geom_bar(width = 1, stat = "identity", position = "dodge")


FLT_DATA_DL1 <- FLT_DATA_19_20 %>% filter(FlightStatusID == 0 & DL1 != 'NULL' & FlightDate >= as.Date(FromDate) & FlightDate <= as.Date(ToDate) ) %>% 
  group_by(DL1) %>% summarise( Cnt = n()) %>% arrange(desc(Cnt)) 
View(FLT_DATA_DL1)


### Crew Data 
FLT_DATA_19_20 %>% filter(FlightStatusID == 0 & FlightDate >= as.Date(FromDate) & FlightDate <= as.Date(ToDate) ) %>% 
  group_by(AcName) %>% summarise( Cnt = max(TotalCrewCount)) %>% arrange(desc(Cnt))  %>% View()

FLT_DATA_19_20 %>% filter(FlightStatusID == 0 & P1EmpID != "NULL" & FlightDate >= as.Date(FromDate) & FlightDate <= as.Date(ToDate) ) %>% 
  count(AcName,P1EmpID, sort = TRUE)  %>%  View()

FLT_DATA_19_20 %>% filter(FlightStatusID == 0 & P2EmpID != "NULL" & FlightDate >= as.Date(FromDate) & FlightDate <= as.Date(ToDate) ) %>% 
  count(P2EmpID, sort = TRUE)  %>%  View()


FLT_DATA_19_20 %>% filter(FlightStatusID == 0 &  P1EmpID != "NULL" & FlightDate >= as.Date(FromDate) & FlightDate <= as.Date(ToDate) ) %>% 
  count(FromCity,ToCity,P1EmpID, sort = TRUE)  %>%  View()

FLT_DATA_19_20 %>% filter(FlightStatusID == 0 & IsDelay == 1 & P1EmpID != "NULL" & FlightDate >= as.Date(FromDate) & FlightDate <= as.Date(ToDate) ) %>% 
  count(P1EmpID, sort = TRUE)  %>% head(10) %>%  View()

FLT_DATA_19_20 %>% head() %>% View()

# 
# # Create data for the graph.
# x <-  c(21, 62, 10,53)
# labels <-  c("London","New York","Singapore","Mumbai")
# 
# piepercent<- round(100*x/sum(x), 1)
# 
# # Give the chart file a name.
# png(file = "city_percentage_legends.jpg")
# 
# # Plot the chart.
# pie(x, labels = piepercent, main = "City pie chart",col = rainbow(length(x)))
# legend("topright", c("London","New York","Singapore","Mumbai"), cex = 0.8,
#        fill = rainbow(length(x)))
# 
# # Save the file.
# dev.off()
