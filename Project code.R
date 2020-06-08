###Load required packages######
library(data.table)
library(RSQLite)
library(dbplyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(naniar)
library(cluster)
library(factoextra)
library(gridExtra)
library(lubridate)
library(mapproj)
library(ggthemes)
library(maps)
library(stats)
library(utils)
library(datasets)
library(moments)
library(tidyverse)
library(corrplot)
library(car)

glimpse(data)
head(data)
sum(is.na.data.frame(data))
miss_var_table(data)
miss_case_table(data)

vis_dat(data)
vis_miss(data)
data2 <- na.omit(data)
sum(is.na.data.frame(data2))
glimpse(data2)
View(data2)

distinct_df = data2 %>% distinct(T)
glimpse(distinct_df)

####Read in fires data####
fires <- readRDS("C:/Users/prc0549/Documents/fires.rds")
fires$DISCOVERY_DATEymd <- as.Date(fires$DISCOVERY_DATE - 2458014.5, origin = '2017-09-18')
fires$FIREMONTH <- month(ymd(fires$DISCOVERY_DATEymd), label = TRUE, abbr = TRUE)

glimpse(fires)

fires %>% 
  group_by(FIRE_YEAR) %>%
  summarize(n_fires = n()) %>%
  ggplot(aes(x = FIRE_YEAR, y = n_fires/1000)) + 
  geom_bar(stat = 'identity', fill = 'darkred') +
  labs(x = '', y = 'Number of wildfires (thousands)', title = 'US Wildfires by Year')+
  theme(plot.title = element_text(hjust = 0.5))

fires %>% 
  filter(FIRE_YEAR == "2006")%>%
  filter(STATE != "AK")%>%
  filter(STATE != "HI")%>%
  filter(STATE != "PR")%>%
  group_by(FIREMONTH) %>%
  summarize(n_fires = n()) %>%
  View()

fires %>% 
  filter(FIRE_YEAR == "2006")%>%
  group_by(FIREMONTH) %>%
  summarize(n_fires = n()) %>%
  View()


fires %>% 
  filter(FIRE_YEAR == "2006")%>%
  group_by(DISCOVERY_DOY) %>%
  summarize(n_fires = n()) %>%
  ggplot(aes(x = DISCOVERY_DOY, y = n_fires)) + 
  geom_line(color = 'orange') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', size = 0.4, color = 'red') + 
  labs(x = '', y = 'Number of wildfires', title = 'US Wildfires by Day of Year')

fires2 <- subset(fires, select=c(FIRE_YEAR,DISCOVERY_DATEymd,FIRE_SIZE,STAT_CAUSE_DESCR,LATITUDE,LONGITUDE,STATE))
fires2 <-fires2 %>% 
  filter(FIRE_YEAR == "2006")
sum(is.na.data.frame(fires2))
fires2$latround <- round(fires2$LATITUDE/.5)*.5
fires2$lonround <- round(((round((fires2$LONGITUDE-65.3333)/0.6666666))*0.6666666)+65.33333,2)
glimpse(fires2)
summary(fires2)

####FFMC data join####
FFMCdata<-as.data.frame(fread("MS Analytics/ANLY 699/Data project/FFMC.tsv"))
FFMCdata$date <- as.Date(FFMCdata$T,"%Y-%m-%d")
FFMCdata$lonround <- round(FFMCdata$lon,2)
glimpse(FFMCdata)
summary(FFMCdata)

FFMCdatajoin <- merge(x=fires2,y=FFMCdata,by.x=c("latround","lonround","DISCOVERY_DATEymd"),by.y=c("lat","lonround","date"),all.x=TRUE)
glimpse(FFMCdatajoin)
colSums(is.na(FFMCdatajoin))
FFMCdatajoin <- na.omit(FFMCdatajoin)
FFMCdatajoin <- subset(FFMCdatajoin, select = -c(LATITUDE, LONGITUDE,T,lon))
str(FFMCdatajoin)

####DC data join####
DCdata<-as.data.frame(fread("MS Analytics/ANLY 699/Data project/DC.tsv"))
DCdata$date <- as.Date(DCdata$T,"%Y-%m-%d")
DCdata$lonround <- round(DCdata$lon,2)

DCdatajoin <- merge(x=FFMCdatajoin,y=DCdata,by.x=c("latround","lonround","DISCOVERY_DATEymd"),by.y=c("lat","lonround","date"),all.x=TRUE)
colSums(is.na(DCdatajoin))
DCdatajoin <- subset(DCdatajoin, select = -c(T,lon))

####DMC data join####
DMCdata<-as.data.frame(fread("MS Analytics/ANLY 699/Data project/DMC.tsv"))
DMCdata$date <- as.Date(DMCdata$T,"%Y-%m-%d")
DMCdata$lonround <- round(DMCdata$lon,2)

DMCdatajoin <- merge(x=DCdatajoin,y=DMCdata,by.x=c("latround","lonround","DISCOVERY_DATEymd"),by.y=c("lat","lonround","date"),all.x=TRUE)
colSums(is.na(DMCdatajoin))
DMCdatajoin <- subset(DMCdatajoin, select = -c(T,lon))

####Temp data join####
tempdata <- as.data.frame(fread("MS Analytics/ANLY 699/Data project/Temp.tsv"))
tempdata$date <- as.Date(tempdata$T,"%Y-%m-%d")
tempdata$lonround <- round(tempdata$lon,2)

tempdatajoin <- merge(x=DMCdatajoin,y=tempdata,by.x=c("latround","lonround","DISCOVERY_DATEymd"),by.y=c("lat","lonround","date"),all.x=TRUE)
colSums(is.na(tempdatajoin))
tempdatajoin <- subset(tempdatajoin, select = -c(T,lon))

####RH data join####
RHdata <- as.data.frame(fread("MS Analytics/ANLY 699/Data project/RH.tsv"))
RHdata$date <- as.Date(RHdata$T,"%Y-%m-%d")
RHdata$lonround <- round(RHdata$lon,2)

RHdatajoin <- merge(x=tempdatajoin,y=RHdata,by.x=c("latround","lonround","DISCOVERY_DATEymd"),by.y=c("lat","lonround","date"),all.x=TRUE)
colSums(is.na(RHdatajoin))
RHdatajoin <- subset(RHdatajoin, select = -c(T,lon))

####Wind data join####
winddata <- as.data.frame(fread("MS Analytics/ANLY 699/Data project/wind.tsv"))
winddata$date <- as.Date(winddata$T,"%Y-%m-%d")
winddata$lonround <- round(winddata$lon,2)

winddatajoin <- merge(x=RHdatajoin,y=winddata,by.x=c("latround","lonround","DISCOVERY_DATEymd"),by.y=c("lat","lonround","date"),all.x=TRUE)
colSums(is.na(winddatajoin))
winddatajoin <- subset(winddatajoin, select = -c(T,lon))

####Precipitation data join####
precdata <- as.data.frame(fread("MS Analytics/ANLY 699/Data project/prec.tsv"))
precdata$date <- as.Date(precdata$T,"%Y-%m-%d")
precdata$lonround <- round(precdata$lon,2)

precdatajoin <- merge(x=winddatajoin,y=precdata,by.x=c("latround","lonround","DISCOVERY_DATEymd"),by.y=c("lat","lonround","date"),all.x=TRUE)
colSums(is.na(precdatajoin))
precdatajoin <- subset(precdatajoin, select = -c(T,lon))

####ISI data join####
ISIdata<-as.data.frame(fread("MS Analytics/ANLY 699/Data project/ISI.tsv"))
ISIdata$date <- as.Date(ISIdata$T,"%Y-%m-%d")
ISIdata$lonround <- round(ISIdata$lon,2)

ISIdatajoin <- merge(x=precdatajoin,y=ISIdata,by.x=c("latround","lonround","DISCOVERY_DATEymd"),by.y=c("lat","lonround","date"),all.x=TRUE)
colSums(is.na(ISIdatajoin))
ISIdatajoin <- subset(ISIdatajoin, select = -c(T,lon))

rm(DCdata, DCdatajoin,DMCdata,DMCdatajoin,FFMCdata,FFMCdatajoin,ISIdata,precdata,precdatajoin,RHdata,RHdatajoin,tempdata,tempdatajoin,winddata,winddatajoin)



####Dataset exploration####
finaldata <- ISIdatajoin
glimpse(finaldata)
finaldata <- subset(finaldata, select=-c(FIRE_YEAR))
summary(finaldata)
str(finaldata)
head(finaldata,20)
tail(finaldata,20)
View (finaldata)

finaldata %>% 
  group_by(STATE) %>%
  summarize(n_fires = n()) %>%
  View()

finaldata %>% 
  group_by(FIREMONTH) %>%
  summarize(n_fires = n()) %>%
  ggplot(aes(x = FIREMONTH, y = n_fires)) + 
  geom_bar(stat = 'identity', fill = 'darkred') +
  labs(x = '', y = 'Number of wildfires', title = 'US 2006 Wildfires by month')+
  theme(plot.title = element_text(hjust = 0.5))

finaldata %>% 
  group_by(FIREMONTH) %>%
  summarize(Firearea = sum(FIRE_SIZE)) %>%
  ggplot(aes(x = FIREMONTH, y = Firearea)) + 
  geom_bar(stat = 'identity', fill = 'darkred') +
  labs(x = '', y = 'Burn area', title = 'US 2006 Wildfires burn area by month')+
  theme(plot.title = element_text(hjust = 0.5))



finaldata$Size_bin <- cut(finaldata$FIRE_SIZE, breaks=c(-Inf,0.25, 9.9, 99.9,299,Inf), labels=c("0-0.25", "0.26-9.9", "10.0-99.9","100-299","300+"))
head(finaldata,20)
ggplot(data = finaldata) +
  geom_bar(mapping = aes(x = Size_bin),fill = 'red')+theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = 'Fire size (acres)', y = 'Number of fires', title = 'Number of Wildfires by area')

finaldata %>% 
  group_by(Size_bin) %>%
  summarize(n_fires = n()) %>%
  View()

finaldata$DOY <- yday(finaldata$DISCOVERY_DATEymd)

finaldata %>% 
  group_by(DOY) %>%
  summarize(n_fires = n()) %>%
  ggplot(aes(x = DOY, y = n_fires)) + 
  geom_line(color = 'red') +
  geom_smooth(method = 'loess', se = FALSE, linetype = 'dashed', size = 0.4, color = 'blue') + 
  labs(x = 'Day of year', y = 'Number of wildfires', title = 'US Wildfires by Day of Year')+theme(plot.title = element_text(hjust = 0.5))

# Add codes for DC and Puerto Rico to the default state lists
state.abb <- append(state.abb, c("DC", "PR"))
state.name <- append(state.name, c("District of Columbia", "Puerto Rico"))

# Map the state abbreviations to state names so we can join with the map data
finaldata$region <- map_chr(finaldata$STATE, function(x) { tolower(state.name[grep(x, state.abb)]) })

# Get the us state map data
state_map <- map_data('state')

finaldata %>% 
  select(region) %>%
  group_by(region) %>%
  summarize(n = n()) %>%
  right_join(state_map, by = 'region') %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) + 
  geom_polygon() + 
  geom_path(color = 'white') + 
  scale_fill_continuous(low = "orange", 
                        high = "darkred",
                        name = 'Number of fires') + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("US Wildfires, 2006") + 
  theme(plot.title = element_text(hjust = 0.5))

#Number of fires by cause
finaldata %>% 
  group_by(STAT_CAUSE_DESCR) %>%
  summarize(n_fires = n()) %>%
  ggplot(aes(x = reorder(STAT_CAUSE_DESCR,n_fires), y = n_fires)) + 
  geom_col( fill = 'darkred') +
  labs(x = 'Cause of wildfire', y = 'Number of wildfires', title = 'US Wildfires by cause') + 
  coord_flip()+ 
  theme(plot.title = element_text(hjust = 0.5))

#Box plot of Fire area by  month
finaldata$FIREMONTH <-month(ymd(finaldata$DISCOVERY_DATEymd), label = TRUE, abbr = TRUE)
ggplot(data = finaldata, aes(x=FIREMONTH,y=FIRE_SIZE)) + geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                                       outlier.size=4)+ 
  labs(x = "Month", y = "Fire burn area", title = "Box plot of Month and Fire area") +
  theme(plot.title = element_text(hjust = 0.5))

#Bar plot of number of fires by month
finaldata %>% 
  group_by(FIREMONTH) %>%
  summarize(n_fires = n()) %>%
  ggplot(aes(x = FIREMONTH, y = n_fires)) + 
  geom_col( fill = 'darkred') +
  labs(x = 'Month', y = 'Number of wildfires', title = 'US Wildfires by month') + 
  theme(plot.title = element_text(hjust = 0.5))

#Bar plot of burn area by month
finaldata %>% 
  group_by(FIREMONTH) %>%
  summarize(Total_Area= sum(FIRE_SIZE)) %>%
  ggplot(aes(x = FIREMONTH, y = Total_Area)) + 
  geom_col( fill = 'darkred') +
  labs(x = 'Month', y = 'Area burnt (in acres)', title = 'US Wildfires burn area by month') + 
  theme(plot.title = element_text(hjust = 0.5))

#Bar plot of burn are by DOW
finaldata$FIREDOW <-wday(ymd(finaldata$DISCOVERY_DATEymd), label = TRUE, abbr = FALSE,week_start=1)
finaldata %>% 
  group_by(FIREDOW) %>%
  summarize(Total_Area= sum(FIRE_SIZE)) %>%
  ggplot(aes(x = FIREDOW, y = Total_Area)) + 
  geom_col( fill = 'darkred') +
  labs(x = 'Day of week', y = 'Area burnt (in acres)', title = 'US Wildfires burn area by day of week') + 
  theme(plot.title = element_text(hjust = 0.5))

#Plotting correlation matrix
finaldata2 <- subset(finaldata, select=c(FIRE_SIZE,C_FFMC,C_DC,C_DMC,M_t,M_rh,M_wdSpd,C_prec,C_ISI))  
finaldata2 <- as.matrix(finaldata2)
corrplot(cor(finaldata2), method="number",type="upper")

glimpse(finaldata)
finaldata <- subset(finaldata, select = -c(region, STATE))
Pythondata <- subset(finaldata, select = -c(DISCOVERY_DATEymd, DOY,region))
glimpse(finaldata)
summary(finaldata)

finaldata2 <- subset(finaldata, select = -c(FIRE_SIZE,latround,lonround,STAT_CAUSE_DESCR,Size_bin,FIREMONTH,FIREDOW))

summary(finaldata2)

ggplot(stack(finaldata2), aes(x = factor(ind, levels = names(finaldata2)), y = values)) + geom_boxplot()

ggplot(data = finaldata, aes(x = "", y = C_DMC)) + geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                                      outlier.size=4)+ 
  labs(x = "Variable", y = "DC values", title = "Box plot of DMC values") +
  theme(plot.title = element_text(hjust = 0.5))

OutVals = boxplot(finaldata$C_DC, plot=FALSE)$out
summary(OutVals)
OutVals2 = boxplot(finaldata$C_DMC, plot=FALSE)$out
summary(OutVals2)

write.csv(Pythondata,"MS Analytics/ANLY 699/Data project/Datafinal.csv", row.names = FALSE)


###Final analysis####
ggplot(data = finaldata, aes(FIRE_SIZE)) + geom_histogram()+ 
  labs(x = "Fire burn area", y = "Count", title = "Histogram of fire burn area") +
  theme(plot.title = element_text(hjust = 0.5))

#There is significant skew in this dataset as there are many small fires.

finaldata$areatransform <- log(finaldata$FIRE_SIZE+1)
ggplot(data = finaldata, aes(areatransform)) + geom_histogram(aes(y = ..density..),colour = "black", fill = "White")+ 
  labs(x = "Fire burn area", y = "Count", title = "Histogram of fire burn area") +
  theme(plot.title = element_text(hjust = 0.5))+
  stat_function(fun = dnorm, args = list(mean = mean(finaldata$areatransform,na.rm=TRUE), sd = sd(finaldata$areatransform,na.rm=TRUE)),col = 'red')

skewness(finaldata$areatransform)
kurtosis(finaldata$areatransform)

#Even with the log transform, there is some skew and kurtosis issues. 




plot(density(finaldata$FIRE_SIZE))
skewness(finaldata$FIRE_SIZE)
skewness(finaldata$areatransform)
plot(density(finaldata$areatransform))

anscombe.test(finaldata$areatransform)

model <- lm(areatransform ~ C_FFMC+C_DMC+C_DC+C_ISI+M_t+M_rh+M_wdSpd+C_prec
            , data = finaldata)
summary(model)
write.csv(finaldata,"MS Analytics/ANLY 699/Data project/Datafinal.csv", row.names = FALSE)