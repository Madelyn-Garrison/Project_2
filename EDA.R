library(tidyverse)
library(ggpointdensity)
library(ggupset)
library(ggcorrplot)

# Read in data
my_sample_initial<-read_csv("Seoul_Bike_Data.csv")

# Add a new variables for month and day, converted from date varaible
my_sample_initial<-my_sample_initial|>
  mutate(month=str_split_i(Date,'/',2),
         month=as.numeric(month),day=str_split_i(Date,'/',1),
         day=as.numeric(day))

# Does the data include all hours for all dates in a calender year?
length(unique(my_sample_initial$Date))
# All dates included
# Check all hours included
(0*365)+(1*365)+(2*365)+(3*365)+(4*365)+(5*365)+(6*365)+(7*365)+(8*365)+(9*365)+(10*365)+(11*365)+(12*365)+
  (13*365)+(14*365)+(15*365)+(16*365)+(17*365)+(18*365)+(19*365)+(20*365)+(21*365)+(22*365)+(23*365)
# Compare to actual
sum(my_sample_initial$Hour)
# All hours included for all dates

# The data set includes dates when the bike sharing was not open
non_func_day<-table(my_sample_initial$`Functioning Day`)
non_func_day
#Are holidays and non-functioning days the same
holiday_non_func_day<-table(my_sample_initial$`Functioning Day`, my_sample_initial$Holiday)
holiday_non_func_day

# I will subset the data to exclude non-functioning dates, as the count for rented bikes is not meaningful
my_sample<-my_sample_initial|>
  filter(`Functioning Day`== 'Yes')

# Histograms and summary statistics for all numeric variables
ggplot(my_sample, aes(x=`Rented Bike Count`)) + 
  geom_histogram()+
  labs(y="Freq")
my_sample|>
  summarize(mean=mean(`Rented Bike Count`), median=median(`Rented Bike Count`), sd=sd(`Rented Bike Count`))

ggplot(my_sample, aes(x=`Temperature(°C)`)) + 
  geom_histogram()+
  labs(y="Freq")
my_sample|>
  summarize(mean=mean(`Temperature(°C)`), median=median(`Temperature(°C)`), sd=sd(`Temperature(°C)`))

ggplot(my_sample, aes(x=`Humidity(%)`)) + 
  geom_histogram()+
  labs(y="Freq")
my_sample|>
  summarize(mean=mean(`Humidity(%)`), median=median(`Humidity(%)`), sd=sd(`Humidity(%)`))

ggplot(my_sample, aes(x=`Wind speed (m/s)`)) + 
  geom_histogram()+
  labs(y="Freq")
my_sample|>
  summarize(mean=mean(`Wind speed (m/s)`), median=median(`Wind speed (m/s)`), sd=sd(`Wind speed (m/s)`))

ggplot(my_sample, aes(x=`Visibility (10m)`)) + 
  geom_histogram()+
  labs(y="Freq")
my_sample|>
  summarize(mean=mean(`Visibility (10m)`), median=median(`Visibility (10m)`), sd=sd(`Visibility (10m)`))

ggplot(my_sample, aes(x=`Dew point temperature(°C)`)) + 
  geom_histogram()+
  labs(y="Freq")
my_sample|>
  summarize(mean=mean(`Dew point temperature(°C)`), median=median(`Dew point temperature(°C)`), 
            sd=sd(`Dew point temperature(°C)`))

ggplot(my_sample, aes(x=`Solar Radiation (MJ/m2)`)) + 
  geom_histogram()+
  labs(y="Freq")
my_sample|>
  summarize(mean=mean(`Solar Radiation (MJ/m2)`), median=median(`Solar Radiation (MJ/m2)`), 
            sd=sd(`Solar Radiation (MJ/m2)`))

ggplot(my_sample, aes(x=`Rainfall(mm)`)) + 
  geom_histogram()+
  labs(y="Freq")
my_sample|>
  summarize(mean=mean(`Rainfall(mm)`), median=median(`Rainfall(mm)`), sd=sd(`Rainfall(mm)`))

ggplot(my_sample, aes(x=`Snowfall (cm)`)) + 
  geom_histogram()+
  labs(y="Freq")
my_sample|>
  summarize(mean=mean(`Snowfall (cm)`), median=median(`Snowfall (cm)`), sd=sd(`Snowfall (cm)`))

# Correlations between numeric variables
numeric_my_sample<-Filter(is.numeric, my_sample)|>
  select(-day, -month)
corr <- cor(numeric_my_sample)
ggcorrplot(corr,
           type = "lower")

# Number of bikes rented vs other numeric variables
mean_bikes_temp<-my_sample|>
  group_by(`Temperature(°C)`)|>
  summarize(mean=mean(`Rented Bike Count`))
ggplot(mean_bikes_temp, aes(x=`Temperature(°C)`, y=mean)) + 
  geom_point(stat = "identity")
ggplot(my_sample, aes(x=`Temperature(°C)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity", aes(color=Seasons))

mean_bikes_humidity<-my_sample|>
  group_by(`Humidity(%)`)|>
  summarize(mean=mean(`Rented Bike Count`))
ggplot(mean_bikes_humidity, aes(x=`Humidity(%)`, y=mean)) + 
  geom_point(stat = "identity")
ggplot(my_sample, aes(x=`Humidity(%)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity", aes(color=Seasons))

mean_bikes_wind<-my_sample|>
  group_by(`Wind speed (m/s)`)|>
  summarize(mean=mean(`Rented Bike Count`))
ggplot(mean_bikes_wind, aes(x=`Wind speed (m/s)`, y=mean)) + 
  geom_point(stat = "identity")
ggplot(my_sample, aes(x=`Wind speed (m/s)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity", aes(color=Seasons))

mean_bikes_vis<-my_sample|>
  group_by(`Visibility (10m)`)|>
  summarize(mean=mean(`Rented Bike Count`))
ggplot(mean_bikes_vis, aes(x=`Visibility (10m)`, y=mean)) + 
  geom_point(stat = "identity")
ggplot(my_sample, aes(x=`Visibility (10m)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity", aes(color=Seasons))

mean_bikes_dew<-my_sample|>
  group_by(`Dew point temperature(°C)`)|>
  summarize(mean=mean(`Rented Bike Count`))
ggplot(mean_bikes_dew, aes(x=`Dew point temperature(°C)`, y=mean)) + 
  geom_point(stat = "identity")
ggplot(my_sample, aes(x=`Dew point temperature(°C)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity", aes(color=Seasons))

mean_bikes_sol<-my_sample|>
  group_by(`Solar Radiation (MJ/m2)`)|>
  summarize(mean=mean(`Rented Bike Count`))
ggplot(mean_bikes_sol, aes(x=`Solar Radiation (MJ/m2)`, y=mean)) + 
  geom_point(stat = "identity")
ggplot(my_sample, aes(x=`Solar Radiation (MJ/m2)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity", aes(color=Seasons))

mean_bikes_rain<-my_sample|>
  group_by(`Rainfall(mm)`)|>
  summarize(mean=mean(`Rented Bike Count`))
ggplot(mean_bikes_rain, aes(x=`Rainfall(mm)`, y=mean)) + 
  geom_point(stat = "identity")
ggplot(my_sample, aes(x=`Rainfall(mm)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity", aes(color=Seasons))

mean_bikes_snow<-my_sample|>
  group_by(`Snowfall (cm)`)|>
  summarize(mean=mean(`Rented Bike Count`))
ggplot(mean_bikes_snow, aes(x=`Snowfall (cm)`, y=mean)) + 
  geom_point(stat = "identity")
ggplot(my_sample, aes(x=`Snowfall (cm)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity", aes(color=Seasons))

# Average bikes rented each hour
mean_bikes_hour<-my_sample |>
  group_by(Hour) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))
print(mean_bikes_hour, n=24)
ggplot(mean_bikes_hour, aes(x=Hour, y=mean_bkcount)) + 
  geom_bar(stat = "identity")

# Average bikes rented per month
mean_bikes_month<-my_sample |>
  group_by(month) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))
print(mean_bikes_month, n=12)
ggplot(mean_bikes_month, aes(x=month, y=mean_bkcount)) + 
  geom_bar(stat = "identity")

# Average bikes rented per season
mean_bikes_season<-my_sample |>
  group_by(Seasons) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))
print(mean_bikes_season, n=4)
ggplot(mean_bikes_season, aes(x=Seasons, y=mean_bkcount)) + 
  geom_bar(stat = "identity")

# How do holidays affect bike rentals?
holiday_count<-table(my_sample$Holiday)
holiday_count

holiday_count_season<-table(my_sample$Holiday, my_sample$Seasons)
holiday_count_season

mean_bikes_holiday_season<-my_sample |>
  group_by(Holiday, Seasons) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))
print(mean_bikes_holiday_season, n=8)
ggplot(mean_bikes_holiday_season, aes(x=Holiday, y=mean_bkcount)) + 
  geom_bar(stat = "identity")

ggplot(mean_bikes_holiday_season, aes(x=Holiday, y=mean_bkcount)) + 
  geom_bar(stat = "identity")+
  facet_wrap(~Seasons)

# Hour trends by season or month
mean_bikes_hour_season<-my_sample |>
  group_by(Hour, Seasons) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))
ggplot(mean_bikes_hour_season, aes(x=Hour, y=mean_bkcount)) + 
  geom_bar(stat = "identity")+
  facet_wrap(~Seasons)

mean_bikes_hour_month<-my_sample |>
  group_by(Hour, month) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))
ggplot(mean_bikes_hour_month, aes(x=Hour, y=mean_bkcount)) + 
  geom_bar(stat = "identity")+
  facet_wrap(~month)

# Point density plot for numeric variables with clear trends
ggplot(data = my_sample, mapping = aes(x = `Temperature(°C)`, y = `Rented Bike Count`)) +
  geom_pointdensity()
ggplot(data = my_sample, mapping = aes(x = `Humidity(%)`, y = `Rented Bike Count`)) +
  geom_pointdensity()
ggplot(data = my_sample, mapping = aes(x = `Wind speed (m/s)`, y = `Rented Bike Count`)) +
  geom_pointdensity()
ggplot(data = my_sample, mapping = aes(x = `Dew point temperature(°C)`, y = `Rented Bike Count`)) +
  geom_pointdensity()