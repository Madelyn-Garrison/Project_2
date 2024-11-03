library(tidyverse)

my_sample<-read_csv("Seoul_Bike_Data.csv")


my_sample<-my_sample|>
  mutate(month=str_split_i(Date,'/',2),
         month=as.numeric(month))




my_sample |>
  group_by(month) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))

xxx<-my_sample |>
  group_by(Hour) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))
print(xxx,n=24)

mean_season<-my_sample |>
  group_by(Seasons) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))

# Barplot
ggplot(mean_season, aes(x=Seasons, y=mean_bkcount)) + 
  geom_bar(stat = "identity")

ggplot(xxx, aes(x=Hour, y=mean_bkcount)) + 
  geom_bar(stat = "identity")

mean_season_hour<-my_sample |>
  group_by(Seasons, Hour) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))

ggplot(mean_season_hour, aes(x=Hour, y=mean_bkcount)) + 
  geom_bar(stat = "identity")+
  facet_wrap(~Seasons)




my_sample |>
  group_by(`Functioning Day`) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`))

##Check consistency
holla<-my_sample |>
  mutate(holiday=ifelse(Holiday=='Holiday',1,0))|>
  group_by(Date) |>
  summarize(mean_bkcount=mean(`Rented Bike Count`), mean_hol<-mean(holiday))


table(my_sample$`Functioning Day`)
table(my_sample$Holiday)
table(my_sample$`Functioning Day`, my_sample$Holiday)


ggplot(my_sample, aes(x=`Rainfall(mm)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity", aes(color=as.factor(month)))

ggplot(my_sample, aes(x=`Snowfall (cm)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity")+
  facet_wrap(~Seasons)

ggplot(my_sample, aes(x=`Temperature(°C)`, y=`Rented Bike Count`)) + 
  geom_point(stat = "identity", aes(color=`Rainfall(mm)`))+
  facet_wrap(~Seasons)


try<-mtcars
data <- as.matrix(mtcars)

# Default Heatmap
heatmap()


just_numeric<-my_sample|>
  group_by(month)|>
  select(-Seasons, -Holiday, -`Functioning Day`, -Date, -`Rented Bike Count`, -Hour)|>
  summarize(across(where(is.numeric), list(mean=mean),
                .names =  "{.col}_{.fn}"))
  


for_hm<-as.matrix(just_numeric[,-1])
heatmap(for_hm)

install.packages("ggpointdensity")

library(ggpointdensity)

ggplot(data = my_sample, mapping = aes(x = `Temperature(°C)`, y = `Rented Bike Count`)) +
  geom_pointdensity()





