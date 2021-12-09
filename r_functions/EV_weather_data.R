source("r_functions/HDD.R")
source("r_functions/CDD.R")
source("r_functions/avg_temp.R")



#list index (first) is the location weather to use and value (second) is the region in the data 
weather_regions = list("Auckland" = "Auckland", "Upper Hutt" = "Wellington","Christchurch" = "Christchurch", "Dunedin" = "Coastal Otago", "Hamilton" = "Waikato", "Rotorua" = "Bay of Plenty","Christchurch" = "Mid Canterbury", "Christchurch" = "North Canterbury", "Christchurch" = "South Canterbury", "Clyde" = "Central Otago", "Nelson" = "Nelson", "Auckland" = "Whangarei", "Auckland" = "Far North", "Upper Hutt" = "Wairarapa", "Auckland" = "Rodney", "Dunedin" = "Waitaki", "Nelson" = "Golden Bay",  "Auckland" = "Coromandel", "Palmerston North" = "Manawatu", "Nelson" = "Marlborough", "Stratford" = "Taranaki", "Napier" = "Hawkes Bay", "Invercargill" = "Southland")
month_length = c(31,28,31,30,31,30,31,31,30,31,30,31)


#main EV data
EV_data = read_csv("../Ftf Efficiency Dataset/ftf_ev_efficiency_distance_models_20211006_v1.0.csv")[-1]
EV_data$weather_region = as.factor(names(weather_regions)[match(EV_data$region,  weather_regions)])

#sort factor by popularity
region_pop = EV_data %>%
  group_by(weather_region) %>% 
  summarise(count =  n_distinct(vehicle)) %>% 
  arrange(-count)
model_pop = EV_data %>%
  group_by(model) %>% 
  summarise(count =  n_distinct(vehicle)) %>% 
  arrange(-count)
EV_data$weather_region = factor(EV_data$weather_region, levels = region_pop$weather_region)
EV_data$model = factor(EV_data$model, levels = model_pop$model)
rm(region_pop)
rm(model_pop)

#remove PHEV
EV_data = EV_data[EV_data$model != "Mitsubishi Outlander" & EV_data$model != "Toyota Prius" & EV_data$model != "Mini Countryman PHEV", ]


#weather data
auckland_weather = read.csv('weather_data/auckland_motat_ews_data.csv',na.strings = "-",stringsAsFactors = T)
auckland_weather$Month = match(auckland_weather$Month, month.abb)
#Wellington does not have NIWA stations so closest is upperhutt
upperhutt_weather = read.csv('weather_data/upperhutt_trentham_ews_data.csv',na.strings = "-",stringsAsFactors = T)
upperhutt_weather$Month = match(upperhutt_weather$Month, month.abb)
christchurch_weather = read.csv('weather_data/christchurch_kyle_st_ews_data.csv',na.strings = "-", stringsAsFactors = T)
christchurch_weather$Month = match(christchurch_weather$Month, month.abb)
#use Dunedin weather for coastal Otago
dunedin_weather = read.csv('weather_data/dunedin_musselburgh_ews_data.csv',na.strings = "-",stringsAsFactors = T)
dunedin_weather$Month = match(dunedin_weather$Month, month.abb)
#use hamilton weather for Waikato
hamilton_weather = read.csv('weather_data/hamilton_ruakura_ews_data.csv', na.strings = "-", stringsAsFactors = T)
hamilton_weather$Month = match(hamilton_weather$Month, month.abb)
#use rotorua for bay of plenty
rotorua_weather = read.csv('weather_data/rotorua_ews_data.csv', na.strings = "-", stringsAsFactors = T)
rotorua_weather$Month = match(rotorua_weather$Month, month.abb)
#use clyde for central Otago
clyde_weather = read.csv('weather_data/clyde_ews_data.csv', na.strings = "-", stringsAsFactors = T)
clyde_weather$Month = match(clyde_weather$Month, month.abb)
nelson_weather = read.csv('weather_data/nelson_ews_data.csv', na.strings = "-", stringsAsFactors = T)
nelson_weather$Month = match(nelson_weather$Month, month.abb)
#use palmerston north for Manawatu
palmerston_weather = read.csv('weather_data/palmerston_north_ews_data.csv', na.strings = "-", stringsAsFactors = T)
palmerston_weather$Month = match(palmerston_weather$Month, month.abb)
#use stratford for Taranaki
stratford_weather = read.csv('weather_data/stratford_ews_data.csv', na.strings = "-", stringsAsFactors = T)
stratford_weather$Month = match(stratford_weather$Month, month.abb)
#use napier for Hawkes Bay
napier_weather = read.csv('weather_data/napier_ews_data.csv', na.strings = "-", stringsAsFactors = T)
napier_weather$Month = match(napier_weather$Month, month.abb)
#use invercargill for southland
invercargill_weather = read.csv('weather_data/invercargill_ews_data.csv', na.strings = "-", stringsAsFactors = T)
invercargill_weather$Month = match(invercargill_weather$Month, month.abb)

#temp calculations
HDD_data = Reduce(rbind, list(HDD(auckland_weather), HDD(upperhutt_weather), HDD(christchurch_weather),
                              HDD(dunedin_weather), HDD(hamilton_weather), HDD(rotorua_weather), HDD(clyde_weather),
                              HDD(nelson_weather), HDD(palmerston_weather), HDD(stratford_weather), HDD(napier_weather),
                              HDD(invercargill_weather)))
CDD_data = Reduce(rbind, list(CDD(auckland_weather), CDD(upperhutt_weather), CDD(christchurch_weather),
                              CDD(dunedin_weather), CDD(hamilton_weather), CDD(rotorua_weather), CDD(clyde_weather),
                              CDD(nelson_weather), CDD(palmerston_weather), CDD(stratford_weather), CDD(napier_weather),
                              CDD(invercargill_weather)))
avg_temp_data = Reduce(rbind, list(avg_temp(auckland_weather), avg_temp(upperhutt_weather), avg_temp(christchurch_weather),
                                   avg_temp(dunedin_weather), avg_temp(hamilton_weather), avg_temp(rotorua_weather), avg_temp(clyde_weather),
                                   avg_temp(nelson_weather), avg_temp(palmerston_weather), avg_temp(stratford_weather), avg_temp(napier_weather),
                                   avg_temp(invercargill_weather)))

HDD_data$HDD = HDD_data$HDD/month_length
CDD_data$CDD = CDD_data$CDD/month_length

EV_data = merge(EV_data, HDD_data, by.x = c("year", "month", "weather_region"), by.y = c("Year", "Month", "City"), all.x = T)
EV_data = merge(EV_data, CDD_data, by.x = c("year", "month", "weather_region"), by.y = c("Year", "Month", "City"), all.x = T)
EV_data = merge(EV_data, avg_temp_data, by.x = c("year", "month", "weather_region"), by.y = c("Year", "Month", "City"), all.x = T)

save(EV_data, weather_regions, file = "processed_data/EV_weather_data.rds")

rm(list=ls())