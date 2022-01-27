library(dplyr)
rm(list=ls())


vkt_quart = read.csv("downloaded_stats/VKT_main_quart.csv")
vkt_yearly = read.csv("downloaded_stats/VKT_main_yearly.csv")
vkt_other_yearly = read.csv("downloaded_stats/VKT_other_yearly.csv")
vkt_type_yearly = read.csv("downloaded_stats/VKT_fuel_vehicle_type_yearly.csv")

#combining the main regions of VKT with the other regions
vkt_yearly = cbind(vkt_yearly, vkt_other_yearly[,-1])
vkt_yearly = subset(vkt_yearly, select = -c(Other.Regions))
names(vkt_yearly)[1] = "year"

#list index (first) is the VKT region to use and value (second) is the weather data region to use
vkt_regions = list("Northland" = "Auckland", "Auckland" = "Auckland", "Gisborne" = "Napier", "Hawes.Bay" = "Napier", "Taranaki" = "Stratford", "Manawatu.Wanganui" = "Palmerston North", "Wellington" = "Upper Hutt", "Nelson.Marlborough" = "Nelson", "Canterbury" = "Christchurch", "Southland" = "Invercargill", "Waikato" = "Hamilton", "Bay.of.Plenty" = "Rotorua", "West.Coast" = "Clyde", "Otago" = "Dunedin")


save(vkt_quart, vkt_yearly, vkt_regions, file = "processed_data/VKT_data.rda")