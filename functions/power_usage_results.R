source("functions/power_usage_model.R")
load("processed_data/EV_weather_data.rda")
load("processed_data/VKT_data.rda")



power_usage = data.frame(a = 1:12)
rownames(power_usage) = month.abb[1:12]
for (vkt_reg in names(vkt_regions)) {
  current = comb_vkt_consumption_weather(vkt_reg)
  colnames(current) = paste(vkt_reg, colnames(current), sep = "_")
  power_usage = cbind(power_usage, current)
}
power_usage = power_usage[,-1]

power_usage$total_fit = rowSums(power_usage[,seq(1, length(colnames(power_usage)), by = 3)])

power_usage_prop = data.frame(a = 1:12)
rownames(power_usage) = month.abb[1:12]

count = 2
for (col in seq(1, length(colnames(power_usage)), by = 3)) {
  power_usage_prop[,count] = power_usage[,col]/mean(power_usage[,col])-1
  names(power_usage_prop)[count] = c(names(vkt_regions), "Total")[count-1]
  count = count + 1
}
power_usage_prop = power_usage_prop[,-1]

#calculating best and worst case for different cars
limit_power_usage = data.frame(a = 1:12)
rownames(limit_power_usage) = month.abb[1:12]

best_car = "Hyundai Ioniq (EV)"
worst_car = "Nissan Leaf (62 kWh)"
indexs = match(c(best_car, worst_car),model_pop$model)
for (vkt_reg in names(vkt_regions)) {

  current_best = comb_vkt_consumption_weather(vkt_reg, indexs[1]-1)
  current_worst = comb_vkt_consumption_weather(vkt_reg, indexs[2]-1)

  
  colnames(current_best) = paste(vkt_reg, best_car, sep = "_")
  colnames(current_worst) = paste(vkt_reg, worst_car, sep = "_")
  limit_power_usage = cbind(limit_power_usage, current_best, current_worst)
}
limit_power_usage = limit_power_usage[,-1]

limit_power_usage = limit_power_usage[,seq(1, length(colnames(limit_power_usage)), by = 3)]

limit_power_usage$total_best = rowSums(limit_power_usage[,seq(1, length(colnames(limit_power_usage)), by = 2)])

limit_power_usage$total_worst = rowSums(limit_power_usage[,seq(2, length(colnames(limit_power_usage)), by = 2)])






save(power_usage, power_usage_prop, limit_power_usage, file = "processed_data/power_usage.rda")

rm(list=ls())