car_power_consumption = function(region = 0,HDD = 0, CDD = 0) {
  
  EV_data_matrix = data.frame(model.matrix(consumption ~ HDD + CDD + weather_region + model,EV_data)[,-1])
  eff_lm_model = lm(EV_data$consumption ~ ., data = EV_data_matrix, na.action=na.omit, weights = EV_data$distance)
  
  new_EV_data_matrix = EV_data_matrix[1,][-1,]
  new_EV_data_matrix[1,] = 0
  new_EV_data_matrix[1, colnames(new_EV_data_matrix)[grepl("weather_region",colnames(new_EV_data_matrix))][region]] = 1
  new_EV_data_matrix[1, colnames(new_EV_data_matrix)[grepl("model",colnames(new_EV_data_matrix))]] = model_pop$freq[-1]
  new_EV_data_matrix$HDD[1] = HDD
  new_EV_data_matrix$CDD[1] = CDD
  
  return(predict(eff_lm_model, new_EV_data_matrix, interval = "confidence"))
  
}

calc_region_usage = function(vkt, weather_region, HDD = 0, CDD = 0) {
  regionIndex = which(levels(EV_data$weather_region) == weather_region)-1
  car_consumption = car_power_consumption(regionIndex, HDD, CDD)
  return(car_consumption*vkt)
}


comb_vkt_consumption = function(vkt_region, HDD = 0, CDD = 0) {
  vkt = vkt_yearly[vkt_yearly$year == 2019,vkt_region]
  weather_region = vkt_regions[vkt_region]
  
  return(calc_region_usage(vkt, weather_region, HDD, CDD))
}

comb_vkt_consumption_weather = function(vkt_region) {
  weather_region = vkt_regions[vkt_region]
  vkt_monthly = data.frame()
  for (i in 1:12){
    HDD = mean(HDD_data$HDD[HDD_data$Month == i & HDD_data$City == weather_region])
    CDD = mean(CDD_data$CDD[CDD_data$Month == i & CDD_data$City == weather_region])
    cat(" ",i, " ", comb_vkt_consumption(vkt_region, HDD, CDD))
    rbind(vkt_monthly, comb_vkt_consumption(vkt_region, HDD, CDD))
  }
  return(vkt_monthly)
}