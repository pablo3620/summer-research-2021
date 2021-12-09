avg_temp = function(data) {
  avg_temp = data %>% 
    group_by(Year, Month, City) %>% 
    summarise(avg_temp = mean(Temp))
  return(avg_temp)
}