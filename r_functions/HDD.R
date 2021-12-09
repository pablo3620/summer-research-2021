HDD = function (data, temp = 16) {
  mon_sum = data.frame(Year = NA, Month = NA, HDD = NA)[-1,]
  for (y in min(data$Year):max(data$Year)){
    for (m in min(data[data$Year == y,]$Month):max(data[data$Year == y,]$Month)) {
      #print(paste(y," ", m))
      adj_temps = -data[data$Year == y & data$Month == m,]$Temp+temp
      #print(sum(adj_temps*(adj_temps > 0))/24)
      mon_sum[nrow(mon_sum) + 1,] = c(y,m, sum(adj_temps*(adj_temps > 0))/24)
    }
  }
  mon_sum$City = data$City[1]
  return(mon_sum)
}
