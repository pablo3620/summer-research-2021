plot_decomp = function(decomp, title = "Decomposition of Time Series", xlab = "Year", ylab = "", breaks = 6) {
  Time = attributes(decomp$x)$tsp
  Time = seq(Time[1],Time[2], length.out=round((Time[2]-Time[1])*Time[3]+1))
  
  # Convert td to data frame
  dat = cbind(Time, with(decomp, data.frame(Observed=x, Trend=trend, Seasonal=seasonal, Random=random)))
  
  plot = ggplot(gather(dat, component, value, -Time), aes(Time, value)) +
    facet_grid(component ~ ., scales="free_y") +
    scale_x_continuous(n.breaks = breaks) +
    geom_line() +
    labs(y=ylab, x=xlab) +
#    ggtitle(title)+
    theme(plot.title=element_text(hjust=0.5)) +
    gg_theme
    
  return(plot)
}