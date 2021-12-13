yearly_line = function(period = 12, count = 10) {
  for (i in 0:count) {
    abline(v = i*period+1, lty = 3)
  }
}