rm(list = ls())
setwd("/Users/jimstankovich/Documents/data_analysis_exc_biostats/Thailand balance of payments")
library(tidyverse)

surplus_colour <- "grey"
deficit_colour <- "pink"

d <- read_csv("current_account_balance_20231013.csv")

n = 50

s <- d[n:1,] %>%
  mutate(colour = ifelse(CAB_13Oct > 0, surplus_colour, deficit_colour))
colnames(s)[1] = "quarter"
yearly_summary <- s %>%
  mutate(year = substr(quarter,4,7)) %>%
  group_by(year) %>%
  summarise(balance = sum(CAB_13Oct) %>% round() %>%
              formatC(format = "d", big.mark=",")) %>%
  mutate(colour = ifelse(balance > 0, "black", "red"))

minCAB = min(s$CAB_13Oct)
maxCAB = max(s$CAB_13Oct)
range = maxCAB - minCAB

pdf("current_account_plot.pdf", width = 9, height = 5.5)
plot(0,0,xlim = c(0,n), ylim = c(minCAB, maxCAB*1.2), col = "white",
     xaxt = "n", xlab = "", ylab = "Thailand current account balance (millions of $US)",
     frame.plot = F)
abline(h = 0)
abline(v = 0:12 * 4, col = "grey", lwd = 0.5)
for(i in 1:n) {
  rect(i-1, 0, i, s$CAB_13Oct[i], col = s$colour[i])
}
text(0:12*4 + 2, rep(maxCAB*1.2,13), 2011:2023, cex = 0.8)
text(0:12*4 + 2, rep(maxCAB*1.1,13), yearly_summary$balance, 
     col = yearly_summary$colour, cex = 0.7)
text(12, minCAB + range * 0.04, "Source: https://app.bot.or.th/BTWS_STAT/statistics/BOTWEBSTAT.aspx?reportID=646&language=eng", cex = 0.5, pos = 4)
text(12, minCAB, "Data downloaded 13 Oct 2023", cex = 0.5, pos = 4)
dev.off()
