rm(list = ls())
library(tidyverse)
library(rgdal)
library(readxl)

setwd("~/Documents/GitHub/translation")

source = "http://statbbi.nso.go.th/staticreport/Page/sector/EN/report/sector_01_11101_EN_.xlsx"
bg_colour = rgb(245/255,239/255,224/255)

d = read_excel("data/population_by_province_and_age.xlsx", skip = 3)
table(d$Province)
table(is.na(d$Province))
d2 = d[!(is.na(d$Province)),]
d3 = d2[is.na(d2$Region) | d2$Region == "Bangkok",]
sum(d3$Total)
d3[1,4]
sum(d3[1,5:110])
colours = c(hsv(1/12,0.5,0.8),hsv(7/12,0.5,0.8),"grey70")
pdf("images/pyramid.pdf", width = 8, height = 9)
par(bg = bg_colour)
par(las = 1)
par(mar = c(5,4,4,6) + 0.1)
plot(0,0,xlim = c(-553000,553000),ylim = c(0,101),col = "white",
     xlab = "Number of people", ylab = "Age",
     xaxt = "n", yaxt = "n", 
     frame.plot = T,
     main = "Thailand population pyramid, 2019")
for(i in 1:102) {
  rect(0, i-1, -d[3,i+4], i, col = colours[1], border = colours[1])
  rect(0, i-1,  d[2,i+4], i, col = colours[2], border = colours[2])
}
abline(h = 0:10 * 10, lwd = 0.5, col = colours[3])
abline(v = -6:6 * 100000, lwd = 0.5, col = colours[3])
age.labels = 0:10 * 10
pops.at = c(-4,-2,0,2,4)*100000
pops.labels = c("400,000","200,000","0","200,000","400,000")
axis(side = 2, at = age.labels, labels = age.labels)
axis(side = 4, at = age.labels, labels = 2019 - age.labels)
axis(side = 1, at = pops.at, labels = pops.labels)
#axis(side = 3, at = pops.at, labels = pops.labels)
par(las = 0)
mtext("Year of birth", side = 4, line = 4)
text(-200000,55,"Female")
text(200000,55,"Male")
text(0,-2,paste("Data from",source),cex = 0.6)
dev.off()

d3$Province2 = sub(" Province","",d3$Province)
length(d3$Province2)
d3$Province2[d3$Province2 == "Khon Kaen "] = "Khon Kaen"
d3$Province2[d3$Province2 == "Satun "] = "Satun"
d3$Province2[d3$Province2 == "Phattalung"] = "Phatthalung"

colnames(d3)[5:106] = 0:101
pop = d3 %>% 
  pivot_longer(
    cols = 5:110,
    names_to = "age_cat",
    values_to = "n") %>%
  select(Province2, age_cat, n) %>%
  rename(province = Province2) %>%
  mutate(age = as.numeric(age_cat))

tab = pop %>%
  group_by(province) %>%
  summarise(total.pop = sum(n))

pop.lt.20 = pop %>%
  filter(!is.na(age)) %>%
  filter(age < 20) %>%
  group_by(province) %>%
  summarise(pop.lt.20 = sum(n))
tab = left_join(tab, pop.lt.20, by = "province")

pop.20.64 = pop %>%
  filter(!is.na(age)) %>%
  filter(age >= 20 & age < 65) %>%
  group_by(province) %>%
  summarise(pop.20.64 = sum(n))
tab = left_join(tab, pop.20.64, by = "province")

pop.65.plus = pop %>%
  filter(!is.na(age)) %>%
  filter(age >= 65) %>%
  group_by(province) %>%
  summarise(pop.65.plus = sum(n))
tab = left_join(tab, pop.65.plus, by = "province")

pop.no.age = pop %>%
  filter(is.na(age)) %>%
  group_by(province) %>%
  summarise(n.no.age = sum(n))
tab = left_join(tab, pop.no.age, by = "province")

tab$n.with.age = tab$total.pop - tab$n.no.age
tab$prop.lt.20 = tab$pop.lt.20 / tab$n.with.age
tab$prop.20.64 = tab$pop.20.64 / tab$n.with.age
tab$prop.65.plus = tab$pop.65.plus / tab$n.with.age

prov.region = d2[-1,1:2]
prov.region$Region[3:27] = "Central"
prov.region$Region[29:45] = "North"
prov.region$Region[47:66] = "Northeast"
prov.region$Region[68:81] = "South"
prov.region = prov.region[prov.region$Province != prov.region$Region |
                            prov.region$Province == "Bangkok",]
prov.region$Province = sub(" Province","",prov.region$Province)
prov.region$Province[prov.region$Province == "Khon Kaen "] = "Khon Kaen"
prov.region$Province[prov.region$Province == "Satun "] = "Satun"
prov.region$Province[prov.region$Province == "Phattalung"] = "Phatthalung"
prov.region = prov.region %>% rename(province = Province)

tab = left_join(tab, prov.region, by = "province")
tab$region.colour = rgb(0.5,0.5,0.5,0.5)
tab$region.colour[tab$Region == "Central"] = hsv(1/12,1,0.8,0.5)
tab$region.colour[tab$Region == "North"] = hsv(3/12,1,0.8,0.5)
tab$region.colour[tab$Region == "Northeast"] = hsv(11/12,1,0.8,0.5)
tab$region.colour[tab$Region == "South"] = hsv(7/12,1,0.8,0.5)

pdf("images/age.by.province.pdf", width = 6, height = 11)
par(bg = bg_colour)
plot(tab$prop.65.plus*100, tab$prop.lt.20*100, pch = 16, col = "white",
     ylab = "% of the population age < 20",
     xlab = "% of the population age 65+",
     asp = 1, main = "Variation in age distributions by province",
     xaxt = "n")
axis(side = 1, at = c(10,15))
abline(v = c(10,15), col = colours[3], lwd = 0.5)
abline(h = c(20,25,30,35), col = colours[3], lwd = 0.5)
abline(a = 20, b = -1, col = colours[3], lwd = 0.5)
abline(a = 25, b = -1, col = colours[3], lwd = 0.5)
abline(a = 30, b = -1, col = colours[3], lwd = 0.5)
abline(a = 35, b = -1, col = colours[3], lwd = 0.5)
abline(a = 40, b = -1, col = colours[3], lwd = 0.5)
abline(a = 45, b = -1, col = colours[3], lwd = 0.5)
abline(a = 50, b = -1, col = colours[3], lwd = 0.5)
text(tab$prop.65.plus*100, tab$prop.lt.20*100, tab$province, cex = 0.4)
text(7.2,16.2,paste("Data from",source),cex = 0.6,pos=4)
text(10,20,"70% of the population age 20-64", srt = -45, pos = 3, cex = 0.7)
text(12,28,"60% of the population age 20-64", srt = -45, pos = 3, cex = 0.7)
points(tab$prop.65.plus*100, tab$prop.lt.20*100, pch = 16, col = tab$region.colour,
       cex = sqrt(tab$total.pop)/400)
legend.sizes = c(193305,500000,1000000,2000000,5666264)
legend.labels = c("193,000","500,000","1,000,000", "2,000,000", "5,670,000")
points(rep(12,5), 34 - c(0,1,2,3,4.3)*0.7, pch = 16, col = rgb(0.5,0.5,0.5,0.5),
       cex = sqrt(legend.sizes)/400)
text(12.5,34.7,"population of province", cex = 0.8)
text(rep(13.8,5), 34 - c(0,1,2,3,4.3)*0.7, legend.labels, cex = 0.7, pos = 2)
text(15.9,34.7,"region",cex = 0.8)
points(rep(15.3,4),34 - 0:3*0.7, 
       col = c(hsv(7/12,1,0.8,0.5),
               hsv(3/12,1,0.8,0.5),
               hsv(11/12,1,0.8,0.5),
               hsv(1/12,1,0.8,0.5)),
       pch = 16, cex = sqrt(1000000)/400)
text(15.6,34 - 0:3*0.7, c("South","North","North-East","Central"), cex = 0.7, pos = 4)
dev.off()

cumsum = pop %>% 
  filter(!is.na(age)) %>%
  group_by(province) %>%
  mutate(n.younger = cumsum(n)) 
cumsum = left_join(cumsum, tab[,c("province","n.with.age")], by = "province")
median.age = cumsum %>% 
  filter(n.younger >= n.with.age / 2) %>%
  group_by(province) %>%
  summarise(median.age = first(age)) %>%
  arrange(median.age)

ra = 1:82 * 3 - 2
rm = 1:82 * 3 - 1
rf = 1:82 * 3 
d$Province[rm] = d$Province[ra]
d$Province[rf] = d$Province[ra]
north3.female = d[d$Province %in% c("Lampang Province","Phrae Province",
                                    "Lamphun Province") & d$Sex == "Female",]
north3.male = d[d$Province %in% c("Lampang Province","Phrae Province",
                                  "Lamphun Province") & d$Sex == "Male",]
north.female.pop = apply(north3.female[,5:106], 2, sum)
north.male.pop = apply(north3.male[,5:106], 2, sum)
max.pop = max(c(north.female.pop,north.male.pop))
pdf("images/pyramid.north.pdf", width = 8, height = 9)
par(bg = bg_colour)
par(las = 1)
par(mar = c(5,4,4,6) + 0.1)
plot(0,0,xlim = c(-max.pop,max.pop),ylim = c(0,101),col = "white",
     xlab = "Number of people", ylab = "Age",
     xaxt = "n", yaxt = "n", 
     frame.plot = T,
     main = "Population pyramid: Lamphun, Phrae and Lampang")
for(i in 1:102) {
  rect(0, i-1, -north.female.pop[i], i, col = colours[1], border = colours[1])
  rect(0, i-1,  north.male.pop[i], i, col = colours[2], border = colours[2])
}
abline(h = 0:10 * 10, lwd = 0.5, col = colours[3])
abline(v = -10:10 * 5000, lwd = 0.5, col = colours[3])
age.labels = 0:10 * 10
pops.at = -3:3*5000
pops.labels = c("15,000","10,000","5000","0","5000","10,000","15,000")
axis(side = 2, at = age.labels, labels = age.labels)
axis(side = 4, at = age.labels, labels = 2019 - age.labels)
axis(side = 1, at = pops.at, labels = pops.labels)
#axis(side = 3, at = pops.at, labels = pops.labels)
par(las = 0)
mtext("Year of birth", side = 4, line = 4)
text(-7500,55,"Female")
text(7500,55,"Male")
text(0,-2,paste("Data from",source),cex = 0.6)
dev.off()

south3.female = d[d$Province %in% c("Narathiwat Province","Yala Province",
                                    "Pattani Province") & d$Sex == "Female",]
south3.male = d[d$Province %in% c("Narathiwat Province","Yala Province",
                                  "Pattani Province") & d$Sex == "Male",]
south.female.pop = apply(south3.female[,5:106], 2, sum)
south.male.pop = apply(south3.male[,5:106], 2, sum)
max.pop = max(c(south.female.pop,south.male.pop))
pdf("images/pyramid.south.pdf", width = 8, height = 9)
par(bg = bg_colour)
par(las = 1)
par(mar = c(5,4,4,6) + 0.1)
plot(0,0,xlim = c(-max.pop,max.pop),ylim = c(0,101),col = "white",
     xlab = "Number of people", ylab = "Age",
     xaxt = "n", yaxt = "n", 
     frame.plot = T,
     main = "Population pyramid: Pattani, Yala and Narathiwat")
for(i in 1:102) {
  rect(0, i-1, -south.female.pop[i], i, col = colours[1], border = colours[1])
  rect(0, i-1,  south.male.pop[i], i, col = colours[2], border = colours[2])
}
abline(h = 0:10 * 10, lwd = 0.5, col = colours[3])
abline(v = -10:10 * 5000, lwd = 0.5, col = colours[3])
age.labels = 0:10 * 10
pops.at = -3:3*5000
pops.labels = c("15,000","10,000","5000","0","5000","10,000","15,000")
axis(side = 2, at = age.labels, labels = age.labels)
axis(side = 4, at = age.labels, labels = 2019 - age.labels)
axis(side = 1, at = pops.at, labels = pops.labels)
#axis(side = 3, at = pops.at, labels = pops.labels)
par(las = 0)
mtext("Year of birth", side = 4, line = 4)
text(-7500,55,"Female")
text(7500,55,"Male")
text(0,-2,paste("Data from",source),cex = 0.6)
dev.off()





