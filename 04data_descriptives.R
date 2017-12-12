################ Vanessa Richardson
################ 6640 Fall 2017 final project
################ data descriptives

################ the data consists of weather and incidence data on 142 districts
################ each district has 396 rows in a dataframe, each row is associated with a particular epiyear/epiweek
################ the dataset is sorted by District, then by epiyear, then by epiweek

library(plyr)
library(ggplot2)
library(stargazer)
library(RColorBrewer)
library(sp)
library(maptools) 
library(lattice)
library(latticeExtra)
library(rgdal)

# colors for maps
cool_scale = brewer.pal(n = 9, name = "YlGnBu")
hot_scale = brewer.pal(n = 9, name = "YlOrRd")
blue_scale = brewer.pal(n = 9, name = "BuPu")
green_scale = brewer.pal(n = 9, name = "Greens")

length(unique(all_int$Province)) # 10 provinces
length(unique(all_int$Region)) # 4 regions

# incidence, temperature, by province

ggplot(data = all_int, aes(x = Epiweek, y = incidence, fill = Province, linetype = Province)) +
geom_smooth() +
ggtitle("Average Malaria Incidence by Province \n2010-2017") +
xlab("Epiweek") +
ylab("Incidence")

ggplot(data = all_int, aes(x = Epiweek, y = tavg, fill = Province, linetype = Province)) +
geom_smooth() +
ggtitle("Average Temperature by Province \n2010-2017") +
xlab("Epiweek") +
ylab("Temperature")

# Regional rainfall patterns

ggplot(data = subset(all_int, Region == "Center"), aes(x = Epiweek, y = raint, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Regional Rainfall, Center") +
xlab("Epiweek") +
ylab("Total Rainfall")

ggplot(data = subset(all_int, Region == "Coastal"), aes(x = Epiweek, y = raint, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Regional Rainfall, Coastal") +
xlab("Epiweek") +
ylab("Total Rainfall")

ggplot(data = subset(all_int, Region == "Northern"), aes(x = Epiweek, y = raint, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Regional Rainfall, Northern") +
xlab("Epiweek") +
ylab("Total Rainfall")

ggplot(data = subset(all_int, Region == "Southern"), aes(x = Epiweek, y = raint, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Regional Rainfall, Southern") +
xlab("Epiweek") +
ylab("Total Rainfall")

ggplot(data = all_int, aes(x = Epiweek, y = raint, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Average Rainfall, Mozambique") +
xlab("Epiweek") +
ylab("Total Rainfall")


# Regional temperature patterns

ggplot(data = subset(all_int, Region == "Center"), aes(x = Epiweek, y = tavg, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Regional Temperature, Center") +
xlab("Epiweek") +
ylab("Average Temperature")

ggplot(data = subset(all_int, Region == "Coastal"), aes(x = Epiweek, y = tavg, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Regional Temperature, Coastal") +
xlab("Epiweek") +
ylab("Average Temperature")

ggplot(data = subset(all_int, Region == "Northern"), aes(x = Epiweek, y = tavg, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Regional Temperature, Northern") +
xlab("Epiweek") +
ylab("Average Temperature")

ggplot(data = subset(all_int, Region == "Southern"), aes(x = Epiweek, y = tavg, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Regional Temperature, Southern") +
xlab("Epiweek") +
ylab("Average Temperature")

ggplot(data = all_int, aes(x = Epiweek, y = tavg, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Average Temperature, Mozambique") +
xlab("Epiweek") +
ylab("Average Temperature")


# Regional incidence, pretty much the same for all the years

ggplot(data = subset(all_int, Region == "Center"), aes(x = Epiweek, y = lag.incidence2, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
xlab("Epiweek") +
ylab("Incidence Lagged 2 Weeks")

ggplot(data = subset(all_int, Region == "Coastal"), aes(x = Epiweek, y = lag.incidence2, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
xlab("Epiweek") +
ylab("Incidence Lagged 2 Weeks")

ggplot(data = subset(all_int, Region == "Northern"), aes(x = Epiweek, y = lag.incidence2, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
xlab("Epiweek") +
ylab("Incidence Lagged 2 Weeks")

ggplot(data = subset(all_int, Region == "Southern"), aes(x = Epiweek, y = lag.incidence2, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
xlab("Epiweek") +
ylab("Incidence Lagged 2 Weeks")


# relationships between weather variables by district,
# averaged over all weeks and years

summary_weather = ddply(all_int, "District", summarize,
  mean_t = round(mean(tavg), 2),
  median_t = round(median(tavg), 2),
  mean_r = round(mean(raint), 2),
  median_r = round(median(raint), 2),
  mean_rh = round(mean(rh), 2),
  median_rh = round(median(rh), 2),
  mean_sd = round(mean(sd), 2),
  median_sd = round(median(sd), 2),
  mean_ps = round(mean(psfc), 2),
  median_ps = round(median(psfc), 2))


cor(summary_weather$mean_t, summary_weather$mean_r, use = "pairwise.complete.obs")
cor(summary_weather$mean_t, summary_weather$mean_rh, use = "pairwise.complete.obs")
cor(summary_weather$mean_t, summary_weather$mean_sd, use = "pairwise.complete.obs")
cor(summary_weather$mean_t, summary_weather$mean_ps, use = "pairwise.complete.obs")

cor(summary_weather$mean_r, summary_weather$mean_rh, use = "pairwise.complete.obs")
cor(summary_weather$mean_r, summary_weather$mean_sd, use = "pairwise.complete.obs")
cor(summary_weather$mean_r, summary_weather$mean_ps, use = "pairwise.complete.obs")

cor(summary_weather$mean_rh, summary_weather$mean_sd, use = "pairwise.complete.obs")
cor(summary_weather$mean_rh, summary_weather$mean_ps, use = "pairwise.complete.obs")

cor(summary_weather$mean_sd, summary_weather$mean_ps, use = "pairwise.complete.obs")

par(mfrow = c(2, 2))

plot(summary_weather$mean_t ~ summary_weather$mean_r, pch = 20,
    xlab = "Mean rainfall per district",
    ylab = "Mean temperature per district",
    main = "Relationship betweeen rainfall and temperature, \n averaged by district, \ncorrelation = -0.310")

plot(summary_weather$mean_r ~ summary_weather$mean_rh, pch = 20,
    xlab = "Mean relative humidity per district",
    ylab = "Mean rainfall per district",
    main = "Relationship between humidity and rainfall, \n averaged by district, \ncorrelation = -0.150")

plot(summary_weather$mean_t ~ summary_weather$mean_ps, pch = 20,
    xlab = "Mean barometric pressure per district",
    ylab = "Mean temperature per district",
    main = "Relationship betweeen PSFC and temperature, \n averaged by district, \ncorrelation = 0.786")

plot(summary_weather$mean_r ~ summary_weather$mean_ps, pch = 20,
    xlab = "Mean barometric pressure per district",
    ylab = "Mean rainfall per district",
    main = "Relationship betweeen PSFC and rainfall, \n averaged by district, \ncorrelation = -0.419")


# relationships between weather variables and incidence, by district,
# averaged over all weeks and years

summary_in = ddply(all_int, "District", summarize,
  mean = round(mean(incidence), 2),
  median = round(median(incidence), 2),
  sd = round(sd(incidence), 2))

cor(summary_in$mean, summary_weather$mean_t, use = "pairwise.complete.obs")
cor(summary_in$mean, summary_weather$mean_r, use = "pairwise.complete.obs")
cor(summary_in$mean, summary_weather$mean_rh, use = "pairwise.complete.obs")
cor(summary_in$mean, summary_weather$mean_sd, use = "pairwise.complete.obs")
cor(summary_in$mean, summary_weather$mean_ps, use = "pairwise.complete.obs")


# looking at malaria incidence

# the distribution is skewed
hist(round(all_int$incidence), col = "blue", main = "Malaria Incidence", xlab = "Incidence")

# large variation in incidence across districts
barplot(summary_in$mean, space = 0.25, xlab = "Districts, n = 142", ylab = "Average incidence", 
    main = "Average Malaria Incidence per District \n2010 - 2017")

# distributions of malaria incidence in a few select districts

par(mfrow = c(2, 3))

hist(round(all_int$incidence[all_int$District == "MABALANE"]), col = "purple", 
    main = "Malaria Incidence in Mabalane", xlab = "Incidence")

hist(round(all_int$incidence[all_int$District == "ALTO MOLOCUE"]), col = "yellow",
    main = "Malaria Incidence in Alto Molocue", xlab = "Incidence")

hist(round(all_int$incidence[all_int$District == "MARAVIA"]), col = "red",
    main = "Malaria Incidence in Maravia", xlab = "Incidence")

hist(round(all_int$incidence[all_int$District == "HOMOINE"]), col = "blue",
    main = "Malaria Incidence in Homoine", xlab = "Incidence")

hist(round(all_int$incidence[all_int$District == "PANDA"]), col = "green",
    main = "Malaria Incidence in Panda", xlab = "Incidence")

hist(round(all_int$incidence[all_int$District == "GOVURO"]), col = "orange",
    main = "Malaria Incidence in Govuro", xlab = "Incidence")


###### mapping average incidence by district

rownames(summary_in) = unique(all_int$DISTCODE)

poly1 = readShapePoly('C:/Users/richarva/Documents/Moz_admin2.shp', IDvar = 'DISTCODE')

polydat_in = SpatialPolygonsDataFrame(poly1, summary_in)

spplot(polydat_in, "mean",
    names.attr = "Average Incidence", 
    colorkey=list(space="right"), scales = list(draw = TRUE), 
    main = "Average Incidence Per District \n2010 - 2017", 
    as.table = TRUE, cuts=5, col.regions = cool_scale)

## graphing incidence by district and year

summary_inyear = ddply(all_int, c("District", "Epiyear"), summarize,
  mean = round(mean(incidence), 2),
  median = round(median(incidence), 2),
  sd = round(sd(incidence), 2))

# no year appears to have a big spike in incidence in any district

ggplot(data = summary_inyear, aes(x = Epiyear, y = mean, fill = District, linetype = District)) +
geom_smooth() +
#geom_line() +
theme(legend.position = "none") +
xlab("Year") +
ylab("Mean Incidence")

ggplot(data = all_int, aes(x = Epiweek, y = incidence, fill = as.factor(Epiyear), linetype = as.factor(Epiyear))) +
geom_smooth() +
ggtitle("Average Incidence, Mozambique")
xlab("Year") +
ylab("Incidence")


# relationships between interventions and incidence, by district,
# averaged over all weeks and years

summary_int = ddply(all_int, "District", summarize,
  mean_net = round(mean(netprotection), 2),
  median_net = round(median(netprotection), 2),
  sd_net = round(sd(netprotection), 2),
  mean_spray = round(mean(sprayprotection), 2),
  median_spray = round(median(sprayprotection), 2),
  sd_spray = round(sd(sprayprotection), 2))

cor(summary_in$mean, summary_int$mean_net, use = "pairwise.complete.obs")
cor(summary_in$mean, summary_int$median_net, use = "pairwise.complete.obs")
cor(summary_in$mean, summary_int$mean_spray, use = "pairwise.complete.obs")
cor(summary_in$mean, summary_int$median_spray, use = "pairwise.complete.obs")

# is incidence by district related to population density?

all_int = data.frame(all_int, "pop_density" = all_int$u5total / all_int$SQKM)

summary_in = ddply(all_int, "District", summarize,
  mean = round(mean(incidence), 2),
  median = round(median(incidence), 2),
  sd = round(sd(incidence), 2))

summary_popden = ddply(all_int, "District", summarize,
  mean = round(mean(pop_density), 2),
  median = round(median(pop_density), 2),
  sd = round(sd(pop_density), 2))

cor(summary_in$mean, summary_popden$mean, use = "pairwise.complete.obs")


# relationships between the weather variables by epiweek,
# averaged over all years and districts

summary_weather_wk = ddply(all_int, "Epiweek", summarize,
  mean_t = round(mean(tavg, na.rm = T), 2),
  median_t = round(median(tavg, na.rm = T), 2),
  mean_r = round(mean(raint, na.rm = T), 2),
  median_r = round(median(raint, na.rm = T), 2),
  mean_rh = round(mean(rh, na.rm = T), 2),
  median_rh = round(median(rh, na.rm = T), 2),
  mean_sd = round(mean(sd, na.rm = T), 2),
  median_sd = round(median(sd, na.rm = T), 2),
  mean_ps = round(mean(psfc, na.rm = T), 2),
  median_ps = round(median(psfc, na.rm = T), 2))

cor(summary_weather_wk$mean_t, summary_weather_wk$mean_r, use = "pairwise.complete.obs")
cor(summary_weather_wk$mean_t, summary_weather_wk$mean_rh, use = "pairwise.complete.obs")
cor(summary_weather_wk$mean_t, summary_weather_wk$mean_sd, use = "pairwise.complete.obs")
cor(summary_weather_wk$mean_t, summary_weather_wk$mean_ps, use = "pairwise.complete.obs")

cor(summary_weather_wk$mean_r, summary_weather_wk$mean_rh, use = "pairwise.complete.obs")
cor(summary_weather_wk$mean_r, summary_weather_wk$mean_sd, use = "pairwise.complete.obs")
cor(summary_weather_wk$mean_r, summary_weather_wk$mean_ps, use = "pairwise.complete.obs")

cor(summary_weather_wk$mean_rh, summary_weather_wk$mean_sd, use = "pairwise.complete.obs")
cor(summary_weather_wk$mean_rh, summary_weather_wk$mean_ps, use = "pairwise.complete.obs")

cor(summary_weather_wk$mean_sd, summary_weather_wk$mean_ps, use = "pairwise.complete.obs")

par(mfrow = c(2, 2))

plot(summary_weather_wk$mean_t ~ summary_weather_wk$mean_r, pch = 20,
    xlab = "Mean rainfall per epiweek",
    ylab = "Mean temperature per epiweek",
    main = "Relationship between rainfall and temperature, \n averaged by epiweek, \ncorrelation = 0.596")

plot(summary_weather_wk$mean_r ~ summary_weather_wk$mean_rh, pch = 20,
    xlab = "Mean relative humidity per epiweek",
    ylab = "Mean rainfall per epiweek",
    main = "Relationship between humidity and rainfall, \n averaged by epiweek, \ncorrelation = 0.602")

plot(summary_weather_wk$mean_t ~ summary_weather_wk$mean_ps, pch = 20,
    xlab = "Mean barometric pressure per epiweek",
    ylab = "Mean temperature per epiweek",
    main = "Relationship betweeen PSFC and temperature, \n averaged by epiweek, \ncorrelation = -0.891")

plot(summary_weather_wk$mean_r ~ summary_weather_wk$mean_ps, pch = 20,
    xlab = "Mean barometric pressure per epiweek",
    ylab = "Mean rainfall per epiweek",
    main = "Relationship betweeen PSFC and rainfall, \n averaged by epiweek, \ncorrelation = -0.843")


# relationships between weather variables and incidence, by epiweek,
# averaged over all years and districts

summary_in_wk_lag2 = ddply(all_int, "Epiweek", summarize,
  mean = round(mean(lag.incidence2, na.rm = T), 2),
  median = round(median(lag.incidence2, na.rm = T), 2),
  sd = round(sd(lag.incidence2, na.rm = T), 2))

summary_in_wk_lag4 = ddply(all_int, "Epiweek", summarize,
  mean = round(mean(lag.incidence4, na.rm = T), 2),
  median = round(median(lag.incidence4, na.rm = T), 2),
  sd = round(sd(lag.incidence4, na.rm = T), 2))

summary_in_wk_lag8 = ddply(all_int, "Epiweek", summarize,
  mean = round(mean(lag.incidence8, na.rm = T), 2),
  median = round(median(lag.incidence8, na.rm = T), 2),
  sd = round(sd(lag.incidence8, na.rm = T), 2))

cor(summary_in_wk_lag2$mean, summary_weather_wk$mean_t, use = "pairwise.complete.obs")
cor(summary_in_wk_lag2$mean, summary_weather_wk$mean_r, use = "pairwise.complete.obs")
cor(summary_in_wk_lag2$mean, summary_weather_wk$mean_rh, use = "pairwise.complete.obs")
cor(summary_in_wk_lag2$mean, summary_weather_wk$mean_sd, use = "pairwise.complete.obs")
cor(summary_in_wk_lag2$mean, summary_weather_wk$mean_ps, use = "pairwise.complete.obs")


cor(summary_in_wk_lag4$mean, summary_weather_wk$mean_t, use = "pairwise.complete.obs")
cor(summary_in_wk_lag4$mean, summary_weather_wk$mean_r, use = "pairwise.complete.obs")
cor(summary_in_wk_lag4$mean, summary_weather_wk$mean_rh, use = "pairwise.complete.obs")
cor(summary_in_wk_lag4$mean, summary_weather_wk$mean_sd, use = "pairwise.complete.obs")
cor(summary_in_wk_lag4$mean, summary_weather_wk$mean_ps, use = "pairwise.complete.obs")


cor(summary_in_wk_lag8$mean, summary_weather_wk$mean_t, use = "pairwise.complete.obs")
cor(summary_in_wk_lag8$mean, summary_weather_wk$mean_r, use = "pairwise.complete.obs")
cor(summary_in_wk_lag8$mean, summary_weather_wk$mean_rh, use = "pairwise.complete.obs")
cor(summary_in_wk_lag8$mean, summary_weather_wk$mean_sd, use = "pairwise.complete.obs")
cor(summary_in_wk_lag8$mean, summary_weather_wk$mean_ps, use = "pairwise.complete.obs")



# relationships between interventions and incidence, by epiweek,
# averaged over all years and districts

summary_int_wk = ddply(all_int, "Epiweek", summarize,
  mean_net = round(mean(netprotection, na.rm = T), 2),
  median_net = round(median(netprotection, na.rm = T), 2),
  sd_net = round(sd(netprotection, na.rm = T), 2),
  mean_spray = round(mean(sprayprotection, na.rm = T), 2),
  median_spray = round(median(sprayprotection, na.rm = T), 2),
  sd_spray = round(sd(sprayprotection, na.rm = T), 2))

cor(summary_in_wk_lag2$mean, summary_int_wk$mean_net, use = "pairwise.complete.obs")
cor(summary_in_wk_lag2$mean, summary_int_wk$median_net, use = "pairwise.complete.obs")
cor(summary_in_wk_lag2$mean, summary_int_wk$mean_spray, use = "pairwise.complete.obs")
cor(summary_in_wk_lag2$mean, summary_int_wk$median_spray, use = "pairwise.complete.obs")


cor(summary_in_wk_lag4$mean, summary_int_wk$mean_net, use = "pairwise.complete.obs")
cor(summary_in_wk_lag4$mean, summary_int_wk$median_net, use = "pairwise.complete.obs")
cor(summary_in_wk_lag4$mean, summary_int_wk$mean_spray, use = "pairwise.complete.obs")
cor(summary_in_wk_lag4$mean, summary_int_wk$median_spray, use = "pairwise.complete.obs")


cor(summary_in_wk_lag8$mean, summary_int_wk$mean_net, use = "pairwise.complete.obs")
cor(summary_in_wk_lag8$mean, summary_int_wk$median_net, use = "pairwise.complete.obs")
cor(summary_in_wk_lag8$mean, summary_int_wk$mean_spray, use = "pairwise.complete.obs")
cor(summary_in_wk_lag8$mean, summary_int_wk$median_spray, use = "pairwise.complete.obs")

par(mfrow = c(1, 3))

plot(summary_in_wk_lag2$mean ~ summary_int_wk$mean_spray, pch = 20,
    xlab = "Mean spray protection", ylab = "Mean incidence lagged 2 weeks",
    main = "Relationship Between Spray and Incidence \nLagged 2 Weeks \nCorrelation = -0.376")

plot(summary_in_wk_lag4$mean ~ summary_int_wk$mean_spray, pch = 20,
    xlab = "Mean spray protection", ylab = "Mean incidence lagged 4 weeks",
    main = "Relationship Between Spray and Incidence \nLagged 4 Weeks \nCorrelation = -0.548")

plot(summary_in_wk_lag8$mean ~ summary_int_wk$mean_spray, pch = 20,
    xlab = "Mean spray protection", ylab = "Mean incidence lagged 8 weeks",
    main = "Relationship Between Spray and Incidence \nLagged 8 Weeks \nCorrelation = -0.748")

### creating maps of population

summary_pop = ddply(all_int, "District", summarize,
  mean_pop = round(mean(u5total), 2),
  median_pop = round(median(u5total), 2),
  sd_pop = round(sd(u5total), 2),
  mean_den = round(mean(pop_density*1000), 2),
  median_den = round(median(pop_density*1000), 2),
  sd_den = round(sd(pop_density*1000), 2))

rownames(summary_pop) = unique(all_int$DISTCODE)

polydat_pop = SpatialPolygonsDataFrame(poly1, summary_pop)

spplot(polydat_pop, "mean_pop",
    names.attr = "Mozambique Population", 
    colorkey=list(space="right"), scales = list(draw = TRUE), 
    main = "Population Per District \n2010 - 2017", 
    as.table = TRUE, cuts=5, col.regions = hot_scale)


### creating maps for the data from Epiweek 4, 2017

all_int2 = all_int[with(all_int, order(DISTCODE)), ]

epiweek4_2017 = subset(all_int2, Epiweek == 4 & Epiyear == 2017)

rownames(epiweek4_2017) = epiweek4_2017$DISTCODE

polydat4_2017 <- SpatialPolygonsDataFrame(poly1, epiweek4_2017)

# province map
spplot(polydat4_2017, "Province",
    names.attr = "Province", 
    colorkey=list(space="right"), scales = list(draw = TRUE), main = "Mozambique Provinces", 
    as.table = TRUE, cuts=10, col.regions = cool_scale)

# region map
spplot(polydat4_2017, "Region",
    names.attr = "Region", 
    colorkey=list(space="right"), scales = list(draw = TRUE), main = "Mozambique Regions", 
    as.table = TRUE, cuts=5, col.regions = hot_scale)

# lagged incidence map
spplot(polydat4_2017, c("incidence", "lag.incidence2", "lag.incidence4", "lag.incidence8"),
    names.attr = c("Incidence", "2-week Incidence", "4-week Incidence", "8-week Incidence"), 
    colorkey=list(space="right"), scales = list(draw = TRUE), main = "Incidence, Week 4, 2017", 
    as.table = TRUE, cuts=5, col.regions = blue_scale)

# weather variables maps
spplot(polydat4_2017, "tavg", names.attr = "Avg temp", 
    colorkey=list(space="right"), scales = list(draw = TRUE), main = "Mozambique Avg Temp, Week 4, 2017", 
    as.table = TRUE, cuts=5, col.regions = blue_scale)

spplot(polydat4_2017, "rh", names.attr = "Rel Humidity", 
    colorkey=list(space="right"), scales = list(draw = TRUE), main = "Mozambique Rel Humidity, Week 4, 2017", 
    as.table = TRUE, cuts=5, col.regions = blue_scale)

spplot(polydat4_2017, "raint", names.attr = "Total rain", 
    colorkey=list(space="right"), scales = list(draw = TRUE), main = "Mozambique Tot Rain, Week 4, 2017", 
    as.table = TRUE, cuts=5, col.regions = blue_scale)

spplot(polydat4_2017, "sd", names.attr = "SVPD", 
    colorkey=list(space="right"), scales = list(draw = TRUE), main = "Mozambique SVPD, Week 4, 2017", 
    as.table = TRUE, cuts=5, col.regions = blue_scale)

spplot(polydat4_2017, "psfc", names.attr = "PSFC", 
    colorkey=list(space="right"), scales = list(draw = TRUE), main = "Mozambique Bar Pressure, Week 4, 2017", 
    as.table = TRUE, cuts=5, col.regions = blue_scale)

# interventions map
spplot(polydat4_2017, c("netprotection", "sprayprotection"),
    names.attr = c("Net Protection", "Spray Protection"), 
    colorkey=list(space="right"), scales = list(draw = TRUE), main = "Protection, Week 4, 2017", 
    as.table = TRUE, cuts=5, col.regions = blue_scale)

       
