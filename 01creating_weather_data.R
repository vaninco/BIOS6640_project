################ Vanessa Richardson
################ 6640 Fall 2017 final project
################ reading in the weather and incidence data; creating lagged incidence

################ the data consists of weather and incidence data on 142 districts
################ each district has 396 rows in a dataframe, each row is associated with a particular epiyear/epiweek
################ the dataset is sorted by District, then by epiyear, then by epiweek

library(EpiWeek)

# using the Google Chrome browser, go to 
# http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2/
# highlight all the files and copy them to the clipboard

files = read.table(file = "clipboard")

list_files = files[, 2]

f = file.path("http://rap.ucar.edu/staff/monaghan/colborn/mozambique/daily/v2", list_files)

all_data = lapply(f, read.table, skip = 3, header = F)

names(all_data) = paste(list_files, sep = "")

col_names = c("year", "mo", "day", "raint", "tavg", "rh", "sd", "psfc")

all_data = lapply(all_data, setNames, col_names)

lapply(all_data, names) # shows you the names

str(all_data) # it's a list of data frames

all_data2 = do.call(rbind, all_data) # creates one big data frame

# creates a date column
all_data2$date = as.Date(paste(all_data2$mo, all_data2$day, all_data2$year, sep = "."), format = "%m.%d.%Y")

# creates epiweek columns
all_data2 = data.frame(all_data2, epiweek = dateToEpiweek(all_data2$date))

all_data2[,12] = rownames(all_data2)

colnames(all_data2) = c(col_names, "date", "Epiyear", "Epiweek", "district")

all_data2$district = substr(all_data2$district, 1, 25)

all_data8 = aggregate(cbind(tavg, rh, sd, psfc) ~ Epiweek + Epiyear + district, all_data2, mean)

all_data7 = aggregate(raint ~ Epiweek + Epiyear + district, all_data2, sum)

agg_data = data.frame(all_data8, "raint" = all_data7$raint)

agg_data2 = subset(agg_data, Epiyear > 2009)



# combining all_data and incidence.csv

# before importing, sort incidence spreadsheet by district -> epiyear -> epiweek

incidence = read.csv("incidence.csv")

merge_data2 = data.frame(agg_data2, incidence)

# cleaning it up

merge_data2 = merge_data2[, c(1, 2, 4:9, 12:19)]

# creating incidence columns

merge_data2 = data.frame(merge_data2, "incidence" = ((merge_data2$cases / merge_data2$u5total) * 1000), 
    "incidence.round" = round(((merge_data2$cases / merge_data2$u5total) * 1000)))

# incidence lagged by 2 weeks
merge_data_lag = data.frame(merge_data2, 
    "lag.incidence2" = unlist(by(merge_data2, merge_data2$District, function(x) c(NA, NA, head(x$incidence, -2)))))

# incidence lagged by 4 weeks
merge_data_lag = data.frame(merge_data_lag, 
    "lag.incidence4" = unlist(by(merge_data_lag, merge_data_lag$District, function(x) c(rep(NA, 4), head(x$incidence, -4)))))

# incidence lagged by 8 weeks
merge_data_lag = data.frame(merge_data_lag, 
    "lag.incidence8" = unlist(by(merge_data_lag, merge_data_lag$District, function(x) c(rep(NA, 8), head(x$incidence, -8)))))

write.table(merge_data_lag, "merge_data_lag.csv", sep = ",")
