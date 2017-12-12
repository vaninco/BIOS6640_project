################ Vanessa Richardson
################ 6640 Fall 2017 final project
################ filling NA values in Province, Region, SQKM, and u5total columns

################ the data consists of weather and incidence data on 142 districts
################ each district has 396 rows in a dataframe, each row is associated with a particular epiyear/epiweek
################ the dataset is sorted by District, then by epiyear, then by epiweek


a = seq(from = 1, to = 56232, by = 396)

################ replacing NAs in the Province column

provnames = all_int$Province[a]

summary(provnames) # shows 1 NA; the dataset has no province listed for the district Cidade De Maputo

# wikipedia Cidade De Maputo
# The city is surrounded by Maputo Province, but is administered as its own province
# set as MAPUTO for now

provnames[is.na(provnames)] = 'MAPUTO'

all_int$Province = rep(provnames, each = 396)


################ replacing NAs in the Region column

regnames = all_int$Region[a]

summary(regnames) # shows 1 NA; no information for Cidade De Maputo

# the province of Maputo is in the Southern region

regnames[is.na(regnames)] = 'Southern'

all_int$Region = rep(regnames, each = 396)


################ replacing NAs in the SQKM column

SQKMs = all_int$SQKM[a]

summary(SQKMs) # shows 1 NA; no information for Cidade De Maputo

############################### google sqkm???

all_int$SQKM = rep(SQKMs, each = 396)


################ replacing NAs in the u5total column

u5totals = all_int$u5total[a]

summary(u5totals) # no information for the population of Cidade De Maputo
###################################### again, can google this??

all_int$u5total = rep(u5totals, each = 396)

write.table(all_int, "all_int.csv", sep = ",")
