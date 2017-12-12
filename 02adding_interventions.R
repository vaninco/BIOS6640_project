################ Vanessa Richardson
################ 6640 Fall 2017 final project
################ incorporating intervention data - spray and nets

################ the data consists of weather and incidence data on 142 districts
################ each district has 396 rows in a dataframe, each row is associated with a particular epiyear/epiweek
################ the dataset is sorted by District, then by epiyear, then by epiweek

library(plyr)

intervention = read.csv("intervention.csv")

# convert the intervention epiyear/epiweek to epiweeks

intervention$ITNyearconvert = mapvalues(intervention$ITNyear, from = 2009:2017, to = -1:7)
intervention$IRSyearconvert = mapvalues(intervention$IRSyear, from = 2009:2017, to = -1:7)

netconversion = intervention$ITNyearconvert*52 + intervention$ITNepiWeek
sprayconversion = intervention$IRSyearconvert*52 + intervention$IRSepiWeek

intervention = data.frame(intervention, netconversion, sprayconversion)

# adding district names to the intervention dataframe

distNameCode = data.frame('Name' = unique(merge_data_lag$District), 'DISTCODE' = unique(merge_data_lag$DISTCODE))

intervention2 = merge(intervention, distNameCode, by = 'DISTCODE')

intervention3 = intervention2[order(intervention2[,10]),]

################ creating a column of percent effectiveness of the net intervention for each district by epiweek

# a function that takes an epiweek number as an intervention start date
# and returns a vector of net percent effectiveness for each epiweek,
# assuming that the effectiveness of the nets declines linearly, reaching 
# 60% effectiveness 24 months after the start date

netconverter = function(convertedEpiWeek) {
district = 0:(396 - convertedEpiWeek)
nets = c(rep(0, (convertedEpiWeek - 1)), 100 - 0.385*district)
}

# creating net percent effectiveness for each unique beginning epiweek

unique(intervention3$netconversion)

wk204 = netconverter(204)
wk108 = netconverter(108)
wk352 = netconverter(352)
wk304 = netconverter(304)
wk172 = netconverter(172)
wk200 = netconverter(200)
wk164 = netconverter(164)
wk168 = netconverter(168)
wk124 = netconverter(124)
wk144 = netconverter(144)
wk132 = netconverter(132)
wk152 = netconverter(152)
wk252 = netconverter(252)
wk212 = netconverter(212)
wk180 = netconverter(180)
wk232 = netconverter(232)
wk356 = netconverter(356)
wk256 = netconverter(256)
wk308 = netconverter(308)
wk140 = netconverter(140)
wk160 = netconverter(160)
wk276 = netconverter(276)
wk184 = netconverter(184)
wk60 = netconverter(60)
wk188 = netconverter(188)
wk292 = netconverter(292)
wk272 = netconverter(272)
wk68 = netconverter(68)
wk176 = netconverter(176)
wk196 = netconverter(196)
wk280 = netconverter(280)
wk100 = netconverter(100)
wk.4 = netconverter(1)
wk112 = netconverter(112)
wk116 = netconverter(116)
wk56 = netconverter(56)

# creating net percent effectiveness for districts that received nets twice

angoche = c(wk304[1:351], wk352[352:396])
nacala = c(wk256[1:355], wk356[356:396])
ilha = c(wk304[1:351], wk352[352:396])
lalaua = c(wk292[1:303], wk304[304:396])
malema = c(wk292[1:355], wk356[356:396])
meconta = c(wk256[1:355], wk356[356:396])
mecuburi = c(wk256[1:355], wk356[356:396])
memba = c(wk256[1:355], wk356[356:396])
mogin = c(wk152[1:355], wk356[356:396])
mogov = c(wk256[1:355], wk356[356:396])
moma = c(wk256[1:355], wk356[356:396])
monapo = c(wk256[1:355], wk356[356:396])
mossuril = c(wk352[1:355], wk356[356:396])
muecate = c(wk292[1:355], wk356[356:396])
murru = c(wk292[1:355], wk356[356:396])
nacala2 = c(wk100[1:355], wk356[356:396])
nacaroa = c(wk.4[1:355], wk356[356:396])
namapa = c(wk256[1:303], wk304[304:396])
rapale = c(wk100[1:355], wk356[356:396])
ribaue = c(wk100[1:355], wk356[356:396])

# how many districts received any intervention?

length(unique(intervention3$DISTCODE))

# which districts received no intervention?

Filter(function (elem) length(which(D == elem)) <= 1, D)

# putting together the net percent effectiveness for each district, including those that received no nets

netprotection1 = c(wk204, wk108, angoche, wk172, wk108, wk200, wk164, wk168, wk124, wk168, wk124,

wk172, wk144, wk132, wk144, wk164, wk152, wk172, wk152, wk204, wk252, wk124, wk164, wk212, wk180, rep(0, 396), wk180, wk232, rep(0, 396), wk180, nacala,

wk304, wk308, rep(0, 396), wk168, rep(0, 396), wk132, wk140, wk124, wk212, wk204, wk160, wk212, wk164, wk124, wk204, wk212, wk276, wk204, ilha,

wk184, wk212, wk60, wk212, wk188, lalaua, wk232, wk60, wk152, wk180, wk172, wk132, wk200, wk272, wk124, wk68, wk172, wk176, wk188, malema)


netprotection2 = c(wk188, wk164, wk180, wk160, wk172, wk196, wk168, wk176, wk188, wk152, wk212, wk152, wk176, wk140, wk188, wk176, meconta, mecuburi,

wk308, wk188, wk280, memba, wk188, rep(0, 396), wk172, wk168, wk272, rep(0, 396), mogin, mogov, moma, monapo,

wk280, wk204, rep(0, 396), wk212, mossuril, wk200, wk184, muecate, wk108, wk188, wk108, murru,

wk124, nacala2, nacaroa, wk168, wk112, namapa,

wk204, wk108, wk108, wk188, wk232, wk116, wk188, wk108, wk212, wk56, wk280, wk276, rapale, ribaue, wk188, wk200, wk124, wk168, wk180, wk164, wk212, wk172)


netprotection = c(netprotection1, netprotection2)

# eliminating negative percents

netprotection[netprotection < 0] = 0

# combining net effectiveness with the existing weather and incidence data

all_int = data.frame(merge_data_lag, netprotection)


################ creating a column of percent effectiveness of the spray intervention for each district by epiweek

# a function that takes an epiweek number as an intervention start date
# and returns a vector of spray percent effectiveness for each epiweek,
# assuming that the effectiveness of the spray declines linearly, reaching 
# 75% effectiveness 6 months after the start date


sprayconverter = function(convertedEpiWeek) {
district = 0:(396 - convertedEpiWeek)
spray = c(rep(0, (convertedEpiWeek - 1)), 100 - 0.962*district)
}

length(intervention3$sprayconversion[!is.na(intervention3$sprayconversion)])

unique(intervention3$sprayconversion)

length(unique(intervention3$sprayconversion))

# 24 districts received 1 spray
# 7 districts received 2 sprays

# creating spray percent effectiveness for districts that received 2 sprays

wkS342 = sprayconverter(342)
wkS298 = sprayconverter(298)
wkS344 = sprayconverter(344)
wkS293 = sprayconverter(293)
wkS304 = sprayconverter(304)
wkS346 = sprayconverter(346)
wkS348 = sprayconverter(348)
wkS302 = sprayconverter(302)
wkS354 = sprayconverter(354)
wkS310 = sprayconverter(310)
wkS356 = sprayconverter(356)
wkS287 = sprayconverter(287)
wkS340 = sprayconverter(340)

ilhaS = c(wkS298[1:341], wkS342[342:396])
lalauaS = c(wkS293[1:343], wkS344[344:396])
malemaS = c(wkS304[1:345], wkS346[346:396])
mecontaS = c(wkS302[1:347], wkS348[348:396])
mecuburiS = c(wkS310[1:353], wkS354[354:396])
membaS = c(wkS287[1:355], wkS356[356:396])
namapaS = c(wkS302[1:339], wkS340[340:396])

# putting together the spray percent effectiveness for each district, including those that received no spray


sprayprotection = c(rep(0, (396*2)), sprayconverter(296), rep(0, (396*7)), sprayconverter(168), rep(0, 396), sprayconverter(169), 

sprayconverter(170), sprayconverter(171), rep(0, (396*11)), sprayconverter(179), rep(0, (396*9)), 

sprayconverter(173), rep(0, 396), sprayconverter(172), sprayconverter(180), rep(0, (396*2)), sprayconverter(181), 

rep(0, (396*3)), sprayconverter(182), rep(0, (396*2)), ilhaS, sprayconverter(183), sprayconverter(184), rep(0, (396*3)), 

lalauaS, rep(0, (396*5)), sprayconverter(174), rep(0, (396*7)), malemaS, sprayconverter(140), rep(0, (396*4)), sprayconverter(175), 

rep(0, 396), sprayconverter(176), sprayconverter(146), rep(0, (396*4)), sprayconverter(148), sprayconverter(152), 

sprayconverter(140), mecontaS, mecuburiS, rep(0, (396*3)), membaS, rep(0, (396*6)), sprayconverter(360),

rep(0, (396*9)), sprayconverter(177), rep(0, (396*10)), namapaS, rep(0, (396*4)), sprayconverter(178), rep(0, (396*17)))


# converting negative percentages to 0s

sprayprotection[sprayprotection < 0] = 0

# adding spray protection to the dataset

all_int = data.frame(all_int, sprayprotection)

write.table(all_int, "all_int.csv", sep = ",")
