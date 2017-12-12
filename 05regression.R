################ Vanessa Richardson
################ 6640 Fall 2017 final project
################ poisson regression

################ the data consists of weather and incidence data on 142 districts
################ each district has 396 rows in a dataframe
################ each row is associated with a particular epiyear/epiweek
################ the dataset is sorted by District, then by epiyear, then by epiweek


########## all simple regressions

library(gee)
library(MuMIn)
library(AER)

all_int = data.frame(all_int, "observations" = rep(1:396, 142))

all_int$observations = as.factor(all_int$observations)

pgt2 = gee(round(lag.incidence2) ~ 1 + tavg, id = District, data = all_int, family = poisson)
coef(summary(pgt2))
2 * pnorm(abs(coef(summary(pgt2))[,5]), lower.tail = FALSE)

pgr2 = gee(round(lag.incidence2) ~ 1 + raint, id = District, data = all_int, family = poisson)
coef(summary(pgr2))
2 * pnorm(abs(coef(summary(pgr2))[,5]), lower.tail = FALSE)

pgrh2 = gee(round(lag.incidence2) ~ 1 + rh, id = District, data = all_int, family = poisson)
coef(summary(pgrh2))
2 * pnorm(abs(coef(summary(pgrh2))[,5]), lower.tail = FALSE)

pgsd2 = gee(round(lag.incidence2) ~ 1 + sd, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgsd2))[,5]), lower.tail = FALSE)

pgps2 = gee(round(lag.incidence2) ~ 1 + psfc, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgps2))[,5]), lower.tail = FALSE)

pgnet2 = gee(round(lag.incidence2) ~ 1 + netprotection, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgnet2))[,5]), lower.tail = FALSE)

pgspr2 = gee(round(lag.incidence2) ~ 1 + sprayprotection, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgspr2))[,5]), lower.tail = FALSE)

# this one will be the same with every lag, pop_density doesn't change
pgpop2 = gee(round(lag.incidence2) ~ 1 + pop_density, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgpop2))[,5]), lower.tail = FALSE)



pgt4 = gee(round(lag.incidence4) ~ 1 + tavg, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgt4))[,5]), lower.tail = FALSE)

pgr4 = gee(round(lag.incidence4) ~ 1 + raint, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgr4))[,5]), lower.tail = FALSE)

pgrh4 = gee(round(lag.incidence4) ~ 1 + rh, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgrh4))[,5]), lower.tail = FALSE)

pgsd4 = gee(round(lag.incidence4) ~ 1 + sd, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgsd4))[,5]), lower.tail = FALSE)

pgps4 = gee(round(lag.incidence4) ~ 1 + psfc, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgps4))[,5]), lower.tail = FALSE)

pgnet4 = gee(round(lag.incidence4) ~ 1 + netprotection, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgnet4))[,5]), lower.tail = FALSE)

pgspr4 = gee(round(lag.incidence4) ~ 1 + sprayprotection, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgspr4))[,5]), lower.tail = FALSE)



pgt8 = gee(round(lag.incidence8) ~ 1 + tavg, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgt8))[,5]), lower.tail = FALSE)

pgr8 = gee(round(lag.incidence8) ~ 1 + raint, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgr8))[,5]), lower.tail = FALSE)

pgrh8 = gee(round(lag.incidence8) ~ 1 + rh, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgrh8))[,5]), lower.tail = FALSE)

pgsd8 = gee(round(lag.incidence8) ~ 1 + sd, id = District, data = all_int, family = poisson)
coef(summary(pgsd8))
2 * pnorm(abs(coef(summary(pgsd8))[,5]), lower.tail = FALSE)

pgps8 = gee(round(lag.incidence8) ~ 1 + psfc, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgps8))[,5]), lower.tail = FALSE)

pgnet8 = gee(round(lag.incidence8) ~ 1 + netprotection, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgnet8))[,5]), lower.tail = FALSE)

pgspr8 = gee(round(lag.incidence8) ~ 1 + sprayprotection, id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(pgspr8))[,5]), lower.tail = FALSE)


#### building models relating lagged incidence to the intervention data,
#### the population density data,
#### and all weather variables that aren't strongly correlated

poissongee2 = gee(round(lag.incidence2) ~ 1 + tavg + raint + pop_density +
    netprotection + sprayprotection, 
    id = District, data = all_int, family = poisson)

coef(summary(poissongee2))
# calculate the p-value from the robust z
2 * pnorm(abs(coef(summary(poissongee2))[,5]), lower.tail = FALSE) 

poissongee4 = gee(round(lag.incidence4) ~ 1 + tavg + raint + pop_density +
    netprotection + sprayprotection, 
    id = District, data = all_int, family = poisson)

coef(summary(poissongee4))
2 * pnorm(abs(coef(summary(poissongee4))[,5]), lower.tail = FALSE)


poissongee8 = gee(round(lag.incidence8) ~ 1 + tavg + raint + pop_density +
    netprotection + sprayprotection, 
    id = District, data = all_int, family = poisson)

coef(summary(poissongee8))
2 * pnorm(abs(coef(summary(poissongee8))[,5]), lower.tail = FALSE)



# models with interaction terms

poissongee22 = gee(round(lag.incidence2) ~ 1 + tavg + raint + psfc + pop_density +
    netprotection + sprayprotection + tavg*raint + netprotection*sprayprotection, 
    id = District, data = all_int, family = poisson)

coef(summary(poissongee22))
# calculate the p-value from the robust z
2 * pnorm(abs(coef(summary(poissongee22))[,5]), lower.tail = FALSE) 

poissongee44 = gee(round(lag.incidence4) ~ 1 + tavg + raint + psfc + pop_density +
    netprotection + sprayprotection + tavg*raint + netprotection*sprayprotection, 
    id = District, data = all_int, family = poisson)

coef(summary(poissongee44))
2 * pnorm(abs(coef(summary(poissongee44))[,5]), lower.tail = FALSE)


poissongee88 = gee(round(lag.incidence8) ~ 1 + tavg + raint + psfc + pop_density +
    netprotection + sprayprotection + tavg*raint + netprotection*sprayprotection, 
    id = District, data = all_int, family = poisson)

coef(summary(poissongee88))
2 * pnorm(abs(coef(summary(poissongee88))[,5]), lower.tail = FALSE)

poissongee23 = gee(round(lag.incidence2) ~ 1 + tavg + raint + pop_density +
    netprotection + sprayprotection + tavg*sprayprotection + tavg*netprotection, 
    id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(poissongee23))[,5]), lower.tail = FALSE)

poissongee24 = gee(round(lag.incidence2) ~ 1 + tavg + raint + pop_density +
    netprotection + sprayprotection + raint*sprayprotection + raint*netprotection +
    netprotection*sprayprotection + netprotection*sprayprotection*raint, 
    id = District, data = all_int, family = poisson)
2 * pnorm(abs(coef(summary(poissongee24))[,5]), lower.tail = FALSE)

# models have overdispersion

dispersiontest(poissongee2)

# comparing model fit

model.sel(poissongee2, poissongee22, rank = QIC)
model.sel(poissongee8, poissongee88, rank = QIC)
model.sel(poissongee4, poissongee44, rank = QIC)


