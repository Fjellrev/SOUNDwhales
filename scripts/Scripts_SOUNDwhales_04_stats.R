##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Soundscapes project 
##
## Stats for overall SPL values 
##in relation to ship traffic
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(nlme)
library(AICcmodavg)
library(sjPlot)

#### Rationale
## 1. Determine best fit in bins 0-5, 0-10, 0-15, 0-20, 0-25, to get an indication of what is the maximum distance 
##     where the vessel traffic still says something meaningful about the SPL
## 2. For <200 Hz, the best was 0-10km. So I wanted to check if it was the 0-5 or the 5-10 that was driving that 
##     fit. I found that it was the 5-10 km.
## 3. For >200 Hz, it was 0-5km for all so I just kept that
## 4. Then, I wanted to figure out which boats had a significant effect for the bins selected (5-10km for <200Hz, 
##     and 0-5km for >200Hz).


#### Process
## Find out which vessel type is more relevant for which distance bin - biased because not all ships occur in all bins
## Create variables that account for a 0-5, 0-10, 0-15, 0-20, 0-25 km

all.spl$sail_010_All.st <- all.spl$sail_05km_All.st + all.spl$sail_10km_All.st
all.spl$sail_015_All.st <- all.spl$sail_05km_All.st + all.spl$sail_10km_All.st + all.spl$sail_15km_All.st
all.spl$sail_020_All.st <- all.spl$sail_05km_All.st + all.spl$sail_10km_All.st + all.spl$sail_15km_All.st + all.spl$sail_20km_All.st 
all.spl$sail_025_All.st <- all.spl$sail_05km_All.st + all.spl$sail_10km_All.st + all.spl$sail_15km_All.st + all.spl$sail_20km_All.st + all.spl$sail_25km_All.st 

## Find out which distance is more important for what
Modnames <- c("Ws + sail_05km_All", 
              "Ws + sail_010km_All",
              "Ws + sail_015km_All",
              "Ws + sail_020km_All",
              "Ws + sail_025km_All")


Cand.models <- list( ) 
Cand.models[[1]] <- gls(LF_200Hz ~ Mean_wind.st + sail_05km_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[2]] <- gls(LF_200Hz ~ Mean_wind.st + sail_010_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[3]] <- gls(LF_200Hz ~ Mean_wind.st + sail_015_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[4]] <- gls(LF_200Hz ~ Mean_wind.st + sail_020_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[5]] <- gls(LF_200Hz ~ Mean_wind.st + sail_025_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


#round to 4 digits after decimal point, give log-likelihood, remove AICc and use AIC instead
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE),
      digits = 4, LL = TRUE) 
summary(gls(LF_200Hz ~ Mean_wind.st + sail_010_All.st , data= all.spl, correlation=corCAR1(form = ~ jDate)))
# the interaction with wind in the above model is not significant so we remove it from the model


## do the same for LF_2khz
Cand.models <- list( ) 
Cand.models[[1]] <- gls(LF_2kHz ~ Mean_wind.st + sail_05km_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[2]] <- gls(LF_2kHz ~ Mean_wind.st + sail_010_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[3]] <- gls(LF_2kHz ~ Mean_wind.st + sail_015_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[4]] <- gls(LF_2kHz ~ Mean_wind.st + sail_020_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[5]] <- gls(LF_2kHz ~ Mean_wind.st + sail_025_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))

print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE),
      digits = 4, LL = TRUE) 
summary(gls(LF_2kHz ~ Mean_wind.st + sail_05km_All.st , data= all.spl, correlation=corCAR1(form = ~ jDate)))
# the interaction with wind in the above model is not significant so we remove it from the model


## do the same for MF
Cand.models <- list( ) 
Cand.models[[1]] <- gls(MF ~ Mean_wind.st + sail_05km_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[2]] <- gls(MF ~ Mean_wind.st + sail_010_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[3]] <- gls(MF ~ Mean_wind.st + sail_015_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[4]] <- gls(MF ~ Mean_wind.st + sail_020_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[5]] <- gls(MF ~ Mean_wind.st + sail_025_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))

print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE),
      digits = 4, LL = TRUE) 
summary(gls(MF ~ Mean_wind.st + sail_05km_All.st , data= all.spl, correlation=corCAR1(form = ~ jDate)))
# the interaction with wind in the above model is not significant so we remove it from the model


###############################################
## Now we find out which boat for those bins?
## Find out if 0-5 is better than 5-10
## LF_200
Modnames <- c("Ws + sail_05km_All", 
              "Ws + sail_10km_All")
Cand.models <- list( ) 
Cand.models[[1]] <- gls(LF_200Hz ~ Mean_wind.st + sail_05km_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[2]] <- gls(LF_200Hz ~ Mean_wind.st + sail_10km_All.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE),
      digits = 4, LL = TRUE) #5-10 is best than 0-5
summary(gls(LF_200Hz ~ Mean_wind.st + sail_10km_All.st , data= all.spl, correlation=corCAR1(form = ~ jDate)))

#Find out which boats in those bins
Modnames <- c("Ws + sail_10km_fishing",
              "Ws + sail_10km_passenger",
              "Ws + sail_10km_cargo",
              "Ws + sail_10km_tanker",
              "Ws + sail_10km_other",
              "Ws + sail_10km_fishing + sail_10km_passenger + sail_10km_cargo + sail_10km_tanker + sail_10km_other")
Cand.models <- list( ) 
Cand.models[[1]] <- gls(LF_200Hz ~ Mean_wind.st + sail_10km_fishing.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[2]] <- gls(LF_200Hz ~ Mean_wind.st + sail_10km_passenger.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[3]] <- gls(LF_200Hz ~ Mean_wind.st + sail_10km_cargo.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[4]] <- gls(LF_200Hz ~ Mean_wind.st + sail_10km_tanker.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[5]] <- gls(LF_200Hz ~ Mean_wind.st + sail_10km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[6]] <- gls(LF_200Hz ~ Mean_wind.st + sail_10km_fishing.st + sail_10km_passenger.st + sail_10km_cargo.st + sail_10km_tanker.st + sail_10km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE),
      digits = 4, LL = TRUE)

summary(gls(LF_200Hz ~ Mean_wind.st + sail_10km_fishing.st + sail_10km_passenger.st + sail_10km_cargo.st + sail_10km_tanker.st + sail_10km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate)))
# the best is the one with all vessels additive 

## So the best for LF_2kHz was 0-5 ALL. Let's find out which boats
Modnames <- c("Ws + sail_05km_fishing",
              "Ws + sail_05km_passenger",
              "Ws + sail_05km_cargo",
              "Ws + sail_05km_tanker",
              "Ws + sail_05km_other",
              "Ws + sail_05km_fishing + sail_05km_passenger + sail_05km_cargo + sail_05km_tanker + sail_05km_other")
Cand.models <- list( ) 
Cand.models[[1]] <- gls(LF_2kHz ~ Mean_wind.st + sail_05km_fishing.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[2]] <- gls(LF_2kHz ~ Mean_wind.st + sail_05km_passenger.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[3]] <- gls(LF_2kHz ~ Mean_wind.st + sail_05km_cargo.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[4]] <- gls(LF_2kHz ~ Mean_wind.st + sail_05km_tanker.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[5]] <- gls(LF_2kHz ~ Mean_wind.st + sail_05km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[6]] <- gls(LF_2kHz ~ Mean_wind.st + sail_05km_fishing + sail_05km_passenger.st + sail_05km_cargo.st + sail_05km_tanker.st + sail_05km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE),
      digits = 4, LL = TRUE)

summary(gls(LF_2kHz ~ Mean_wind.st + sail_05km_fishing.st + sail_05km_passenger.st + sail_05km_cargo.st + sail_05km_tanker.st + sail_05km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate)))



## So the best for MF was 0-5 ALL. Let's find out which boats
Modnames <- c("Ws + sail_05km_fishing",
              "Ws + sail_05km_passenger",
              "Ws + sail_05km_cargo",
              "Ws + sail_05km_tanker",
              "Ws + sail_05km_other",
              "Ws + sail_05km_fishing + sail_05km_passenger + sail_05km_cargo + sail_05km_tanker + sail_05km_other")
Cand.models <- list( ) 
Cand.models[[1]] <- gls(MF ~ Mean_wind.st + sail_05km_fishing.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[2]] <- gls(MF ~ Mean_wind.st + sail_05km_passenger.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[3]] <- gls(MF ~ Mean_wind.st + sail_05km_cargo.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[4]] <- gls(MF ~ Mean_wind.st + sail_05km_tanker.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[5]] <- gls(MF ~ Mean_wind.st + sail_05km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[6]] <- gls(MF ~ Mean_wind.st + sail_05km_fishing + sail_05km_passenger.st + sail_05km_cargo.st + sail_05km_tanker.st + sail_05km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE),
      digits = 4, LL = TRUE)

summary(gls(MF ~ Mean_wind.st + sail_05km_fishing.st + sail_05km_passenger.st + sail_05km_cargo.st + sail_05km_tanker.st + sail_05km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate)))

### Run models
m1 <- gls(LF_200Hz ~ Mean_wind.st + sail_10km_fishing.st + sail_10km_passenger.st + sail_10km_cargo.st + sail_10km_tanker.st + sail_10km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
summary(m1)
m2 <- gls(LF_2kHz ~ Mean_wind.st + sail_05km_fishing.st + sail_05km_passenger.st + sail_05km_cargo.st + sail_05km_tanker.st + sail_05km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
summary(m2)
m3 <- gls(MF ~ Mean_wind.st + sail_05km_fishing.st + sail_05km_passenger.st + sail_05km_cargo.st + sail_05km_tanker.st + sail_05km_other.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
summary(m3)

tab_model(m1,m2, m3,
          string.pred = "Coefficient",
          string.ci = " Conf.Int (95%)",
          string.p = "P-Value", show.aic = TRUE)