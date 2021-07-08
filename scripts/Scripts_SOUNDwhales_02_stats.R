##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Soundscapes project 
##
## Stats for "whale free" SPL values 
##in relation to ship traffic
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(tidyverse)
library(kableExtra)
library(nlme)
library(AICcmodavg)
library(sjPlot)


# create new column to merge vessel types "other" and "unknown"
all.spl <- all.spl %>%
  rowwise() %>%
  mutate(sail_025km_other2.st = sum(sail_025km_other.st,sail_025km_unknown.st),
         sail_050km_other2.st = sum(sail_050km_other.st,sail_050km_unknown.st),
         sail_075km_other2.st = sum(sail_075km_other.st,sail_075km_unknown.st),
         sail_100km_other2.st = sum(sail_100km_other.st,sail_100km_unknown.st),
         sail_125km_other2.st = sum(sail_125km_other.st,sail_125km_unknown.st))

load("data/all_spl_V2.RData") #already has the above included, but this is just to show how it came about if we need to change

#############################
##create a vector of names to trace back models in set
## This names should match what is included in the models tested

#Modnames <- paste("model", 1:length(Cand.models), sep = " ") #either use this to just have model numbering, or the below

Modnames <- c("Ws * Wd + S25_all", "Ws + Wd + S25_all", "Ws * Wd + S25_fishing + S25_passenger + S25_cargo + S25_tanker + S25_other", 
              
              "Ws * Wd + S50_all", "Ws + Wd + S50_all", "Ws * Wd + S50_fishing + S50_passenger + S50_cargo + S50_tanker + S50_other ", 
              
              "Ws * Wd + S75_all", "Ws + Wd + S75_all", "Ws * Wd + S75_fishing + S75_passenger + S75_cargo + S75_tanker + S75_other", 
              
              "Ws * Wd + S100_all", "Ws + Wd + S100_all", "Ws * Wd + S100_fishing + S100_passenger + S100_cargo + S100_tanker + S100_other", 
              
              "Ws * Wd + S125_all", "Ws + Wd + S125_all", "Ws * Wd + S125_fishing + S125_passenger + S25_cargo + S25_tanker + S125_other", 
              
              "Ws * Wd + Ssum", "Ws + Wd + Ssum")


############
# 200 Hz ###


Cand.models <- list( ) 

Cand.models[[1]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_025km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[2]] <- gls(F_200Hz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_025km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[3]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_025km_fishing.st +  sail_025km_passenger.st + sail_025km_cargo.st + sail_025km_tanker.st + sail_025km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[4]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_050km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[5]] <- gls(F_200Hz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_050km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[6]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_050km_fishing.st +  sail_050km_passenger.st + sail_050km_cargo.st + sail_050km_tanker.st + sail_050km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[7]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_075km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[8]] <-gls(F_200Hz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_075km_all.st, data= all.spl, correlation=corCAR1(form = ~  jDate))
Cand.models[[9]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_075km_fishing.st +  sail_075km_passenger.st + sail_075km_cargo.st + sail_075km_tanker.st + sail_075km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[10]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_100km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[11]] <- gls(F_200Hz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_100km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[12]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_100km_fishing.st +  sail_100km_passenger.st + sail_100km_cargo.st + sail_100km_tanker.st + sail_100km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[13]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_125km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[14]] <- gls(F_200Hz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_125km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[15]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_125km_fishing.st +  sail_125km_passenger.st + sail_125km_cargo.st + sail_125km_tanker.st + sail_125km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[16]] <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sum.sail_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[17]] <- gls(F_200Hz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sum.sail_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))

#Cand.models[[18]] <- gls(F_200Hz ~ 1 + sail_025km.st, data= all.spl, correlation=corCAR1(form = ~ jDate))



##round to 4 digits after decimal point, give log-likelihood, remove AICc and use AIC instead
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE),
      digits = 4, LL = TRUE) 

##view the table in Viewer (rather than in console) and save as html
aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE) %>%
  kbl(caption = "< 200 Hz") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>% 
  save_kable(file = "table1_200.html", self_contained = TRUE)

############
# 2 kHz ###


Cand.models <- list( ) 

Cand.models[[1]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_025km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[2]] <- gls(F_2kHz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_025km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[3]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_025km_fishing.st +  sail_025km_passenger.st + sail_025km_cargo.st + sail_025km_tanker.st + sail_025km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[4]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_050km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[5]] <- gls(F_2kHz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_050km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[6]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_050km_fishing.st +  sail_050km_passenger.st + sail_050km_cargo.st + sail_050km_tanker.st + sail_050km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[7]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_075km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[8]] <-gls(F_2kHz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_075km_all.st, data= all.spl, correlation=corCAR1(form = ~  jDate))
Cand.models[[9]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_075km_fishing.st +  sail_075km_passenger.st + sail_075km_cargo.st + sail_075km_tanker.st + sail_075km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[10]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_100km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[11]] <- gls(F_2kHz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_100km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[12]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_100km_fishing.st +  sail_100km_passenger.st + sail_100km_cargo.st + sail_100km_tanker.st + sail_100km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[13]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_125km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[14]] <- gls(F_2kHz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_125km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[15]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_125km_fishing.st +  sail_125km_passenger.st + sail_125km_cargo.st + sail_125km_tanker.st + sail_125km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[16]] <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sum.sail_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[17]] <- gls(F_2kHz ~ Mean_wind.st+Mean_wind_dir_cat_simple + sum.sail_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))

#Cand.models[[18]] <- gls(F_2kHz ~ 1 + sail_025km.st, data= all.spl, correlation=corCAR1(form = ~ jDate))

##round to 4 digits after decimal point, give log-likelihood, remove AICc and use AIC instead
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE),
      digits = 4, LL = TRUE) 

##view the table in Viewer (rather than in console) and save as html
aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE) %>%
  kbl(caption = "200 - 2000 kHz") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>% 
  save_kable(file = "table2_2000.html", self_contained = TRUE)


############
# MF ###


Cand.models <- list( ) 

Cand.models[[1]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_025km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[2]] <- gls(MF ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_025km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[3]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_025km_fishing.st +  sail_025km_passenger.st + sail_025km_cargo.st + sail_025km_tanker.st + sail_025km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[4]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_050km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[5]] <- gls(MF ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_050km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[6]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_050km_fishing.st +  sail_050km_passenger.st + sail_050km_cargo.st + sail_050km_tanker.st + sail_050km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[7]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_075km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[8]] <-gls(MF ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_075km_all.st, data= all.spl, correlation=corCAR1(form = ~  jDate))
Cand.models[[9]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_075km_fishing.st +  sail_075km_passenger.st + sail_075km_cargo.st + sail_075km_tanker.st + sail_075km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[10]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_100km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[11]] <- gls(MF ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_100km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[12]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_100km_fishing.st +  sail_100km_passenger.st + sail_100km_cargo.st + sail_100km_tanker.st + sail_100km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[13]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_125km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[14]] <- gls(MF ~ Mean_wind.st+Mean_wind_dir_cat_simple + sail_125km_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[15]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_125km_fishing.st +  sail_125km_passenger.st + sail_125km_cargo.st + sail_125km_tanker.st + sail_125km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))


Cand.models[[16]] <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sum.sail_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
Cand.models[[17]] <- gls(MF ~ Mean_wind.st+Mean_wind_dir_cat_simple + sum.sail_all.st, data= all.spl, correlation=corCAR1(form = ~ jDate))

#Cand.models[[18]] <- gls(MF ~ 1 + sail_025km.st, data= all.spl, correlation=corCAR1(form = ~ jDate))

##round to 4 digits after decimal point, give log-likelihood, remove AICc and use AIC instead
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE),
      digits = 4, LL = TRUE) 

##view the table in Viewer (rather than in console) and save as html
aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, second.ord = FALSE) %>%
  kbl(caption = "> 2 kHz") %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>% 
  save_kable(file = "table3_MF.html", self_contained = TRUE)


#####################################
##### RUN MODELS

m1 <- gls(F_200Hz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_025km_fishing.st +  sail_025km_passenger.st + sail_025km_cargo.st + sail_025km_tanker.st + sail_025km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
summary(m1)

m2 <- gls(F_2kHz ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_025km_fishing.st +  sail_025km_passenger.st +  sail_025km_cargo.st + sail_025km_tanker.st + sail_025km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
summary(m2)

m3 <- gls(MF ~ Mean_wind.st*Mean_wind_dir_cat_simple + sail_075km_fishing.st +  sail_075km_passenger.st + sail_075km_cargo.st + sail_075km_tanker.st + sail_075km_other2.st, data= all.spl, correlation=corCAR1(form = ~ jDate))
summary(m3)

## Save model output as html table

## first check how it looks
tab_model(m1,m2, m3,
          string.pred = "Coefficient",
          string.ci = " Conf.Int (95%)",
          string.p = "P-Value", show.aic = TRUE)

## then change the labels to match and save as html
tab_model(m1,m2,m3, 
          pred.labels = c("Intercept", "Wind speed", 
                          "Wind direction (N-E)","Wind direction (S-W)", "Wind direction (W-N)",  
                          "Sailing time 0-25 km fishing", "Sailing time 0-25 km passenger", "Sailing time 0-25 km cargo", "Sailing time 0-25 km tanker", "Sailing time 0-25 km other",
                          "Wind speed:Wind direction (N-E)", "Wind speed:Wind direction (S-W)", 
                          "Wind speed:Wind direction (W-N)", 
                          "Sailing time 50-75 km fishing", "Sailing time 50-75 km passenger", "Sailing time 50-75 km cargo", "Sailing time 50-75 km tanker", "Sailing time 50-75 km other"),
          dv.labels = c("< 200 Hz", "200 to 2000 Hz", "> 2000 Hz"),
          string.pred = "Coeffcient",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value",
          file="outputs/Statistics_GLS_Results.html")

#####################################
##### DIAGNOSTIC PLOTS

## The residuals "bounce randomly" around the 0 line. This suggests that the assumption that the relationship is linear is reasonable.
## The residuals roughly form a "horizontal band" around the 0 line. This suggests that the variances of the error terms are equal.
## No one residual "stands out" from the basic random pattern of residuals. This suggests that there are no outliers.

plot(m1, abline=0)


## normality and skewness
##If the skewness is between -0.5 and 0.5, the data are fairly symmetrical.
##If the skewness is between -1 and -0.5 (negatively skewed) or between 0.5 and 1 (positively skewed), the data are moderately skewed.
##If the skewness is less than -1 (negatively skewed) or greater than 1(positively skewed), the data are highly skewed.

hist(residuals(m1), breaks=5, freq=FALSE)
hist(residuals(m1), breaks=10, freq=FALSE)
library(moments)
print(skewness(residuals(m1)))
