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

load("data/all_spl_V2.RData")

# create new column to merge vessel types "other" and "unknown"
all.spl %>%
  mutate(sail_025km_other = sail_025km_other + sail_025km_unknown,
         sail_050km_other = sail_050km_other + sail_050km_unknown,
         sail_075km_other = sail_075km_other + sail_075km_unknown,
         sail_100km_other = sail_100km_other + sail_100km_unknown,
         sail_125km_other = sail_125km_other + sail_125km_unknown) %>%
  dplyr::select(Date, jDate, F_200Hz, F_2kHz, MF, Mean_wind, Mean_wind_dir, Mean_wind_dir_cat_simple,
                contains("km_"), -contains(".st"), -contains("unknown")) %>%
  # dplyr::mutate(across(contains("sail_"), ~log(.x + 1e-20))) ->
  as.data.frame() %>%
  # dplyr::mutate(across(contains("sail_"), ~scale(.x))) %>%
  dplyr::mutate(Mean_wind = scale(Mean_wind)) ->
mdf

rm(all.spl)

m <- list() 

m[[1]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_025km_all, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[2]] <- gls(F_200Hz ~ Mean_wind+Mean_wind_dir_cat_simple + sail_025km_all, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[3]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_025km_fishing +  sail_025km_passenger + sail_025km_cargo + sail_025km_tanker + sail_025km_other, data = mdf, correlation=corCAR1(form = ~ jDate))

m[[4]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_050km_all, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[5]] <- gls(F_200Hz ~ Mean_wind+Mean_wind_dir_cat_simple + sail_050km_all, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[6]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_050km_fishing +  sail_050km_passenger + sail_050km_cargo + sail_050km_tanker + sail_050km_other, data = mdf, correlation=corCAR1(form = ~ jDate))

m[[7]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_075km_all, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[8]] <- gls(F_200Hz ~ Mean_wind+Mean_wind_dir_cat_simple + sail_075km_all, data = mdf, correlation=corCAR1(form = ~  jDate))
m[[9]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_075km_fishing +  sail_075km_passenger + sail_075km_cargo + sail_075km_tanker + sail_075km_other, data = mdf, correlation=corCAR1(form = ~ jDate))

m[[10]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_100km_all, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[11]] <- gls(F_200Hz ~ Mean_wind+Mean_wind_dir_cat_simple + sail_100km_all, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[12]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_100km_fishing +  sail_100km_passenger + sail_100km_cargo + sail_100km_tanker + sail_100km_other, data = mdf, correlation=corCAR1(form = ~ jDate))

m[[13]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_125km_all, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[14]] <- gls(F_200Hz ~ Mean_wind+Mean_wind_dir_cat_simple + sail_125km_all, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[15]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_125km_fishing +  sail_125km_passenger + sail_125km_cargo + sail_125km_tanker + sail_125km_other, data = mdf, correlation=corCAR1(form = ~ jDate))

m[[16]] <- gls(F_200Hz ~ 1, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[17]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_025km_fishing + sail_025km_cargo, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[18]] <- gls(F_200Hz ~ Mean_wind * Mean_wind_dir_cat_simple + sail_025km_fishing + sail_025km_cargo, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[19]] <- gls(F_200Hz ~ Mean_wind * Mean_wind_dir_cat_simple + sail_025km_fishing, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[20]] <- gls(F_200Hz ~ Mean_wind * Mean_wind_dir_cat_simple + sail_025km_cargo, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[21]] <- gls(F_200Hz ~ Mean_wind, data = mdf, correlation=corCAR1(form = ~ jDate))
m[[22]] <- gls(F_200Hz ~ sail_025km_cargo, data = mdf, correlation=corCAR1(form = ~ jDate))
# m[[16]] <- gls(F_200Hz ~ Mean_wind*Mean_wind_dir_cat_simple + sum.sail_all, data = mdf, correlation=corCAR1(form = ~ jDate))
# m[[17]] <- gls(F_200Hz ~ Mean_wind+Mean_wind_dir_cat_simple + sum.sail_all, data = mdf, correlation=corCAR1(form = ~ jDate))

#m[[18]] <- gls(F_200Hz ~ 1 + sail_025km.st, data = mdf, correlation=corCAR1(form = ~ jDate))

mod.names <- sapply(m, function(x) gsub("MF ~ ", "", as.character(x$call[2])))
mod.names[which(mod.names == "1")] <- "null model"

View(aictab(cand.set = m, sort = TRUE, modnames = mod.names, second.ord = FALSE, method = "ML"))

bestmod <- m[[3]]
summary(bestmod)
qqnorm(bestmod, abline = c(0,1))
plot(fitted(bestmod), residuals(bestmod)); abline(h=0, col = "red")

tab_model(bestmod,
          string.pred = "Coefficient",
          string.ci = " Conf.Int (95%)",
          string.p = "P-Value", show.aic = TRUE)
