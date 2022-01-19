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
library(MASS)

load("data/all_spl_V3.RData")

# create new column to merge vessel types "other" and "unknown"
all_spl %>%
  mutate(sail_05km_other = sail_05km_other + sail_05km_unknown,
         sail_10km_other = sail_10km_other + sail_10km_unknown,
         sail_15km_other = sail_15km_other + sail_15km_unknown,
         sail_20km_other = sail_20km_other + sail_20km_unknown,
         sail_25km_other = sail_25km_other + sail_25km_unknown) %>%
  dplyr::select(Date, jDate, LF_200Hz, LF_2kHz, MF, Mean_wind, Mean_wind_dir, Mean_wind_dir_cat_simple, PA,
                contains("km_"), -contains(".st"), -contains("unknown")) %>%
  dplyr::mutate(across(contains("sail_"), ~log10(.x + 0.001))) %>%
  as.data.frame() %>%
  # dplyr::mutate(across(contains("sail_"), ~scale(.x))) %>%
  dplyr::mutate(Mean_wind = scale(Mean_wind)) %>%
  ungroup()->
mdf



m <- list() 

m[[1]] <- gls(LF_2kHz ~ Mean_wind*Mean_wind_dir_cat_simple + sail_25km_All + sail_05km_All + sail_15km_All + sail_10km_All + sail_20km_All  + PA, data= mdf, correlation=corCAR1(form = ~ jDate), method = "ML")
m[[2]] <- gls(LF_2kHz ~ Mean_wind*Mean_wind_dir_cat_simple , data = all_spl, correlation=corCAR1(form = ~ jDate), method = "ML")
m[[3]] <- gls(LF_2kHz ~ Mean_wind , data = all_spl, correlation=corCAR1(form = ~ jDate), method = "ML")
m[[4]] <- gls(LF_2kHz ~ 1, data = all_spl, correlation=corCAR1(form = ~ jDate), method = "ML")
m[[5]] <- gls(LF_2kHz ~ Mean_wind + sail_25km_All + PA, data= mdf, correlation=corCAR1(form = ~ jDate), method = "ML")
m[[6]] <- gls(LF_2kHz ~ Mean_wind + sail_10km_All, data= mdf, correlation=corCAR1(form = ~ jDate), method = "ML")

  
  stepAIC(m[[1]])

mod.names <- sapply(m, function(x) gsub("LF_2kHz ~ ", "", as.character(x$call[2])))
mod.names[which(mod.names == "1")] <- "null model"

View(aictab(cand.set = m, sort = TRUE, modnames = mod.names, second.ord = FALSE, method = "ML"))

bestmod <- m[[6]]
summary(bestmod)
qqnorm(bestmod, abline = c(0,1))
plot(fitted(bestmod), residuals(bestmod)); abline(h=0, col = "red")

tab_model(bestmod,
          string.pred = "Coefficient",
          string.ci = " Conf.Int (95%)",
          string.p = "P-Value", show.aic = TRUE)
