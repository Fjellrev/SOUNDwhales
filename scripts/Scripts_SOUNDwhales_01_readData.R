##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Soundscapes project
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr); library(ggplot2)

load("data/SPLvsCumSail_day.RData")

combi %>%
  dplyr::filter(Dist.bin == "cumSailTime25_h") %>%
  ggplot() + geom_point( aes(y = Mean_SPL, x=Sail.time, size = Octave_groups, colour = Date))
