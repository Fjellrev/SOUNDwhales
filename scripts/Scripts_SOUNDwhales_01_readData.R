##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Soundscapes project 
##
## Linking ship traffic to 
## hydrophone recordings
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr); library(ggplot2)

load("data/SPLvsCumSail_day.RData")
sailDF <- readRDS("data/CumulatedSailTime_all_shipTypes_5to125km.rds")


## 1 ----
  
  
  combi %>%
    dplyr::filter(Dist.bin == "cumSailTime25_h") %>%
    ggplot() + geom_point( aes(y = Mean_SPL, x=Sail.time, size = Octave_groups, colour = Date))


## 2 ----

  p <- ggplot(data = saildf) +
       geom_point(aes(x = date, y = log(cumSailTime_h), colour = radius)) +
       geom_smooth(aes(x = date, y = log(cumSailTime_h), colour = radius), span = 0.1, n =1000) +
       theme_bw() + scale_colour_viridis_d() +
       theme(panel.grid.major = element_line(color = "#00000020", size = .3),
             axis.ticks.length = unit(-0.2, "cm"),
             axis.text.x = element_text(margin = margin(t= 10, r = 30, b = 0, l = 30, unit = "pt"), size = 25),
             axis.text.y = element_text(margin = margin(t= 10, r = 10, b = 30, l = 0, unit = "pt"), size = 15),
             axis.title = element_text(size = 15)) +
       labs(x = "", y = "Daily cumulated sailing time (h)")
  
  ggsave(plot = P, "outputs/Daily_cumulated_sailing_time.png", dpi = 150, width = 30, height = 15, units = "cm", device = "png")
  
  combi %>%
    left_join(sailDF, by = c("Date" = "date")) %>%
    select(Date, Octave_groups, Mean_SPL, cumSailTime_h, radius) ->
  combi
  
  combi %>% dplyr::filter(radius%in% c("0-5 km") & Octave_groups == "200Hz") %>%
    ggplot() + geom_point( aes(y = Mean_SPL, x=cumSailTime_h, size = Date, colour = radius))
  
  