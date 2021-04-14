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
sailDF <- readRDS("data/CumulatedSailTime_5to125km.rds")

combi %>%
  left_join(sailDF, by = c("Date" = "date")) %>%
  select(Date, Octave_groups, Mean_SPL, cumSailTime_h, radius, vesselType) ->
combi

types <- c("all_shipTypes", "cargo", "fishing", "other", "passenger", "tanker", "unknown")
type  <- types[6]

       sailDF %>% filter(vesselType == type) %>%
       ggplot() +
       geom_point(aes(x = date, y = (cumSailTime_h), colour = radius)) +
       geom_smooth(aes(x = date, y = (cumSailTime_h), colour = radius), span = 0.1, n =1000) +
       theme_bw() + scale_colour_viridis_d() +
       theme(panel.grid.major = element_line(color = "#00000020", size = .3),
             axis.ticks.length = unit(-0.2, "cm"),
             axis.text.x = element_text(margin = margin(t= 10, r = 30, b = 0, l = 30, unit = "pt"), size = 25),
             axis.text.y = element_text(margin = margin(t= 10, r = 10, b = 30, l = 0, unit = "pt"), size = 15),
             axis.title = element_text(size = 15)) +
       labs(title = paste0("vessel type: ", type), x = "", y = "Daily cumulated sailing time (h)")
  
  ggsave(plot = P, "outputs/Daily_cumulated_sailing_time.png", dpi = 150, width = 30, height = 15, units = "cm", device = "png")

  combi %>% dplyr::filter(radius%in% c("5-10 km") & vesselType == "unknown") %>%
    ggplot() + geom_point( aes(y = Mean_SPL, x=cumSailTime_h, size = Date, colour = Octave_groups))

# view everything as facets to identify types of vessels that may contribute to higher noise levels (fishing and passenger vessels clearly do)
  combi %>% 
    dplyr::filter(radius%in% c("5-10 km")) %>% 
    ggplot()+
      geom_point(aes(y=Mean_SPL, x=cumSailTime_h, color=Octave_groups)) +
      geom_smooth(aes(y=Mean_SPL, x=cumSailTime_h, color=Octave_groups), method="glm") +
      facet_wrap(.~vesselType, ncol= 3)
    
# lm
    summary(lm (Mean_SPL ~ cumSailTime_h * vesselType, data=combi[combi$radius%in%c("5-10 km"),]))
    summary(lm (Mean_SPL ~ cumSailTime_h * vesselType, data=combi[combi$radius%in%c("0-5 km"),]))
    
    
# strangely, fishing gets very small values and passenger gives a negative coefficient?
  #one thing that would be relevant here would be to test the multivariate response
    # Mean_SPL * Octave_groups
  