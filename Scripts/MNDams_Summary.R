library(sf)
library(dplyr)
library(ggplot2)

st_layers("Data/struc_mn_dams_inventory_pub.gpkg")

mndams<-st_read("Data/struc_mn_dams_inventory_pub.gpkg", layer="dam_locations")

head(mndams)

sum(na.omit(mndams$hazard)=="High")/length(mndams$hazard) # 2.6% are classified as High-Hazard
sum(na.omit(mndams$hazard)=="High")  # that is 45 of them.

sum(na.omit(mndams$hazard)=="Significant")/length(mndams$hazard) # 8.0% are "Significant"

mndams$hazard<-ifelse(is.na(mndams$hazard)==TRUE,"No Rating",mndams$hazard)

hazardGroup<- mndams %>% group_by(hazard) %>% 
  summarize(
    Dams=n()
  ) %>%
  st_drop_geometry()

hazardGroup

# reorder and plot: 
hazardGroup <- hazardGroup %>%
  mutate(hazard = factor(hazard, levels = c("No Rating", "No Hazard", "Low", "Significant", "High")))


mean(na.omit(mndams$year_completed))
median((na.omit(mndams$year_completed)))


ggplot(hazardGroup, aes(x = hazard, y = Dams)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_bw() +
  labs(x = "Hazard", y = "Number of Dams", title = "Dams by Hazard Rating")

# Make a map: 10.6% are "Significant" or "High" Hazard Dams! 

sigOrHigh<-subset(mndams,hazard=="Significant" | hazard=="High")

st_layers("Data/bdry_counties_in_minnesota.gpkg")
mn_counties<-st_read("Data/bdry_counties_in_minnesota.gpkg", layer="mn_county_boundaries")

sigOrHigh<-sigOrHigh %>%
  mutate(hazard = factor(hazard, levels = c("Significant", "High")))

ggplot() +
  geom_sf(data = mn_counties, fill = "lightgray", color = "black") +
  geom_sf(data = sigOrHigh, aes(color = hazard), size = 3) +
  scale_color_manual(values = c("Significant" = "orange", "High" = "red")) +
  theme_bw() +
  labs(title = "High and Significant Hazard Dams in Minnesota",
       color = "Hazard Level")
