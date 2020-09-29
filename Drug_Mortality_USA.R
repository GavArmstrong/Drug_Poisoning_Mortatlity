library(tidyverse)
library(magrittr)
library(readxl)
library(viridis)

library(ggplot2)
library(sf)
library(maps)

library(gganimate)

library(timeR)

#library(extrafont)
#font_import()

rm(list = ls())

# Set work directory
#setwd("Google Drive/Projects/Drug Poisoning Mortatlity/")

# Read data
Drug <- read_csv(file="NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv")

# Use the Black and White as a base for maps
theme_set(theme_bw())

# Read USA multipolygons from the Maps package
USA <- st_as_sf(maps::map("usa", plot=FALSE, fill=TRUE)) %>%
  mutate(ID = str_to_title(ID))

# # Read State multipolygons from the Maps package
# States <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
#   mutate(ID = str_to_title(ID)) %>%
#   as_tibble() %>%
#   rename(State = ID)

# Read County multipolygons from the Maps package
Counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) %>%
  mutate(ID = as.character(ID)) %>%
  left_join(maps::county.fips, by=c("ID"="polyname")) %>%
  separate(col="ID", into=c("State", "County"), sep=",", extra="merge", fill="left") %>%
  mutate_at(vars(State, County), str_to_title) %>%
  as_tibble()

# Patch missing fips
Counties %<>% mutate(fips = if_else(State == "Florida" & County == "Okaloosa",
                                    as.integer(12091),
                                    fips),
                     fips = if_else(State == "Louisiana" & County == "St Martin",
                                    as.integer(22099),
                                    fips),
                     fips = if_else(State == "North Carolina" & County == "Currituck",
                                    as.integer(37053),
                                    fips),
                     fips = if_else(State == "Texas" & County == "Galveston",
                                    as.integer(48167),
                                    fips),
                     fips = if_else(State == "Virginia" & County == "Accomack",
                                    as.integer(51001),
                                    fips),
                     fips = if_else(State == "Washington" & County == "Pierce",
                                    as.integer(53053),
                                    fips),
                     fips = if_else(State == "Washington" & County == "San Juan",
                                    as.integer(53055),
                                    fips))

# Join the drug data to Counties
County_Drug <- Counties %>%
  left_join(Drug %>%
              select(-County, -State),
            by=c("fips"="FIPS")) %>%
  select(Year,
         geom,
         MBDR = `Model-based Death Rate`)

County_Drug %<>% filter(Year %in% c(2003, 2004, 2005, 2006, 2009, 2012, 2015, 2018))


# Plotting the data
First_Plot <- ggplot(data=USA) +
  geom_sf(fill = "#472A7A",
          lwd=0,
          color="black") +
  geom_sf(data=County_Drug,
          aes(geometry=geom,
              fill = MBDR),
          lwd=0,
          color=NA) +
  scale_fill_viridis() +
  ggtitle("Drug Mortality Rate ({frame_time})") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "#000f0e"),
        plot.background = element_rect(fill = "#000f0e"),
        plot.title = element_text(color="white",
                                  size=20),
        legend.position = "none") +
  transition_time(Year)

Timer <- createTimer(precision = "ms")
Timer$start("Event 1")

# We have 16 years of data
animate(First_Plot, width=1800, height=1800, duration=4, fps=10, nframes=32)
anim_save("First_Anim.gif")

Timer$stop("Event 1")

# Simple plot of State boundaries
# ggplot(USA) +
#   geom_sf(fill = "#451954",
#           lwd=0,
#           color="black") +
#   geom_sf(data=Counties,
#           aes(geometry=geom,
#               fill = `Model-based Death Rate`),
#           lwd=0,
#           color="white") +
#   scale_fill_viridis() +
#   transition_time(Year) +
#   ease_aes('linear') +
#   theme(axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.position = "none")

#ggsave("Drug Mortality by County.png", device="png")
