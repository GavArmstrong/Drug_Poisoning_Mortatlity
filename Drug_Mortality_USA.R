library(tidyverse)
library(magrittr)

library(ggplot2)
library(sf)
library(maps)

library(gganimate)

library(timeR)

library(RColorBrewer)

library(extrafont)
#font_import()

rm(list = ls())

# Set work directory
#setwd("Google Drive/Projects/Drug Poisoning Mortatlity/")

# Read data
Drug <- read_csv(file="NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv")

# Use the Black and White as a base for maps
#theme_set(theme_bw())

# Read USA multipolygons from the Maps package
USA <- st_as_sf(maps::map("usa", plot=FALSE, fill=TRUE)) %>%
  mutate(ID = str_to_title(ID))

# Read State multipolygons from the Maps package
States <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  mutate(ID = str_to_title(ID)) %>%
  as_tibble() %>%
  rename(State = ID)

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
         MBDR = `Model-based Death Rate`) %>%
  mutate(MBDR = as.numeric(MBDR))

County_Drug %<>% filter(Year %in% c(2003,
                                    2004,
                                    2005,
                                    #2006,
                                    2007,
                                    #2008,
                                    2009,
                                    #2010,
                                    2011,
                                    #2012,
                                    2013,
                                    2014,
                                    2015,
                                    2016,
                                    2017,
                                    2018)) %>%
  as_tibble()

# Customize the palette colors
my_palette <- brewer.pal(n=9, name="GnBu")[4:9]
my_palette[6] <- "#102535"
my_palette[7] <- "#01101e"

#limit <- c(min(County_Drug$MBDR)-20.0, max(County_Drug$MBDR))

# Plotting the data
First_Plot <- ggplot(data=USA) +
  geom_sf(fill = my_palette[1],
          lwd=0,
          color="black") +
  geom_sf(data=County_Drug,
                        aes(geometry=geom,
                            fill = MBDR),
                        lwd=0,
                        color=NA) +
  scale_fill_gradientn(colors=my_palette
                       #labels=list("0", "25", "50", "75", "100")
                       ) +
  guides(fill = guide_colorbar(ticks=FALSE)) +
  geom_sf(data=States,
          fill=NA,
          lwd=1,
          aes(geometry=geom)) +
   ggtitle("Drug Poisoning Mortality Rate by County ({frame_time})",
           subtitle = "Source: https://www.cdc.gov/nchs/data-visualization/drug-poisoning-mortality/") +
  labs(fill="Deaths per\n100,000 people") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "#0e231f"),
        plot.background = element_rect(fill = "#0e231f"),
        legend.title = element_text(color="white",
                                    family="Arial Rounded MT Bold",
                                    size=30),
        legend.text = element_text(color="white",
                                   family="Arial",
                                   size=30,
                                   hjust=1),
        legend.key.height = unit(2.1,"cm"),
        legend.key.width = unit(1.3,"cm"),
        legend.justification = c(1,0),
        legend.position = c(0.98,0.04),
        legend.background = element_blank(),
        plot.title = element_text(color="white",
                                  family="Arial Rounded MT Bold",
                                  size=50),
        plot.subtitle = element_text(color="white",
                                     family="Arial",
                                     size=30,
                                     vjust=-1),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  transition_time(Year)

Timer <- createTimer(precision = "ms")
Timer$start("Event 1")

# We have 16 years of data
animate(First_Plot, width=1800, height=1150,
        duration=6, fps=5,
        start_pause=5, end_pause=9)

anim_save("Drug Mortality by US County 2.gif")

Timer$stop("Event 1")

#ggsave("Drug Mortality by County.png", device="png")
