library(tidyverse)
library(magrittr)
library(maps)
library(sf)
library(readxl)
library(viridis)
library(extrafont)

font_import()

library(ggplot2)

rm(list = ls())

# Set work directory
#setwd("Google Drive/Projects/Drug Poisoning Mortatlity/")

# Read data
Drug <- read_csv(file="NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv")

# Void theme for maps
theme_set(theme_bw())

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


# Simple plot of State boundaries
ggplot(USA) +
  geom_sf(fill = "antiquewhite1",
          lwd=0,
          color="black") +
  geom_sf(data=States,
          aes(geometry=geom),
          lwd=0.2,
          color="#002240") +
  geom_sf(data=Counties,
          aes(geometry=geom),
          lwd=0.1,
          color="#4C974C")

ggsave("Cause of Death by County.png", device="png")
