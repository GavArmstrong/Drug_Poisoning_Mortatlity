US Drug Poisoning Mortatlity by County 2003 - 2018
==================================================

How have drug poisoning mortatlity rates in the US changed over recent
years?

The Center for Disease Control (CDC) provides estimates for the drug
poisoning mortality rates in the US on national, state, and county
levels
(<https://www.cdc.gov/nchs/data-visualization/drug-poisoning-mortality/>).

    ## # A tibble: 6 x 12
    ##    FIPS  Year State `FIPS State` County Population `Model-based De~
    ##   <dbl> <dbl> <chr>        <dbl> <chr>       <dbl>            <dbl>
    ## 1  1001  2003 Alab~            1 Autau~      46800             6.40
    ## 2  1001  2004 Alab~            1 Autau~      48366             6.49
    ## 3  1001  2005 Alab~            1 Autau~      49676             6.37
    ## 4  1001  2006 Alab~            1 Autau~      51328             7.71
    ## 5  1001  2007 Alab~            1 Autau~      52405             8.06
    ## 6  1001  2008 Alab~            1 Autau~      53277             8.26
    ## # ... with 5 more variables: `Standard Deviation` <dbl>, `Lower Confidence
    ## #   Limit` <dbl>, `Upper Confidence Limit` <dbl>, `Urban/Rural Category` <chr>,
    ## #   `Census Division` <dbl>

We want an effective method of displaying this data so that trends in
drug mortatlities become apparent. One way to achieve this is using a
choropleth (a thematic map displaying statistical information). There
are many different packages capable of creating choropleths in R.

In this case we will display the drug mortatlity rate ("Model-based
Death Rate") for each county of the USA over the period 2003 to 2018
using the county maps available in the "maps" package.

    Counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) %>%
      mutate(ID = as.character(ID)) %>%
      left_join(maps::county.fips, by=c("ID"="polyname"))

Using the county FIPS numbers available in both tables we can join the
drug mortality rates to the map data:

    County_Drug <- Counties %>%
      left_join(Drug %>%
                  select(-County, -State),
                by=c("fips"="FIPS")) %>%
      select(Year,
             geom,
             MBDR = `Model-based Death Rate`)

Now that we combined our data we can start to form our plot.

    Main_Plot <- ggplot(data=USA) +
      geom_sf(fill = my_palette[1],
              lwd=0,
              color="black") +
      geom_sf(data=County_Drug,
                            aes(geometry=geom,
                                fill = MBDR),
                            lwd=0,
                            color=NA) +
      scale_fill_gradientn(colors=my_palette) +
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

We want to make a gif from these plots that transitions over the Year
variable. This can be achieved using the "animate"-function from the
gganimate-package.

    animate(Main_Plot, width=1800, height=1800,
            duration=6, fps=5,
            start_pause=5, end_pause=9)

    anim_save("Drug Mortality by US County.gif")

<img src="Drug Mortality by US County.gif"/>
