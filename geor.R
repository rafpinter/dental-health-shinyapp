install.packages("geobr")
library(geobr)
library(tidyverse)
library(plotly)

# Read specific municipality at a given year
mun <- read_municipality(code_muni=1200179, year=2017)

# Read all municipalities of given state at a given year
mun <- read_municipality(code_muni=33, year=2010) # or
mun <- read_municipality(code_muni="RJ", year=2010)

# Read all municipalities in the country at a given year
mun <- read_municipality(code_muni="all", year=2018)


state <- read_state(code_state="SC", year=2018)
state

city <- read_municipality(code_muni =  4205407)
city

loc <- read_health_facilities()
loc_SC <- loc[loc$abbrev_state == "SC",]
loc_SC

meso <- read_intermediate_region(year=2017)
states <- read_state(year=2019)

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())


# Plot all Brazilian states
plot <- ggplot() +
  geom_sf(data=state, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  no_axis +
  geom_sf(data = loc_SC, size = 2, shape = 20, color = "black")

plot

ggplotly(plot)
  #geom_point(data = loc, aes(x = , y = ))


