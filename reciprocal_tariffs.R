

rm(list=ls())

library(readr)
library(ggplot2)
library(tidyverse)
library(tidygeocoder)

## read in data
tariffs <- readxl::read_excel("/Users/jackconnors/Downloads/trade.xlsx")

eu_members <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
  "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
  "Spain", "Sweden"
)

# Extract the EU row
eu_row <- tariffs %>% filter(Country == "EU")

# Start with region & country info
eu_expanded <- tibble(Country = eu_members) %>%
  #mutate(Country = region) %>%
  mutate(
    `Goods Imported` = eu_row$`Goods Imported`,
    `Goods Exported` = eu_row$`Goods Exported`,
    Ratio = eu_row$Ratio,
    `Proposed Tariff` = eu_row$`Proposed Tariff`,
    `formula tariff` = eu_row$`formula tariff`
  )

# Overwrite country name with actual country name
tariffs_cleaned <- tariffs %>%
  filter(Country != "EU") %>%
  bind_rows(eu_expanded)

### rename country to region
tariffs_region <- tariffs_cleaned %>% 
  rename(region = Country) %>% 
  mutate(Ratio = Ratio * 100)


## define map grid
wrld <- ggplot2::map_data("world")

## merge data
world_tariffs <- wrld %>% 
  left_join(tariffs_region, by= "region")

library(showtext)
font_add_google(name = "Roboto Slab", family = "roboto-slab")

### final plot
ggplot() +
  geom_polygon(data = world_tariffs, aes(long, lat, group = group, 
                                         fill = Ratio),
               color= "white", linewidth=0.4) +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "grey90",
                       breaks = c(-100, 0, 80),     
                       labels = c("-100", "0", "80")) +
coord_map("mollweide")  +
  theme_void() +
  labs(
    title = "US Trade Deficits",
    subtitle = "% of Goods Imported to Exported by Country",
    fill = "Deficit %",
    caption = "Source: White House"
  ) +
theme(
  plot.title = element_text(size=24, face="bold", hjust=0.5,
                            family = "roboto-slab"),
  plot.subtitle = element_text(size=20, face="bold", hjust=0.5,
                               family = "roboto-slab"),
  plot.caption = element_text(size=14, family = "roboto-slab"),
  legend.position = c(0.2, 0.4),
 # plot.background = element_rect(fill="black"),
  legend.title = element_text(size=16,family = "roboto-slab"),
  legend.text = element_text(size=14, family = "roboto-slab")
)

### calc how many countries run a surplus against us
  ## 35 of our 48 trading partners run a surplus against us
  ## 72 % of our trade partners export more to us than they import

### number of trading partners
dim(tariffs)

## number of countries with trade deficits
tariffs %>% 
  mutate(deficit = ifelse(Ratio > 0, 1, 0)) %>% 
  reframe(
    countries_w_deficit = sum(deficit)
  )
  
