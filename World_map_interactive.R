library(gganimate)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(tidyverse)
library(countrycode)
library(plotly)

world_bank <- read.csv("world_bank.csv")

str(world_bank)


######## Data Preparation #####################

world_bank <- world_bank %>%
  rename(pop=Population..total..SP.POP.TOTL.,
         gdpPercap=GDP.per.capita..PPP..constant.2017.international.....NY.GDP.PCAP.PP.KD.,
         CO2_emis=CO2.emissions..metric.tons.per.capita...EN.ATM.CO2E.PC.,
         year=ï..Time)

####### Getting continent variable ######

world_bank$continent <- countrycode(sourcevar = world_bank[, "Country.Code"],
                                    origin = "genc3c",
                                    destination = "continent")



####### Adjusting the variables ########

world_bank$pop <- as.integer(world_bank$pop)
world_bank$pop_million <- world_bank$pop/1000000


world_bank$gdpPercap <- as.numeric(world_bank$gdpPercap)
world_bank$gdpPercap_1000 <- world_bank$gdpPercap/1000



world_bank$year <- as.integer(world_bank$year)

world_bank$CO2_emis <- as.numeric(world_bank$CO2_emis)

world_bank_complete <- na.omit(world_bank)


####### PLOT ########

#### Add hover variable (used later) ####
world_bank_complete <- world_bank_complete %>%
  mutate(hover = paste(Country.Name, "\n",
                       lapply(world_bank_complete[,5], round, 2), "tons per capita", "\n",
                       lapply(world_bank_complete[,10], round,2), "GDP per capita in 1000$"))




world_co2_graph <- plot_geo(world_bank_complete,
                            frame =~year) %>%
  add_trace(locations = ~Country.Code,
            z=~CO2_emis,
            zmin=0,
            zmax=20,
            color=~CO2_emis,
            text=~hover,
            hoverinfo=~"text",
            colorscale="Inferno") %>%
  layout(geo=list(projection = list( type = "orthographic")),
         font = list(family="Rubik Black", size=11),
    title="CO2 emission in metric tons per capita 2006-2018")%>%
  colorbar(title="CO2 in t") %>%
  config(displayModeBar=FALSE)
            

world_co2_graph


