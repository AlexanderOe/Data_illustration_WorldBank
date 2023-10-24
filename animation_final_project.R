library(gganimate)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(tidyverse)
library(countrycode)

########### Loading & Checking Data #######
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



##### Starting to Plot ######
world_bank_graph <- world_bank_complete %>% 
  ggplot(aes(x=gdpPercap_1000, y=CO2_emis, color=continent, size=pop_million, label=Country.Name)) +
  geom_point(alpha=0.7) +
  geom_text(data = subset(world_bank_complete, 
                          Country.Code=="USA"|Country.Code=="CHN"),
            show.legend = FALSE)+
  theme_fivethirtyeight() +
  guides(color = guide_legend(override.aes = list(size = 4)))+
  labs(title = expression("CO2 Emission with GDP per Capita by country"),
       x = "GDP per capita",
       y="CO2 emission per capita",
       caption = "Source: World Bank",
       color="Continent",
       size="Population in Mio")+
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text = element_text(family ="Rubik",size = 11),
        legend.key.size = unit(.5, "cm")) +
  scale_color_brewer(palette = "Set2")
world_bank_graph

world_bank_graph.animation <- world_bank_graph +
  transition_time(year) +
  labs(subtitle =" Year: {frame_time}")

world_bank_graph.animation


