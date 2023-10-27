Visit the Website: https://alexanderoe.github.io/Data_illustration_WorldBank/ to see the illustrations and interactive graphs

# The Environmental Kuznets Curve - A data analysis
The subsequent investigation is related to the hypothesis of the Environmental Kuznets Curve (EKC) which states that emissions decline with increasing GDP as soon as a certain economic wealth is reached. Hence, it assumes a quadratic relationship between GDP and emission.   
If this hypothesis holds, the established saying of "Green growth" might be feasible. However, the following analysis of World Bank data from 2006-2018 indicates that the EKC hypothesis has to be rejected.  
The investigation is structured as follows. First, there is an animated graph followed by an interactive world map which represents a descriptive analysis to visualize the underlying data. It is continued by a regression analysis and a brief machine learning chapter two show possible examination methods.  


# Animated Graph

```{r packgraph, include=FALSE}
library(gganimate)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(tidyverse)
library(countrycode)
library(plotly)
```


Used Packages - gganimate, ggplot2, dplyr, ggthemes, tidyverse, countrycode, plotly
```{r animation, warning=FALSE}
########### Loading & Checking Data #######
world_bank <- read.csv("world_bank.csv")



######## Data Preparation #####################

world_bank <- world_bank %>%
  rename(pop=Population..total..SP.POP.TOTL.,
         gdpPercap=GDP.per.capita..PPP..constant.2017.international.....NY.GDP.PCAP.PP.KD.,
         CO2_PerCap=CO2.emissions..metric.tons.per.capita...EN.ATM.CO2E.PC.,
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

world_bank$CO2_PerCap <- as.numeric(world_bank$CO2_PerCap)

world_bank_complete <- na.omit(world_bank)


###### starting the plot ######
world_bank_graph <- world_bank_complete %>% 
  ggplot(aes(x=gdpPercap_1000, y=CO2_PerCap, color=continent, size=pop_million, label=Country.Name)) +
  geom_point(alpha=0.7) +
  geom_text(data = subset(world_bank_complete, 
                          Country.Code=="USA"|Country.Code=="CHN"),
            show.legend = FALSE)+
  theme_fivethirtyeight() +
  guides(color = guide_legend(override.aes = list(size = 4)))+
  labs(title = expression("CO2 Emission with GDP per Capita by Country"),
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
```


# Spatial Graph

```{r spatial, warning=FALSE}
####### PLOT ########

#### Add hover variable (used later) ####
world_bank_complete <- world_bank_complete %>%
    mutate(hover = paste(Country.Name, "\n",
                       lapply(world_bank_complete[,5], round, 2), "CO2 tons per capita", "\n",
                       lapply(world_bank_complete[,10], round,2), "GDP per capita in 1000$"))




world_co2_graph <- plot_geo(world_bank_complete,
                            frame =~year) %>%
  add_trace(locations = ~Country.Code,
            z=~CO2_PerCap,
            zmin=0,
            zmax=20,
            color=~CO2_PerCap,
            text=~hover,
            hoverinfo=~"text",
            colorscale="Inferno") %>%
  layout(geo=list(projection = list( type = "orthographic")),
         font = list(family="Rubik Black", size=11),
    title="CO2 emission in metric tons per capita 2006-2018")%>%
  colorbar(title="CO2 in t") %>%
  config(displayModeBar=FALSE)
            

world_co2_graph

```

# Regression Analysis
```{r estimationspack, include=FALSE}
library(h2o)
library(ranger)
library(plm)
library(sjPlot)
library(PanJen)
```

Used Packages - h2o, ranger, plm, sjPlot, PanJen
```{r estimations, warning=FALSE}

# Creation of continent dummys
world_bank_complete$continent <- as.factor(world_bank_complete$continent)

# Creating panel data
panel_worldbank_data <- pdata.frame(world_bank_complete,c("Country.Code","year"))


# Find the best specification for the CO2 model
form<-formula(log(CO2_PerCap) ~ pop_million + continent, 
              data = panel_worldbank_data)

fxlist= list(
  linear = function(x) x,
  sqr = function(x) x^2,
  poly = function(x) x^3,
  log = function(x) log(x),
  inverse = function(x) 1/(x)
)

model<-choose.fform(data=panel_worldbank_data,variable="gdpPercap_1000",
                    base_form=form, functionList=fxlist)


### Random and Fixed effects estimation #####
random_effectes_reg <- plm(log(CO2_PerCap) ~ log(gdpPercap_1000) + pop_million, # + continent, 
                          data = panel_worldbank_data, model="random", index = c("Country.Code","year"))

fixed_effectes_reg <- plm(log(CO2_PerCap) ~ log(gdpPercap_1000) + pop_million, # + continent, 
                           data = panel_worldbank_data, model="within", index = c("Country.Code","year"))

##### Hausmanntest ###
phtest(fixed_effectes_reg,random_effectes_reg)

# RE in inconsistent

#### Setting up correlated random effects model ####
#### Taking the mean of the time varying variables ####

#  Average per Country of population in Million #
aggre_table<-aggregate(panel_worldbank_data$pop_million, 
                       by=list(panel_worldbank_data$Country.Code), FUN=mean)
for(i in unique(panel_worldbank_data$Country.Code)){
  panel_worldbank_data$pop_million_mean[panel_worldbank_data$Country.Code==i]<-aggre_table[aggre_table[,1]==i,2]}


#  Average per Country of GDP per Capita #
aggre_table<-aggregate(panel_worldbank_data$gdpPercap_1000, 
                       by=list(panel_worldbank_data$Country.Code), FUN=mean)
for(i in unique(panel_worldbank_data$Country.Code)){
  panel_worldbank_data$gdpPercap_1000_mean[panel_worldbank_data$Country.Code==i]<-aggre_table[aggre_table[,1]==i,2]}



### Model estimation of Correlated Random effects ####
corr_random_effectes_reg <- plm(log(CO2_PerCap) ~ log(gdpPercap_1000) + pop_million + continent 
                                  + gdpPercap_1000_mean, 
                           data = panel_worldbank_data, model="random")

# Pop_milion_mean is taken out due to multicollinarity. gdpPercap_1000_mean is significant, hence
# CRE estimation is necessary as predicted by the Hausmann test

tab_model(corr_random_effectes_reg, file="output.html", 
          title="Correlated Random Effects", vcov.type = "HC3", robust=TRUE)
```

# Machine Learning
```{r machinelearingpack, include=FALSE}
library(randomForest)
library(randomUniformForest)
library(plotmo)
library(gbm)
```
Used Packages - randomForest, randomUniformForest, plotmo, gbm
```{r machineLearning, warning=FALSE}
######################################################
###### Defining Functions for Machine Learning #######
###### data partitioning ###################
TVHsplit<-function(df, split = c(0.5, 0.5),
                   labels = c("T", "V"), iseed = 1176){
  set.seed(iseed)
  flags <- sample(labels, size = nrow(df),
                  prob = split, replace = TRUE)
  return(flags)
}

#### Performance Test ######
ValidationRsquared<-function(validObs, validHat){
  resids <- validHat - validObs
  yBar <- mean(validObs)
  offset <- validObs - yBar
  num <- sum(resids^2)
  denom <- sum(offset^2)
  Rsq <- 1 - num/denom
  return(Rsq)
}
# End of defining functions

### generate a training and a validation dataset ####
WorldBankFlag <- TVHsplit(world_bank_complete, split = c(0.7, 0.3),
                         labels = c("T", "V"))
WorldBankTrain <- world_bank_complete[which(WorldBankFlag == "T"), ]
WorldBankValid <- world_bank_complete[which(WorldBankFlag == "V"), ]


# Machine learning estimation - Random Forest
rfCO2_model<-randomForest(CO2_PerCap ~ gdpPercap_1000 + year + pop_million + continent + Country.Name, 
                          data = WorldBankTrain, importance = TRUE)

# Performance Test
rfCO2_modelHatV <- predict(rfCO2_model, newdata = WorldBankValid)
ValidationRsquared(WorldBankValid$CO2_PerCap, rfCO2_modelHatV)

# Visualization of Machine Learning Estimation
plot(rfCO2_model)
varImpPlot(rfCO2_model)


# compare predictions to actual results
prediction_results_forest <- data.frame(WorldBankValid$CO2_PerCap, rfCO2_modelHatV, 
                                 WorldBankValid$Country.Name, WorldBankValid$year) 
colnames(prediction_results_forest) <- c("Actual CO2 pp", "Forest prediction CO2 pp", 
                                      "Country", "Year")
head(prediction_results_forest, n=10L)



##########################################################
# Machine learning estimation - Gradient Boosting Machines

gbm_co2_Model <- gbm(CO2_PerCap ~ gdpPercap_1000 + year + pop_million + continent,
                        data = WorldBankTrain,
                        distribution = "gaussian",
                        interaction.depth = 40,
                        shrinkage = 0.05,
                        n.trees = 2000,
                        n.minobsinnode = 10)


# Performance Test
gbm_co2_ModelHatV <- predict(gbm_co2_Model, newdata = WorldBankValid,
                            n.trees = 1000)
ValidationRsquared(WorldBankValid$CO2_PerCap, gbm_co2_ModelHatV)


# Visualization of Machine Learning Estimation
plotmo(gbm_co2_Model)


# compare predictions to actual results
prediction_results_gbm <- data.frame(WorldBankValid$CO2_PerCap, gbm_co2_ModelHatV, 
                                        WorldBankValid$Country.Name, WorldBankValid$year) 
colnames(prediction_results_gbm) <- c("Actual CO2 pp", "GBM prediction CO2 pp", 
                                      "Country", "Year")
head(prediction_results_gbm, n=10L)

```
