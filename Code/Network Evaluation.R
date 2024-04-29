### Set up and Import ----
# Import library
library(tidyverse)
library(ggpubr)

# Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import the Parcel Info and Component Csv files
parcelAddress <- read.csv("../Data/ParcelIDed.csv")
parcelComponent <- read.csv("../Data/ParcelToComp.csv")

# Join the two together
AddressComp <- parcelAddress %>%
  left_join(parcelComponent, by = 'Pid')


### Summarize component characteristics ----

# Mode function because R does not have one, and I want the return the most
# common name and address in the components
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Summarizes the number of parcels in each component and the most common
# address and owner name in the component. Dataframe can be read as an
# interactive table
CompSum <- AddressComp %>% filter(!is.na(CompID)) %>%
  group_by(CompID) %>%
  summarize(n = n(),
            name = Mode(OwnerName),
            address = Mode(OwnerAddress))

# Trims the 5 largest values off for the density plot
CompSum_small <- CompSum %>% filter(n < 400)

# Creates density plot
g1 <- ggplot(data = CompSum_small, aes(x = n))+
  geom_density(color='blue', fill = 'blue') +
  labs(x = "Component Size", y = "Density",
       title = "Density of Network Component Size",
       subtitle = "34,862 parcels in 7156 components",
       caption = "* excludes the five largest components")+
  theme(text=element_text(size=24))

### Summarize characteristics of the original method ----
CatSum <- AddressComp %>% filter(RealOwner != ' ') %>%
  group_by(RealOwner) %>%
  summarize(n = n(),
            name = Mode(OwnerName),
            address = Mode(OwnerAddress))

CatSum_small <- CatSum %>% filter(n < 450) 

g2 <- ggplot(data = CatSum_small, aes(x = n))+
  geom_density(color= 'red', fill = 'red')+
  labs(x = "Cluster Size", y = "Density",
       title = "Density of Manual Cluster Size",
       subtitle = "6,462 parcels in 105 clusters",
       caption = "* excludes the five largest clusters")+
  theme(text=element_text(size=24))

# Creates a single plot of the two density graphs
ggarrange(g1, g2, ncol = 2)
