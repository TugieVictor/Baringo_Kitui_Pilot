pacman::p_load(shiny, shinydashboard, DT, dplyr, plotly, ggplot2, shiny, ggplot2, 
               tidyverse, data.table, plotly, ggrepel, shinydashboard, shinyWidgets, 
               shinythemes, ggtext, ggcorrplot, shinycssloaders, scales, shinydashboardPlus, gotop)




Baringo_Data <- data.table(read_csv("data/Baringo_Sites.csv"))


Baringo_County <- read.csv("data/Baringo.csv")

Baringo_County <- Baringo_County %>% 
  mutate(Site_Data = as.factor(Site_Data))

B_Subcounty <- read.csv("data/Subcounty.csv", header = T, stringsAsFactors = T)


people_data <- c("Population (KNBS, 2019)", "No.of Households (KNBS, 2019)", "Targeted Households")


Kitui_Data <- data.table(read_csv("data/Kitui_Sites.csv"))

Kitui_County <- read.csv("data/Kitui_long.csv")

K_Subcounty <- data.table(read_csv("data/Kitui_subcounties.csv"))

Kitui_Subcounties <- c("Kitui Rural", 
                       "Kitui West", 
                       "Kitui East", 
                       "Kitui Central", 
                       "Kitui South",
                       "Mwingi North",
                       "Mwingi West",
                       "Mwingi East")


