#================================ Load Libraries=============================#
library(tidyverse)
library(ggthemes)
library(gganimate)
theme_set(theme_clean())
#================================ Read File =================================#
Prices <- read_csv(file="2000_2021_Basic_Fuel_Prices.csv") %>% 
  clean_names()


Prices <- Prices %>% 
  mutate(petrol_93_unleaded = round(petrol_93_unleaded/100,2),
         petrol_95_unleaded = round(petrol_95_unleaded/100,2)) %>% 
  rename(date=new_date)

#================================ Visualisation =============================#
Viz_Price <- Prices %>% 
  ggplot()+
  geom_line(aes(date,petrol_95_unleaded,group=1),color="#0f204b")+
  labs(x= "date",
       y = "basic fuel cost (R)",
       title = "Basic Fuel Costs",
       subtitle="Jan-2000 to April-2021: 95 Unleaded Petrol",
       caption = "DATA SOURCE: Department of Mineral Resources and Energy.2021.Fuel Price Archive.\nAvailable from:http://www.energy.gov.za/files/esources/petroleum/petroleum_arch.html\nDATA VISUALISATION: Sivuyile Nzimeni")+
  theme(text = element_text(family = "Arial Narrow"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,face = "italic"),
        plot.caption = element_text(hjust = 0.5))

Viz_Price <- Viz_Price+
  transition_reveal(date)

gganimate::anim_save(filename = "2000_2020_Basic_Prices.gif",
                     animation = Viz_Price)


