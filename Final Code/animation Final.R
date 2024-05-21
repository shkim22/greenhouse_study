library(dplyr)
library(corrplot)
library(readxl)
library(tidyr)
library(gganimate)
library(gifski)
df <- read.csv("combinedFINAL.csv", header = TRUE)
head(df)
summary(df)
dim(df)

df1 <- df
df1[df1 == "" | df1 == " " | df1 == "void" | df1 == "NA"] <- NA



df2 <- df1 %>% select(-c(Country_Code, Country_Name))
x = (7990-(apply(is.na(df2), 2, sum)))/7790
x

df3 <- df1 %>% select(-c(Country_Code))
df3 = na.omit(df3)
df3 <- df3 %>% select(Country_Name, Year, GHG_without_LUCF, population)
df3$Year
class(df3$Year)
df3$Year <- format(as.Date(df3$Year, format = "%d/%m/%y"),"%Y")
df3$Year
class(df3$Year)
rand_countries <- sample(unique(df3$Country_Name), size = 15)
df3 <- filter(df3, Country_Name %in% rand_countries)
Graph = ggplot(df3, aes(population, GHG_without_LUCF, 
                        size = population/1000000, 
                        color = Country_Name)) +
  geom_point()+ 
  geom_text(aes(label = df3$Country_Name, group = df3$Country_Name), size = 4, nudge_y = 1.5, color = "black")+
  scale_color_viridis_d()+
  scale_y_log10()+
  scale_x_log10()+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  scale_size(range = c(1, 19), name = "Population [M]")+
  theme_gray()+
  labs(title = 'Year:{frame_time}', x = ' Population', y = ' GHG Emissions')+
  transition_time(as.integer(Year)) +
  ease_aes('linear')

animate(Graph, duration = 25, fps = 20, width = 1000, height = 500, renderer = gifski_renderer())
anim_save("out.gif", animation = Graph)
