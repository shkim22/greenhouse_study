library(dplyr)
library(corrplot)
library(readxl)
library(tidyr)
library(gganimate)
library(gifski)
df <- read.csv("combined8.csv", header = TRUE)
head(df)
summary(df)
dim(df)

#library("ggpubr")
#ggscatter(df, x = "population", y = "Total.excluding.LUCF", 
#          conf.int = TRUE, cor.coef = TRUE, 
#          cor.method = "pearson", xlab = "Population", 
#          ylab = "GHG w/o LUCF")



#cor(x = "Year", y = "Total.excluding.LUCF", method = "pearson", use = "complete.obs")
#cor(x = "Year", y = "population", method = "pearson", use = "complete.obs")

df1 <- df
df1[df1 == "" | df1 == " " | df1 == "void" | df1 == "NA"] <- NA



df2 <- df1 %>% select(-c(X, Country_Code, Country_Name, Year))
apply(is.na(df2), 2, sum)

df3 <- df1 %>% select(-c(X, Country_Code, patent_applications, unemployment,
                         Government.expenditure.on.education..total....of.GDP., 
                         battery_electric_share, plugin_hybrid_share, 
                         battery_electric_number, plugin_hybrid_number,
                         full_mild_hybrid_number, petrol_number, diesel_gas_number, 
                         Researchers.in.R.D..per.million.people.))
df3 = na.omit(df3)
df3 <- df3 %>% select(Country_Name, Year, Total.excluding.LUCF, population)
df3$Year <- format(as.Date(df3$Year, format = "%d/%m/%Y"),"%Y")
class(df3$Year)
rand_countries <- sample(unique(df3$Country_Name), size = 15)
df3 <- filter(df3, Country_Name %in% rand_countries)
Graph = ggplot(df3, aes(population, Total.excluding.LUCF, 
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


#dataMatrix = data.matrix(df3)
#print(dataMatrix)
#Remove NAs
#noNA = na.omit(dataMatrix)

#corrplot(cor(na.omit(dataMatrix)))
#plot.new()
