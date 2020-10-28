library(tidyverse)
library(ggplot2)
game <- read.csv("vgsales.csv")
game %>% select(-Rank) %>% filter(Year != "N/A" ) %>% filter(Year != "2020") %>% filter(Year != "2017") %>% 
  filter(Publisher!= "N/A") -> FilterGame
FilterGame$Year <- as.numeric(as.character(FilterGame$Year))


FilterGame %>% group_by(Publisher) %>% summarise(n = n()) %>% filter(n <= 3) -> low_publisher_df
low_publisher_df$Publisher <- as.character(low_publisher_df$Publisher)
low_publisher <- c()
for(index in c(1:nrow(low_publisher_df))){
  low_publisher <- c(low_publisher,low_publisher_df$Publisher[index])
} 
FilterGame %>% filter(Publisher != low_publisher) -> FilterGame



#10?????????????????????????????????????????????????????????
FilterGame %>% select(Publisher,Global_Sales) %>%  group_by(Publisher) %>% summarise(sum=sum(Global_Sales)) %>% 
  arrange(desc(sum)) %>% head(10) %>% 
  ggplot()+geom_col(mapping=aes(fill=Publisher,x=Publisher,y=sum)) + coord_flip()

#10??????????????????????????????????????????
FilterGame %>% select(Name,Global_Sales) %>% group_by(Name) %>% summarise(sum=sum(Global_Sales)) %>% 
  arrange(desc(sum)) %>% head(10) %>%
  ggplot()+geom_col(mapping=aes(fill=Name,x=Name,y=sum)) + coord_flip()

#????????? shooter top 10
FilterGame%>%select(Name,NA_Sales,Genre) %>% filter(Genre=="Shooter") %>% arrange(desc(NA_Sales)) %>% head(10) %>% 
  ggplot()+geom_col(mapping=aes(fill=Name,x=Name,y=NA_Sales)) + coord_flip()

#????????????????????????????????????????????????????????????????????? platform
FilterGame %>% select(Platform,Genre) %>% group_by(Platform) %>% 
  ggplot()+geom_bar(aes(x=Platform,fill=Genre),position = 'stack')+ coord_flip()+theme_minimal()

FilterGame %>%  group_by(Platform) %>% 
  ggplot()+geom_bar(aes(x=reorder(Platform,-Genre),fill=Genre),position = 'stack')+ coord_flip()+theme_minimal()




dlibrary(corrplot)
corr <- cor(select(FilterGame,c(6,7,8,9,10)))
corrplot(corr,
         type="upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45)

