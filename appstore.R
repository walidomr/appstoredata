##APP STORE
library(tidyverse)
library(MASS)
library(vcd)
setwd('~/Desktop//Appstore/')
# source: https://www.kaggle.com/ramamet4/app-store-apple-data-set-10k-apps


# "id" : App ID
# "track_name": App Name
# "size_bytes": Size (in Bytes)
# "currency": Currency Type
# "price": Price amount
# "rating_count_tot": User Rating counts (for all version)
# "rating_count_ver": User Rating counts (for current version)
# "user_rating" : Average User Rating value (for all version)
# "user_rating_ver": Average User Rating value (for current version)
# "ver" : Latest version code
# "cont_rating": Content Rating
# "prime_genre": Primary Genre
# "sup_devices.num": Number of supporting devices
# "ipadSc_urls.num": Number of screenshots showed for display
# "lang.num": Number of supported languages
# "vpp_lic": Vpp Device Based Licensing Enabled

df <- read.csv('AppleStore.csv')

summary(df)

length(filter(df, rating_count_tot == 300))

df %>%
  filter(rating_count_tot==300) %>%
  length()

ggplot(df) + 
  geom_bar(mapping = aes(x = currency, fill= currency))

ggplot(df, mapping = aes(x = prime_genre, y = user_rating)) + 
  geom_boxplot()

#Rating vs Price
df2 <- group_by(df,prime_genre)
price_rating <- summarise(df2,
                  count=n(),
                  mean_rating = mean(user_rating,na.rm=T),
                  mean_price = mean(price))
price_rating2<- filter(price_rating,mean_rating>2.5) 
require(gridExtra)
plot1<-ggplot(price_rating , aes(x=mean_rating,y=mean_price)) + 
  geom_point(aes(size=count),color = 'red',alpha=5/6) +
  geom_smooth(se=F)
plot2<-ggplot(price_rating2 , aes(x=mean_rating,y=mean_price)) + 
  geom_point(aes(size=count),color='red',alpha=5/6) +
  geom_smooth(se=F)
grid.arrange(plot1, plot2, nrow=1)

#content rating vs user rating with size and price
content_user <- group_by(df,cont_rating)
content_user2 <- summarise(content_user,
                           mean_userrating = mean(user_rating),
                           mean_size = mean(size_bytes),
                           mean_price = mean(price))
plot3<-ggplot(filter(content_user,price>0),aes(x=cont_rating,y=user_rating)) +
  geom_point(aes(size=size_bytes,color=price), alpha=1/3) +
  geom_smooth() + 
  geom_jitter()
plot3

plot4<-ggplot(filter(df,cont_rating == "17+"),aes(x=prime_genre,y=user_rating)) +
  geom_point(aes(size=size_bytes,color=price), alpha=1/3) +
  geom_smooth() + 
  geom_jitter() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1))
plot4

plot5 <- ggplot(filter(df,prime_genre == "Games"),aes(x=cont_rating,y=user_rating))+
  geom_point(aes(size=size_bytes,color=price), alpha=1/3) +
  geom_smooth() + 
  geom_jitter() + 
  labs(title="User Rating Distribution for Games",
       x="Content Rating", y = "User Rating") + 
  theme(plot.title = element_text(hjust = 0.5))

plot5

summary(filter(df,prime_genre == "Games")$cont_rating)

plot6 <- ggplot(filter(df,prime_genre == "Games", cont_rating == "17+"),
                aes(x=user_rating,y=price)) +
  geom_point(aes(size=size_bytes), alpha=1/3) +
  geom_smooth() + 
  geom_jitter() + 
  labs(title="User Rating vs Price for Games rated '17+'",
  x="User Rating", y = "Price") + 
  theme(plot.title = element_text(hjust = 0.5))

plot6
plot7 <- ggplot(filter(df,prime_genre == "Games", cont_rating == "12+"),
                aes(x=user_rating,y=price)) +
  geom_point(aes(size=size_bytes), alpha=1/3) +
  geom_smooth() + 
  geom_jitter() + 
  labs(title="User Rating vs Price for Games rated '12+'",
       x="User Rating", y = "Price") + 
  theme(plot.title = element_text(hjust = 0.5))

plot8 <- ggplot(filter(df,prime_genre == "Games", cont_rating == "4+"),
                aes(x=user_rating,y=price)) +
  geom_point(aes(size=size_bytes), alpha=1/3) +
  geom_smooth() + 
  geom_jitter() + 
  labs(title="User Rating vs Price for Games rated '4+'",
       x="User Rating", y = "Price") + 
  theme(plot.title = element_text(hjust = 0.5))

plot9 <- ggplot(filter(df,prime_genre == "Games", cont_rating == "9+"),
                aes(x=user_rating,y=price)) +
  geom_point(aes(size=size_bytes), alpha=1/3) +
  geom_smooth() + 
  geom_jitter() + 
  labs(title="User Rating vs Price for Games rated '9+'",
       x="User Rating", y = "Price") + 
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(plot8, plot9, plot7,plot6, nrow=2)

max(filter(df,prime_genre=='Games')$price)

#ratingcount as a measure of engangement
summary(df$rating_count_tot)
boxplot(df$rating_count_tot)

qplot(df$prime_genre,df$rating_count_tot)+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))

df3<- group_by(df,prime_genre)
genre_count <- summarise(df3,
                         total_count = sum(rating_count_tot))

ggplot(df, aes(x=prime_genre,y=(rating_count_tot))) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1))


df4 <- group_by(df,cont_rating)
price_contrating <- summarise(df4,
                              mean_price = mean(price),
                              count = length(id))

cont_genre <- table(df$prime_genre,df$cont_rating)
mosaicplot(cont_genre) #hmmm

