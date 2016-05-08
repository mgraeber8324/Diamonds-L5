library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
data("diamonds")
pf <- read.delim("pseudo_facebook.tsv")
head(pf)

#hist of dia price facet by color and use cut to color hist
ggplot(diamonds, aes(x = price, fill = cut)) +
  geom_histogram() +
  facet_wrap(~color, ncol = 2) +
  scale_fill_brewer(type = "qual") +
  labs(title = "diamond prices by color (D-J)")

#note histogram appears logrithmic - replot
ggplot(diamonds, aes(x = price, fill = cut)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~color, ncol = 2) +
  scale_fill_brewer(type = "qual") +
  labs(title = "diamond prices by color (D-J)") +
  scale_x_log10()
ggsave("Diamond prices by color cut-colored.png")

#price by table colored by cut scatter
ggplot(diamonds, aes(x = table, y = price, color = cut)) +
  geom_point() +
  scale_color_brewer(type = "qual")
#table range for ideal is 52-57
#table range for premium is 53-59

#add vol to diamonds, pipe into plot
diamonds %>% 
  mutate(volume=x*y*z) %>% 
  filter(volume > 0, volume <= quantile(volume, 0.99)) %>%
  ggplot(aes(x=volume, y=price, color=clarity)) +
    geom_point() +
    scale_y_log10() +
    scale_color_brewer(type = "div")

#PSUEDO_FACEBOOK

#proportion of friendships initiated, year_joined, year_joined.bucket to pf
pf <- pf %>%
  mutate(prop_initiated = ifelse(friend_count > 0, 
                                 friendships_initiated/friend_count,0)) %>% 
  mutate(year_joined = floor(2014 - tenure/365),
         year_joined.bucket = cut(year_joined, breaks = c(2004,2009,2011,2012,2014)))

#line plots median proportion of friendships initiated against tenure
pf %>% filter(!is.na(year_joined.bucket), tenure >= 1) %>% #filters out NA & 0 values
  ggplot(aes(x = tenure, y = prop_initiated, color = year_joined.bucket)) + 
  geom_line(stat = 'summary', fun.y = median) + #median stat on proportion of friendships
  labs(title = "Median Proportion of Frienships Initiated by Tenure",
       x = "tenure", y = "proportion of friendships initiated")
ggsave("Median Proportion of Frienships Initiated by Tenure Colored.png")

#smooth previous plot
pf %>% filter(!is.na(year_joined.bucket), tenure >= 1) %>% #filters out NA & 0 values
  ggplot(aes(x = 30*round(tenure/30), y = prop_initiated, color = year_joined.bucket)) + 
  geom_line(stat = 'summary', fun.y = median) + #median stat on proportion of friendships
  labs(title = "Median Proportion of Frienships Initiated by Tenure",
       x = "tenure", y = "proportion of friendships initiated")
ggsave("Median Proportion of Frienships Initiated by Tenure Colored_Smoothed.png")
#geom_smooth does not alter the previous graph because I introduced
# the median stat as a wrapper instead of an additional variable in the pf dataframe
#the group that joined (2012,2014] initiated the greatest number of friendships

#mean of friendships initiated
pf %>% group_by(year_joined.bucket) %>% 
  summarise(mean(prop_initiated))
#newer group wants/needs to seek out friends (initiate the friendship)

#diamonds price/carat ratio vs cut colored by color faceted by clarity scatter
diamonds %>% ggplot(aes(x = cut, y = price/carat, color = color)) +
  geom_jitter() +
  facet_wrap(~clarity, ncol = 2) +
  scale_color_brewer(type = 'div') +
  labs(title = "Diamond Price/Carat vs Cut")
#geom_jitter instead of _point to combat overplotting
ggsave("Diamond Price/Carat vs Cut.jpg")
