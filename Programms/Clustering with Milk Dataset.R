library(cluster.datasets)
install.packages("cluster.datasets")

data("all.mammals.milk.1956")
head(all.mammals.milk.1956)
library(tidyverse)
library(gridExtra)
plot1 <- all.mammals.milk.1956 %>% 
  ggplot(aes(x = "all mammals", y = water)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "blue") +
  labs(x = "", y="percentage of water")

plot2 <- all.mammals.milk.1956 %>% 
  ggplot(aes(x = "all mammals", y = protein)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "orange") +
  labs(x = "", y="percentage of protien")

plot3 <- all.mammals.milk.1956 %>% 
  ggplot(aes(x = "all mammals", y = fat)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "green") +
  labs(x = "", y="percentage of fat")

plot4 <- all.mammals.milk.1956 %>% 
  ggplot(aes(x = "all mammals", y = lactose)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "yellow") +
  labs(x = "", y="percentage of lactose")

plot5 <- all.mammals.milk.1956 %>% 
  ggplot(aes(x = "all mammals", y = ash)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "pink") +
  labs(x = "", y="percentage of ash")

grid.arrange(plot1,plot2,plot3,plot4,plot5)

set.seed(123)

input <- all.mammals.milk.1956[,2:6]
kmeans(input,centers = 3,nstart = 20)

wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(input, nc = 20)
