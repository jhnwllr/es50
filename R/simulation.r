# simulate es50 for intuition 

library(dplyr)

c(seagull = 1e3,beetle=10,frog=20) %>% entropart::Hurlbert(3)
c(seagull = 10,beetle=10,frog=20) %>% entropart::Hurlbert(3)
c(seagull = 100,beetle=100,frog=100) %>% entropart::Hurlbert(3)
c(beetle1=1,beetle2=1,beetle3=1,beetle4=1) %>% entropart::Hurlbert(3)
c(beetle1=10,beetle2=1,beetle3=1,beetle4=1) %>% entropart::Hurlbert(3)



