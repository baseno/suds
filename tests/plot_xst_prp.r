require(dplyr)
require(lubridate)
require(ggplot2)

sx <- readRDS("./structure_existente.rds") %>% mutate(Qout=ifelse(Qout<0,0,Qout))
px <- readRDS("./pipe_existente.rds")
rx <- readRDS("./runoff_existente.rds")

sr <- readRDS("./structure_proposta.rds") %>% mutate(Qout=ifelse(Qout<0,0,Qout))
pr <- readRDS("./pipe_proposta.rds")
rr <- readRDS("./runoff_proposta.rds")

ggplot(sr)  + geom_line(aes(x=dt,y=Qout)) + facet_wrap(~name)
ggplot(px)  + geom_line(aes(x=dt,y=Qout)) + facet_wrap(~name)
