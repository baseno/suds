setwd("/home/delgado/proj/suds/tests")
require(dplyr)
require(lubridate)
require(ggplot2)

sx <- readRDS("./structure_existente.rds") %>% mutate(Qout=ifelse(Qout<0,0,Qout),Qin=ifelse(Qin<0,0,Qin))
px <- readRDS("./pipe_existente.rds") %>% mutate(Qout=ifelse(Qout<0,0,Qout),Qin=ifelse(Qin<0,0,Qin))
rx <- readRDS("./runoff_existente.rds")
sbx <- readRDS("./sb_existente.rds")

sr <- readRDS("./structure_proposta.rds") %>% mutate(Qout=ifelse(Qout<0,0,Qout),Qin=ifelse(Qin<0,0,Qin))
pr <- readRDS("./pipe_proposta.rds") %>% mutate(Qout=ifelse(Qout<0,0,Qout),Qin=ifelse(Qin<0,0,Qin))
rr <- readRDS("./runoff_proposta.rds")
sbr <- readRDS("./sb_proposta.rds")

sr %>% filter(name==17) %>% pull(Qoverflow) %>% max
pr %>% filter(name==17) %>% pull(Qout) %>% max
px %>% filter(name==17) %>% pull(Qout) %>% max



ggplot(sr %>% filter(name %in% c(5,39,42)))  + geom_line(aes(x=datetime,y=Qin)) + facet_wrap(~name)
ggplot(px %>% filter(name == 17))  + geom_line(aes(x=datetime,y=Qout)) + facet_wrap(~name)
ggplot(sbx %>% filter(name == 17))  + geom_line(aes(x=dt,y=hi)) + facet_wrap(~name)
