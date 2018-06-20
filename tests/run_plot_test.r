setwd("/home/delgado/proj/suds")
source("./tests/test_run.r")
require(ggplot2)

ggplot(runoff) + geom_line(aes(x=dt,y=runoff)) + facet_wrap(~name)
ggplot(pipe) + geom_line(aes(x=dt,y=Qout)) + facet_wrap(~name)
ggplot(structure) + geom_line(aes(x=dt,y=Qout)) + facet_wrap(~name)

