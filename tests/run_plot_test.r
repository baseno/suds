source("./tests/test_run.r")
require(ggplot2)

ggplot(pipe) + geom_line(aes(x=dt,y=Qout)) + facet_wrap(~name)
