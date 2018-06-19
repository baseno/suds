source("./tests/test_run.r")
require(ggplot2)

ggplot(runoff) + geom_line(aes(x=dt,y=runoff)) + facet_wrap(~name)
