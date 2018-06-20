setwd("/home/delgado/proj/suds")
source("./R/initial_states_proposta.r")


require(suds)
require(hydraulics)

I0 <- I0[0:200,]

subbasin_out <- subbasin.template

l.rps <- loop(subbasin_out,I0,strahler)

runoff <- l.rps[[1]] %>% saveRDS(.,"./tests/runoff_proposta.rds")
pipe <- l.rps[[2]] %>% saveRDS(.,"./tests/pipe_proposta.rds")
structure <- l.rps[[3]] %>% saveRDS(.,"./tests/structure_proposta.rds")

