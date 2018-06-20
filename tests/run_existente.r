setwd("/home/delgado/proj/suds")
source("./R/initial_states_existente.r")


require(suds)
require(hydraulics)

k <- 0
I0 <- I0[0:200,]

list.str <- network %>% distinct(strahler) %>% pull %>% sort
subbasin_out <- subbasin.template

l.rps <- loop(subbasin_out,I0,list.str)

runoff <- l.rps[[1]] %>% saveRDS(.,"./tests/runoff_existente.rds")
pipe <- l.rps[[2]] %>% saveRDS(.,"./tests/pipe_existente.rds")
structure <- l.rps[[3]] %>% saveRDS(.,"./tests/structure_existente.rds")

