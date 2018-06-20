setwd("/home/delgado/proj/suds")
source("./R/initial_states_existente.r")


require(suds)
require(hydraulics)

k <- 0
r.list <- list()
p.list <- list()
s.list <- list()
sb.list <- list()
I0 <- I0[0:200,]

list.str <- network %>% distinct(strahler) %>% pull %>% sort
subbasin_out <- subbasin.template

source("./R/loop.r")

runoff <- do.call("rbind",r.list) %>% saveRDS(.,"./tests/runoff_existente.rds")
pipe <- do.call("rbind",p.list) %>% saveRDS(.,"./tests/pipe_existente.rds")
structure <- do.call("rbind",s.list) %>% saveRDS(.,"./tests/structure_existente.rds")

