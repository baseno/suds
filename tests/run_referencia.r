ini <- Sys.time()
setwd("/home/delgado/proj/suds")
source("./R/initial_states_referencia.r")


require(suds)
require(hydraulics)

#I0 <- I0[0:20,]

subbasin_out <- subbasin.template
l.rps <- loop_runoff(subbasin_out,I0,strahler)

runoff <- l.rps[[1]] %>% saveRDS(.,"./tests/runoff_referencia.rds")
sb <- l.rps[[2]] %>% saveRDS(.,"./tests/sb_referencia.rds")

cat("\n",Sys.time()-ini,"\n")
