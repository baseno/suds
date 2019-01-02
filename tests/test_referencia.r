ini <- Sys.time()


setwd("/home/delgado/proj/suds")


source("./R/initial_states_referencia.r")

require(suds)
require(hydraulics)

#I0 <- I0[0:200,]

subbasin_out <- subbasin.template

subbasin_initial <- subbasin_out

