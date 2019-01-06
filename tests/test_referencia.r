ini <- Sys.time()


setwd("/home/delgado/proj/suds")


source("./R/initial_states_referencia.r")

require(suds)
require(hydraulics)

#I0 <- I0[0:200,]

subbasin_out <- subbasin.template

subbasin_initial <- subbasin_out

    updown <- select(strahler, subbacia,flows_to) %>% rename(upstream=subbacia,downstream=flows_to)
    network <- updown2idup(updown) %>%
        filter(id>0) %>%
        rename(subbacia=id) %>%
        left_join(.,select(strahler,subbacia,strahler)) %>%
        rename(name=subbacia)

    list.str <- network %>% distinct(strahler) %>% pull %>% sort
    k <- 0
    r.list <- list()
    sb.list <- list()

    subbasin_out <- subbasin_initial
    for(dti in seq(1,nrow(I0)))
    {
        subbasin_out <- mutate(subbasin_out,i=slice(I0,dti) %>% pull(value))
        cat("date: ",dti,"\n","nrows of subbasin: ",nrow(subbasin_out),"\n")
        
        for(stra in list.str)
        {
            cat("date: ",dti,"\n","nrows of subbasin: ",nrow(subbasin_out),"\n","strahler number: ",stra,"\n")
            
            subbasin <- subbasin_out
            k=k+1
            network.subset <- network %>% filter(strahler==stra)
            cat(network.subset$strahler[1],"\n")
            subbasin <- gatherAffluentStrahler(network.subset,subbasin)
            runoff <- lossModel(subbasin)
            subbasin <- updateSubbasinAfterLossModel(subbasin,runoff)

            runoff <- routeRunoff(subbasin) %>% mutate(dt=dti)
            subbasin <- updateSubbasinAfterRunoff(subbasin,runoff)


