setwd("/home/delgado/proj/suds")
source("./R/prepare_initial_states.r")
require(suds)
require(hydraulics)


   #### actualizar tudo na df subbasin!!!
k <- 0
r.list <- list()
p.list <- list()
s.list <- list()
sb.list <- list()
I0 <- I0[70:100,]


#dti=I0$dt[2]

list.str <- network %>% distinct(strahler) %>% pull %>% sort
subbasin_out <- subbasin.template

for(dti in seq(1,nrow(I0)))
{
    subbasin_out <- mutate(subbasin_out,i=slice(I0,dti) %>% pull(value))
    cat("date: ",dti,"\n","nrows of subbasin: ",nrow(subbasin_out),"\n")

    for(str in list.str)
    {

        subbasin <- subbasin_out
        k=k+1
    #    str <- str+1
        network.subset <- network %>% filter(strahler==str)
#        subbasin <- gatherAffluentStrahler(network.subset,subbasin)
        runoff <- lossModel(subbasin)
#        subbasin <- updateSubbasinAfterLossModel(subbasin,runoff)
        
#        runoff <- routeRunoff(subbasin)
#        r.list[[k]] <- runoff %>% mutate(dt=dti)
#        subbasin <- updateSubbasinAfterRunoff(subbasin,runoff)
    
        
    }
}

    runoff <- do.call("rbind",r.list)
    pipe <- do.call("rbind",p.list)
    structure <- do.call("rbind",s.list)

