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
I0 <- I0[70:100,] %>% as.data.table


#dti=I0$dt[2]

list.str <- network %>% distinct(strahler) %>% pull %>% sort
subbasin_out <- subbasin.template %>% as.data.table

subbasin_out[c.factor>0.8]


for(dti in seq(1,nrow(I0)))
{
    subbasin_out <- mutate(subbasin_out,i=I0[dti,value])
    cat("date: ",dti,"\n","nrows of subbasin: ",nrow(subbasin_out),"\n")

    for(str in list.str)
    {

        subbasin <- subbasin_out
        k=k+1
    #    str <- str+1
        network.subset <- network %>% filter(strahler==str)
        subbasin <- gatherAffluentStrahler(network.subset,subbasin)
        runoff <- lossModel(subbasin)
        subbasin <- updateSubbasinAfterLossModel(subbasin,runoff)
        
        runoff <- routeRunoff(subbasin)
        r.list[[k]] <- runoff %>% mutate(dt=dti)
        subbasin <- updateSubbasinAfterRunoff(subbasin,runoff)
        
        pipe <-  routePipe(subbasin)
        p.list[[k]] <- pipe %>% mutate(dt=dti)
        subbasin <- updateSubbasinAfterPipe(subbasin,pipe)

       
        
        structure <-  routeStructure(subbasin)
        s.list[[k]] <- structure %>% mutate(dt=dti)
        subbasin <- updateSubbasinAfterStructure(subbasin,structure)

        subbasin <- computeEffluent(runoff,pipe,structure,subbasin)

        anti_subbasin <- anti_join(subbasin_out,subbasin,by="name")

        subbasin_out <- bind_rows(subbasin,anti_subbasin)
#        cat(nrow(subbasin_out) - nrow(anti_subbasin),"\n")
        
    }
}

    runoff <- do.call("rbind",r.list)
    pipe <- do.call("rbind",p.list)
    structure <- do.call("rbind",s.list)

