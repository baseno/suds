for(dti in seq(1,nrow(I0)))
{
    subbasin_out <- mutate(subbasin_out,i=slice(I0,dti) %>% pull(value))
    cat("date: ",dti,"\n","nrows of subbasin: ",nrow(subbasin_out),"\n")

    for(str in list.str)
    {

        subbasin <- subbasin_out
        k=k+1
        network.subset <- network %>% filter(strahler==str)

        subbasin <- gatherAffluentStrahler(network.subset,subbasin)
        runoff <- lossModel(subbasin)
        subbasin <- updateSubbasinAfterLossModel(subbasin,runoff)
        
        runoff <- routeRunoff(subbasin)
        r.list[[k]] <- runoff %>% mutate(dt=dti)
        subbasin <- updateSubbasinAfterRunoff(subbasin,runoff)
        runoff <-  runoff %>%
            mutate(datetime=I0$dt[dti])
        
        pipe <-  routePipe(subbasin)
        p.list[[k]] <- pipe %>% mutate(dt=dti)
        subbasin <- updateSubbasinAfterPipe(subbasin,pipe)
        pipe <-  pipe %>%
            mutate(datetime=I0$dt[dti])

        
        structure <-  routeStructure(subbasin)
        s.list[[k]] <- structure %>% mutate(dt=dti)
        subbasin <- updateSubbasinAfterStructure(subbasin,structure)
        structure <-  structure %>%
            mutate(datetime=I0$dt[dti])


        subbasin <- computeEffluent(subbasin)

        anti_subbasin <- anti_join(subbasin_out,subbasin,by="name")

        subbasin_out <- bind_rows(subbasin,anti_subbasin)        
    }
}
