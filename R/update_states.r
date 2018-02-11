



#' gather upstream affluent, only one strahler order can be given, should loop through  strahler in main loop
#' @return subbasin
#' @param subbasin
#' @param network.subset
#' @export
gatherAffluentStrahler <- function(network.subset, subbasin)
{
    subbasin.affluent <- list()
    if(network.subset$strahler[1]==1)
    {
        
        subbasin.subset <- subbasin %>% filter(name %in% pull(network.subset,name))
    } else
    {
        for(i in distinct(network.subset,name) %>% pull)
        {
            upstr <- network.subset %>% filter(name==i) %>% pull(upstream)
            affl <- subbasin %>% filter(name %in% upstr) %>% summarise(sum(effluent)) %>% pull
            subbasin.affluent[[i]] <- subbasin %>% filter(name==i) %>% mutate(affluent=affl)
        }
        subbasin.subset <- do.call("rbind",subbasin.affluent)
    }
    
    return(subbasin.subset)
}

#' compute basin's effluent from all surface processes
#' @return subbasin
#' @param runoff includes volume, runoff_in and runoff_out in subbasin
#' @param pipe includes volume, qin and qout in pipe
#' @param structure includes volume, qin and qout in structure
#' @param subbasin
#' @export
computeEffluent <- function(runoff, pipe, structure, subbasin)
{
#    runoff <- computeRunoff(subbasin,runoff)
    subbasin <- left_join(subbasin,select(runoff,name,runoff_out),by=name) %>%
        rename(runoff=runoff_out)

#    pipe <- routePipe(subbasin,pipe)
    subbasin <- left_join(subbasin,select(pipe,name,Qout),by=name) %>%
        rename(pipe_out=Qout)

#    structure <- routeStructure(subbasin,structure)
    subbasin <- left_join(subbasin,select(structure,name,Qin,Qout,Qoverflow,V),by=name) %>%
        rename(str_in=Qin,str_out=Qout,str_over=Qover,str_v=V)
    return(subbasin)
}


lossModel <- function(subbasin)
{
    runoff <- select(subbasin,name,i,hi,area,c.factor,L,n,i,S) %>%
        mutate(runoff_in=ifelse((i/3600-hi)<0,0,0.001*area*(i/3600-hi)*c.factor)) %>%
        select(name,runoff_in)

    return(runoff)
}

updateSubbasinAfterLossModel <- function(subbasin,runoff)
{
    subbasin.updated <- left_join(select(subbasin,-runoff),select(runoff,name,runoff_in),by="name") %>%
        rename(runoff=runoff_in)

    return(subbasin.updated)
}


#' route runoff
#' @export
routeRunoff <- function(subbasin)
{
    runoff <- select(subbasin,name,runoff,runoff.V,area,c.factor,L,n,i,S) %>%
        mutate(runoff_out=runoff,V=runoff.V) %>% ## still need to route runoff
        select(name,runoff,runoff_out,V)

        
    return(runoff)
}

updateSubbasinAfterRunoff <- function(subbasin,runoff)
{
    subbasin.updated <- left_join(select(subbasin,-runoff.V,-runoff,-runoff.out),select(runoff,name,V,runoff,runoff_out),by="name") %>%
        rename(runoff.V=V,runoff.out=runoff_out)

    return(subbasin.updated)
}


#' route through pipe
#' @export
routePipe <- function(subbasin)
{
    pipe <- select(subbasin,name,affluent,runoff.out,pipe.V,Kpipe,X,step) %>%
        mutate(Qin=affluent+runoff.out,V=0,Qout=0) %>%
        rename(Vprevious=pipe.V,K=Kpipe,dt=step) %>%
        Q_muskingum %>%
        V_muskingum %>%
        select(name,Qin,Qout,V)
    
    return(pipe)
}

updateSubbasinAfterPipe <- function(subbasin,pipe)
{
    subbasin.updated <- left_join(select(subbasin,-pipe.V,-pipe.Qin,-pipe.Qout),select(pipe,name,V,Qin,Qout),by="name") %>%
        rename(pipe.V=V,pipe.Qin=Qin,pipe.Qout=Qout)

    return(subbasin.updated)
}

#' route through structure
#' @export
routeStructure <- function(subbasin)
{
    structure <- select(subbasin,name,pipe.Qout,structure.V,Qoutmax,volume_lago) %>%
        mutate(Qin=pipe.Qout,Vprevious=structure.V,Qout=0) %>%
        Virtual_retention %>%
        Qoverflow_ret_str %>%
        Qoutflow_ret_str %>%
        Actual_retention
    
    return(structure)
}

updateSubbasinAfterStructure <- function(subbasin,structure)
{
    subbasin.updated <- left_join(select(subbasin,-structure.V,-structure.Qin,-structure.Qout),select(structure,name,V,Qin,Qout),by="name") %>%
        rename(structure.V=V,structure.Qin=Qin,structure.Qout=Qout)

    return(subbasin.updated)
}



## state_surface
                                        #- inlet_time
#- IDF
#- loss_model

### loop

#' loop in time
#' @export
loopTime <- function(I0,subbasin,network.subset)
{

    #### actualizar tudo na df subbasin!!!
    k <- 0
    r.list <- list()
    p.list <- list()
    s.list <- list()
    sb.list <- list()
    dti=I0$dt[1]
    for(dti in I0$dt)
    {
        subbasin <- mutate(subbasin,i=filter(I0,dt==dti) %>% pull(value))
        for(str in strahler)
        {
            k <- k+1
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

            subbasin <- computeEffluent(r.list[[i]],p.list[[i]],s.list[[i]],subbasin) %>% mutate(dt=dti)
            sb.list[[k]] <- subbasin
        }        
    }
    runoff <- do.call("rbind",r.list)
    pipe <- do.call("rbind",p.list)
    structure <- do.call("rbind",s.list)
    sb <- do.call("rbind",sb.list)

    return(sb)

}




#' summary catchment
#' @param swc soil water content
#' @param et evapotranspiration
#' @param runoff surface runoff
#' @param qupstream
#' @param qaffl outlet discharge before structure
#' @param qeffl outlet discharge after structure
#' @export
makeSummary <- function()
{
}

## state_connector





