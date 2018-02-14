



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
        for(ii in distinct(network.subset,name) %>% pull)
        {
            upstr <- network.subset %>% filter(name==ii) %>% pull(upstream)
            affl <- subbasin %>% filter(name %in% upstr) %>% summarise(sum(effluent)) %>% pull
            subbasin.affluent[[ii]] <- subbasin %>% filter(name==ii) %>% mutate(affluent=affl)
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

#' compute effective runoff with loss model
#' @return runoff
#' @param subbasin is the global dataframe including rainfall intensity
#' @export
lossModel <- function(subbasin)
{
    dt <- subbasin %>% pull(step) %>% .[1]
    I <- subbasin %>% pull(i) %>% .[1]

    r <- I*dt/3600
    
    runoff <- select(subbasin,name,hi,he,hi.max,area,c.factor,L,n,S)    

    if(r==0) ## in case it does not rain...
    {
        runoff <- runoff %>%
            mutate(hi=hi+he*step/(24*60*60),runoff_in=0) %>% ## assign zero in runoff generation and evaporate from soil
            mutate(hi=ifelse(hi>hi.max,hi.max,hi))  %>% ## evaporate only until the soil compartment is empty 
            select(name,runoff_in,hi) 
    } else ## in case it rains...
    {
        runoff <- runoff %>%
            mutate(runoff_in=ifelse(hi > r,0,0.001*area*(r-hi)*c.factor), ## generate runoff in case the soil compartment is full (this should be replaced by green ampt...)
                   hi=ifelse(hi > r,hi-r,0)) %>% ## fill the soil compartment until it is totally full, ie hi=0
            select(name,runoff_in,hi)
    }

    return(runoff)
}


#' update subbasin dataframe after use of lossModel
#' @return subbasin.updated
#' @param subbasin
#' @param runoff from lossModel
#' @export
updateSubbasinAfterLossModel <- function(subbasin,runoff)
{
    subbasin.updated <- select(subbasin,-runoff,-hi) %>%
        left_join(.,select(runoff,name,runoff_in,hi),by="name") %>%
        rename(runoff=runoff_in)

    return(subbasin.updated)
}


#' route runoff from lossModel givin in subbasin
#' @return runoff
#' @param subbasin
#' @export
routeRunoff <- function(subbasin)
{
    runoff <- select(subbasin,name,runoff,runoff.V,area,c.factor,L,n,i,S) %>%
        mutate(runoff_out=runoff,V=runoff.V) %>% ## still need to route runoff
        select(name,runoff,runoff_out,V)


    return(runoff)
}

#' update subbasin dataframe after runoff was routed
#' @return subbasin.updated
#' @param subbasin
#' @param runoff as given by routeRunoff
#' @export
updateSubbasinAfterRunoff <- function(subbasin,runoff)
{
    subbasin.updated <- left_join(select(subbasin,-runoff.V,-runoff,-runoff.out),select(runoff,name,V,runoff,runoff_out),by="name") %>%
        rename(runoff.V=V,runoff.out=runoff_out)

    return(subbasin.updated)
}


#' route through pipe
#' @return pipe
#' @param subbasin
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

#' update subbasin after pipe routing
#' @return subbasin.updated
#' @param subbasin
#' @param pipe as given by routePipe
#' @export
updateSubbasinAfterPipe <- function(subbasin,pipe)
{
    subbasin.updated <- left_join(select(subbasin,-pipe.V,-pipe.Qin,-pipe.Qout),select(pipe,name,V,Qin,Qout),by="name") %>%
        rename(pipe.V=V,pipe.Qin=Qin,pipe.Qout=Qout)

    return(subbasin.updated)
}

#' route through structure
#' @return structure
#' @param subbasin
#' @export
routeStructure <- function(subbasin)
{
    structure <- select(subbasin,name,pipe.Qout,structure.Qoverflow,structure.V,Qoutmax,volume_lago,step) %>%
        mutate(Qin=pipe.Qout,Vprevious=structure.V,Qout=0) %>%
        rename(dt=step,Vmax=volume_lago,Qoverflow=structure.Qoverflow,V=structure.V) %>%
        Virtual_retention %>%
        Qoverflow_ret_str %>%
        Qoutflow_ret_str %>%
        Actual_retention

    return(structure)
}

#' update subbasin after structure
#' @return subbasin.updated
#' @param subbasin
#' @param structure as given by routeStructure
#' @export
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
loopTime <- function(I0,subbasin,network)
{

    #### actualizar tudo na df subbasin!!!
    k <- 0
    r.list <- list()
    p.list <- list()
    s.list <- list()
    sb.list <- list()
    for(dti in I0$dt)
    {
        subbasin <- mutate(subbasin,i=filter(I0,dt==dti) %>% pull(value))
        for(str in network %>% distinct(strahler) %>% pull %>% sort)
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

            sb.list[[k]] <- subbasin

#            select(subbasin,name,runoff.out,pipe.Qout)
#            subbasin <- computeEffluent(r.list[[i]],p.list[[i]],s.list[[i]],subbasin) %>% mutate(dt=dti)
        }
        subbasin <- do.call("rbind",sb.list)
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
