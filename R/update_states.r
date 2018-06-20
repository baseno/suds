#' main loop
#' @return list of runoff, pipe and structure tbl
#' @param subbasin_initial
#' @export
loop <- function(subbasin_initial,I0,strahler)
{
    updown <- select(strahler, subbacia,flows_to) %>% rename(upstream=subbacia,downstream=flows_to)
    network <- updown2idup(updown) %>%
        filter(id>0) %>%
        rename(subbacia=id) %>%
        left_join(.,select(strahler,subbacia,strahler)) %>%
        rename(name=subbacia)

    list.str <- network %>% distinct(strahler) %>% pull %>% sort
    k <- 0
    r.list <- list()
    p.list <- list()
    s.list <- list()
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
            runoff <-  runoff %>%
                mutate(datetime=I0$dt[dti]) %>%
                select(-dt)
            r.list[[k]] <- runoff
        
            pipe <-  routePipe(subbasin) %>% mutate(dt=dti)
            subbasin <- updateSubbasinAfterPipe(subbasin,pipe)
            pipe <-  pipe %>%
                mutate(datetime=I0$dt[dti]) %>%
                select(-dt)
            p.list[[k]] <- pipe

            
            structure <- routeStructure(subbasin) %>% mutate(dt=dti)
            subbasin <- updateSubbasinAfterStructure(subbasin,structure)
            structure <-  structure %>%
                mutate(datetime=I0$dt[dti]) %>%
                select(-dt)
            s.list[[k]] <- structure
            
            
            subbasin <- computeEffluent(subbasin)
            
            anti_subbasin <- anti_join(subbasin_out,subbasin,by="name")
            
            subbasin_out <- bind_rows(subbasin,anti_subbasin)        
        }
    }

    r <- do.call("rbind",r.list)
    p <- do.call("rbind",p.list)
    s <- do.call("rbind",s.list)
    return(list(r,p,s))
    
}


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
            mutate(runoff_in=ifelse(hi > r,0,0.001*area*(r-hi)*c.factor/dt), ## generate runoff in case the soil compartment is full (this should be replaced by green ampt...)
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
#' @return `structure`, a data frame with columns, `name`, `V`, `Vvirtual`, `Qoverflow`, `Qin`, `Qout`
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




#' compute basin's effluent from all surface processes
#' @return subbasin.effl
#' @param subbasin is the subbasin state after structure update
#' @export
computeEffluent <- function(subbasin)
{

    subbasin.effl=subbasin %>%
        mutate(effluent=runoff.out+pipe.Qout+structure.Qoverflow+structure.Qout)

    return(subbasin.effl)
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
