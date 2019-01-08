#' compute effective runoff with green-ampt. Needs testing.
#' @return soil_state 
#' @param soil_state is a data frame with soil water content 'theta', 'theta_previous' and excess 'runoff'.
#' @param soil_param is a data frame with soil properties like 'porosity', soil_depth', 'phi' (matric suction depth), 'K' (saturated conductivity), 'dt' (desired time step for green-ampt computation) and 'model_step' (the time step of the model where green ampt is being applied to)
#' @param meteo is a data frame with 'i_t' (rainfall intensity in mm/h), 
#' @export
soil_model <- function(soil_state,soil_param,meteo)
{
 ##   i_t,theta_previous,porosity,soil_depth,phi,K,dt,model_step
    runoff=0
    tt <- 0
    Ft <- 0
    Li <- 0
    K <- soil_param$K
    if(is.na(K) | K<=0 | (soil_param$porosity-soil_state$theta) <= 0)
    {
        theta_i <- soil_state$theta
        theta_final <- soil_state$theta
        Ft <- 0
    } else
    {
        theta_i <- soil_state$theta
        
        dTheta <- soil_param$porosity-theta_i
        
        
        while(Li<soil_param$soil_depth)
        {
            tt <- tt+soil_param$dt
            Ft0=Ft
            ft0=calc_ft(soil_param$K,soil_param$phi,dTheta,Ft0)
            Ft=calc_Ft(Ft0,soil_param$phi,dTheta,soil_param$K,soil_param$dt,meteo$i_t)
            ft <- calc_ft(soil_param$K,soil_param$phi,dTheta,Ft)
            
            Li <- Ft/dTheta    ## calculate depth of wetting front        
            if(tt==soil_param$model_step) break;
        }
    
        ## calculate soil water content after this time step
        theta_final <- (soil_param$porosity*Li+theta_i*(soil_param$soil_depth-Li))/soil_param$soil_depth
    }


    soil_state=mutate(soil_state,theta_previous=theta_i,theta=theta_final,runoff=meteo$i_t*soil_param$model_step-Ft)

    return(soil_state)
}


#' compute actual evapotranspiration
#' @return new_soil_state
#' @param soil_state is a data frame with soil water content 'theta', 'theta_previous' and excess 'runoff'.
#' @param soil_param is a data frame with 'ra', 'rs' and soil parameters as in function _soil_model_.
#' @export
actET <- function(soil_state,soil_param,meteo)
{
    if(is.infinite(soil_param$rs))
    {
        potET <- penman(meteo$T,meteo$q,meteo$p,meteo$vpd,meteo$Rn,meteo$G,soil_param$ra)*0.5*soil_param$model_step/24 ## only during day light
        soilwater <- soil_state$runoff*soil_param$model_step/24
        actet <- min(soilwater,potET)
        new_soil_state <- mutate(soil_state,runoff=runoff-actet,et=actet)
    } else
    {
        potET <- penmon_day(meteo$T,meteo$q,meteo$p,meteo$vpd,meteo$Rn,meteo$G,soil_param$ra,soil_param$rs)*0.5*soil_param$model_step/24  ## only during day light in mm
        soilwater <- soil_state$theta*soil_param$soil_depth
        actet <- min(soilwater,potET)
        new_soil_state <- mutate(soil_state,theta=(soilwater-actet)/soil_param$soil_depth,et=actet)
    }
    return(new_soil_state)
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
    
    runoff <- select(subbasin,name,hi,he,hi.max,area)
    
    if(r==0) ## in case it does not rain...
    {
        runoff <- runoff %>%
            mutate(loss=if(he*step/(24*60*60) < (hi.max-hi),he*step/(24*60*60),hi.max-hi),hi=hi+he*step/(24*60*60),runoff_in=0)  %>% ## assign zero in runoff generation and evaporate from soil
            mutate(hi=ifelse(hi>hi.max,hi.max,hi))  %>% ## evaporate only until the soil compartment is empty 
            select(name,runoff_in,hi,loss) 
    } else ## in case it rains...
    {
        runoff <- runoff %>%
            mutate(loss=0,runoff_in=ifelse(hi > r,0,0.001*area*(r-hi)/dt), ## generate runoff in case the soil compartment is full (this should be replaced by green ampt...)
                   hi=ifelse(hi > r,hi-r,0)) %>% ## fill the soil compartment until it is totally full, ie hi=0
            select(name,runoff_in,hi,loss)
    }

    return(runoff)
}
