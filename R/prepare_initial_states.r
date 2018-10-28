require(lubridate)
require(dplyr)
require(wasa.ops)
require(hydraulics)                                        #require(sf)

input.data <- "~/proj/SUDS_Famalicao/CAPITULO_2/modelo_rede_drenagem/run_model/"

step=5*60

### prepare precipitation

p <- read.table(paste0(input.data,"/precipitacao_raw"),sep=",") %>% as_tibble


I0h <- p[,-3:-4] %>%
    mutate(dt=dmy_hm(V1)) %>%
    rename(value=V2) %>%
    select(dt,value) %>%
    mutate(year=year(dt),month=month(dt),day=day(dt),hour=hour(dt)) %>%
    mutate_at(vars(year,month, day, hour),as.integer)

t5m <- seq(I0h$dt[1],I0h$dt[length(I0h$dt)],paste0(step," sec"))

tdf <- data_frame(year=year(t5m),month=month(t5m),day=day(t5m),hour=hour(t5m),minute=minute(t5m),second=second(t5m)) %>% mutate_all(as.integer)

I0 <- left_join(tdf,I0h,by=c("year","month","day","hour")) %>%
    mutate(dt=ymd_hms(paste(year,month,day,hour,minute,second))) %>%
    select(dt,value)


## prepare subbasin

losses.sb <- readRDS(paste0(input.data,"/losses.rds")) %>% rename(name=subbacia) %>% as_tibble

#subbasinsf <- readRDS(paste0(input.data,"/parm_sb.rds")) %>%
#    st_as_sf %>%
#    as_tibble %>%
#    mutate(dt=5*60) %>%

subbasin <- readRDS(paste0(input.data,"/parm_sb.rds")) %>%
    .@data %>%
    as_tibble %>%
    mutate(step=step) %>%
    rename(L=longest_path,S=av_slope) %>%
    mutate(volume_lago=ifelse(is.na(volume_lago),0,volume_lago)) %>% ## editing retention structures
    rename(name=subbacia) %>%
    left_join(.,select(losses.sb,name,initial.existente,permanent.existente),by="name") %>%
    rename(hi.max=initial.existente,he=permanent.existente) %>%
    mutate(hi=hi.max) %>%
    mutate(X=0.2) %>%### muskingum
    mutate(affluent=0,effluent=0) %>%
    mutate(c.factor=c_existente) %>%
    mutate(Qoutmax=0,volume_lago=0) %>%  ### only for existente!!!
    mutate(pipe.V=0,pipe.Qin=0,pipe.Qout=0) %>% ## state variables
    mutate(runoff.V=0,runoff=0,runoff.out=0) %>% ## state variables
    mutate(structure.V=0,structure.Qin=0,structure.Qout=0,structure.Qoverflow=0) ## state variables



### add K, phi, porosity, soildepth



subbasin.template <- subbasin

##### define network

strahler <- read.table(paste0(input.data,"/strahler.existente"),header=T,sep="\t")
updown <- select(strahler, subbacia,flows_to) %>% rename(upstream=subbacia,downstream=flows_to)

network <- updown2idup(updown) %>%
    filter(id>0) %>%
    rename(subbacia=id) %>%
    left_join(.,select(strahler,subbacia,strahler)) %>%
    rename(name=subbacia)


