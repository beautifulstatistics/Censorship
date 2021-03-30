setwd("~/Thesis")
library(tidyverse)

bl <- read.csv('blacklist.csv',header=TRUE)

names(bl) <- bl[1,]
bl <- bl[-1,]
names(bl) <- c('zh','en','link','d1','r1','d2','r2','d3','r3',
               'd4','r4','d5','r5','d6','r6','remarks')

bl <- tibble(bl)
################################

bl$d2[154] <- "2016-04-06"
bl$d2[1383] <- "2013-09-01"
bl$d2[1617] <- "2013-03-31"
bl$d2[1732] <- "2013-01-04"
bl$d2[1733] <- "2013-01-04"
bl$d2[3258] <- "2011-10-31"

bl$d2 <- gsub('\\.','-',bl$d2)
bl$d3 <- gsub('\\.','-',bl$d3)

bln <- bl %>%
    filter(d2 == '') %>%
    select(-remarks) %>%
    as.data.frame

blc <- bl %>%
    filter(d2 != '') %>%
    select(-remarks,-zh,-en,-link) %>%
    mutate(all_of(across(matches('d[1-6]'),function(x)
        as.numeric(as.Date(x)-as.Date(d1))+1))) %>%
    as.data.frame

#################################
status <- function(v){
    vs <- v[seq(2,12,2)]
    
    fpos <- ifelse('F' %in% vs, which('F' == vs), c(0))
    npos <- ifelse('N' %in% vs, which('N' == vs), c(0))
    ypos <- which(vs == 'Y')
    
    yton <- c()
    ytof <- c()
    for(i in 1:length(ypos)){
        if((ypos[i] + 1) %in% npos){
            yton <- c(yton,ypos[i]) 
        }
        if((ypos[i] + 1) %in% fpos){
            ytof <- c(ytof,ypos[i])
        }
    }
    
    fton <- c()
    if(fpos != 0){
        for(i in 1:length(fpos)){
            if((fpos[i] + 1) %in% npos){
                fton <- c(fton,fpos[i]) 
            }
        }
    }
    
    yton <- ifelse(length(yton) != 0, yton, c(0))
    ytof <- ifelse(length(ytof) != 0, ytof, c(0))
    fton <- ifelse(length(fton) != 0, fton, c(0))
    
    list(fpos=fpos,npos=npos,ypos=ypos,yton=yton,ytof=ytof,fton=fton)
}

mk_df <- function(v){
    st <- status(v)
    vd <- as.numeric(v[seq(1,12,2)])
    
    df <- data.frame()
    if(st$yton[1] != 0){
        for(i in st$yton){
            df <- rbind(df,data.frame(y=vd[i],
                                      cens='interval',
                                      ub=vd[i+1],
                                      weights=1/length(st$yton),
                                      type='yton'))
        }
    }
    
    if(st$ytof[1] != 0){
        for(i in st$ytof){
            df <- rbind(df,data.frame(y=vd[i],
                                      cens='interval',
                                      ub=vd[i+1],
                                      weights=1/length(st$ytof)/2,
                                      type='ytof'))
        }
    }
    
    if(st$fton[1] != 0){
        for(i in st$fton){
            df <- rbind(df,data.frame(y=vd[i],
                                      cens='interval',
                                      ub=vd[i+1],
                                      weights=1/length(st$fton),
                                      type='fton'))
        }
    }
    
    if(st$npos[1] == 0 & st$fpos[1] == 0){
        df <- rbind(df,data.frame(y=vd[rev(st$ypos)[1]],
                                  cens='right',
                                  ub=vd[rev(st$ypos)[1]] + 1,
                                  weights=1,
                                  type='ally'))
    }
    
    return(df)
}

bl_ll <- apply(blc,MARGIN=1,FUN=mk_df)

bld <- data.frame()
for(i in bl_ll){
    bld <- rbind(bld,i)
}

rm(list=c('bl_ll','mk_df','status','i','blc'))

