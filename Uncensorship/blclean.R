setwd("~/Censorship/Uncensorship/")
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
bl$d4[2445] <- "2012-04-26"

bl$d2 <- gsub('\\.','-',bl$d2)
bl$d3 <- gsub('\\.','-',bl$d3)

bln <- bl %>%
    rownames_to_column('blrow') %>%
    filter(d2 == '') %>%
    select(-remarks) %>%
    mutate(d1 = (as.numeric(Sys.Date() - as.Date(d1))+1)/365.25) %>%
    as.data.frame

blc <- bl %>%
    rownames_to_column('blrow') %>%
    filter(d2 != '') %>%
    select(-remarks,-zh,-en,-link) %>%
    mutate(all_of(across(matches('d[1-6]'),function(x)
        (as.numeric(as.Date(x)-as.Date(d1))+1)/365.25))) %>%
    as.data.frame

#################################

status <- function(v){
    vs <- v[seq(2,12,2)]
    
    tivs <- function(t){
        if(t %in% vs) return(which(t == vs))
        return(c(0))
    }
    
    fpos <- tivs('F')
    npos <- tivs('N')
    ypos <- tivs('Y')
    
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
    
    lro <- function(obj){
        if(length(obj) != 0) return(obj)
        return(c(0))
    }
    
    yton <- lro(yton)
    ytof <- lro(ytof)
    fton <- lro(fton)
    yend <- ifelse(rev(ypos)[1] > rev(npos)[1] & 
                       rev(ypos)[1] > rev(fpos)[1],
                   rev(ypos)[1],c(0))
    fend <- ifelse(rev(fpos)[1] > rev(npos)[1] &
                       rev(fpos)[1] > rev(ypos)[1],
                   rev(fpos)[1],c(0))
    
    list(fpos=fpos,npos=npos,ypos=ypos,
         yton=yton,ytof=ytof,
         fton=fton,
         yend=yend,
         fend=fend)
}

mk_df <- function(v){
    v <- unname(unlist(v))
    blrow <- v[1]
    v <- v[2:length(v)]
    
    st <- status(v)
    vd <- as.numeric(v[seq(1,12,2)])
    
    df <- data.frame()
    if(st$yton[1] != 0){
        for(i in st$yton){
            df <- rbind(df,data.frame(y=vd[i],
                                      cens='interval',
                                      ub=vd[i+1],
                                      weights=i+1,
                                      type='yton'))
        }
    }
    
    if(st$ytof[1] != 0){
        for(i in st$ytof){
            df <- rbind(df,data.frame(y=vd[i],
                                      cens='interval',
                                      ub=vd[i+1],
                                      weights=i+.5,
                                      type='ytof'))
            
        }
    }
    
    if(st$fton[1] != 0){
        for(i in st$fton){
            df <- rbind(df,data.frame(y=vd[i],
                                      cens='interval',
                                      ub=vd[i+1],
                                      weights=i+1,
                                      type='fton'))
        }
    }
    
    if(st$yend[1] != 0) {
        df <- rbind(df,data.frame(y=vd[st$yend],
                                  cens='right',
                                  ub=vd[st$yend]+1,
                                  weights=st$yend+1,
                                  type='yend'))
    } else if(st$fend[1] != 0) {
        df <- rbind(df,data.frame(y=vd[st$fend],
                                  cens='right',
                                  ub=vd[st$fend]+1,
                                  weights=st$fend+1,
                                  type='fend'))
    }
    
    df$weights <- df$weights/sum(df$weights)
    df$blrow <- blrow
    
    return(df)
}

bl_ll <- apply(blc,MARGIN=1,FUN=mk_df)

bld <- data.frame()
for(i in bl_ll){
    bld <- rbind(bld,i)
}

rm(list=c('bl_ll','mk_df','status','i','blc'))

