library(tidyr)
library(dplyr)
library(splines)

plot_observed_and_prediction <- function(x_axis,test_data0,fit,rate_per_casualty=T,rate_per_striker=T){
  plot_se <- F
  items <- unique(test_data0[[x_axis]])
  
  raw <- predictions <- prediction.lower <- prediction.upper <- rep(0,length(items))
  for(k in 1:length(items)){
    test_data1 <- test_data0
    test_data1 <- test_data1[test_data1[[x_axis]]==items[k],]
    distance <- 1
    if(rate_per_casualty) distance <- distance*sum(unique(test_data1$cas_distance))
    if(rate_per_striker) distance <- distance*sum(unique(test_data1$strike_distance))
    if(nrow(test_data1)>0){
      raw[k] <- sum(test_data1$count)/distance
      suppressWarnings(pred_list <- predict(fit,newdata=test_data1,type='link',se.fit=plot_se))
      if(plot_se==F) test_data1$pred <- pred_list
      if(plot_se==T){
        test_data1$pred <- pred_list[[1]]
        prediction.lower[k] <- sum(exp(test_data1$pred-pred_list[[2]]))/distance
        prediction.upper[k] <- sum(exp(test_data1$pred+pred_list[[2]]))/distance
      }
      predictions[k] <- sum(exp(test_data1$pred))/distance
    }
  }
  title <- suppressWarnings(paste0(
    paste(c('f','m')[unique(test_data0$cas_male)+1],collapse='/'),' ',paste(c(range(test_data0$cas_age)),collapse='-'),'yo ',paste(unique(test_data0$cas_mode),collapse='/'),
    '\nhit by ',paste(c('f','m')[unique(test_data0$strike_male)+1],collapse='/'),' ',paste(c(range(test_data0$strike_age)),collapse='-'),'yo ',paste(unique(test_data0$strike_mode),collapse='/'),
    '\non ',paste(unique(test_data0$roadtype),collapse='/'),' in ',paste(c(range(test_data0$year)),collapse='-'),'; ',paste(unique(test_data0$cas_severity),collapse='/')
  ))
  cat(paste0('Plotted ',suppressWarnings(paste0(
    paste(c('female','male')[unique(test_data0$cas_male)+1],collapse='/'),' ',paste(c(range(test_data0$cas_age)),collapse='-'),'-year-old ',paste(unique(test_data0$cas_mode),collapse='/'),
    ' road users, \nhit by ',paste(c('female','male')[unique(test_data0$strike_male)+1],collapse='/'),' ',paste(c(range(test_data0$strike_age)),collapse='-'),'-year-old ',paste(unique(test_data0$strike_mode),collapse='/'),
    ' road users, \non ',paste(unique(test_data0$roadtype),collapse='/'),' roads, in ',paste(c(range(test_data0$year)),collapse='-'),'; ',paste(unique(test_data0$cas_severity),collapse='/')
  )),'\n'))
  ylab <- 'number of injuries'
  if(rate_per_casualty) ylab <- paste0(ylab,'/cas bn km')
  if(rate_per_striker) ylab <- paste0(ylab,'/str bn km')
  ylim <- max(predictions,prediction.upper,raw)
  cols <- c('darkorange2','navyblue')
  x11();
  if(x_axis%in%c('strike_age','year','cas_age')){
    par(mar=c(5,5,4,1));
    plot(items,raw,frame=F,typ='b',col=cols[1],pch=1,lwd=2,cex.axis=1.25,cex.lab=1.25,xlab=x_axis,
      ylab=ylab,ylim=c(0,1.4*ylim),main=title)
    if(plot_se==T) arrows(x0=items,y0=prediction.lower,y1=prediction.upper,angle=90,code=3,length=0.1,col='gray')
    lines(items,predictions,typ='b',col=cols[2],pch=1,lwd=2)
    legend(x=min(items),y=1.35*ylim,legend=c('Observed','Prediction'),bty='n',cex=1.25,lty=1,pch=1,lwd=2,col=cols)
  }else{
    par(mar=c(7,5,4,1));
    bar <- barplot(rbind(raw,predictions),beside=T,col=cols,las=2,cex.lab=1.25,names=items,
      cex.axis=1.25,ylim=c(0,1.4*ylim),ylab=ylab,xlab=x_axis,main=title)
    if(plot_se==T) arrows(x0=bar[2,],y0=prediction.lower,y1=prediction.upper,angle=90,code=3,length=0.1,col='gray')
    legend(x=bar[1,1],y=1.35*ylim,legend=c('Observed','Prediction'),bty='n',cex=1.25,fill=cols)
  }
}

if(file.exists('test_data.Rdata')){
  test_data <- readRDS('test_data.Rdata')
}else{
  ## load data
  ss19.0 <- readRDS('~/overflow_dropbox/ITHIM/InjuryModel/ss19.0.Rdata')
  distances <- readRDS('~/overflow_dropbox/ITHIM/InjuryModel/predictDistances12.Rdata')
  travel_distances_c_predict <- distances$travel_distances_c_predict
  travel_distances_s_predict <- distances$travel_distances_s_predict
  
  ## get all covariates
  all_modes <- dimnames(travel_distances_c_predict)[[1]]
  all_c_ages <- dimnames(travel_distances_c_predict)[[2]]
  all_s_ages <- dimnames(travel_distances_s_predict)[[2]]
  all_years <- dimnames(travel_distances_c_predict)[[4]]
  all_roads <- dimnames(travel_distances_c_predict)[[5]]
  
  ss19.1 <- ss19.0
  ssg <-
    group_by(ss19.1,year,cas_male,cas_severity,cas_mode,strike_mode,cas_age,strike_age,roadtype,strike_male) %>% 
    summarise(count=n()) %>% 
    droplevels() %>% 
    as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
    complete(year,cas_male,cas_severity,cas_mode,strike_mode,cas_age,strike_age,roadtype,strike_male,fill=list(count=0)) 
  test_data <- ssg %>%  mutate(
    cas_distance = apply(cbind(ssg$cas_mode,factor(ssg$cas_age),factor(ssg$cas_male),factor(ssg$year),ssg$roadtype),1,
      function(x)sum(travel_distances_c_predict[which(all_modes==casualty_mode[x[1]]),
        which(all_c_ages==cas_ages[x[2]]),
        which(c(0,1)==cas_gen[x[3]]),
        which(all_years==years[x[4]]),
        which(all_roads==road[x[5]]),1:2])),
    strike_distance = apply(cbind(ssg$strike_mode,factor(ssg$strike_age),factor(ssg$strike_male),factor(ssg$year),ssg$roadtype),1,
      function(x)travel_distances_s_predict[which(all_modes==str_mode[x[1]]),
        which(all_s_ages==str_ages[x[2]]),
        which(c(0,1)==str_gen[x[3]]),
        which(all_years==years[x[4]]),
        which(all_roads==road[x[5]])])
  )
  test_data <- subset(test_data,strike_distance>0&cas_distance>0)
  saveRDS(test_data,'test_data.Rdata')
}
