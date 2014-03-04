boxplot.avg <- function(glm.mlevel){
  ## plot average (fixed effect) proportion of active TB
  ## from the data
  ##
  ## TODO ##
  ## test
  ##
  
  ## calculate within LA averages
  glm.dat <- ddply(glm.mlevel, 'la', transform, laAvg = median(prop))
  
  ## reorder rows from smallest to largest average value
  glm.dat <- glm.dat[order(glm.dat$laAvg),]
  
  ## remove unused la levels
  glm.dat$la <- droplevels(glm.dat$la)
  
  ## reorder la levels from smallest to largest mean/median value
  #glm.dat$la <- reorder(glm.dat$la, new.order=unique(glm.dat$la))
  glm.dat$la <- factor(glm.dat$la, levels = unique(glm.dat$la))
  
  head(glm.dat)
  
  # box plot tb rates by la (data)
  q <- qplot(y=prop, x=la, geom="boxplot", data=head(glm.dat,2000), ylim=c(0,0.1))
  q + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  q <- qplot(y=prop, x=la, geom="boxplot", data=tail(glm.dat,1000), ylim=c(0,0.1))
  #q <- q + geom_hline(yintercept=0.001)
  q + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
## END FUNCTION ##


scatplot.glmer <- function(param.grid, glm.mlevel, cov){
  ##
  ##
  ## cov: covariate value (string) e.g. "FEMALE"
  
  ## parameter full grid
  q.grid <- qplot(param.grid$agegrp3[param.grid$sex==cov], 
                    param.grid$pred[param.grid$sex==cov], main=paste(cov, "predicted"), xlab="", ylab="", ylim=c(0,0.05), alpha=I(1/5), size = I(4))#, geom="jitter")#, color=glm.mlevel$la[glm.mlevel$sex=="FEMALE"])
  q.grid + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ## data
  q.dat <- qplot(glm.mlevel$agegrp3[glm.mlevel$sex==cov], 
             glm.mlevel$prop[glm.mlevel$sex==cov], main=paste(cov, "data"), xlab="", ylab="", ylim=c(0,0.02), alpha=I(1/3), size = I(5))#, color=glm.mlevel$la[glm.mlevel$sex=="FEMALE"], geom="jitter")
  q.dat + theme(axis.text.x = element_text(angle = 90, hjust = 1))

  grid.arrange(q.dat, q.grid, nrow=2)
}
## END FUNCTION ##


plot_datpred <- function(x, group, dat, fit, strat="", stratname=NA){
  ## plot predicted curve and data points
  ## for var1 (e.g. agegrp3) against proportion active TB
  ## stratified by var2 (e.g. ethgrp)
  ##
  ## x: var1 name (string)
  ## group: var2 name (string)
  ## dat: data of TB cases
  ## fit: associated regression results for dat
  
  ## subset data
  if(!is.na(stratname)){
    #   dat <- dat[dat[,substitute(strat)]==strat,]
    dat <- dat[dat[,stratname]==strat,]
    fit <- fit[[strat]]
  }
  
  param.grid <- expand.grid(levels(droplevels(dat[,x])), levels(droplevels(dat[,group])))
  colnames(param.grid) <- c(x, group)
  param.grid <- cbind(param.grid, pop=1)
  
  param.grid$phat <- predict(fit, newdata=param.grid, type="response")
  
  ggplot(dat, aes_string(x=x, y="prop", colour=group)) + 
    geom_point(position=position_jitter(w=0.1)) + 
    geom_line(data=param.grid, aes_string(x=x, y="phat", colour=group, group=group)) + ylim(0,max(param.grid$phat))  + 
    ggtitle(strat)
    #ggtitle(unique(dat[,strat]))
}
## END FUNCTION ##






