
covProp <- function(data.la, data.cov, cov_pop){
  ## create combined array of:
  ##  - covariate (e.g. ethnic group)
  ##  - sub-population size
  ##  - number of TB notifications and proportions
  ## for each LA
  ##
  ## data.la: ETS local authority data
  ## data.cov: ETS covariate (e.g. ethnic group) data
  ## cov_pop: covariate (e.g. ethnic group) sizes by LA from ONS
  
  ## ethnic group and Local Authority cross-tab counts
  cov.tab <- table(data.la, data.cov)#, useNA="always")
  cov.tab <- as.data.frame.matrix(cov.tab)
  ## remove column with empty text
  cov.tab <- cbind(la=rownames(cov.tab), cov.tab[,colnames(cov.tab)!="V1"])
  
  ## column names eg ethnic groups
  cov.names <- names(cov.tab)[names(cov.tab)!="la"]
  
  #  ## TODO ## alternatively, long format may be better throughout 
  #   ## convert to a long dataframe
  #   ## equivalent to melt()
  #   ethgrp.tab.melt <- as.data.frame(ethgrp.tab)
  #   names(ethgrp.tab.melt) <- c("la", "ethgrp", "tb")
  #   
  #   ethgrp.join <- merge(ethgrp.tab.melt, ethnicity_ONS.melt, by=c("la","ethgrp"))
  #   ethgrp.join <- cbind(ethgrp.join, prop=ethgrp.join$tb/ethgrp.join$pop)
  #   ## to obtain wide format for given variable  
  #   ## select apropriate value.var
  #   #dcast(data=head(ethgrp.join), la~ethgrp, value.var="tb")
  
  
  ## join TB notification and sub-population arrays by LA names
  cov.join <- merge(cov.tab, cov_pop[,c("la",cov.names)], by="la", all=TRUE, suffixes=c(".tb",".pop"))
  
  
  ## proportions of sub-populations with notified TB
  prop <- cov.join[, paste(cov.names, ".tb", sep="")]/cov.join[, paste(cov.names, ".pop", sep="")]
  names(prop) <- gsub("\\.tb", ".prop", names(prop))
  cov.join <- data.frame(cov.join, prop, check.names=FALSE)
  
  
cov.join
}


bplot <- function(fit, data, var){
  ##
  ## boxplot of regression prediction and explanatory variable e.g. la
  ##
  
  pred <- predict(fit, type="response")
  lst <- c(Obs=split(data$tb, data[,var]), Pred=split(pred, data[,var]))
  lst <- lst[c(rbind(names(lst)[1:(length(names(lst))/2)], names(lst)[((length(names(lst))/2)+1):length(names(lst))]))]
  par(mar = c(9, 4, 4, 2) + 0.1)
  boxplot(lst, col=rep(c("gray","red"), len=length(lst)), las=2, ylim=c(0,100), ylab="Number of active TB cases") 
}
## END FUNCTION ##


fitSep <- function(glm.dat, sepvar, regvars){
  ## simple Poisson regression fit
  ## separated by given covariate value, e.g. LA,
  ## including sub-population sizes as offset
  ##  
  ## sepvar: separation variable (string)
  ## glm.dat: regression data set (dataframe)
  ## regvars: regression explanatory variables (string)
  ##          Can be single-level variable name or higher-level "(1|<name>)" format
  ## call: fitSep(glm.dat, sepvar="ethgrp", regvars="la")
  
  names <- levels(droplevels(glm.dat[,sepvar]))

  res <- list()
  
  for (nm in names){
    
    glm.dat.nm <- subset(glm.dat, glm.dat[,sepvar]==nm)
    
    ## if passed multilevel variables need to separate the variable name with the syntax (1|<name>)
    regvars.rm <- gsub("\\(1\\|", "", regvars)
    regvars.rm <- gsub("\\)", "", regvars.rm)
    
    ## check that there are at least 2 levels of each explanatory variable
    ## return logical for each variable
    multilevels <- apply(glm.dat.nm[,regvars.rm,drop=FALSE], 2, function(y)
                                                                          length(unique(y))>1)
    
    if(prod(multilevels)==TRUE){
      
      formula <- as.formula(paste("tb ~ offset(log(pop)) + ", paste(regvars, collapse="+")))
      print(nm); print(formula)
      print(summary(fit <- glm(formula, family=poisson(link=log), data=glm.dat.nm)))
    }
    
    res[[nm]] <- fit
  }
  
res
}
## END FUNCTION ##


plotRE <- function(u0){
  ## plot random effects
  ## with standard errors
  
  u0se <- sqrt(attr(u0[[1]], "postVar")[1,,])
  
  ethgrpID <- seq_along(rownames(u0[[1]]))
  u0tab <- cbind(ethgrpID, u0[[1]], u0se)
  colnames(u0tab) <- c("ethgrpID", "u0", "u0se")
  u0tab <- u0tab[order(u0tab$u0),]
  u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
  colnames(u0tab)[4] <- "u0rank"
  u0tab <- u0tab[order(u0tab$ethgrpID),]
  
  plot(u0tab$u0rank, exp(u0tab$u0), type="n", xlab="u_rank", ylab="conditional modes of r.e. for ethnic groups")
  segments(u0tab$u0rank, exp(u0tab$u0-1.96*u0tab$u0se), u0tab$u0rank, exp(u0tab$u0+1.96*u0tab$u0se))
}
## END FUNCTION ##


getformula <- function(names)
  ## full single-level model with additional group level covariates
  as.formula(paste("tb ~ offset(log(pop)) + agegrp3 + sex + ", paste(names, collapse=" + "), sep=""))




