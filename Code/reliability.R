library(polycor)
source("Code/utility functions.R")


  
ef1s <- hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy1stdpo),as.ordered(anes2012$efficacy1stdpre))))$correlations[2,1]
ef1r <- hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy1revpo),as.ordered(anes2012$efficacy1revpre))))$correlations[2,1]

ef2s <- hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy2stdpo),as.ordered(anes2012$efficacy2stdpre))))$correlations[2,1]
ef2r <- hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy2revpo),as.ordered(anes2012$efficacy2revpre))))$correlations[2,1]

ef3s <- hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy3stdpo),as.ordered(anes2012$efficacy3stdpre))))$correlations[2,1]
ef3r <- hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy3revpo),as.ordered(anes2012$efficacy3revpre))))$correlations[2,1]

ef4s <- hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy4stdpo),as.ordered(anes2012$efficacy4stdpre))))$correlations[2,1]
ef4r <- hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy4revpo),as.ordered(anes2012$efficacy4revpre))))$correlations[2,1]

reliabilityanalysis <- rbind(c(ef1s,ef1r,ef1s-ef1r),c(ef2s,ef2r,ef2s-ef2r),c(ef3s,ef3r,ef3s-ef3r),c(ef4s,ef4r,ef4s-ef4r))
rownames(reliabilityanalysis) <- c("Efficacy 1","Efficacy 2","Efficacy 3","Efficacy 4")
colnames(reliabilityanalysis) <- c("Standard","Revised","Difference")

save(reliabilityanalysis,file="./output/reliabilityanalysisagreeable.RData")

load("reliabilityanalysis.RData")
library(xtable)   
sink("output/reliability1.txt")
print(xtable(reliabilityanalysis,caption="Test-Retest Reliability Estimates for Agree-Disagree Item and Construc Items",align="lccc"),caption.placement="top")
sink()