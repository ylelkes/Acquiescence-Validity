library(polycor)

ef1s <- data.frame(sapply(FUN=kros,do.call(rbind,hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy1stdpo),as.ordered(anes2012$efficacy1stdpre))))[c(1,5)])[c(2,4),1]))
ef1r <- data.frame(sapply(FUN=kros,do.call(rbind,hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy1revpo),as.ordered(anes2012$efficacy1revpre))))[c(1,5)])[c(2,4),1]))

ef2s <- data.frame(sapply(FUN=kros,do.call(rbind,hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy2stdpo),as.ordered(anes2012$efficacy2stdpre))))[c(1,5)])[c(2,4),1]))
ef2r <- data.frame(sapply(FUN=kros,do.call(rbind,hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy2revpo),as.ordered(anes2012$efficacy2revpre))))[c(1,5)])[c(2,4),1]))

ef3s <- data.frame(sapply(FUN=kros,do.call(rbind,hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy3stdpo),as.ordered(anes2012$efficacy3stdpre))))[c(1,5)])[c(2,4),1]))
ef3r <- data.frame(sapply(FUN=kros,do.call(rbind,hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy3revpo),as.ordered(anes2012$efficacy3revpre))))[c(1,5)])[c(2,4),1]))

ef4s <- data.frame(sapply(FUN=kros,do.call(rbind,hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy4stdpo),as.ordered(anes2012$efficacy4stdpre))))[c(1,5)])[c(2,4),1]))
ef4r <- data.frame(sapply(FUN=kros,do.call(rbind,hetcor(na.omit(data.frame(as.ordered(anes2012$efficacy4revpo),as.ordered(anes2012$efficacy4revpre))))[c(1,5)])[c(2,4),1]))



reliabilityanalysis <- rbind(c(ef1s,ef1r),c(ef2s,ef2r),c(ef3s,ef3r),c(ef4s,ef4r))
rownames(reliabilityanalysis) <- c("Efficacy 1","Efficacy 2","Efficacy 3","Efficacy 4")
colnames(reliabilityanalysis) <- c("Standard","Revised","Difference")

save(reliabilityanalysis,file="reliabilityanalysis.RData")

#######

ef1s <- hetcor(na.omit(data.frame(as.ordered(anes2012agree$efficacy1stdpo),as.ordered(anes2012agree$efficacy1stdpre))))$correlations[2,1]
ef1r <- hetcor(na.omit(data.frame(as.ordered(anes2012agree$efficacy1revpo),as.ordered(anes2012agree$efficacy1revpre))))$correlations[2,1]

ef2s <- hetcor(na.omit(data.frame(as.ordered(anes2012agree$efficacy2stdpo),as.ordered(anes2012agree$efficacy2stdpre))))$correlations[2,1]
ef2r <- hetcor(na.omit(data.frame(as.ordered(anes2012agree$efficacy2revpo),as.ordered(anes2012agree$efficacy2revpre))))$correlations[2,1]

ef3s <- hetcor(na.omit(data.frame(as.ordered(anes2012agree$efficacy3stdpo),as.ordered(anes2012agree$efficacy3stdpre))))$correlations[2,1]
ef3r <- hetcor(na.omit(data.frame(as.ordered(anes2012agree$efficacy3revpo),as.ordered(anes2012agree$efficacy3revpre))))$correlations[2,1]

ef4s <- hetcor(na.omit(data.frame(as.ordered(anes2012agree$efficacy4stdpo),as.ordered(anes2012agree$efficacy4stdpre))))$correlations[2,1]
ef4r <- hetcor(na.omit(data.frame(as.ordered(anes2012agree$efficacy4revpo),as.ordered(anes2012agree$efficacy4revpre))))$correlations[2,1]

reliabilityanalysis <- rbind(c(ef2s,ef2r,ef2s-ef2r),c(ef3s,ef3r,ef3s-ef3r),c(ef4s,ef4r,ef4s-ef4r),c(ef5s,ef5r,ef5s-ef5r))
rownames(reliabilityanalysis) <- c("Efficacy 1","Efficacy 2","Efficacy 3","Efficacy 4")
colnames(reliabilityanalysis) <- c("Standard","Revised","Difference")

save(reliabilityanalysis,file="reliabilityanalysisagreeable.RData")
