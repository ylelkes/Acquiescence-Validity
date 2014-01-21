vars <- with(anes2012agree,data.frame(efficacy1stdpre,efficacy1revpre,efficacy2stdpre,efficacy2revpre,efficacy3stdpre,efficacy3revpre,efficacy4stdpre,efficacy4revpre,efficacy1stdpo,efficacy1revpo,efficacy2stdpo,efficacy2revpo,efficacy3stdpo,efficacy3revpo,efficacy4stdpo,efficacy4revpo))

library(ggplot2)
l <- data.frame(matrix(nrow=ncol(vars),ncol=2))
for(i in 1:ncol(vars)){
  a <- hetcor(as.ordered(vars[,i]),as.ordered(anes2012agree$strengthofpid))
  j <- c(a$correlations[2,1],a$std.errors[2,1])
  l[i,] <- j
}
l$question <- c("Efficacy 1","Efficacy 1","Efficacy 2","Efficacy 2","Efficacy 3","Efficacy 3","Efficacy 4","Efficacy 4","Efficacy 1","Efficacy 1","Efficacy 2","Efficacy 2","Efficacy 3","Efficacy 3","Efficacy 4","Efficacy 4")

l$wave <- c(rep("Wave 1",8),rep("Wave 2",8))
l$type <- as.factor(rep(c("Agree-Disagree","Construct Specific"),8))

names(l) <- c("mean","se","question","wave","type")
widea <- data.frame(l[l$type=="Agree-Disagree",],l[l$type=="Construct Specific",])
widea[,1]>widea[,6]
mean(widea[,1])
mean(widea[,6])

panela <- ggplot(l,aes(x=mean,y=type))+geom_point()+geom_errorbarh(aes(xmax=mean+1.96*se,xmin=mean-1.96*se))+facet_grid(wave~question,scales="free")+theme_bw()+xlab("Polychoric Correlation")+ylab("Response Form")+ggtitle(expression(R["PID Strength , Political Efficacy"]))

#########
l <- data.frame(matrix(nrow=ncol(vars),ncol=2))
for(i in 1:ncol(vars)){
  a <- hetcor(as.ordered(vars[,i]),as.numeric(anes2012agree$pctchance))
  j <- c(a$correlations[2,1],a$std.errors[2,1])
  l[i,] <- j
}
l$question <- c("Efficacy 1","Efficacy 1","Efficacy 2","Efficacy 2","Efficacy 3","Efficacy 3","Efficacy 4","Efficacy 4","Efficacy 1","Efficacy 1","Efficacy 2","Efficacy 2","Efficacy 3","Efficacy 3","Efficacy 4","Efficacy 4")

l$wave <- c(rep("Wave 1",8),rep("Wave 2",8))
l$type <- as.factor(rep(c("Agree-Disagree","Construct Specific"),8))

names(l) <- c("mean","se","question","wave","type")
panelb <- ggplot(l,aes(x=mean,y=type))+geom_point()+geom_errorbarh(aes(xmax=mean+1.96*se,xmin=mean-1.96*se))+facet_grid(wave~question,scales="free")+theme_bw()+xlab("Polychoric Correlation")+ylab("Response Form")+ggtitle(expression(R["Percent Chance of Voting , Political Efficacy"]))
wideb <- data.frame(l[l$type=="Agree-Disagree",],l[l$type=="Construct Specific",])
wideb[,1]>wideb[,6]
mean(widea[,1])
mean(widea[,6])
#####
#########
l <- data.frame(matrix(nrow=ncol(vars),ncol=2))
for(i in 1:ncol(vars)){
  a <- hetcor(as.ordered(vars[,i]),as.numeric(anes2012agree$polactivity))
  j <- c(a$correlations[2,1],a$std.errors[2,1])
  l[i,] <- j
}
l$question <- c("Efficacy 1","Efficacy 1","Efficacy 2","Efficacy 2","Efficacy 3","Efficacy 3","Efficacy 4","Efficacy 4","Efficacy 1","Efficacy 1","Efficacy 2","Efficacy 2","Efficacy 3","Efficacy 3","Efficacy 4","Efficacy 4")

l$wave <- c(rep("Wave 1",8),rep("Wave 2",8))
l$type <- as.factor(rep(c("Agree-Disagree","Construct Specific"),8))

names(l) <- c("mean","se","question","wave","type")
widec <- data.frame(l[l$type=="Agree-Disagree",],l[l$type=="Construct Specific",])

panelc <- ggplot(l,aes(x=mean,y=type))+geom_point()+geom_errorbarh(aes(xmax=mean+1.96*se,xmin=mean-1.96*se))+facet_grid(wave~question,scales="free")+theme_bw()+xlab("Polychoric Correlation")+ylab("Response Form")+ggtitle(expression(R["Political Activity , Political Efficacy"]))
widec[,1]>widec[,6]
mean(widea[,1])
mean(widea[,6])
pdf("./output/plotvalidityagrees.pdf",width=12,height=12)
multiplot(panela,panelb,panelc)
dev.off()