summary.fitmfblock <-
function(object,jac.repar=NULL,...){

if(is.null(jac.repar)){ 
if(is.na(object$repar)==FALSE){
if(is.numeric(object$mfb$distcmn[[2]])==FALSE){

#the whole thing is NA
#want new parametrization 

TAB <- cbind(Estimate =rep(NA,object$repar),StdErr = rep(NA,object$repar),"Z value"=rep(NA,object$repar),"Pr(>|z|)" = rep(NA,object$repar));

}else{est<-object$opt$solution
se <- sqrt(diag(as.matrix(object$acv)))
zval <- est/se
TAB <- cbind(Estimate =est,StdErr = se,"Z value"=zval,"Pr(>|z|)" = 2*pnorm(-abs(zval)));}

}else{

if(is.numeric(object$mfb$distcmn[[2]])==FALSE){

#old parametrization
idx=0
for(i in c("distcmn","distspec","distn")){
if(sum(names(l__fc)==object$mfb[[i]][[1]])>0){

idx=sum(idx,l__fc[[object$mfb[[i]][[1]]]][[2]])


}else{

idx=sum(idx,dist_env$l__fc[[object$mfb[[i]][[1]]]][[2]])

}



}

idx=sum(idx,2*length(object$mfb$k))


TAB <- cbind(Estimate =rep(NA,idx),StdErr = rep(NA,idx),"Z value"=rep(NA,idx),"Pr(>|z|)" = rep(NA,idx));

}else{est<-object$opt$solution
se <- sqrt(diag(as.matrix(object$acv)))
zval <- est/se
TAB <- cbind(Estimate =est,StdErr = se,"Z value"=zval,"Pr(>|z|)" = 2*pnorm(-abs(zval)));}

##row names for TAB


if(sum(names(l__fc)==object$mfb$distcmn[[1]])>0){nm=rep("distcmn",l__fc[[object$mfb$distcmn[[1]]]][[2]])}else{n=rep("distcmn",dist_env$l__fc[[object$mfb$distcmn[[1]]]][[2]])}

if(sum(names(l__fc)==object$mfb$distspec[[1]])>0){nm=c(nm,rep("distspec",l__fc[[object$mfb$distspec[[1]]]][[2]]))}else{nm=c(nm,rep("distspec",dist_env$l__fc[[object$mfb$distspec[[1]]]][[2]]))}

if(sum(names(l__fc)==object$mfb$distn[[1]])>0){nm=c(nm,rep("distn",l__fc[[object$mfb$distn[[1]]]][[2]]))}else{nm=c(nm,rep("distn",dist_env$l__fc[[object$mfb$distn[[1]]]][[2]]))}

dimnames(TAB)[[1]]<-c(nm,rep("coefcmn",length(object$mfb$k)),rep("coefspec",length(object$mfb$k)))

}

}


else{

#calculate number of parameters:
idx=0
for(i in c("distcmn","distspec","distn")){
if(sum(names(l__fc)==object$mfb[[i]][[1]])>0){

idx=sum(idx,l__fc[[object$mfb[[i]][[1]]]][[2]])


}else{

idx=sum(idx,dist_env$l__fc[[object$mfb[[i]][[1]]]][[2]])

}



}

idx=sum(idx,2*length(object$mfb$k))




stopifnot(is.function(jac.repar))

if(is.numeric(object$mfb$distcmn[[2]])==FALSE){

#we want old parameter space

TAB <- cbind(Estimate =rep(NA,idx),StdErr = rep(NA,idx),"Z value"=rep(NA,idx),"Pr(>|z|)" = rep(NA,idx));

}else{
est<-c(object$mfb$distcmn[[2]],object$mfb$distspec
[[2]],object$mfb$distn[[2]],object$mfb$coefcmn,object$mfb$coefspec)

J<-jac.repar(object$opt$solution)
stopifnot(is.matrix(J),dim(J)==c(length(object$opt$solution),length(est)))

if(is.matrix(object$acv)==FALSE){

se<-rep(NA,idx)
TAB <- cbind(Estimate =est,StdErr = se,"Z value"=se,"Pr(>|z|)" = se);
}
else{
se<-sqrt(diag(t(J)%*%object$acv%*%J))
zval <- est/se
TAB <- cbind(Estimate =est,StdErr = se,"Z value"=zval,"Pr(>|z|)" = 2*pnorm(-abs(zval)));}

}


#row names for TAB

if(sum(names(l__fc)==object$mfb$distcmn[[1]])>0){nm=rep("distcmn",l__fc[[object$mfb$distcmn[[1]]]][[2]])}else{nm=rep("distcmn",dist_env$l__fc[[object$mfb$distcmn[[1]]]][[2]])}

if(sum(names(l__fc)==object$mfb$distspec[[1]])>0){nm=c(nm,rep("distspec",l__fc[[object$mfb$distspec[[1]]]][[2]]))}else{nm=c(nm,rep("distspec",dist_env$l__fc[[object$mfb$distspec[[1]]]][[2]]))}

if(sum(names(l__fc)==object$mfb$distn[[1]])>0){nm=c(nm,rep("distn",l__fc[[object$mfb$distn[[1]]]][[2]]))}else{nm=c(nm,rep("distn",dist_env$l__fc[[object$mfb$distn[[1]]]][[2]]))}


dimnames(TAB)[[1]]<-c(nm,rep("coefcmn",length(object$mfb$k)),rep("coefspec",length(object$mfb$k)))


}


res <- list(coefficients=TAB);
class(res) <- "summary.fitmfblock";
res;

}






