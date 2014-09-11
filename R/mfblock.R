mfblock <-
function(distcmn=stop("distcmn argument needed"),distspec=stop("distspec argument needed"),distn=stop("distn argument needed"),coefcmn=stop("coefcmn argument needed"),coefspec=stop("coefspec argument needed"),k=stop("k argument needed")){


stopifnot(is.list(distcmn),is.list(distspec),is.list(distn),length(distcmn)==2,length(distspec)==2,length(distn)==2,is.character(distcmn[[1]]),is.character(distspec[[1]]),is.character(distn[[1]]),is.numeric(c(distcmn[[2]],distspec[[2]],distn[[2]]))); 


if(sum(names(l__fc)==distcmn[[1]])>0){if(length(distcmn[[2]])!= l__fc[[distcmn[[1]]]][[2]]){stop("length of distcmn[[1]][[2]] is not valid");}} else if(sum(names(dist_env$l__fc)==distcmn[[1]])>0){if(length(distcmn[[2]])!= dist_env$l__fc[[distcmn[[1]]]][[2]]){stop("length of distcmn[[1]][[2]] is not valid");}}
else{stop("distcmn[[1]] is not valid");}

if(sum(names(l__fc)==distspec[[1]])>0){if(length(distspec[[2]])!= l__fc[[distspec[[1]]]][[2]]){stop("length of distspec[[1]][[2]] is not valid");}} else if(sum(names(dist_env$l__fc)==distspec[[1]])>0){if(length(distspec[[2]])!= dist_env$l__fc[[distspec[[1]]]][[2]]){stop("length of distspec[[1]][[2]] is not valid");}}
else{stop("distspec[[1]] is not valid");}

if(sum(names(l__fc)==distn[[1]])>0){if(length(distn[[2]])!= l__fc[[distn[[1]]]][[2]]){stop("length of distn[[1]][[2]] is not valid");}} else if(sum(names(dist_env$l__fc)==distn[[1]])>0){if(length(distn[[2]])!= dist_env$l__fc[[distn[[1]]]][[2]]){stop("length of distn[[1]][[2]] is not valid");}}
else{stop("distn[[1]] is not valid");}


stopifnot(is.integer(k),sum(k>0)==length(k));

stopifnot(length(coefcmn)==length(k),length(coefspec)==length(k));

l2<-list();

l2$distcmn=distcmn;
l2$distspec=distspec;
l2$distn=distn;
l2$coefspec=coefspec;
l2$coefcmn=coefcmn;
l2$k=k; 

class(l2)="mfblock";

l2$dim=sum(k);

l2; 


}
