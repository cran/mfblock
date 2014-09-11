rmfblock <-
function(n=stop("argument n is needed"),x=stop("argument x is needed")){


stopifnot(is.integer(as.integer(n)),length(as.integer(n))==1,class(x)=="mfblock")

n=as.integer(n)

par<-c(x$distcmn[[2]],x$distspec[[2]],x$distn[[2]],x$coefcmn,x$coefspec)

idx<-numeric()
rdist<-list()

j=1
for(i in c(x$distcmn[[1]],x$distspec[[1]],x$distn[[1]])){

if(sum(names(l__fc)==i)>0){

idx=c(idx,l__fc[[i]][[2]])


rdist[[j]]=l__fc[[i]][[1]];

}else{

idx=c(idx,dist_env$l__fc[[i]][[2]])

rdist[[j]]=dist_env$l__fc[[i]][[1]];

}

j=j+1
}


idx<-cumsum(c(idx,length(x$k),length(x$k)));


z0=rdist[[1]](n,par[1:idx[1]])

z2=rdist[[3]](n*x$dim,par[(idx[2]+1):idx[3]])

#define the coefficient vector


z1<-numeric()

for(i in 1:length(x$k)){

z1=c(z1,rep(rdist[[2]](n,par[(idx[1]+1):idx[2]]),x$k[i]))

}

zz0=rep(par[(idx[3]+1):idx[4]],n*x$k)
zz1=rep(par[-(1:idx[4])],n*x$k)

matrix(zz0*z0+zz1*z1+z2,nrow=n)

}
