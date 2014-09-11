

smm<-
function(x=stop("argument x needed"),dist=stop("argument dist needed"),k=stop("argument k needed"),repar=NULL,s=stop("argument s needed"),q=stop("argument q needed"),acv=FALSE,eff=NULL,b=NULL,eps=NULL,x0=NULL,lower=stop("argument lower needed"),upper=stop("argument upper needed"),opts=list("algorithm"="NLOPT_LN_SBPLX","maxeval"=800,"xtol_abs"=0.005),seed=as.integer(Sys.time())){

#check that x is a valid object:

stopifnot(is.matrix(x));

k=as.integer(k)

stopifnot(is.integer(k))

stopifnot(sum(k>0)==length(k));


#do a check for the dimension of x with k

stopifnot(sum(k)==dim(x)[2]);

#do a basic check for s:

stopifnot(is.numeric(s) && s>0 && length(s)==1)

#do a basic check for q:

stopifnot(sum(q>0)==length(q) && sum(q<1)==length(q))

#do a basic check for b:

if((is.null(eff)==FALSE && is.na(eff)) || (acv==TRUE && is.matrix(eff)==FALSE)){stopifnot(is.numeric(b) && b>0 && length(b)==1);}

#check algorithm specified in opts

if(is.null(opts$algorithm)){opts$algorithm="NLOPT_LN_SBPLX"}
if(is.null(opts$maxeval)){opts$maxeval=800}
if(is.null(opts$xtol_abs)){opts$xtol_abs=0.005}


#do a basic check for dist:


idx<-numeric()
rdist<-list()

stopifnot(is.character(dist),length(dist)==3)

for(i in 1:3){
if(sum(names(l__fc)==dist[i])>0){

idx=c(idx,l__fc[[dist[i]]][[2]])

#save the simulation functions corresponding to dist:

rdist[[i]]=l__fc[[dist[i]]][[1]];

}else if(sum(names(dist_env$l__fc)==dist[i])>0){

idx=c(idx,dist_env$l__fc[[dist[i]]][[2]])

#save the simulation functions corresponding to dist:

rdist[[i]]=dist_env$l__fc[[dist[i]]][[1]];

}

else{stop("dist argument is invalid")}

}



idx<-cumsum(c(idx,length(k),length(k)));


#save length of parameter 

if(is.null(repar)){lenpar=idx[length(idx)]} else{lenpar=repar[[2]]};


#do a basic check on upper and lower

stopifnot(length(lower)==length(upper));

if(length(lower)!=lenpar){stop("length(lower) is not valid");};



if(is.null(repar)==FALSE){
stopifnot(is.list(repar),length(repar)==2,is.numeric(repar[[2]]),repar[[2]]>0,length(lower)==repar[[2]],length(upper)==repar[[2]],sum(is.na(repar[[1]](lower)))==0,sum(is.infinite(repar[[1]](lower)))==0,sum(is.na(repar[[1]](upper)))==0,sum(is.infinite(repar[[1]](upper)))==0)

if(length(repar[[1]](upper))!=idx[length(idx)] || length(repar[[1]](lower))!=idx[length(idx)]){stop("length(repar[[1]](upper)) or length(repar[[1]](lower)) is not valid");};} 




if(is.null(x0)==FALSE){if(is.matrix(x0)){

if(dim(x0)[2]==lenpar){} else{stop("x0 has wrong dimension"); };} else{stop("x0 should be a matrix.");};}else{

f__1<-function(x){runif(100,min=lower[x],max=upper[x]);}
M<-apply(FUN=f__1,X=as.matrix(1:lenpar,nrow=1),MARGIN=1);} 




l4<-list()
l4$code<-0
class(l4)="fitmfblock"
l4$opt=NA
l4$acv=NA
l4$bmacv=NA
l4$mfb$distcmn[[1]]=list(dist[1],NA)
l4$mfb$distspec[[1]]=list(dist[2],NA)
l4$mfb$distn<-list(dist[3],NA)
class(l4$mfb)="mfblock"
l4$mfb$coefspec=NA
l4$mfb$coefcmn=NA
l4$mfb$k=k
l4$mfb$dim=sum(k)


k2<-as.integer(c(0,cumsum(k)));

if(is.null(repar)){l4$repar=NA}else{l4$repar=lenpar}

k2<-c(0,cumsum(k));
m_T1<-function(xx,w){


.Call("m_T22",as.numeric(xx[w,]),k,as.integer(k2),q,dim(x));

}




if(is.null(repar)){

rfCopula2<-function(par){

#get your random data

set.seed(seed)
z0=rdist[[1]](s,par[1:idx[1]])

set.seed(seed)
z2=rdist[[3]](s*dim(x)[2],par[(idx[2]+1):idx[3]])




z1<-numeric()
for(i in 1:length(k)){

set.seed(seed)
z1=c(z1,rep(rdist[[2]](s,par[(idx[1]+1):idx[2]]),k[i]))

}

zz0=rep(par[(idx[3]+1):idx[4]],s*k)
zz1=rep(par[(idx[4]+1):lenpar],s*k)

y<-zz0*z0+zz1*z1+z2


for(i in 1:dim(x)[2]){y[(s*(i-1)+1):(s*i)]=ecdf(y[(s*(i-1)+1):(s*i)])(y[(s*(i-1)+1):(s*i)])}
y
}} else{


rfCopula2<-function(par){
par1=repar[[1]](par)
set.seed(seed)
z0=rdist[[1]](s,par1[1:idx[1]])

set.seed(seed)
z2=rdist[[3]](s*dim(x)[2],par1[(idx[2]+1):idx[3]])

#define the coefficient vector

z1<-numeric()

for(i in 1:length(k)){

set.seed(seed)
z1=c(z1,rep(rdist[[2]](s,par1[(idx[1]+1):idx[2]]),k[i]))

}

zz0=rep(par1[(idx[3]+1):idx[4]],s*k)
zz1=rep(par1[-(1:idx[4])],s*k)

y<-zz0*z0+zz1*z1+z2


for(i in 1:dim(x)[2]){y[(s*(i-1)+1):(s*i)]=ecdf(y[(s*(i-1)+1):(s*i)])(y[(s*(i-1)+1):(s*i)])}
y

}



}




x_1<-as.numeric(x)

for(i in 1:dim(x)[2]){x_1[(dim(x)[1]*(i-1)+1):(dim(x)[1]*i)]=ecdf(x_1[(dim(x)[1]*(i-1)+1):(dim(x)[1]*i)])(x_1[(dim(x)[1]*(i-1)+1):(dim(x)[1]*i)])}

y2<-.Call("m_T22",x_1,k,as.integer(k2),q,dim(x))

dim(x_1)=dim(x)

####
if(is.null(eff)==FALSE){if(is.matrix(eff)){l4$bmacv=eff} else if(is.na(eff)){l4$bmacv<-dim(x)[1]*cov(boot::boot(data=x_1,statistic=m_T1,R=b)$t);}  else{stop("eff is not valid")};acvqr<-qr(l4$bmacv);if(acvqr$rank!=dim(l4$bmacv)[1]){l4$code=1;return(l4);};Q<-function(par){v1<-(y2-.Call("m_T22",rfCopula2(par),k,as.integer(k2),q,as.integer(c(s,dim(x)[2]))));t(v1)%*%solve(acvqr,v1);};}

else{Q<-function(par){v1<-(y2-.Call("m_T22",rfCopula2(par),k,as.integer(k2),q,as.integer(c(s,dim(x)[2]))));as.numeric(t(v1)%*%(v1));};}; 

if(is.null(x0)){rz<-apply(X=M,FUN=Q,MARGIN=1);names(rz)=1:100;rz<-sort(rz);M5<-M[as.numeric(names(rz[1:3])),];
pval<-rz[1];for(i in 1:3){res_T<-nloptr::nloptr(x0=M5[i,],eval_f=Q,lb=lower,ub=upper,opts=opts);if(res_T$status<0){l4$code=2;return(l4);}else{if(res_T$objective<pval){pval=res_T$objective;l4$opt<-res_T;}};};}
else{pval<-1/0;for(i in 1:dim(x0)[1]){res_T<-nloptr::nloptr(x0=x0[i,],eval_f=Q,lb=lower,ub=upper,opts=opts);if(res_T$status<0){l4$code=2;return(l4);}else{if(res_T$objective<pval){pval=res_T$objective;l4$opt<-res_T;};};}};




sol<-repar[[1]](l4$opt$solution)

l4$mfb=mfblock(distcmn=list(dist[1],sol[1:idx[1]]),distspec=list(dist[2],sol[(idx[1]+1):idx[2]]),distn=list(dist[3],sol[(idx[2]+1):idx[3]]),coefcmn=sol[(idx[3]+1):idx[4]],coefspec=sol[(idx[4]+1):idx[5]],k=k)



if(acv){

if(is.null(eps)|| sum((l4$opt$solution+eps)>upper) || sum((l4$opt$solution-eps)<lower)){l4$code=4;return(l4)};

G1<-(matrix(rep(l4$opt$solution,lenpar),nrow=lenpar)+diag(eps,lenpar));
G2<-G1-diag(2*eps,lenpar);
f<-function(par){.Call("m_T22",rfCopula2(par),k,as.integer(k2),q,as.integer(c(s,dim(x)[2])))}
G=(apply(FUN=f,MARGIN=2,X=G1)-apply(FUN=f,MARGIN=2,X=G2))/(2*eps);

}

if(is.null(eff)==FALSE && acv){A1<-t(G)%*%solve(acvqr,G);acvqr2<-qr(A1);if(acvqr2$rank!=dim(A1)[1]){l4$code=3;return(l4)};A2<-solve(acvqr2,diag(1,dim(A1)[1]));l4$acv=(1/sqrt(dim(x)[1]) + 1/sqrt(s))^2*A2;}else{if(acv){l4$bmacv<-dim(x)[1]*cov(boot::boot(data=x_1,statistic=m_T1,R=b)$t);acvqr2<-qr(t(G)%*%G);if(acvqr2$rank!=dim(G)[2]){l4$code=3;return(l4)};A1<-solve(acvqr2,diag(1,dim(G)[2]));l4$acv=(1/sqrt(dim(x)[1])+ 1/sqrt(s))^2*A1%*%t(G)%*%l4$bmacv%*%G%*%A1;};};l4}

