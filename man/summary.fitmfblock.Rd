\name{summary.fitmfblock}

\alias{summary.fitmfblock}
\alias{print.summary.fitmfblock}

\title{Summarizing and Printing fitmfblock objects}


\description{

Summary method for class \sQuote{fitmfblock}.  

}
\usage{

\method{summary}{fitmfblock}(object,jac.repar=NULL,...)

\method{print}{summary.fitmfblock}(x,...)}

\arguments{
  \item{object}{
An object of class \sQuote{fitmfblock} which is the result of a call to \sQuote{smm()}. 
}

\item{jac.repar}{A function which represents the Jacobian matrix of the repar function.}

\item{x}{
An object of class \sQuote{summaryfitmfblock}. 
}

\item{\dots}{
Further arguments passed to or from other summary methods.
}
}

\value{An object of class \sQuote{summary.fitmfblock} which is a matrix object with the following named columns:

\item{Estimate}{The estimate.}

\item{se}{The standard error of the estimate.}

\item{Z value}{The z-value of the estimate.}

\item{Pr(>|z|)}{The two-sided p-value of the estimate.}

If a reparametrization was used to get \sQuote{object} --\sQuote{object} is the return value of a call to \sQuote{smm()} with the argument repar set -- and jac.repar=NULL, then the Estimate column corresponds to the estimate in the new parameter space. If a reparametrization was used and jac.repar is specified, then the \sQuote{Estimate} column corresponds to the estimate in the initial parameter space and the standard error column is found by using the delta method.  

}


\seealso{

\code{\link{smm}}
\code{\link{mfblock}}

}

\examples{
\dontrun{
#Create object of class 'mfblock':

z<-mfblock(distcmn=list("skewt",c(5,1.3)),distspec=list("t",5),distn=list("t",5),
coefcmn=c(1.3,0.8,0.5), coefspec=c(0.75,1.2,1.8),k=as.integer(c(1,2,2)))

#Generate random deviates

x<-rmfblock(10000,z)

#Set the distribution types

dtype<-c("skewt","t","t")

#set the quantile vector 

q1<-c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)

#Set upper and lower bounds for the parameters

lower<-c(rep(0.05,2),rep(0,6))
upper<-c(rep(1.5,2),rep(2.5,6))

#Define the reparametrization (equate the degrees of freedom for all three distributions)

repar<-list();
repar[[1]]<-function(x){c(1/x[1], x[2], 1/x[1], 1/x[1], x[3:8])}
repar[[2]]<-8 

#Define the jacobian of repar

jac.repar<-function(x){res<-c(-1/(x[1]^2),
rep(0,7),0,1,rep(0,6),-1/(x[1]^2),rep(0,7),-1/(x[1]^2),rep(0,7),
rbind(matrix(rep(rep(0,6),2),nrow=2),diag(1,6))); dim(res)=c(8,10);res
}

#Call smm() on the data

fit<-smm(x,dist=dtype,k=z$k,repar=repar,s=10000,q=q1,acv=TRUE,
b=500,eps=0.001,lower=lower,upper=upper,x0=matrix((2*lower+upper)/3,nrow=1),
opts=list(maxtime=30,print_level=1,algorithm="NLOPT_LN_NELDERMEAD"),seed=0)

print(fit$opt)

#Get a summary for fit

sumfit<-summary(fit,jac.repar=jac.repar)

#Print it out

sumfit

}

}


