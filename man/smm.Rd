\name{smm}
\alias{smm}
\title{Simulated Method of Moments}
\description{
Fits a multifactor block equidependence model using the simulated method of moments. 
}
\usage{
smm(x,dist,k,repar=NULL,s,q,acv=FALSE,eff=NULL,b=NULL,eps=NULL,x0=NULL,
lower,upper,opts=list("algorithm"="NLOPT_LN_SBPLX","maxeval"=800,"xtol_abs"=0.005),
seed=as.integer(Sys.time()))
}


\arguments{
  \item{x}{
A matrix object representing the data that is to be fitted. 
}

\item{dist}{A character vector of length 3 giving the distribution types. The first element is for the common factor, the second is for the specific factor and the third is for the noise. Corresponds to the first element of the list objects distcmn,distspec and distn in \sQuote{mfblock()}.}

\item{k}{A block partition vector. Specifies the block structure corresponding to the data and corresponds to the \sQuote{k} argument in \sQuote{mfblock()}.}

\item{repar}{A list that specifies a reparametrization. The first element of repar should be a function that maps a numeric to another numeric belonging to the original parameter space. The second element specifies the length of the vectors in the new parameter space.}

\item{s}{
The number of random deviates to be used in the simulations. 
}

  \item{q}{
A numeric vector -- the quantile vector -- specifying the quantiles to be used for the quantile dependence measures. 

}
  \item{acv}{
A logical object. If set to TRUE, an attempt will be made to return an asymptotic covariance matrix for the estimator. 
}
  \item{eff}{
If set to NA, a bootstrap procedure is implemented to estimate an asymptotic covariance (see details) and the \sQuote{efficient} estimator is returned. If set to a matrix object, that matrix is used as an approximation to an asymptotic covariance and no bootstrap procedure is needed. If set to NULL, the default, no attempt is made to derive the \sQuote{efficient} estimator. 
}
  \item{b}{
Defines the number of bootstraps used in the bootstrapping procedure to approximate an asymptotic covariance (see details). 
}
  \item{eps}{
The step length used in a two sided numerical derivative computed at the estimator value which is needed when acv=TRUE. 
}
  \item{x0}{
A numeric used as the starting value in \sQuote{nloptr()}(the optimization tool used by \sQuote{smm()}). If set to NULL, the default, 100 points are randomly selected within the feasible region defined by lower and upper. Then 3 points among the 100 points are found where the objective function is smallest and used as starting values in \sQuote{nloptr()}. 
}

\item{lower}{A numeric vector representing a lower bound for the parameters.}

\item{upper}{A numeric vector representing an upper bound for the parameters.}


\item{opts}{A list object. The elements should be valid named options for \sQuote{nloptr()}. A full description of all valid options is shown by calling a \sQuote{nloptr.print.options()}.}



\item{seed}{The seed value used for the simulations.}

}
\details{

Implements the \eqn{\textit{simulated method of moments estimation for copula-based multivariate models}}{simulated method of moments estimation for copula-based multivariate models} as outlined in Oh and Patton (2013). Given T observations, first consider a vector of sample dependence measures,\eqn{m_T}, which does not depend on the individual marginals (Spearman's rank correlation and quantile dependence with the quantiles specified by the argument q). Second, approximate the value of \eqn{m(\theta)}{m(theta)}, the true values of the vector of dependence measures under a \eqn{\theta}{theta} model specification, using s simulations and denote it by \eqn{m_S(\theta)}{m_S(theta)} Third, the estimator is derived as the minimizer of \deqn{(m_T-m_S(\theta))^{'}\cdot W \cdot (m_T-m_S(\theta))}{(m_T-m_S(theta))' * W * (m_T-m_S(theta))}

with \eqn{W}{W} equal to either the identity matrix when eff=NULL or the inverse of an approximation to the asymptotic covariance of \eqn{\sqrt{T}(m_T-m)}{sqrt(T)*(m_T-m)} as \eqn{T \rightarrow \infty}{T goes to infinity} with \eqn{m}{m} denoting the vector of dependence measures under the true model parameters. As outlined in Oh and Patton(2013), when \eqn{W}{W} is equal to the asymptotic covariance for \eqn{m_T}{m_T}, the estimator is efficient in some sense. 

If the method is applied to fit the data to an \sQuote{mfblock} specification, then a \eqn{\theta}{theta} model specification would correspond to equating the vector c(distcmn[[2]],distspec[[2]],distn[[2]],coefcmn,coefspec) to \eqn{\theta}{theta} when repar=NULL and to repar[[1]]\eqn{(\theta)}{(theta)} otherwise. 

Since the asymptotic covariance for \eqn{\sqrt{T}(m_T-m)}{sqrt{T}*(m_T-m)} is not known, an approximation is either specified by the user when eff is set to a matrix or approximated via the bootstrap when eff=NA.}

\value{Returns an object of class \sQuote{fitmfblock} which is a list containing the following elements:

\item{code}{An integer. A code value equal to 0 indicates that \sQuote{smm} worked as intended. A code value equal to 1 occurs when eff is not NULL and the asymptotic covariance for the dependence measures --either pre-specified or found by bootstrapping-- is not deemed to be invertible. In this scenario, the objective function to be minimized cannot be defined. A code value of 2 occurs when the optimization procedure failed to converge to an approximate minimizer. A code value of 3 occurs when acv was set to TRUE but cannot be computed due to the singularity of a matrix. A code value of 4 occurs when the numerical derivative cannot be computed because eps is too large and functon evaluation is done at points outside the feasible region defined by the arguments \sQuote{lower} and \sQuote{upper}.}

\item{bmacv}{The asymptotic covariance of \eqn{\sqrt{T}(m_T-m)}{sqrt(T)*(m_T-m)} as \eqn{T\rightarrow \infty}{T tends to infinity} (see details). It is computed either via the bootstrap when eff=NA or specified initially by setting eff to a matrix. If eff=NULL, then bmacv is set to NA.}

\item{opt}{The return value of \sQuote{nloptr()}. opt$solution will hold the estimator.}

\item{acv}{When the argument acv is set to TRUE, acv is the asymptotic covariance for the estimator. When the argument acv is set to FALSE, acv is returned as NA.}

\item{mfb}{An object of class \sQuote{mfblock} with model parameters corresponding to the estimator found by \sQuote{smm()}.}

\item{repar}{An integer which equals repar[[2]], the length of vectors in the new parameter space, when a reparametrization was used and equals NA otherwise.}

}
\references{

Oh, H.O. \eqn{\&}{&} Patton A.J. (2013). Simulated Method of Moments Estimation for Copula-Based Multivariate Models. Journal of the American Statistical Association, June 2013,Vol.108, No. 502, Theory and Methods. doi:10.1080/01621459.2013.785952  

Oh, H.O. \eqn{\&}{&} Patton A.J. (2012). Modelling Dependence in High Dimensions with Factor Copulas (Working Paper). Retrieved rom Duke University website: \url{http://public.econ.duke.edu/~ap172/}

}


\seealso{


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

lower<-c(rep(0.1,2),rep(0.3,6))
upper<-c(rep(1,2),rep(2,6))

#Define the reparametrization

repar<-list();
repar[[1]]<-function(x){c(1/x[1], x[2], 1/x[1], 1/x[1], x[3:8])}
repar[[2]]<-8 

#Non-efficient estimator with no acv returned and no starting points specified:

fit<-smm(x,dist=dtype,k=z$k,s=10000,q=q1,lower=lower,upper=upper,repar=repar,opts=list(maxtime=15))

#Check if it worked 

fit$code

#Efficient estimator with acv returned

fit<-smm(x,dist=dtype,k=z$k,repar=repar,s=10000,q=q1,acv=TRUE,eff=NA,
b=100,eps=0.001,lower=lower,upper=upper,opts=list(maxtime=10))

#We can set eff to fit$bmacv

if(is.matrix(fit$bmacv)){
fit<-smm(x,dist=dtype,k=z$k,repar=repar,s=10000,q=q1,acv=TRUE,
eff=fit$bmacv, eps=0.001,lower=lower,upper=upper,opts=list(maxtime=10))}

#Add some options to "nloptr"
fit<-smm(x=x,k=z$k,dist=dtype,repar=repar,s=10000,q=q1,lower=lower,upper=upper,
opts=list(ftol_abs=0.005,print_level=1,maxtime=10))

#With starting values (should always be a matrix)

fit<-smm(x=x,dist=dtype,k=z$k,repar=repar,q=q1,s=1000,lower=lower,
upper=upper,x0=matrix((lower+upper)/2,nrow=1),
opts=list(maxeval=800,ftol_abs=0.005,maxtime=10)) 

#Change the algorithm

fit<-smm(x=x,dist=dtype,k=z$k,repar=repar,q=q1,s=1000,lower=lower,upper=upper,
opts=list(maxeval=900,ftol_abs=0.005,print_level=1,algorithm="NLOPT_LN_NELDERMEAD",maxtime=10))
}
}


\keyword{smm}
\keyword{copula}
\keyword{multifactor}
\keyword{finance}
\keyword{multivariate}

