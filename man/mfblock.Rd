\name{mfblock}
\alias{mfblock}

\title{Create object of class \sQuote{mfblock}}


\description{
Returns an object of class \sQuote{mfblock} which represents a multifactor block equidependence model.  
}
\usage{
mfblock(distcmn, distspec, distn, coefcmn, coefspec, k)
}

\arguments{

\item{distcmn}{
A list of length 2 which completely specifies the distribution for the common (\sQuote{market}) factor. The first element is a character vector giving the label for the type of distribution and the second element is a numeric vector containing the parameters that identify the distribution. 
}

\item{distspec}{
A list of length 2 which completely specifies the distribution for the specific (\sQuote{industry}) factors.
}
  \item{distn}{
A list of length 2 which completely specifies the distribution for the noise. 

}
  \item{coefcmn}{
A numeric vector of length equal to the number of blocks (\sQuote{industries}). This vector gives the common factor loading across different blocks.
}
  \item{coefspec}{
A numeric vector of length equal to the number of blocks (\sQuote{industries}). This vector gives the specific factor loading across different blocks. 
}
  \item{k}{
A numeric vector, the block partition vector, representing the partition of the variables into distinct blocks. 
}
 
}
\details{

An object of class \sQuote{mfblock} is a list describing a specification of a multifactor block eqidependent multivariate model as described by Oh and Patton (2012). More precisely, we consider a multivariate random variable of the form 

\deqn{X_i=\beta_{j,1}Z_0 + \beta_{j,2}Z_{j} + e_i }{X_i=b_{j,1}*Z_0 + b_{j,2}*Z_j + e_i}

with \eqn{j} the block index corresponding to \eqn{i}. Furthermore, \eqn{Z_{0}}{Z_0} is the common factor, \eqn{Z_{j}}{Z_j} are the specific factors, and \eqn{e_i} is the noise. The factors and the noise are all independent. 

This specification is used by Oh and Patton (2012) to model the dependence structure of the noise sequence in time series models of equities. The \eqn{Z_{0}}{Z_0} factor can be interpreted as a \sQuote{market} factor and the specific \eqn{Z_{j}}{Z_j} factors can be interpreted as \sQuote{industry} specific factors. 

\sQuote{mfblock()} will simply verify that the specification is valid and that the various arguments are consistent with each other. For instance, it will check that the length of the \sQuote{coefcmn} and \sQuote{coefspec}   equal the number of blocks and it will check that the distribution types are valid.

}


\value{

Returns a list made up of the arguments to \sQuote{mfblock()}  as well as an extra element, dim, which gives the number of variables for the model. 
}


\examples{

#mfblock specification with 5 variables and 3 blocks.

#Here k=c(1,2,2) would indicate that X_1 belongs to the
#first block; X_2 and X_3 belong to the second block, 
#and finally X_4 and X_5 belong to the last block. 

#All the distributions are of type "t"

z<-mfblock(distcmn=list("t",2),distspec=list("t",5),distn=list("t",3),
coefcmn=c(0.7,1.2,0.1),coefspec=
c(1.5,1.2,1.8),k=as.integer(c(1,2,2)))

}
\keyword{multifactor}
\keyword{block}
\keyword{copula}

\references{
Oh, H.O. \eqn{\&}{&} Patton A.J. (2013). Simulated Method of Moments Estimation for Copula-Based Multivariate Models.
Journal of the American Statistical Association, June 2013,Vol.108, No. 502, Theory and Methods. doi:10.1080/01621459.2013.785952 

Oh, H.O. \eqn{\&}{&} Patton A.J. (2012). Modelling Dependence in High Dimensions with Factor Copulas (Working Paper). Retrieved rom Duke University website: \url{http://public.econ.duke.edu/~ap172/}}

\seealso{\code{\link{rmfblock}}}