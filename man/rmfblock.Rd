\name{rmfblock}
\alias{rmfblock}
\title{Simulation}

\description{

Random generation of a multivariate following a multifactor block equidependence model specification. 

}
\usage{
rmfblock(n, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of observations. 
}
  \item{x}{
An object of class \sQuote{mfblock} which specifies the parameters of the multifactor block equidependence model. 
}
}

\examples{
#Create object of class 'mfblock'

z<-mfblock(distcmn=list("skewt",c(5,1.3)),distspec=list("t",5),
distn=list("t",5),coefcmn=c(1.3,0.8,0.5)
,coefspec=c(0.75,1.2,1.8),k=as.integer(c(1,2,2)))

#Simulate

rmfblock(1000,z);

}

\seealso{\code{\link{mfblock}}}

