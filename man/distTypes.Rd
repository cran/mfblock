\name{distTypes} 
\alias{add_dist}
\alias{rm_dist} 
\alias{get_dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add,Get and Remove Distribution Types for Factors}

\description{Functions to add,get or remove distribution types for the common factor (\sQuote{distcmn}),specific factor (\sQuote{distcmn}) and noise (\sQuote{distcmn}) for an mfblock specification. Upon loading the package, the only available distribution type labels are \dQuote{normal},\dQuote{t} and \dQuote{skewt}.} 

\usage{
#adding a distribution type

add_dist(label,simfun,n,sample)

#getting a distribution type

get_dist(label)

#removing a distribution type

rm_dist(label)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{label}{
A character vector representing the labels for the
distribution type(s). 
}
  \item{simfun}{
The function that simulates the new type of distribution. It must of the form simfun(n,x) with \sQuote{n} specifying the number of simulations and \sQuote{x} a numeric vector specifying the parameters for the distribution.  
}
  \item{n}{
The length of the second argument of simfun -- the number of parameters needed to identify the distribution. 
}

\item{sample}{A numeric vector that can be passed as the second argument to simfun. It is used for checking purposes.}

}

\examples{

\donttest{

#Adding the Cauchy distribution

#Step 1: write a wrapper function for simulation 

rcauchy2<-function(n,x){rcauchy(n,x[1],x[2])}

#Step 2: register the new type

add_dist(label="cauchy",simfun=rcauchy2,n=2,sample=list(10,c(1,2))) 

#Get the cauchy distribution

get_dist("cauchy")

#Remove the cauchy distribution

rm_dist("cauchy")

#Check that it worked 

is.null(get_dist("cauchy"))
}
}


