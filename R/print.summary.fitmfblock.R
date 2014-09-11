print.summary.fitmfblock <-
function(x,...)
{

printCoefmat(x$coefficients, P.values=TRUE, has.Pvalue=TRUE)

}
