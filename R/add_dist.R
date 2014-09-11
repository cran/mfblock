add_dist <-
function(label=stop("label is missing"),simfun=stop("simfun is missing"),n=stop("n is missing"),sample=stop("sample is missing")){



stopifnot(is.character(label),is.function(simfun),is.numeric(n),length(n)==1,is.list(sample),length(sample)==2);

stopifnot(is.numeric(sample[[1]]),is.numeric(sample[[2]]),sample[[1]]>0,length(sample[[2]])==n);



stopifnot(is.numeric(simfun(sample[[1]],sample[[2]])),length(simfun(sample[[1]],sample[[2]]))==sample[[1]]);

dist_env$l__fc[[label]]<-c(simfun,n);


}

