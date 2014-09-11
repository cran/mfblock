rm_dist<-function(label=stop("label needed")){
stopifnot(is.character(label));
if(sum(names(dist_env$l__fc)==label)>0){dist_env$l__fc[label]=NULL}

else{};

}

