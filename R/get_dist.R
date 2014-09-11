get_dist<-function(label=stop("label needed")){
stopifnot(is.character(label));

l<-list()
for(i in label){

if(sum(names(l__fc)==i)>0){l[[i]]=l__fc[[i]]} else if(sum(names(dist_env$l__fc)==i)>0){l[[i]]=dist_env$l__fc[[i]]}else{l[[i]]=NULL};


}

l
}

