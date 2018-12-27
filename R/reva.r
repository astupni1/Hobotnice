###### thinking about cluster homogeneity
##### this was inspired by a collaborator looking at a pca plot and
##### wondering if there was a good way to quantify how
##### well-clustered each class of samples is.
### I didn't want to actually do a formal clustering
#### so it really just came down to a question of whether
##### nearby samples share the same class.

#### a knn approach is implemented below
### along with a kernel approach 
#### there is a silhouette-like plot and a statistic
#### interpreted as the probabablity that nearby samples share the same class

### k nearest neighbor version
#### inner function, one samp at a time
.neighbor=function(samp,distM,class,k=3){
    
        neighbors=names(sort(distM[samp,]))[1:k]
        classS=class[samp]
        classN=mean(class[neighbors]==classS)
    return(classN)}


#### outer function, the entire dataset
neighbor=function(dst,class,k=3){
    distM=as.matrix(dst)
    diag(distM)=NA
    samps=colnames(distM)
    ans=sapply(samps,.neighbor,distM=distM,class=class,k=k)
    return(ans)}

### kernel density based version, bandwidth can be selected using a knn rule
#### inner function, one samp at a time
.neighborK=function(samp,distM,class,k=3,bw=NULL){
    if(is.null(bw)) bw=sort(distM[samp,])[k]
    classS=class[samp]
        classN=class[setdiff(colnames(distM),samp)]
 densN=dnorm(distM[samp,setdiff(colnames(distM),samp)],sd=bw)
 densN=densN/sum(densN)
 return(sum((classN==classS)*densN))
}

#### outer function, the entire dataset
neighborK=function(dst,class,k=3,bw=NULL){
    distM=as.matrix(dst)
    diag(distM)=NA
    samps=colnames(distM)
    ans=sapply(samps,.neighborK,distM=distM,class=class,k=k,bw=bw)
    return(ans)}


######## plotting function, returns mean scores per group

neighbor.plot=function(neighborCt,class,classCols=NULL,means=T){
    if(is.null(classCols)){
        classCols=1:length(unique(class))
        names(classCols)=unique(class)}
    ord=order(match(class,names(classCols)),neighborCt)
     barplot(neighborCt[ord],xlim=c(-.01,1.8),col=classCols[as.character(class)[ord]],border=classCols[as.character(class)[ord]],axisnames=F,cex.axis=1.5,cex.lab=1.5,horiz=T,axes=F,space=0.5)
    box()
     axis(side=1,at=seq(0,1,length=4),labels=round((0:3)/3,2),cex.axis=1.5)
    legend('topright', fill=classCols,legend=names(classCols),cex=1.5)
   
        return(round(tapply(neighborCt[ord],as.character(class)[ord],mean,na.rm=T),2))
       
    }




