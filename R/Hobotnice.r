######################################
#Hobotnice functions
######################################

library('edgeR')

#######################################################################



Hobot_stat <- function(distance_matrix, annotation){

	rank.m <- as.matrix(distance_matrix)
	re_rank.m <- rank.m
	re_rank.m[lower.tri(rank.m)] <- rank(rank.m[lower.tri(rank.m)])
	re_rank.m[upper.tri(rank.m)] <- rank(rank.m[upper.tri(rank.m)])
	all_sum <- sum(re_rank.m)

	inclass_sum <- 0
	classes <- unique(annotation[,1])
	for (clas in classes){
#		clas_samples <- rownames(anno[anno[,1] == clas,])
		clas_samples <- rownames(annotation[which(annotation == clas), ,drop=FALSE])
		inclass_sum <- inclass_sum + sum(re_rank.m[clas_samples,clas_samples])
	}

	return(inclass_sum/all_sum)

}


Hobot_distr <- function(N ,distance_matrix, annotation){

	hobots <- vector()
	for (i in 1:100000){
		sample_anno <- annotation
		sample_anno[,1] <- sample(annotation[,1])
		hobots <- c(hobots, Hobot_stat(sampleDists, sample_anno))
	}

	return(hobots)
}


Hobot_pval <- function(Test_hobot ,Hobots){
	p_val <- mean(Hobots <= Test_hobot)
	return(p_val)

}

#######################################################################
















