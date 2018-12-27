#' classEVA is triangle-counting-based measure of the correspondence of a distance matrix 
#' and a clessification of the points that the distance between is.
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#'
#' @return The value of the classEVA statistisc. It is the share of 'good' triangles and it is between 1 (ideal case) and 0. 
#'
#' @author Alexander Favorov, \email{favorov@sensi.org}
#' @keywords distance classification 
#'
#' @examples
#' classEVA(classes,distances)
#'
classEVA<-function(classes,distances) {
  distmat<-as.matrix(distances)
  dims<-dim(distmat)
  if (length(dims)!=2 || dims[1]!=dims[2]) stop("Nonsquare distamce matrix")
  if (length(classes) != dims[1]) stop("Classification is not of the same length with distance list")
	inclass<-as.numeric(as.factor(classes))
	if(class(inclass)!='numeric') stop('Classification is not numeric/char/factor vector')
	if (1==length(unique(inclass))) {
	  warning("There is only one class for classEVA")
	  return (1)    
	}
	len<-length(inclass)
	triangles<-0
	good.triangles<-0
	for (I in 1:(len-2)) 
		for (J in (I+1):(len-1)) 
			for (K in (J+1):len) {
				L<-M<-N<-0;
				#we will map I J K to L M N
				#so that if we succeed
				# class(L)==class(M) and N is other 
				if (inclass[J] == inclass[I]){ # I and J are the same class
					if (inclass[J]==inclass[K]) next; #all the same, pass
					L<-I;M<-J;N<-K;
					triangles<-triangles+1;
				} else { #I and J are different
					if (inclass[J] == inclass[K]) { #J and K are the same, I is different
						L<-J; M<-K; N<-I; 
						triangles<-triangles+1
					} else if (inclass[I] == inclass[K]) { #I and K are the same, J is different
						L<-I;M<-K;N<-J; 
						triangles<-triangles+1
					} else next #all are different, pass
				} # L M N mapped
				l<-distmat[M,N]
				m<-distmat[N,L]
				n<-distmat[L,M]
				if (n>m || n>l) next; #good triangle if n is the least
				if (n==m && n==l) {good.triangles = good.triangles + 1/3; next} #resolve = = = tie
				if (n==m || n==l) {good.triangles = good.triangles + 1/2; next}  #resolve simple = tie
				good.triangles = good.triangles + 1 #if we are here, it is a good one
			} 
  if (0==triangles) return(0)
	return (good.triangles/triangles)
}

