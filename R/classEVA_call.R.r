source('classEVA.R')
data(iris)
distances<-dist(iris[,1:4])
classes<-iris[,5]
ceva<-classEVA(classes,distances)
print(paste0("ClassEVA = ",ceva))
classes_perm<-classes[sample(length(classes))]
ceva_perm<-classEVA(classes_perm,distances)
print(paste0("ClassEVA - permuted = ",ceva_perm))

