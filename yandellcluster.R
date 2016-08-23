
#This script will cluster both rows and columns in input.csv file
#using hierarchical clustering.
inputfilename = "input.csv"
outputfilename = "output.csv"
mymodule = read.csv(inputfilename, row.names = 1)
dim(mymodule)
m = t( as.matrix(mymodule) )
m[m == 0] <- NA
d = as.dist((1 - cor(m, use="pairwise.complete.obs"))/2) ##dist(m)
d[is.na(d)] <- 1
h = hclust(d)
m2 = as.matrix(mymodule)
m2[m2 == 0] <- NA
d2 = as.dist((1-cor(m2, use="pairwise.complete.obs"))/2) ##dist(m2)
d2[is.na(d2)] <- 1 ## better value?
h2 = hclust(d2)
mymodule.clustered = mymodule[h$order,h2$order]
write.csv( mymodule.clustered, file = outputfilename, row.names = T)
image(seq(nrow(m)), seq(ncol(m)),
  log10(m[h2$order,h$order]), col=rainbow(10,start=1/3,end=2/3))
