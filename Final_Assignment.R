setwd("C:/Users/arian/Desktop/EKPA/R/final_assignment/option2")
dat = read.table("League of Legents Champion Stats 13.3.data",sep=";" , header = T)


setwd("C:/Users/arian/Desktop/EKPA/R/final_assignment/option1")
dat = read.table("breast-cancer-wisconsin.data",sep="," , header = F)
#summary(dat)
colnames(dat) = c("Sampldate code number","Clump Thickness","Uniformity of Cell Size","Uniformity of Cell Shape" , "Marginal Adhesion" , "Single Epithelial Cell Size" , "Bare Nuclei" , "Bland Chromatin" , "Normal Nucleoli" , "Mitoses " , "Class" )
summary(dat)

str(dat) #report of the dataframe observations
table(complete.cases(dat)) #check
pmatrix = scale(dat[,2:10])
summary(pmatrix)

#Calculation of the points distances
d = dist(pmatrix)
d1=dist(pmatrix, upper = TRUE)
d2=dist(pmatrix,diag = TRUE)
d3=dist(pmatrix,upper = TRUE, diag = TRUE)

#Hierarchical clustering
c = hclust(d, method="ward.D2")

totwins=c()
for (k in 2:10) {
k_c1=kmeans(pmatrix,k)
totwins[k] <- k_c1$tot.withinss
}
plot(1:10, totwins, xlab = "Number of clusters" , ylab = "Total within sum of squares")
lines(1:10, totwins)

#Even tho the NbClust method gives 3 clusters as the optimal practice
#we can notice that our Data have a colume with up to 4 classes, so we can 
#check if the class column values are reliable
#The diagram above gives us that we can use less than 6 clusters 
#considering the kmeans method
#Our data has a Class column
groups <- cutree(c,k=4)
table(dat$Class,groups)

#Considering the NbClust Result
#Hierarchical clustering
plot(c)
rect.hclust(c,k=3)
groups <- cutree(c,k=3)

#k-means clustering
k_cl=kmeans(pmatrix,3)

#Comparison of kmean and Hclust with 3 clusters
table(groups,k_cl$cluster)


