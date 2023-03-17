setwd("C:/Users/arian/Desktop/EKPA/R/final_assignment/option2")
dat = read.table("League of Legents Champion Stats 13.3.data",sep=";" , header = T)


setwd("C:/Users/arian/Desktop/EKPA/R/final_assignment/option1")
dat = read.table("breast-cancer-wisconsin.data",sep="," , header = F)
#summary(dat)
colnames(dat) = c("Sampldate_code_number","Clump_Thickness","Uniformity_of_Cell_Size","Uniformity_of_Cell_Shape" , "Marginal_Adhesion" , "Single_Epithelial_Cell_Size" , "Bare_Nuclei" , "Bland_Chromatin" , "Normal_Nucleoli" , "Mitoses " , "Class" )
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

#########################################################
#Tree model initialization
#The purpose is to predict how dangerous an instance is
#meaning if it is benigh or malignant, based on the 
#info that we have from the other columns
library(rpart)
library(rattle)
library(rpart.plot)

tree_model <- rpart(Class~ . , data=dat[,2:11] , method="anova")
summary(tree_model)
plot(tree_model)
text(tree_model)
fancyRpartPlot(tree_model)

#check tree model
#Predicting some Class values and checking them
res <- predict(tree_model,dat[600:699,2:10])
view(cbind(dat[600:699,11],res))

###########################################################
#Regression Model
modelreg=lm(dat$Class~dat$Mitoses +dat$Uniformity_of_Cell_Size + dat$Bare_Nuclei +dat$Clump_Thickness)
summary(modelreg)

p=predict(modelreg,as.data.frame(dat$Mitoses +dat$Uniformity_of_Cell_Size + dat$Bare_Nuclei +dat$Clump_Thickness))
rbind(dat$Class,p)

