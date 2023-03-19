setwd("C:/Users/arian/Desktop/EKPA/R/final_assignment/option2")
dat = read.table("League of Legents Champion Stats 13.3.data",sep=";" , header = T)


setwd("C:/Users/arian/Desktop/EKPA/R/final_assignment/option1")
dat = read.table("breast-cancer-wisconsin.data",sep="," , header = F)
#summary(dat)
colnames(dat) = c("Sampldate_code_number","Clump_Thickness","Uniformity_of_Cell_Size","Uniformity_of_Cell_Shape" , "Marginal_Adhesion" , "Single_Epithelial_Cell_Size" , "Bare_Nuclei" , "Bland_Chromatin" , "Normal_Nucleoli" , "Mitoses " , "Class" )
summary(dat)

###################################################
#Clustering - hier. clustering & k-means
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
#we can notice that our Data have a colume with up to 2 classes, so we can 
#check if the class column values are reliable-benigh (2) or malignant (4) 
#The diagram above gives us that we can use less than 6 clusters 
#which cannot be worth doing for this problem
#Our data has a Class column - it might get 2 values - categorical var
#benigh (2) or malignant (4) / first or second
#So c the hclust model considers all the other data except for Class domain
#and we can notice that it assigns them as supposed into 2 dif clusters
groups <- cutree(c,k=2)
table(dat$Class,groups)

#Considering the NbClust Result
#Hierarchical clustering
plot(c)
rect.hclust(c,k=2)
groups <- cutree(c,k=2)

#k-means clustering
k_cl=kmeans(pmatrix,2)

#Comparison of kmeans with real class cases
table(dat$Class,k_cl$cluster)

#Comparison of kmeans and Hclust with 2 clusters
#We can notice that they have slight differences
#kmeans (12 and 20 off) / hclust (9 and 20 off).
#We can check the similarity to each other too
table(groups,k_cl$cluster)


#########################################################
#Tree model initialization
#The purpose is to predict how dangerous an instance is
#meaning if it is benigh or malignant, based on the 
#info that we have from the other columns
library(rpart)
library(rattle)
library(rpart.plot)

#Turn Class into categorical var
dat$Class <- as.factor(dat$Class)
str(dat)

tree_model <- rpart(Class~ . , data=dat[,2:11] , method="anova")
summary(tree_model)
plot(tree_model)
text(tree_model)
fancyRpartPlot(tree_model)

#check tree model
#Predicting some Class values and checking them
res <- predict(tree_model,dat[600:699,2:10])

#Processing the p float value in order to make it being able to take 2 categorical values
res1 = c()
for (i in 1:length(res)){res1[i]=round(res[i], digits = 0)}
view(cbind(dat[600:699,11],res1))
#Note that instead of having 2 and 4 we have 1 and 2 indicating these
#two categorical values.
#We can tell that the model misses only couple of cells.


###########################################################
#Regression Model
modelreg=lm(dat$Class~dat$Mitoses +dat$Uniformity_of_Cell_Size + dat$Bare_Nuclei +dat$Clump_Thickness)
summary(modelreg)

#It is ok if included data.frame(x=X_new) - X_new extra values for prediction by model.
#In essence p are the predicted values for the dat input
p=predict(modelreg,as.data.frame(dat))

#Processing the p float value in order to make it being able to take 2 categorical values
p1 = c()
for (i in 1:length(p)){p1[i]=round(p[i], digits = 0)}
rbind(dat[1:699,]$Class,p1[1:699])
view(cbind(dat[600:699,11],p1[600:699]))
#We can tell that the model misses only couple of cells.
