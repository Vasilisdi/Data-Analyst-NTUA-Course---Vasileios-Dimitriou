setwd("C:/Users/arian/Desktop/EKPA/R/final_assignment/option2")
dat = read.table("League of Legents Champion Stats 13.3.data",sep=";" , header = T)

library(rpart)
library(rattle)
library(rpart.plot)

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

#The following to not be considered.
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

#Considering the NbClust Result
#Hierarchical clustering
plot(c)
rect.hclust(c,k=2)
groups <- cutree(c,k=2)
table(Actual_state = dat$Class,hier_clust_model = groups)

#k-means clustering
k_cl=kmeans(pmatrix,2)

#Comparison of kmeans with real class cases
table(Actual_state = dat$Class, model_pred = k_cl$cluster)
view(cbind(dat$Class,2*k_cl$cluster))

#Comparison of kmeans and Hclust with 2 clusters
#We can notice that they have slight differences
#kmeans (12 and 20 off) / hclust (9 and 20 off).
#We can check the similarity to each other too
table( hier_clust_model = groups, kmeans_model = k_cl$cluster)


#########################################################
#Tree model initialization
#The purpose is to predict how dangerous an instance is
#meaning if it is benigh or malignant, based on the 
#info that we have from the other columns


#Before I make use of this I store it
#in another value so as to make use of it later on
#datClass=dat$Class
#Turn Class into categorical var - factor
datClass = dat$Class
dat$Class <- as.factor(dat$Class)
str(dat)

tree_model <- rpart(Class~ . , data=dat[1:599,2:11])
summary(tree_model)
plot(tree_model)
text(tree_model)
fancyRpartPlot(tree_model)

#check tree model
#Predicting some Class values and checking them
res <- predict(tree_model,dat[600:699,2:10],type = "class")

#Processing the p float value in order to make it being able to take 2 categorical values
view( cbind(Actual_state =2*as.numeric(dat[600:699,11]),Pred = 2*as.numeric(res)))
table(Actual_state = dat[600:699,11],Pred = res)
table( hier_clust_model = groups[600:699], Tree_Model = res)
table( kmeans_model = k_cl$cluster[600:699], Tree_Model = res)



#Note that instead of having 2 and 4 we have 1 and 2 indicating these
#two categorical values.
#We can tell that the model misses only couple of cells.

############################################################
#Regression Model
#Takes into consideration all domains - whole data
#modelreg=lm(as.numeric(Class)~.,data=dat[,2:10])
modelreg=lm(datClass~.,x=dat[1:599,2:10])
summary(modelreg)

#It is ok if included data.frame(x=X_new) - X_new extra values for prediction by model.
#In essence p are the predicted values for the dat input
p=predict(modelreg,  x = dat[600:699,2:10])

#Processing the p float value in order to make it being able to take 2 categorical values
p1 = c()
for (i in 1:length(p)){p1[i]=round(p[i], digits = 0)}
rbind(dat[600:699,]$Class,p1)
view(cbind(read_data=2*as.numeric(dat[600:699,11]),prediction=p1))
table(2*as.numeric(dat[600:699,11]),p1[600:699])
#We can tell that the model misses only couple of cells.



###########################################################
#Failed - couldnt make it work like - very bad convergion because considers only 1 domain
#Regression Model
#modelreg=lm(datClass~dat$Mitoses +dat$Uniformity_of_Cell_Size + dat$Bare_Nuclei +dat$Clump_Thickness)
#modelreg <- lm(datClass[1:599]~dat$Mitoses [1:599] +dat$Uniformity_of_Cell_Size[1:599] + dat$Bare_Nuclei[1:599] +dat$Bland_Chromatin[1:599] +dat$Clump_Thickness[1:599] + dat$Normal_Nucleoli[1:599] + dat$Single_Epithelial_Cell_Size[1:599] + dat$Marginal_Adhesion[1:599] + dat$Uniformity_of_Cell_Shape[1:599])
x <- dat$Uniformity_of_Cell_Size[1:599]
modelreg <- lm(datClass[1:599]~ x  )
summary(modelreg)

#It is ok if included data.frame(x=X_new) - X_new extra values for prediction by model.
#In essence p are the predicted values for the dat input
p=predict(modelreg,  data.frame(x = dat$Uniformity_of_Cell_Size[1:599]))

#Processing the p float value in order to make it being able to take 2 categorical values
p1 = c()
for (i in 1:length(p)){p1[i]=round(p[i], digits = 0)}
rbind(dat[600:699,]$Class,p1)
view(cbind(read_data=2*as.numeric(dat[600:699,11]),prediction=p1))
table(2*as.numeric(dat[600:699,11]),p1)
#We can tell that the model misses only couple of cells.
