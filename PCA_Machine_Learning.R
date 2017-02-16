#Script Name: PCA.R
#Create By: MAJ Tomaziefski
#Date: 3 JAN 17

#this script conducts PCA of the TWV data set.

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 

source("161227_data_setup.R")

#create a folder to write out data and figures to
#check for data folder.  Create one if none exists

if (!file.exists("./PCA")) {dir.create("./PCA")}
pca_path = "P:/030134 TWV FP & Mobility/030134 Working Files/Operational Impact Workshops/20160908-Compiled Results and Analysis/20161206_r_folder/PCA/"

mydata = combined_data_wide %>%
  filter(joint_phase == "III") %>%
  select(11:25, 29:32)

colnames(mydata) = c("M/IED","SC","SIED","RPG","MC","IDF","NK","TRF","SSM","MS","CCS","DS","ROP","TD","FORD","CL","SPT","OR","OFR") 

fit <- princomp(mydata, cor=TRUE, scores = TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="line") # scree plot 
abline(h = 1.0)
scores=as.data.frame(as.table(fit$scores)) # the principal components loadings = as.data.frame(as.table(fit$loadings))
biplot(fit, xlabs = rep(".", nrow(mydata)))

#print plot to file
dev.copy(tiff, file = paste0(pca_path,"overall_pca.tif"), width = 1200, height = 800)
dev.off() 

mydata_phIV = combined_data_wide %>%
  filter(joint_phase == "IV") %>%
  select(11:25, 29:32)

colnames(mydata_phIV ) = c("M/IED","SC","SIED","RPG","MC","IDF","NK","TRF","SSM","MS","CCS","DS","ROP","TD","FORD","CL","SPT","OR","OFR") 

fit <- princomp(mydata_phIV, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit, xlabs = rep(".", nrow(mydata_phIV )))

#print plot to file
dev.copy(tiff, file = paste0(pca_path,"phIV_pca.tif"), width = 1200, height = 800)
dev.off() 

i=NULL
loadings = data.frame(index = 1:19)
for(i in 1:19){
  col =fit$loadings[i,i]
  cbind(loadings, col)
}

require(tidyr)
loadings = as.data.frame(as.table(fit$loadings))
loadings_wide = melt(loadings, id = 1, measure.vars = 2)

lt = spread(loadings,Var2,Freq)

write.csv()

fa1 <- factanal(scale(mydata, scale = TRUE), factor < 8)


mydata_cs = scale(mydata, center = TRUE, scale = TRUE)

fa1 <- factanal(mydata_cs, factor = 3)
fa1
summary(fa1)


####machine learning, classifying by echelon, either EAB or BCT, pre-prccessing using pca
require(caret)
require(dplyr)
require(tidyr)
require(reshape2)
ml_data = combined_data_wide %>%
  filter(joint_phase == "III") %>%
  select(8, 11:32) %>%
  mutate(echelon, echelon = ifelse(echelon == "EAB","EAB","BCT"))


ml_data_cs = scale(ml_data[2:23], center = TRUE, scale = TRUE)
ml_data_cs = cbind(ml_data[,1],ml_data_cs)

set.seed(500)
intrain = createDataPartition(y = ml_data_cs$echelon, p = .75, list = FALSE)
training = ml_data_cs[intrain,]
testing = ml_data_cs[-intrain,]


pre_proc = preProcess(training[,-1], method = "pca", thresh = .8)
train_pca = predict(pre_proc, training[,-1])
model_fit_pca = train(train_pca, training$echelon, method = "glm")

test_pca = predict(pre_proc, testing[,-1])
confusionMatrix(testing$echelon, predict(model_fit_pca,test_pca))

model_fit = train(echelon ~., data= training, method="glm")

predictions <- predict(model_fit,newdata=testing)

confusionMatrix(predictions,testing$echelon)

print(model_fit$finalModel)


#####Random Forest Prediction for grouped unit type ######################################################
ml_data_gut = combined_data_wide %>%
  filter(joint_phase == "III") %>%
  select(6, 11:32) 

ml_data_gutcs = scale(ml_data_gut[2:23], center = TRUE, scale = TRUE)
ml_data_gutcs = cbind(ml_data_gut[,1],ml_data_gutcs)

set.seed(500)
intrain_gut = createDataPartition(y = ml_data_gutcs$grouped_unit_type, p = .75, list = FALSE)
training_gut = ml_data_gutcs[intrain_gut,]
testing_gut = ml_data_gutcs[-intrain_gut,]


mod_fit_tree = train(grouped_unit_type ~., method = "rf", data = training_gut, prox = TRUE)
print(mod_fit_tree$finalModel)

getTree(mod_fit_tree$finalModel, k = 2)

prediction_mod_fit_tree = predict(mod_fit_tree, testing_gut)
testing_gut$predRight = prediction_mod_fit_tree == testing_gut$grouped_unit_type
prediction_out = as.data.frame(table(prediction_mod_fit_tree,testing_gut$grouped_unit_type))


prediction_out = dcast(prediction_out, prediction_mod_fit_tree~ Var2, value.var = "Freq") 

write.csv(prediction_out, file = paste0(pca_path,"prediction.csv"))


confusionMatrix(prediction_mod_fit_tree, testing_gut$grouped_unit_type)

