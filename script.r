# set the working directory to the current folder
# setwd("your\path\to\the\folder")

options(warn = -1)

# load needed packages
library(tree)           |> suppressMessages()
library(randomForest)   |> suppressMessages()
library(caret)          |> suppressMessages()
library(dplyr)          |> suppressMessages()
library(tidyr)          |> suppressMessages()
library(ggplot2)        |> suppressMessages()
# if some packages are not installed, install them first
# install.packages("package_name")

# read the data and set the column names, drop unused columns
leaves <- read.csv("data/leaf.csv", header = FALSE)
names(leaves) <- c("Species", "nth_species_observation", "Eccentricity", "Aspect_Ratio","Elongation", "Solidity", "Stochastic_Convexity", "Isoperimetric_Factor", "Maximal_Indentation_Depth", "Lobedness", "Average_Intensity", "Average_Contrast", "Smoothness", "Third_moment", "Uniformity", "Entropy")
leaves$nth_species_observation <- NULL
# Species is a qualitative variable but it's read as a quantitative
leaves$Species <- as.factor(leaves$Species)




# custo LOOCV function due to specific requirements

effF <- c() #eff forest
effT <- c() #eff tree

for(j in 1:nrow(leaves)){
    d_test <- leaves[j,]
    d_learn <- leaves[-j,]
    modelF <- randomForest(Species~.,data=d_learn,mtry=4)
    modelT <- tree(Species~.,data=d_learn,control=tree.control(339,mindev=0))
    prdF <- predict(modelF,newdata=d_test)
    prdT <- predict(modelT,newdata=d_test)
    #prdT is a 1x30 matrix while prdF was a factor
    prdT <- as.data.frame(prdT)
    prdT_temp <- names(prdT)
    prdT_temp <- as.factor(prdT_temp)
    prdT_response <- prdT_temp[prdT[1,]==max(prdT[1,])]
    effF <- c(effF,as.numeric(prdF==leaves[j,1]))
    effT <- c(effT,as.numeric(prdT_response==leaves[j,1]))
} 

# assess the accuracy of the model
muF <- mean(effF) ; sigmaF <- sd(effF)
muT<-mean(effT) ; sigmaT<-sd(effT) 
# print the results
cat("\t\t\t mu\t\t\tsigma\n")
paste("Random forest: \t", muF,"\t", sigmaF, "\n") |> cat()
paste("Tree: \t\t", muT, "\t",sigmaT, "\n") |> cat()

# for more understandable results, assign a label to each species
species_names <- c("Quercus suber", "Salix atrocinerea", "Populus nigra", "Alnus sp",  "Quercus robur", "Crataegus monogyna",  "Ilex aquifolium", "Nerium oleander",  "Betula pubescens", "Tilia tomentosa", "Acer palmaturu", "Celtis sp", "Corylus avellana", "Castanea sativa", "Populus alba", "Primula vulgaris", "Erodium sp",  "Bougainvillea sp", "Arisarum vulgare", "Euonymus japonicus", "Ilex perado ssp azorica", "Magnolia soulangeana", "Buxus sempervirens",   "Urtica dioica",  "Podocarpus sp", "Acca sellowiana",  "Hydrangea sp", "Pseudosasa japonica",  "Magnolia grandiflora",  "Geranium sp" )
levels(leaves$Species) <- species_names

# split the dataset
index_learn <- sample(c(1:nrow(leaves)))[1:(nrow(leaves)*0.7)]
data_learn <- leaves[index_learn,]
data_test <- leaves[-index_learn,]

# learn a forest and then use it to predict the test values
rf <- randomForest(Species~.,data=data_learn,mtry=4)
pred <- predict(rf,newdata=data_test)
cm <- confusionMatrix(pred,data_test$Species)  # confusion matrix

#handle the cf in order to plot it 
cm_backup <- cm # we need to keep the original structure for later
cm <-cm$table |> as.data.frame()
cm <- cm |> 
    mutate(
        Prediction = factor(Prediction),
        Reference  = factor(Reference, levels = rev(unique(Reference)))
    )

plt <- ggplot(cm, aes(x=Prediction, y=Reference, fill=Freq)) +
    geom_tile() + theme_bw() + coord_equal() +
    scale_fill_distiller(palette="Reds", direction=1) +
    guides(fill=F) + # removing legend for `fill`
    labs(title = "Confusion matrix") + # using a title instead
    geom_text(aes(label=Freq), color="black", ) + # printing values
    theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1, face = "bold"),
        axis.text.y = element_text(face = "bold"))

# export the plot
print("Confusion matrix saved as confusion_matrix.png")
ggsave("confusion_matrix.png", plt, width = 21, height = 14, units = "cm")

cm <- cm_backup$table
Acc <- sum(diag(cm))/sum(cm)
Err <-  1-Acc

a <- vector(mode = "numeric", length = 30) # 30 leave species
for (i in 1:30)    a[i] <- cm[i,i] / sum(cm[,i]) 
wAcc <- mean(a, na.rm = TRUE)


# print the results
paste("Accuracy: \t\t", Acc, "\n") |> cat()
paste("Error: \t\t\t", Err, "\n") |> cat()
paste("Weighted accuracy: \t", wAcc, "\n") |> cat()