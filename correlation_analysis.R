#Feature Selection/Engineering

#Correlation analysis
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software#compute-correlation-matrix


install.packages("Hmisc");
library(Hmisc)
install.packages("stringi")
library(stringi)

source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//loadAnswers.R");
dataf =loadAnswers();
summary(dataf)



featuresdf<- data.frame(dataf$Answer.duration,dataf$Answer.confidence,dataf$Answer.difficulty,stri_length(dataf$Answer.explanation), 
                        dataf$Worker.age,dataf$Worker.score,dataf$Worker.yearsOfExperience,
                        dataf$Code.LOC,dataf$Code.complexity);
colnames(featuresdf) <- c("Answer.duration","Answer.confidence","Answer.difficulty","Answer.explanationSize", 
                          "Worker.age","Worker.score","Worker.yearsOfExperience",
                          "Code.LOC","Code.complexity");
featuresM<- data.matrix(featuresdf) #rcorr requires a matrix

corOutput<- rcorr(featuresM,type  = "spearman");

#Correlation coeficients
corOutput$r

#p-values
corOutput$P
