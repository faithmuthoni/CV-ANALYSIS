#Script for analysis of CV Data Set

#PART ONE:VISUALIZING THE POPULATION DISTRIBUTION

#Import the Data as a .csv file 
CV_Data_Sheet1 <- read.csv("C:/Users/Faith Muthoni/Downloads/CV Data - Sheet1.csv")

#Attach the Data Set for easier manipulation
attach(CV_Data_Sheet1)

#Confirm the Working Directory
getwd()

#Since we are looking for gender distribution,
#We get the data type for gender
class(Gender)

#We get the number of Male and Female Applicants
table(Gender)

#Assign the name 'count' to represent these numbers
count=table(Gender)

#Represent the Data as a Pie Chart
pie(count, main="Gender Distribution of Applicants")

#Frame the Pie Chart
box()

#Getting the Percentage Distribution
percent=table(Gender)/186

#Getting the figures for percent
table(percent)

#Representing percentages as a Bar Graph
barplot(percent, main="Percentage of Applicants", xlab="Gender", ylab="%", las=1, names.arg=c("Female", "Male"), ylim=c(0,1))

#The Data is now Visualized

#PART 2:VISUALIZING TOP SKILLS
#Get class of Gender
class(Gender)

#Convert Gender from Character to Factor so as to allow sorting
Gender=as.factor(Gender)

#Check levels of Gender
levels(Gender)


#Get number of Male and Female
table(Gender)

#Filter out the rows where gender was missing, ie N/A
People=CV_Data_Sheet1[complete.cases(Gender), ]

#Have People as a separate data set and open it 
View(People)
attach(People)

#Separate People into Males and Females
MaleData=People[Gender=="Male", ]
View(MaleData)
FemaleData=People[Gender=="Female", ]
View(FemaleData)

#Install 'tm' package==TextMining
#Open the package
library('tm')

#Combining the text
MaleSkills=paste(MaleData$Skills, collapse=" ")

#Setting the source
MaleSkills_souce=VectorSource(MaleSkills)

#Setting the corpus
MaleCorpus=Corpus(MaleSkills_souce)

#Cleaning the Data
MaleCorpus=tm_map(MaleCorpus, content_transformer(tolower))
MaleCorpus=tm_map(MaleCorpus, removePunctuation)
MaleCorpus=tm_map(MaleCorpus, stripWhitespace)
MaleCorpus=tm_map(MaleCorpus, removeWords, stopwords("english"))

#Making a Document Term Matrix
MaleDTM=DocumentTermMatrix(MaleCorpus)
MaleDTM1=as.matrix(MaleDTM)

#Finding the most frequent terms
frequency=colSums(MaleDTM1)
frequency=sort(frequency, decreasing = TRUE)

#Display the top skills for men
frequency

#The top 3 languages for Male applicants were Java, PHP and Python.

#Open the package
library('tm')

FemaleSkills=paste(FemaleData$Skills, collapse=" ")

#Setting the source
FemaleSkills_souce=VectorSource(FemaleSkills)

#Setting the corpus
FemaleCorpus=Corpus(FemaleSkills_souce)

#Cleaning the Data
FemaleCorpus=tm_map(FemaleCorpus, content_transformer(tolower))
FemaleCorpus=tm_map(FemaleCorpus, removePunctuation)
FemaleCorpus=tm_map(FemaleCorpus, stripWhitespace)
FemaleCorpus=tm_map(FemaleCorpus, removeWords, stopwords("english"))

#Making a Document Term Matrix
FemaleDTM=DocumentTermMatrix(FemaleCorpus)
FemaleDTM1=as.matrix(FemaleDTM)

#Finding the most frequent terms
frequency1=colSums(FemaleDTM1)
frequency1=sort(frequency1, decreasing = TRUE)

#Display the top skills for women



