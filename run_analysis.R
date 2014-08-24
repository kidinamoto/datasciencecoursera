feature.ad<-"./UCI HAR Dataset/features.txt"

xtrain.ad<-"./UCI HAR Dataset/train/X_train.txt"
xtest.ad<-"./UCI HAR Dataset/test/X_test.txt"

ytrain.ad<-"./UCI HAR Dataset/train/y_train.txt"

f2<-read.table(xtrain.ad)

f3<-read.table(xtrain.ad)
f4<-read.table(xtest.ad)
#Question 1
# x train & test
xdata<-rbind(f3,f4)

feature<-read.table(feature.ad)
feature.names<-feature[,2]
# feature data with names
colnames(xdata)<-feature.names

#subject data

subtrain.ad<-"./UCI HAR Dataset/train/subject_train.txt"
subtrain<-read.table(subtrain.ad)

subtest.ad<-"./UCI HAR Dataset/test/subject_test.txt"
subtest<-read.table(subtest.ad)
subdata<-rbind(subtrain,subtest)
# add the column name
colnames(subdata)<-"Subject"

#Activity data
ytrain.ad<-"./UCI HAR Dataset/train/y_train.txt"
ytrain<-read.table(ytrain.ad)

ytest.ad<-"./UCI HAR Dataset/test/y_test.txt"
ytest<-read.table(ytest.ad)

ydata<-rbind(ytrain,ytest)
# add the column name
colnames(ydata)<-"Activity"

# combine all the three parts together
full.table<-cbind(xdata,subdata,ydata)
#Question 2
#Extracts only the measurements on the mean and standard deviation 
#for each measurement. 
subset1<-full.table[,grep('mean()',colnames(xdata))]
subset2<-full.table[,grep('std()',colnames(xdata))]
subset<-cbind(subset1,subset2)  # the subset with only mean and std measurements

#Question 3
mgsub <- function(pattern, replacement, x, ...) {
        if (length(pattern)!=length(replacement)) {
                stop("pattern and replacement do not have the same length.")
        }
        result <- x
        for (i in 1:length(pattern)) {
                result <- gsub(pattern[i], replacement[i], result, ...)
        }
        result
}

old<-c(1,2,3,4,5,6)
subs<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")

#use this function to subsitute the items with meaningful string
full.table$Activity<-mgsub(old,subs,full.table$Activity)

#Question 4
# remove the - and () gsub("-|\\()", "",test)
# add meaningful prefix to the name gsub("^t","Time",test) gsub("^f","Fast Fourier Transform ",test)
## add axis to X Y Z 
names<-colnames(full.table)
oddexpress<-c("^t","^f","\\()","-","X","Y","Z","min","max","std","[1-9]")
newexpress<-c("Time","Fast_Fourier_Transform_",'',"_","X_axis","Y_axis","Z_axis","minimun","maximum","standard_deviatio",'')
colnames(full.table)<-mgsub(oddexpress,newexpress,names)

# combine the replicated colunms together
full.table[,461]<-rowSums(full.table[,461:502])
full.table<-full.table[,-(462:502)]

full.table[,382]<-rowSums(full.table[,382:423])
full.table<-full.table[,-(383:423)]

full.table[,303]<-rowSums(full.table[,303:344])
full.table<-full.table[,-(304:344)]

#use this function to get a summary report for the data dictionary
sink(file="./summary.txt")
summary(full.table)
sink()









