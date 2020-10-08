# read the file containing the dataset

partdata <- read.xlsx("machine-data.xlsx", sheetIndex=2, header=T)

# do not consider the first column

partdata$PartNo <- NULL

# apply the kmeans function and store the result in object pk.result

pk.result <- kmeans(partdata, 3) # set 3 clusters
summary(pk.result)

#plot the result

plot(partdata[c("ProcessingTime", "EnergyUse")], col = pk.result$cluster)

# use colors codes 1:3 plotting character 8 and character expansion factor 2

points(pk.result$centers[,c("ProcessingTime","EnergyUse")], col = 1:3, pch = 8, cex=2)

# read the data
                 
                 qdata <- read.xlsx("QualityData.xlsx", sheetIndex=1, header=T)
                 
                 # use the library: cluster
                 
                 library(cluster)
                 
                 # the pam function for three clusters corresponding to quality level specified
                 # purpose is to see clusters that emerge
                 
                 pam.result <- pam(qdata, 3)
                 summary(pam.result)
                 
                 # checking the clusters against the quality level
                 
                 table(pam.result$clustering, qdata$QUALITY)
                 
                 # specification of layout of the cluster
                 
                 layout(matrix(c(1,2),1,2)) # 2 graphs per page
                 
                 # the plotted result
                 
                 plot(pam.result)
                 
                 

# read the data
                 
                 qdata <- read.xlsx("QualityData.xlsx", sheetIndex=1, header=T)
                 
                 # call library cluster
                 
                 library(cluster)
                 
                 # take a sample of 60 records from the dataset
                 
                 idx <- sample(1:dim(qdata)[1], 60)
                 qdataSample <- qdata[idx,]
                 
                 # like before, leave out the QUALITY variable
                 
                 qdataSample$QUALITY <- NULL
                 
                 # use the average method for clustering
                 
                 hc <- hclust(dist(qdataSample), method="ave")
                 summary(hc)
                 
                 plot(hc, hang = -1, labels=qdata$QUALITY[idx])
                 
                 # cut tree into 3 clusters
                 
                 rect.hclust(hc, k=3)
                 
