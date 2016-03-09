#Read the dataset in
data = read.csv('DailyKos.csv')

#Build a Hierarchical Clustering Model
distance = dist(data, method = 'euclidean')
hc = hclust(distance, method = 'ward.D')

#Plot the Dendrogram
plot(hc, labels = FALSE)

#Assigning each observation to a cluster
clusters = cutree(hc, k = 11)
rect.hclust(hc, k = 11)
group = table(clusters)
print(group)

#Spliting the dataset for each cluster
hierCluster1 = subset(data, clusters == 1)
hierCluster2 = subset(data, clusters == 2)
hierCluster3 = subset(data, clusters == 3)
hierCluster4 = subset(data, clusters == 4)
hierCluster5 = subset(data, clusters == 5)
hierCluster6 = subset(data, clusters == 6)
hierCluster7 = subset(data, clusters == 7)
hierCluster8 = subset(data, clusters == 8)
hierCluster9 = subset(data, clusters == 9)
hierCluster10 = subset(data, clusters == 10)
hierCluster11 = subset(data, clusters == 11)

#Finding the six most common words in each cluster
tail(sort(colMeans(hierCluster1)))
tail(sort(colMeans(hierCluster2)))
tail(sort(colMeans(hierCluster3)))
tail(sort(colMeans(hierCluster4)))
tail(sort(colMeans(hierCluster5)))
tail(sort(colMeans(hierCluster6)))
tail(sort(colMeans(hierCluster7)))
tail(sort(colMeans(hierCluster8)))
tail(sort(colMeans(hierCluster9)))
tail(sort(colMeans(hierCluster10)))
tail(sort(colMeans(hierCluster11)))

#Clustering the documents in K-means Clustering
set.seed(1234)
k = kmeans(data, centers = 11)
kClusters = k$cluster
table(kClusters)

#Spliting the dataset for each K-means clustering
kCluster1 = subset(data, kClusters == 1)
kCluster2 = subset(data, kClusters == 2)
kCluster3 = subset(data, kClusters == 3)
kCluster4 = subset(data, kClusters == 4)
kCluster5 = subset(data, kClusters == 5)
kCluster6 = subset(data, kClusters == 6)
kCluster7 = subset(data, kClusters == 7)
kCluster8 = subset(data, kClusters == 8)
kCluster9 = subset(data, kClusters == 9)
kCluster10 = subset(data, kClusters == 10)
kCluster11 = subset(data, kClusters == 11)

#Finding the six most common words for each cluster
tail(sort(colMeans(kCluster1)))
tail(sort(colMeans(kCluster2)))
tail(sort(colMeans(kCluster3)))
tail(sort(colMeans(kCluster4)))
tail(sort(colMeans(kCluster5)))
tail(sort(colMeans(kCluster6)))
tail(sort(colMeans(kCluster7)))
tail(sort(colMeans(kCluster8)))
tail(sort(colMeans(kCluster9)))
tail(sort(colMeans(kCluster10)))
tail(sort(colMeans(kCluster11)))
