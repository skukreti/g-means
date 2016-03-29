G-means
============================================
The following R scripts implement the G-means implementation from "Learning the k in k-means": Hammerly and Elkans (NIPS 2004)

The code is structured as:

-> GMeans.R: contains the code for gmeans() and gmeans_visualize()
-> Analysis.R: contains code for analyzing what value of k is best for the given dataset
-> data.csv: containes sample dataset on which analysis is based.
-> BestFit.png: shows the best fit plot (k = 8).
-> AlphaVsFit.png: shows multiple plots for different values of alpha

Function Description:
=====================
gmeans(dataset, alpha = 0.0001)
-------------------------------
The function expects a matrix as a dataset and significance level alpha (by default is 0.0001).
On completion, the function returns the kmeans model fit to the optimal number of clusters based on the value of alpha.

gmeans_visualize(dataset, model, alpha=0.0001, sig=0.95)
--------------------------------------------------------
The function takes the dataset, model, alpha and significance value and uses it to plot a 2D scatterplot and ellipsoid plot (for sig confidence interval)


Analysis.R handles input for data, and calls the necessary methods to create plots for SSE and scatter plots.

BestFit.png shows the SSE plot vs number of clusters for k-means. The error rate flattens out near 6-8 clusters which corroborates the value achieved by gmeans()
AlphaVsFit.png shows the k values with different values of alpha:

| alpha  |  k  |
|--------|-----|
| 0.0050 |  35 |
| 0.0010 |  28 |
| 0.0001 |  17 |
| 0.0005 |   8 |

Analysis.R also creates a pdf for each plot
