# Online-Shoppers-Purchasing-Intention-Analysis

This project focuses on analyzing the purchasing intentions of online shoppers using a dataset containing 12,330 records and 18 variables. The analysis employs both classification and clustering techniques to understand shopper behavior and predict revenue generation. The K-Nearest Neighbors (KNN) algorithm is utilized for classification, while K-means clustering is applied to identify distinct shopper segments. The insights derived from this analysis can help online retailers optimize their marketing strategies and improve revenue generation.

### Tools & Technologies  
- R - Programming Language for Data Analysis 
- R Packages:
  ```bash
  caTools - For dataset splitting
  dplyr - For data manipulation
  ggplot2 - For data visualization
  class - For KNN implementation
  caret - For machine learning framework
  corrplot - For correlation visualization
  cluster - For clustering algorithms
  factoextra - For multivariate analysis and visualization
  ggfortify - For advanced cluster visualizations
  ```

### Key features

- Classification Analysis - Utilizes the KNN model to predict whether an online shopping session generates revenue based on various features such as page views and time spent on pages.
- Clustering Analysis - Implements K-means clustering to segment shoppers into distinct groups based on their behavior, helping to identify trends and outliers.
- Data Visualization - Employs ggplot2 for visualizing the results of both classification and clustering, including confusion matrices and cluster analysis plots.
- Performance Evaluation - Analyzes model accuracy and error rates, optimizing the K value in the KNN model to achieve the best predictive performance.
The dashboard provides actionable insights into the local property market, assisting investors, policy makers, and developers in making informed decisions.

📊 Dashboard  
![image alt](https://github.com/Peheni01/Online-Shoppers-Purchasing-Intention-Analysis/blob/74486691cf5f9e7c3733eb0adfa08169a3b47af0/1.%20dashboard.jpg)
![image alt](https://github.com/Peheni01/Online-Shoppers-Purchasing-Intention-Analysis/blob/74486691cf5f9e7c3733eb0adfa08169a3b47af0/2.%20dashboard.png)

### References 
- [UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/468/online+shoppers+purchasing+intention+dataset) 
- [Sakar, C.O., Polat, S.O., Katircioglu, M., & Kastro, Y. (2018). Real-time prediction of online shoppers’ purchasing intention using multilayer perceptron and LSTM recurrent neural networks. Neural Computing and Applications, 31(10), 6893–6908. doi](https://doi.org/10.1007/s00521-018-3523-0)
