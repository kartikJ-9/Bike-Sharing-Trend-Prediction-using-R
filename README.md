# Bike-Sharing-Trend-Prediction-using-R

This was my project for Machine Learning Algorithm Course. As a beginner to the Data Science and Machine Learning domain, I have used R as a powerful tool to build the various Machine Learning models and perform analysis on the trends of the Bike Sharing.

This dataset was taken from the AI Lab of University of Porto. And the dataset contains bike rental statistics over a course of 2 years considering external conditions as features.

Preprocessing:

For the sake of exploratory simplicity, the size of the dataset is cut down to 10,000. But, note that, by doing so, it poses a severe threat of unforeseen bias. So, a series of steps was followed for preprocessing. We use a random number generator and shuffle the dataset based upon the number of count. Now, cut down the dataset, thereby, preventing bias.

We have also added a base_price classifier, for the company to dedcide upon the base price, as to, whether it should be low or not! Preprocessing is mostly done in Python due to the dependency of the Pandas Library

Models Used:

-Regression
-Linear Discriminant Analysis
-Neural Networks
-Smoothing Splines
-Boosting

The split ratio for train:test is 75:25. 

Future Work:

-Cross Validation to perform validation. It is often found that cross validation increases the accuracy and interpretability of the model.
-Subset selection: To perform best subset selection, forward subset, and backward subset selection.
-Build Random Forests, Local Regression, Ridge and Lasso Models


