# Capital-Bikeshare-Traffic-Planning
Project as a part of coursework for Introduction to Machine Learning Class in UT Austin's MS Business Analytics Program.

## Goal:
The popularity of bike sharing as a convenient mode of transportation for employees and students in cities has been on the rise. In Austin, Lime and Bird have become particularly popular among students for their daily commutes. In light of this, we sought to understand how the demand for bike sharing services is affected by the number of registered and casual users. To do this, we used trip data from Capital Bikeshare to predict the hourly ridership of bike users.

In our analysis, we employed various techniques such as Regularized Regression, KNN, Trees, and Tree-Based Ensemble Methods to predict hourly ridership. We found that XGBoost, which is a tree-based ensemble method, performed the best with an R-Square of 88.7%. This was achieved by fine-tuning the parameters using Grid Search.

## Conclusion: 

All codes and insights are available in this repository in the form of R codes and PPTs/PDFs.

1. Our analysis revealed that the square root model provided the best fit for the data when running multiple linear regression. This suggests that there is at least a square relationship present in the data.

2. The coefficients of our Square Root MLR model were found to be consistent with the results of our initial Exploratory Data Analysis for both registered and casual users.

3. As expected, a non-parametric model such as trees was found to be most effective for our dataset, which included multiple nominal categorical variables without an inherent order.

4. Our findings indicate that the Hour of the Day was the most significant factor affecting registered users, while temporal and weather factors were less important.

5. On the other hand, for casual riders, the Hour of the Day remained the most important variable, but the Temperature was also found to be a significant influence.

## Business Insights:

1. To increase usage among casual riders, we recommend implementing a weekend pass option that takes advantage of the correlation we found between the number of casual riders and weekends.

2. To optimize pricing for casual riders, we suggest using dynamic pricing based on weather conditions, such as increasing prices on hot and sunny weekend days when our model predicts higher demand.

3. Our model allows Capital Bikeshare to make more accurate predictions about demand and supply, providing a valuable tool for planning and optimization.

