# House-Price-Analysis
#Determine the value of residential properties by predicting the market price using a set of attributes

#The project involved variable selection and the creation of a machine learning models from scratch that forecasts the price of houses based on variables #like the number of rooms, area, etc. as closely as possible to the original values. A machine learning model for predicting housing prices was created #using linear regression, lasso regression and stepwise regression.

Data Cleaning: Our first approach involves looking at the ‘predict_property_data’ and analyzing what property descriptors may influence the ‘sale_price’ of a property which is our dependent variable. The steps mentioned below involve a series of decisions based on statistical analysis and research using the data dictionary given.
1. We first remove the columns which have null values greater than 50% compared to the total data available:

2. Next, we filter for columns which according to the codebook do not play a major role in predictive modelling except ‘geo_school_elem_district’ , ‘geo_school_hs_district’ , ‘meta_town_code’. These may play a major role in determining the sales price given our knowledge and video references.

3. With the remaining columns which have null values, we run a function which performs highest occurrence mode replacement to the categorical columns. We further replace the numerical columns nulls with medians and logical columns with ‘0’.

4. Lastly, after treating all the null values from the data, we perform an outlier detection. As can be seen in the plot below (Appendix 1), we remove the outliers for 4 columns namely- ‘char_bldg_sf’, ‘char_hd_sf’, ‘sale_price’, ‘econ_midincome’.

Modeling: We divide the data into a training set (70%) and a test set (30%). Empirical studies show that the best results are obtained if we use 20-30% of the data for testing, and the remaining 70-80% of the data for training [1]. The model then trains itself using this training data for the selected attributes to predict the outcome using the test data. This model learns with every iteration and becomes efficient enough to predict the sales price of any given property with the attributes/predictors. We first use a simple linear regression to train the model followed by forward, backward and stepwise regression to test variable selection accuracy. Though the later mentioned models provide an easy way to select relevant variables, there outputs (regression coefficients, confidence intervals, p-values, and R2) are highly biased and thus may not reflect the real accuracy. Also, these models taken a considerable amount of time to run and hence we will not include them in our result. Lastly, we use the lasso regression model which provides a greater accuracy compared to other regression models. It tends to use the shrinkage penalty to force certain coefficients to zero.
The MSE we get after performing the linear regression is the least as compared to the other models. For the lasso regression model, we get a lower MSE value which does not categorize it as the best candidate for this prediction.


linear regression performs the best among the three models used (Linear regression, Lasso regression, Stepwise regression).

