# Housing Price Analysis

View the published document on [Rpubs](https://rpubs.com/nathchan/1098465).

## Introduction

Washington is a state known for its diverse landscapes, vibrant cities, and a strong economy. It offers a wide range of housing options, from urban apartments to suburban homes, and even rural properties. The decision to move to Washington involves various factors, and one of the most critical considerations is the cost of housing.

To make an informed decision about moving to Washington, it's essential to understand the housing market in the area, including factors such as price trends, location preferences, and the availability of different types of properties.

This repository contains a statistical analysis of housing prices in Washington. The King County dataset (kc_housing) is a great first dataset for anyone looking to get into regression and data analysis. There are great opportunities to implement transformations to the data. My motivation for exploring the kc_housing dataset is my parents who planned to move to Washington State after my sister and I left for college. Learning about statistical computing in R as well as basic regression analysis through my education at the University of Michigan, I wanted to apply what I learned.

## Data Source

For this analysis, we have sourced the kc_housing dataset containing information about housing prices in King County, Washington. The dataset includes a wide range of variables:

- `id`: unique ID
- `date`: date of the sale
- `price`: selling price of the house
- `bedrooms`: number of bedrooms
- `bathrooms`: number of bathrooms 
- `living`: size of the interior living space (in square feet)
- `lot`: size of the lot (in square feet)
- `floors`: number of floors
- `waterfront`: is the housing located on the waterfront? (yes or no)
- `view`: a *numeric* rating of 1 to 5 for the quality of the view (higher = better)
- `condition`: the condition of the house (neutral, good, or great)
- `above`: size of the interior living space above ground level (in square feet)
- `basement`: size of the interior living space below ground level (in square feet)
- `year`: original year of construction
- `zipcode`: zip code
- `living15`: average size of the interior living space for the 15 nearest neighbors (in square feet)
- `lot15`: average size of the lot for the 15 nearest neighbors (in square feet)
- `renovated`: was the house renovated in the previous 25 years? (yes or no)

## Objectives

The main objectives of this analysis are as follows:

1. **Understand Price Trends**: To identify the trends in housing prices in King County, Washington through construction of a quality linear regression model that aims to predict price through taking into account a multitude of quantitative and qualitative variables.

2. **Refine Data Preprocessing Skills**: To modify a dataset with inconsistencies, inaccuracies, and errors into one that is well structured, consistent, and reliable. This includes the removal of entries that contain missing values, incorrect values, or duplicates.

3. **Conduct Exploratory Data Analysis (EDA)**: Examining and summarizing a dataset to understand its main characteristics, patterns, and potential outliers through visualization to potentially utilize transformations such as logarthmic or quadratic transformations to meet assumptions of linear models.
   
4. **Improve With Iterations of Each Model**: Make use of markers such as Multiple R-squared, Adjusted R-squared, and Variance Inflation Factor (VIF) to assess the goodness of fit, the quality of a regression model, and the presence of multicollinearity.

## Methodology

The analysis will be conducted using various statistical techniques and exploratory data analysis, including data visualization, regression analysis, and other modeling methods. The findings will be presented in a clear and understandable manner.

