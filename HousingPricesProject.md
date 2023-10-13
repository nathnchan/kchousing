Housing Prices in King County, Washington
================
Nathan Chan

2023-09-13

``` r
housing <- read.csv("kc_housing.csv", stringsAsFactors = TRUE)
```

This data set includes the following variables:

- `id`: unique ID
- `date`: date of the sale
- `price`: selling price of the house
- `bedrooms`: number of bedrooms
- `bathrooms`: number of bathrooms
- `living`: size of the interior living space (in square feet)
- `lot`: size of the lot (in square feet)
- `floors`: number of floors
- `waterfront`: is the housing located on the waterfront? (yes or no)
- `view`: a *numeric* rating of 1 to 5 for the quality of the view
  (higher = better)
- `condition`: the condition of the house (neutral, good, or great)
- `above`: size of the interior living space above ground level (in
  square feet)
- `basement`: size of the interior living space below ground level (in
  square feet)
- `year`: original year of construction
- `zipcode`: zip code
- `living15`: average size of the interior living space for the 15
  nearest neighbors (in square feet)
- `lot15`: average size of the lot for the 15 nearest neighbors (in
  square feet)
- `renovated`: was the house renovated in the previous 25 years? (yes or
  no)

``` r
library("car")
library("ggmap")
library("ggplot2")
library("mapdata")
library("maps")
library("dplyr")
library("ggthemes")
library("knitr")
library("kableExtra")
library("cowplot")
```

![](HousingPricesProject_files/figure-gfm/Washington%20State%20Map-1.png)<!-- -->

> King County is a prominent and populous county located in the Pacific
> Northwest region of the United States. It is situated in the western
> part of the state and encompasses the city of Seattle, the largest
> city in Washington. The county is also known for its stunning natural
> beauty, including Puget Sound, Cascade Range, and Mount Rainier. On
> this map, King County is highlighted in purple.

> Let’s take a look at some different scatterplots that relate some of
> our variables to price, so we can get a high level understanding of
> some of the potential relationships we may have here.

![](HousingPricesProject_files/figure-gfm/Creation%20of%20a%20Scatterplot%20Matrix%20of%20the%20Dataset-1.png)<!-- -->

> I think that it is interesting that the number of bathrooms is not as
> clear cut as the number of bedrooms. This may indicate some half
> bathrooms and even quarter bathrooms (which I didn’t know was even a
> thing!).

![](HousingPricesProject_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

> I think that it is interesting that the number of bathrooms is not as
> clear cut as the number of bedrooms. This may indicate some half
> bathrooms and even quarter bathrooms (which I didn’t know was even a
> thing!). When it comes to the views and presence of a waterfront, it
> appears that a higher rated view and being on a waterfront raises the
> price ceiling.

![](HousingPricesProject_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

> The above variable also aligns with the “living” variable given that
> not many of these housing units have basements and that they could be
> smaller apartments. What is surprising is that the condition of the
> housing unit as well as if the housing unit has been renovated appears
> to not make a difference in price at first. I think this may be
> because a lot of the buildings in this county are much newer. This is
> something we will check on later.

> Something that I want to look at are the average home prices within
> each zipcode, as real estate markets are highly localized. Different
> zipcodes within a city or region can exhibit varying property values
> due to factors like neighborhood amenities, school quality, proximity
> to job centers, and overall desirability.

    ## # A tibble: 6 × 2
    ##   Zipcode `Average House Price`
    ##     <int>                 <dbl>
    ## 1   98039              3640000 
    ## 2   98040              2290000 
    ## 3   98004              2112143.
    ## 4   98033              2002150 
    ## 5   98008              1689758.
    ## 6   98105              1598429.

    ## # A tibble: 6 × 2
    ##   Zipcode `Average House Price`
    ##     <int>                 <dbl>
    ## 1   98032               283500 
    ## 2   98003               281071.
    ## 3   98188               266362.
    ## 4   98001               239075 
    ## 5   98002               232151.
    ## 6   98168               231983.

> Taking a look at the top 6 most expensive zipcodes in this dataset, we
> see the areas: Medina, Mercer Island, and Kirkland. All of these areas
> have a high cost of living, with Medina even having a cost of living
> that is 120% greater than the national average! They all have
> desirable qualities such as being near water and having a close
> proximity to Seattle for those who want easy access to the city. On
> the other hand the 6 least expensive zipcodes in this dataset host
> cities such as Tukwila, Burien, and SeaTac where these cities are
> actually slightly below the national average cost of living. When
> looking at location, these cities are not near the water as well as
> being much further from Seattle which could explain the urban effect
> not being as prominent as the top 6.

> I wanted to take a closer look at some of the frequencies we see for
> some of our variables, so I constructed a few histograms:

![](HousingPricesProject_files/figure-gfm/Printing%20Plots-1.png)<!-- -->![](HousingPricesProject_files/figure-gfm/Printing%20Plots-2.png)<!-- -->![](HousingPricesProject_files/figure-gfm/Printing%20Plots-3.png)<!-- -->![](HousingPricesProject_files/figure-gfm/Printing%20Plots-4.png)<!-- -->![](HousingPricesProject_files/figure-gfm/Printing%20Plots-5.png)<!-- -->![](HousingPricesProject_files/figure-gfm/Printing%20Plots-6.png)<!-- -->

> Some takeaways I have from these histograms is that the houses in this
> dataset are fairly evenly distributed amongst the zipcodes which could
> indicate this being an accurate representation. Also, many of the
> houses are built fairly similarily with the lot sizes being much
> smaller and the living spaces taking up a large portion of the lot.
> This indicates that there is not much space for a backyard or
> frontyard and usage of parks is probably standard which could lead to
> an increase cost of living due to taxes for upkeep of public spaces.
> While speaking about lot sizes, it also noted that there are a few
> outliers here that have a square footage of around half a million. The
> price of houses in Washington are also significantly more expensive
> than my hometown as majority of the homes are in the 7 figure range.

> I am thinking about tossing out the basement variable entirely after
> also doing a quick search about houses not having basements on the
> West Coast (which is somewhat unusual coming from a Midwest
> perspective). Nonetheless, just to confirm I wanted to look at its
> relationship with price.

![](HousingPricesProject_files/figure-gfm/Price%20v.%20Basement%20Size-1.png)<!-- -->

![](HousingPricesProject_files/figure-gfm/Basement%20Size%20+%20Zipcode%20Area-1.png)<!-- -->

> As seen here, there is very little correlation between the two due to
> there actually being no basement in many homes, as stated before. I
> think we can rule this variable out.

![](HousingPricesProject_files/figure-gfm/Price%20v.%20Living%20Room%20Size%20of%2015%20Nearest%20Neighbors-1.png)<!-- -->

> It appears that price and the living room size of the nearest 15
> neighbors have a little bit of a quadratic relationship which we will
> account for in the model.

![](HousingPricesProject_files/figure-gfm/Price%20v.%20Lot%20Size-1.png)<!-- -->

> Also noted earlier, the histogram of lot size was extremely right
> skewed. I conducted a log transformation to make the distribution
> appear more normal. This will also be accounted for in the model.

![](HousingPricesProject_files/figure-gfm/Relating%20Bathrooms%20and%20Bedrooms%20to%20Price-1.png)<!-- -->![](HousingPricesProject_files/figure-gfm/Relating%20Bathrooms%20and%20Bedrooms%20to%20Price-2.png)<!-- -->

> It appears that there is a positive relationship with the number of
> bathrooms and price of a house, but for the bedrooms it appears to not
> have that much of a relationship, which makes me wonder if there is an
> interaction between these two. Why would you want to have a house with
> 6 bedrooms when you only have 2 bathrooms?

    ## 
    ## Call:
    ## lm(formula = price ~ bedrooms * bathrooms, data = housing)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2400317  -385775  -120061   231973  4643665 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          381317     214389   1.779  0.07585 .  
    ## bedrooms            -269166      61446  -4.380 1.42e-05 ***
    ## bathrooms            247263      91906   2.690  0.00735 ** 
    ## bedrooms:bathrooms   100807      22258   4.529 7.26e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 627100 on 556 degrees of freedom
    ## Multiple R-squared:  0.4339, Adjusted R-squared:  0.4309 
    ## F-statistic: 142.1 on 3 and 556 DF,  p-value: < 2.2e-16

> The summary shows that there is a possible interaction between number
> of bedrooms and bathrooms, as the statistically significant
> demonstrates as proof.

    ## # A tibble: 16 × 2
    ##    zipcode avg_price_no_water_over_million
    ##      <int>                           <dbl>
    ##  1   98006                        1650000 
    ##  2   98004                        1527500 
    ##  3   98027                        1520000 
    ##  4   98105                        1490000 
    ##  5   98144                        1400000 
    ##  6   98033                        1386667.
    ##  7   98040                        1320000 
    ##  8   98112                        1294000 
    ##  9   98109                        1260000 
    ## 10   98177                        1200000 
    ## 11   98199                        1180000 
    ## 12   98053                        1160000 
    ## 13   98074                        1130000 
    ## 14   98103                        1130000 
    ## 15   98117                        1080000 
    ## 16   98005                        1040000

![](HousingPricesProject_files/figure-gfm/Waterfront%20Boxplot-1.png)<!-- -->

> As seen earlier with the zipcodes, the zipcodes where there was a
> possibility for owning a house on a waterfront were much more
> expensive than ones that weren’t. This boxplot also confirms that
> statement due to the median price being much higher for homes on a
> waterfront.

![](HousingPricesProject_files/figure-gfm/Renovation%20Boxplot-1.png)<!-- -->

> Renovations appear to not make a difference in price. Maybe the houses
> are relatively new. Let’s check.

![](HousingPricesProject_files/figure-gfm/Renovation%20and%20Construction%20Date-1.png)<!-- -->

> Looking from the 1950s and onwards, it appears that there has been an
> increase in construction. However, it appears that houses in genereal
> haven’t been renovated as seen with the homes built before 1950.

![](HousingPricesProject_files/figure-gfm/Boxplot%20of%20Price%20by%20View%20Rating,%20-1.png)<!-- -->

> It appears that the view rating has a slightly positive correlation
> with the price of homes. I wanted to group the zipcodes into two
> different areas: Downtown Seattle and Other. I made a new column
> called Zipcode Area

    ##          id       date  price bedrooms bathrooms living    lot floors
    ## 1  65000085   7/8/2014 430000        3      2.00   1550   6039    1.0
    ## 2  87000283 11/18/2014 359950        3      2.50   1980   7800    2.0
    ## 3  98030660  3/11/2015 815000        4      2.50   3880   7208    2.0
    ## 4 109210280 11/11/2014 220000        4      2.25   1950   7280    2.0
    ## 5 121029034  6/24/2014 549000        2      1.00   2034  13392    1.0
    ## 6 121039042  3/13/2015 425000        3      2.75   3610 107386    1.5
    ##   waterfront view condition above basement year zipcode living15 lot15
    ## 1         no    1     great   830      721 1942   98126     1330  6042
    ## 2         no    1   neutral  1980        1 1999   98055     1700 20580
    ## 3         no    1   neutral  3880        1 2006   98075     3280  7221
    ## 4         no    1      good  1950        1 1979   98023     1910  7280
    ## 5        yes    5     great  1159      876 1947   98070     1156 15961
    ## 6        yes    4   neutral  3130      481 1918   98023     2630 42126
    ##   renovated      zipcodeArea
    ## 1        no Downtown Seattle
    ## 2        no            Other
    ## 3        no            Other
    ## 4        no            Other
    ## 5        no            Other
    ## 6        no            Other

![](HousingPricesProject_files/figure-gfm/graph%20for%20abv-1.png)<!-- -->

> It looks like there is a small difference between Downtown Seattle and
> other areas when looking at lot size. There are no observations in
> Downtown Seattle that are above 100,000 square feet. This could be
> most likely due to construction guidelines.

> After some exploratory data analysis, I am ready to construct a model:

``` r
model1 <-lm(price ~ living + waterfront + zipcodeArea + log(lot) + I(living15^2) + year + bathrooms*bedrooms, data = housing3)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = price ~ living + waterfront + zipcodeArea + log(lot) + 
    ##     I(living15^2) + year + bathrooms * bedrooms, data = housing3)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1240980  -191427     2325   178602  2179257 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         5.595e+06  1.406e+06   3.978 7.86e-05 ***
    ## living              4.096e+02  2.654e+01  15.433  < 2e-16 ***
    ## waterfrontyes       5.792e+05  4.303e+04  13.462  < 2e-16 ***
    ## zipcodeAreaOther   -1.018e+05  3.993e+04  -2.550  0.01105 *  
    ## log(lot)           -1.008e+05  2.000e+04  -5.040 6.32e-07 ***
    ## I(living15^2)       3.276e-02  5.950e-03   5.505 5.66e-08 ***
    ## year               -2.290e+03  7.110e+02  -3.221  0.00135 ** 
    ## bathrooms          -1.714e+05  6.077e+04  -2.820  0.00497 ** 
    ## bedrooms           -2.151e+05  3.754e+04  -5.728 1.67e-08 ***
    ## bathrooms:bedrooms  6.845e+04  1.359e+04   5.037 6.41e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 369600 on 550 degrees of freedom
    ## Multiple R-squared:  0.8055, Adjusted R-squared:  0.8023 
    ## F-statistic: 253.1 on 9 and 550 DF,  p-value: < 2.2e-16

> An adjusted R-squared value of 0.8023 (or 80.23%) suggests that
> approximately 80.23% of the variability in the dependent variable is
> explained by the independent variables included in the regression
> model. In other words, the model accounts for about 80.23% of the
> variance in the data. Not bad. The p-value is also very small (2.2e16)
> which indicates statistical significance.

``` r
vif(model1)
```

    ## there are higher-order terms (interactions) in this model
    ## consider setting type = 'predictor'; see ?vif

    ##             living         waterfront        zipcodeArea           log(lot) 
    ##           4.246220           1.554670           1.441908           1.493667 
    ##      I(living15^2)               year          bathrooms           bedrooms 
    ##           2.323150           1.707737          11.433322           5.058192 
    ## bathrooms:bedrooms 
    ##          18.510285

> Using the vif function, I can see that there is limited
> multicollinearity except the variables regarding bathrooms and
> bedrooms. Logically this makes sense, as a home with more bedrooms is
> more likely to have more bathrooms to match. Let’s see if removing the
> interaction or one of the variables makes a better model.

    ## 
    ## Call:
    ## lm(formula = price ~ living + waterfront + zipcodeArea + log(lot) + 
    ##     I(living15^2) + year + bathrooms + bedrooms, data = housing3)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1340617  -185399     4878   174594  2214697 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       6.315e+06  1.430e+06   4.417 1.20e-05 ***
    ## living            4.274e+02  2.688e+01  15.898  < 2e-16 ***
    ## waterfrontyes     5.748e+05  4.396e+04  13.077  < 2e-16 ***
    ## zipcodeAreaOther -1.006e+05  4.080e+04  -2.466  0.01395 *  
    ## log(lot)         -1.016e+05  2.044e+04  -4.972 8.86e-07 ***
    ## I(living15^2)     2.858e-02  6.021e-03   4.747 2.63e-06 ***
    ## year             -2.954e+03  7.139e+02  -4.138 4.04e-05 ***
    ## bathrooms         9.242e+04  3.149e+04   2.935  0.00348 ** 
    ## bedrooms         -5.808e+04  2.140e+04  -2.715  0.00684 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 377700 on 551 degrees of freedom
    ## Multiple R-squared:  0.7966, Adjusted R-squared:  0.7936 
    ## F-statistic: 269.7 on 8 and 551 DF,  p-value: < 2.2e-16

    ## 
    ## Call:
    ## lm(formula = price ~ living + waterfront + zipcodeArea + log(lot) + 
    ##     I(living15^2) + year + bathrooms, data = housing3)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1305098  -193209     5474   179435  2275013 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       5.820e+06  1.426e+06   4.081 5.14e-05 ***
    ## living            4.112e+02  2.636e+01  15.597  < 2e-16 ***
    ## waterfrontyes     6.144e+05  4.171e+04  14.730  < 2e-16 ***
    ## zipcodeAreaOther -9.606e+04  4.100e+04  -2.343 0.019484 *  
    ## log(lot)         -1.029e+05  2.055e+04  -5.009 7.39e-07 ***
    ## I(living15^2)     2.808e-02  6.053e-03   4.639 4.38e-06 ***
    ## year             -2.757e+03  7.143e+02  -3.860 0.000127 ***
    ## bathrooms         7.051e+04  3.061e+04   2.303 0.021651 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 379800 on 552 degrees of freedom
    ## Multiple R-squared:  0.7938, Adjusted R-squared:  0.7912 
    ## F-statistic: 303.6 on 7 and 552 DF,  p-value: < 2.2e-16

> Looking at the adjusted R-squared value we see for the removal of the
> interaction, it is 0.7936 and for removal of the bedrooms variable it
> is 0.7912. Both are slightly lower than the original model, but
> nonetheless lower. I am going to make the decision to keep the
> interaction in the model as it has a high adjusted R-squared, and I
> believe it shows a difference in price with number of bedrooms that
> would not have been uncovered before.
