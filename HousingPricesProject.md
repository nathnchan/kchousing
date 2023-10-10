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
```

    ## Loading required package: carData

``` r
library("ggmap")
```

    ## Loading required package: ggplot2

    ## The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
    ## which was just loaded, will retire in October 2023.
    ## Please refer to R-spatial evolution reports for details, especially
    ## https://r-spatial.org/r/2023/05/15/evolution4.html.
    ## It may be desirable to make the sf package available;
    ## package maintainers should consider adding sf to Suggests:.
    ## The sp package is now running under evolution status 2
    ##      (status 2 uses the sf package in place of rgdal)

    ## ℹ Google's Terms of Service: <https://mapsplatform.google.com>
    ## ℹ Please cite ggmap if you use it! Use `citation("ggmap")` for details.

``` r
library("ggplot2")
library("mapdata")
```

    ## Loading required package: maps

``` r
library("maps")
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'
    ## 
    ## The following object is masked from 'package:car':
    ## 
    ##     recode
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("ggthemes")
```

``` r
states <- map_data("state")
head(states)
```

    ##        long      lat group order  region subregion
    ## 1 -87.46201 30.38968     1     1 alabama      <NA>
    ## 2 -87.48493 30.37249     1     2 alabama      <NA>
    ## 3 -87.52503 30.37249     1     3 alabama      <NA>
    ## 4 -87.53076 30.33239     1     4 alabama      <NA>
    ## 5 -87.57087 30.32665     1     5 alabama      <NA>
    ## 6 -87.58806 30.32665     1     6 alabama      <NA>

``` r
wa_df <- subset(states,region%in%"washington")

counties <- map_data("county")
wa_county <- subset(counties, region == "washington")
wa_base <- ggplot(data = wa_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "grey")

kcmap <- wa_base + theme_nothing() +
  geom_polygon(data = wa_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

king_county <- subset(wa_county, subregion == "king")

wa_base + theme_void() +
geom_polygon(data = wa_county, fill = NA, color = "white") +
geom_polygon(color = "black", fill = NA) +
geom_polygon(data = king_county, fill = "mediumslateblue", color = "white")
```

![](HousingPricesProject_files/figure-gfm/Washington%20State%20Map-1.png)<!-- -->

``` r
pairs(housing[,c("price", "lot", "basement", "living", "living15", "lot15")])
```

![](HousingPricesProject_files/figure-gfm/Creation%20of%20a%20Scatterplot%20Matrix%20of%20the%20Dataset-1.png)<!-- -->

``` r
pairs(housing[,c("price", "bedrooms", "bathrooms", "view", "waterfront", "zipcode")])
```

![](HousingPricesProject_files/figure-gfm/Creation%20of%20a%20Scatterplot%20Matrix%20of%20the%20Dataset-2.png)<!-- -->

``` r
pairs(housing[,c("price", "above", "floors", "bedrooms", "bathrooms")])
```

![](HousingPricesProject_files/figure-gfm/Creation%20of%20a%20Scatterplot%20Matrix%20of%20the%20Dataset-3.png)<!-- -->

``` r
pairs(housing[,c("price", "condition", "renovated")])
```

![](HousingPricesProject_files/figure-gfm/Creation%20of%20a%20Scatterplot%20Matrix%20of%20the%20Dataset-4.png)<!-- -->

``` r
length(unique(housing$zipcode))
```

    ## [1] 68

``` r
zipcode_prices <- housing%>%select("price", "zipcode")
price_per_zipcode <- zipcode_prices%>%group_by(zipcode)%>%summarise(avghouseprice
                                                                    = mean(price))
price_per_zipcode_ordered <- price_per_zipcode%>%arrange(desc(avghouseprice))
head(price_per_zipcode_ordered)
```

    ## # A tibble: 6 × 2
    ##   zipcode avghouseprice
    ##     <int>         <dbl>
    ## 1   98039      3640000 
    ## 2   98040      2290000 
    ## 3   98004      2112143.
    ## 4   98033      2002150 
    ## 5   98008      1689758.
    ## 6   98105      1598429.

``` r
tail(price_per_zipcode_ordered)
```

    ## # A tibble: 6 × 2
    ##   zipcode avghouseprice
    ##     <int>         <dbl>
    ## 1   98032       283500 
    ## 2   98003       281071.
    ## 3   98188       266362.
    ## 4   98001       239075 
    ## 5   98002       232151.
    ## 6   98168       231983.

``` r
hist(housing$price,col="mediumslateblue")
```

![](HousingPricesProject_files/figure-gfm/Histograms-1.png)<!-- -->

``` r
hist(housing$lot,col="mediumslateblue")
```

![](HousingPricesProject_files/figure-gfm/Histograms-2.png)<!-- -->

``` r
hist(housing$basement,col="mediumslateblue")
```

![](HousingPricesProject_files/figure-gfm/Histograms-3.png)<!-- -->

``` r
hist(housing$living,col="mediumslateblue")
```

![](HousingPricesProject_files/figure-gfm/Histograms-4.png)<!-- -->

``` r
hist(housing$zipcode,col="mediumslateblue")
```

![](HousingPricesProject_files/figure-gfm/Histograms-5.png)<!-- -->

``` r
hist(housing$bathrooms,col="mediumslateblue")
```

![](HousingPricesProject_files/figure-gfm/Histograms-6.png)<!-- -->

``` r
ggplot(data = housing, aes(x = price, fill = "")) +
  geom_histogram(binwidth = 250000, color = "black") +
  scale_fill_manual(values = "mediumslateblue") +
  labs(title = "Histogram of Price",
       x = "Price",
       y = "Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_fivethirtyeight() +
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

![](HousingPricesProject_files/figure-gfm/Histograms-7.png)<!-- -->

``` r
ggplot(data = housing, aes(x = year, fill = "")) +
  geom_histogram(binwidth = 5, color = "black") +
  scale_fill_manual(values = "mediumslateblue") +
  labs(title = "Histogram of Years Constructed",
       x = "Year",
       y = "Frequency") +
  theme_minimal() +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

![](HousingPricesProject_files/figure-gfm/Histograms-8.png)<!-- -->

``` r
hist(log(housing$lot),col="mediumslateblue")
```

![](HousingPricesProject_files/figure-gfm/Individual%20Histograms%20of%20log(Variables)-1.png)<!-- -->

``` r
hist(log(housing$basement),col="mediumslateblue")
```

![](HousingPricesProject_files/figure-gfm/Individual%20Histograms%20of%20log(Variables)-2.png)<!-- -->

``` r
hist(log(housing$living),col="mediumslateblue")
```

![](HousingPricesProject_files/figure-gfm/Individual%20Histograms%20of%20log(Variables)-3.png)<!-- -->

``` r
hist(log(housing$price),col="mediumslateblue")
```

![](HousingPricesProject_files/figure-gfm/Individual%20Histograms%20of%20log(Variables)-4.png)<!-- -->

``` r
price_basement_lm <- lm(price ~ basement, data = housing)
r_squared_basement <- summary(price_basement_lm)$r.squared

ggplot(housing, aes(x = basement, y = price)) +
  geom_point() +
  labs(x = "Basement Size", y = "Price") +
  ggtitle("Price vs. Basement Size") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = "lm", se = FALSE, color = "blueviolet") +
  theme_fivethirtyeight() +
  geom_text(aes(label = paste("R-squared = ", round(r_squared_basement, 2)), x = max(basement), y = min(price)), hjust = 1) +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](HousingPricesProject_files/figure-gfm/Price%20v.%20Basement%20Size-1.png)<!-- -->

``` r
ggplot(housing, aes(x = living15, y = price)) +
  geom_point() +
  labs(x = "Living Room Size of 15 Nearest Neighbors", y = "Price") +
  ggtitle("Price vs. Living Room Size of 15 Nearest Neighbors") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = "gam", se = FALSE, 
              color = "blueviolet") +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](HousingPricesProject_files/figure-gfm/Price%20v.%20Living%20Room%20Size%20of%2015%20Nearest%20Neighbors-1.png)<!-- -->

``` r
ggplot(housing, aes(x = bathrooms, y = price)) +
  geom_point() +
  labs(x = "Number of Bathrooms", y = "Price") +
  ggtitle("Price vs. Number of Bathrooms") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = "gam", se = FALSE, 
              color = "blueviolet") +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](HousingPricesProject_files/figure-gfm/Price%20v.%20Number%20of%20Bathrooms-1.png)<!-- -->

``` r
ggplot(housing, aes(x = log(lot), y = price)) +
  geom_point() +
  labs(x = "Log of Lot Size", y = "Price") +
  ggtitle("Price vs. Log of Lot Size") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

![](HousingPricesProject_files/figure-gfm/Price%20v.%20Lot%20Size-1.png)<!-- -->

``` r
housing3 <- housing
housing3$basement <- housing3$basement + 1
```

``` r
bathroom_price <- housing%>%select(price, bathrooms)
bathroom_price$bathrooms <-as.factor(bathroom_price$bathrooms)

ggplot(bathroom_price, aes(x = bathrooms, y = price)) +
  geom_boxplot(fill = "lightslateblue", color = "black", width = 0.5) +
  labs(title = "Boxplot of Price by Number of Bathrooms", x = "Number of Bathrooms", y = "Price") +
  scale_y_continuous(labels = scales::comma) +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

![](HousingPricesProject_files/figure-gfm/Relating%20Bathrooms%20and%20Bedrooms%20to%20Price-1.png)<!-- -->

``` r
bedroom_price <- housing%>%select(price, bedrooms)
bedroom_price$bedrooms <-as.factor(bedroom_price$bedrooms)

ggplot(bedroom_price, aes(x = bedrooms, y = price)) +
  geom_boxplot(fill = "lightslateblue", color = "black", width = 0.5) +
  labs(title = "Boxplot of Price by Number of Bedrooms", x = "Number of Bedrooms", y = "Price") +
  scale_y_continuous(labels = scales::comma) +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

![](HousingPricesProject_files/figure-gfm/Relating%20Bathrooms%20and%20Bedrooms%20to%20Price-2.png)<!-- -->

``` r
summary(lm(price ~ bedrooms * bathrooms, data = housing))
```

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

``` r
no_water_over_million <- housing %>% filter(waterfront == "no", price >= 1000000) %>% group_by(zipcode)%>%summarise(avg_price_no_water_over_million=mean(price)) %>% arrange(desc(avg_price_no_water_over_million))
no_water_over_million
```

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

``` r
ggplot(housing, aes(x = waterfront, y = price)) +
  geom_boxplot(fill = "lightslateblue", color = "black", width = 0.5) +
  labs(title = "Boxplot of Price by Waterfront Presence", x = "Waterfront Presence", y = "Price") +
  scale_x_discrete(labels = c("no" = "Not on Waterfront", "yes" = "On Waterfront")) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size = 12))
```

![](HousingPricesProject_files/figure-gfm/Waterfront%20Boxplot-1.png)<!-- -->

``` r
ggplot(housing, aes(x = renovated, y = price)) +
  geom_boxplot(fill = "lightslateblue", color = "black", width = 0.5) +
  labs(title = "Boxplot of Price by Renovation", x = "Renovation", y = "Price") +
  scale_x_discrete(labels = c("no" = "Not Renovated", "yes" = "Renovated")) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size = 12)) +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

![](HousingPricesProject_files/figure-gfm/Renovation%20Boxplot-1.png)<!-- -->

``` r
price_year_lm <- lm(price ~ year, data = housing)
r_squared_year <- summary(price_year_lm)$r.squared

ggplot(housing, aes(x = year, y = price)) +
  geom_point() +
  labs(x = "Year", y = "Price") +
  ggtitle("Price vs. Year Constructed") +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = "loess", se = FALSE, color = "blueviolet") +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_text(aes(label = paste("R-squared = ", round(r_squared_year, 2)), x = max(year), 
                y = min(price)), hjust = 1) +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](HousingPricesProject_files/figure-gfm/Price%20v%20Year-1.png)<!-- -->

``` r
custom_colors <- c("mediumslateblue", "darkslateblue")

ggplot(data = housing, aes(x = year, fill = renovated)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Construction Dates of Homes",
       x = "Year",
       y = "Frequency",
       fill = NULL) +
  scale_fill_manual(values = custom_colors, labels = c("no" = "Not Renovated", "yes" = 
                                                         "Renovated")) +
  theme_minimal() +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(axis.title = element_text())
```

![](HousingPricesProject_files/figure-gfm/Renovation%20and%20Construction%20Date-1.png)<!-- -->

``` r
view_price <- housing%>%select(price, view)
view_price$view <-as.factor(view_price$view)

ggplot(view_price, aes(x = view, y = price)) +
  geom_boxplot(fill = "lightslateblue", color = "black", width = 0.5) +
  labs(title = "Boxplot of Price by View Rating", x = "View Rating", y = "Price") +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(legend.position = "none") +
  theme(axis.title = element_text())
```

![](HousingPricesProject_files/figure-gfm/Boxplot%20of%20Price%20by%20View%20Rating-1.png)<!-- -->

``` r
housing3$zipcodeArea <- NA

for (i in 1:nrow(housing3)) {
  if (housing3$zipcode[i] %in% c("98177", "98133", "98155", 
                                 "98125", "98117", "98103",
                                 "98115", "98107", "98105",
                                 "98199", "98119", "98109",
                                 "98102", "98112", "98122",
                                 "98121", "98101", "98154",
                                 "98104", "98144", "98134",
                                 "98106", "98126", "98116",
                                 "98136", "98108", "98118",
                                 "98146", "98168", "98178")) {
    housing3$zipcodeArea[i] <- "Downtown Seattle"
  } else {
    housing3$zipcodeArea[i] <- "Other"
  }
}
housing3$zipcodeArea <- as.factor(housing3$zipcodeArea)
housing3
```

    ##             id       date   price bedrooms bathrooms living    lot floors
    ## 1     65000085   7/8/2014  430000        3      2.00   1550   6039    1.0
    ## 2     87000283 11/18/2014  359950        3      2.50   1980   7800    2.0
    ## 3     98030660  3/11/2015  815000        4      2.50   3880   7208    2.0
    ## 4    109210280 11/11/2014  220000        4      2.25   1950   7280    2.0
    ## 5    121029034  6/24/2014  549000        2      1.00   2034  13392    1.0
    ## 6    121039042  3/13/2015  425000        3      2.75   3610 107386    1.5
    ## 7    121039083   2/6/2015  629000        3      1.75   1460  12367    2.0
    ## 8    185000118  2/23/2015  212000        4      2.00   1880   7500    1.0
    ## 9    203100690  4/25/2015 1080000        4      2.75   3160  42733    2.0
    ## 10   217500135  4/21/2015  450000        4      2.25   2040   9565    1.0
    ## 11   221029019  4/28/2015  400000        3      2.50   2090  32718    2.0
    ## 12   222029026  9/17/2014  340000        2      0.75   1060  48292    1.0
    ## 13   225039175  5/13/2014  525000        5      3.00   2450   4591    2.0
    ## 14   226059106   1/2/2015  489500        3      1.75   2090  65558    1.0
    ## 15   258100040 11/24/2014  335000        3      2.00   1490   8847    1.0
    ## 16   272000620  12/2/2014  290000        2      1.00    900   2728    2.0
    ## 17   284000025  4/20/2015 1410000        2      2.00   2180  18525    1.0
    ## 18   284000095  9/22/2014 1200000        2      2.25   2160  17861    2.0
    ## 19   305500140  5/12/2015  365000        3      2.50   2200   4052    2.0
    ## 20   319500080  6/18/2014  764000        4      2.50   2790   7938    2.0
    ## 21   323059146  4/17/2015  343000        3      1.00   1410  18600    1.0
    ## 22   333100295 11/24/2014 3120000        3      3.50   4490  56609    2.0
    ## 23   339600110  9/23/2014  395000        3      2.50   1610   3755    2.0
    ## 24   372000040  10/3/2014  304000        3      1.75   1720   6000    1.0
    ## 25   421049254  10/2/2014  179000        2      1.00    990   8760    1.0
    ## 26   425079001  4/23/2015  499950        3      2.50   3230 129578    1.0
    ## 27   439200035 11/17/2014  740000        4      2.75   2560   6900    1.0
    ## 28   510002065  3/23/2015  700000        4      1.00   1980   4560    1.5
    ## 29   510002506  8/25/2014  459500        2      1.50   1170   1079    3.0
    ## 30   518500460  10/8/2014 2230000        3      3.50   3760   5634    2.0
    ## 31   518500480  8/11/2014 3000000        3      3.50   4410  10756    2.0
    ## 32   518500700   5/7/2014  630000        2      2.25   2550   5663    1.0
    ## 33   521049200   7/8/2014  819000        3      2.75   3176  13391    2.0
    ## 34   526069024  5/12/2014  950000        5      3.00   4530 258746    1.5
    ## 35   534000195 11/13/2014  510000        3      1.75   1370   6700    1.0
    ## 36   585000095 12/16/2014  625000        3      2.50   2180   5000    1.0
    ## 37   594000115  5/12/2014  615000        2      1.75   2040  28593    1.5
    ## 38   622049114  2/18/2015 2130000        3      2.50   5403  24069    2.0
    ## 39   623059016  7/17/2014 1100000        4      3.25   3190  11774    2.0
    ## 40   624069035  12/9/2014 2750000        4      4.00   4130   5575    2.0
    ## 41   624069108  8/12/2014 3200000        4      3.25   7000  28206    1.0
    ## 42   625059036  8/13/2014 2700000        5      4.00   4230  27295    2.0
    ## 43   626049115  11/5/2014  405000        4      2.50   2620   8960    1.0
    ## 44   629800520   9/3/2014 1210000        4      3.25   4330  26162    2.0
    ## 45   685000115  10/7/2014 2150000        8      6.00   4340   9415    2.0
    ## 46   686400060  8/20/2014  521000        4      2.25   1890   8034    1.0
    ## 47   723069089  7/15/2014  575000        4      2.50   2550  56628    2.0
    ## 48   724069023  4/14/2015 1250000        1      1.25   1810   5070    1.5
    ## 49   724069059   5/9/2014 2400000        3      2.25   3000  11665    1.5
    ## 50   726049190  2/18/2015  431000        3      1.00   1810   7200    1.0
    ## 51   727500030  7/15/2014  815000        3      1.50   1370   8671    1.0
    ## 52   740500040  10/1/2014  265000        4      1.00   1860   8505    1.0
    ## 53   766000240  9/15/2014  225000        4      2.00   2220  14120    1.0
    ## 54   777100005 11/22/2014 1650000        3      2.25   2750   6203    1.0
    ## 55   809002290  5/19/2014 1190000        4      3.00   2240   6000    1.5
    ## 56   809002765 10/22/2014  610000        3      1.00   1180   3400    1.5
    ## 57   822039084  3/11/2015 1350000        3      2.50   2753  65005    1.0
    ## 58   824059211 11/17/2014  800000        4      1.75   2150   9148    1.0
    ## 59   825059178  9/23/2014 2570000        4      3.75   4475  20424    2.0
    ## 60   825079024   5/6/2015  785000        3      2.75   2990 207781    2.0
    ## 61   889000025  8/11/2014  599000        3      1.75   1650   1180    3.0
    ## 62   908000010 12/12/2014  360000        5      2.50   2880   6902    1.0
    ## 63   922059161  9/22/2014  365000        3      2.00   2140  26600    1.0
    ## 64   952003480 11/17/2014  445000        4      1.00   1460   4600    1.0
    ## 65   993001629 11/17/2014  265000        3      2.75   1120    881    3.0
    ## 66  1025039086  9/16/2014 1880000        3      2.50   3280  29111    2.0
    ## 67  1081330180 12/22/2014  627000        4      2.50   2750  11830    2.0
    ## 68  1085623710  7/14/2014  447055        4      2.50   2448   4949    2.0
    ## 69  1088810040  3/20/2015  627250        4      2.50   2830  10677    2.0
    ## 70  1105000015   6/9/2014  417000        2      1.00    920   6600    1.0
    ## 71  1105000588  4/21/2015  349500        3      1.00   1400   3538    1.0
    ## 72  1118001370   1/2/2015 1570000        3      2.75   2340   8828    1.0
    ## 73  1121039059  5/22/2014  503000        2      1.75   2860  59612    1.0
    ## 74  1176001124  2/24/2015  598950        3      2.50   1480   1531    3.0
    ## 75  1223039173  4/29/2015  450000        4      1.75   2190  11625    1.0
    ## 76  1224059053 10/27/2014 1700000        5      2.00   2500  15250    2.0
    ## 77  1231000510  9/22/2014  263000        3      1.75   1490   3800    1.0
    ## 78  1231000895  11/5/2014  986000        4      3.50   2840   5900    2.0
    ## 79  1245002391 10/22/2014 1400000        5      4.25   4230   6907    2.0
    ## 80  1247600105 10/20/2014 5110000        5      5.25   8010  45517    2.0
    ## 81  1250203070  5/14/2014 1400000        3      2.50   2550   7200    2.0
    ## 82  1257201010   5/4/2015  698000        2      1.00   1510   4080    1.0
    ## 83  1269200229  7/23/2014 1380000        3      3.25   3786  38038    1.0
    ## 84  1313500070  8/20/2014  249000        3      1.50   1580   7200    1.0
    ## 85  1324049015 11/11/2014 2490000        4      2.50   3440  23954    1.5
    ## 86  1325059083  5/27/2014  830000        4      2.50   1850  50662    1.0
    ## 87  1326069191   2/2/2015  334000        3      2.25   1840   9781    2.0
    ## 88  1337800220   9/8/2014 1000000        4      2.50   2230   3600    2.0
    ## 89  1338300180  7/29/2014 1400000        4      2.25   3960   8640    2.0
    ## 90  1352300520  1/13/2015  294000        3      3.00   1670   4120    1.5
    ## 91  1402600110  2/26/2015  392000        4      2.25   2360   7733    2.0
    ## 92  1402950550   1/7/2015  332000        4      2.50   2470   7780    2.0
    ## 93  1423200110  5/13/2015  180000        2      1.00    800   9450    1.0
    ## 94  1423910670  5/27/2014  305000        4      1.00   2100   9288    1.0
    ## 95  1432701380   4/7/2015  263000        3      1.00   1250   7560    1.0
    ## 96  1438000010  9/12/2014  569995        4      2.50   2650   6875    2.0
    ## 97  1446400785  4/22/2015  228950        3      1.00   1120   6625    1.0
    ## 98  1453602284  11/3/2014  296000        2      2.00   1320   2040    3.0
    ## 99  1455100355   7/8/2014 1680000        3      2.50   3490   8343    2.0
    ## 100 1455600062 10/21/2014  689000        3      2.50   2080   9612    1.0
    ## 101 1525059190  9/12/2014 1040000        5      3.25   4770  50094    1.0
    ## 102 1551500130  5/22/2014  180000        4      1.50   1740   7292    1.0
    ## 103 1568100920   4/8/2015 1950000        4      2.50   3440  14554    2.0
    ## 104 1574700140   8/8/2014  550000        3      1.75   1830   9720    1.0
    ## 105 1624079088  8/11/2014  415000        3      2.75   3900 111514    2.0
    ## 106 1646501845  9/13/2014  570000        3      2.00   1270   3090    2.0
    ## 107 1678400105  2/12/2015  339000        4      1.50   2390   7480    1.5
    ## 108 1721800190   4/9/2015  300000        2      1.50   1300   6120    1.0
    ## 109 1724069059  5/24/2014 2000000        5      4.00   4580   4443    3.0
    ## 110 1724069060   5/7/2015 1080000        2      3.25   1550   1767    3.0
    ## 111 1724069062  7/14/2014 1370000        2      3.25   2700   3444    3.0
    ## 112 1724069079  3/19/2015 1450000        2      3.25   2070   3128    2.0
    ## 113 1725079061  7/10/2014  500000        3      1.75   1640  47044    1.0
    ## 114 1727500280  8/20/2014  480000        4      2.25   1770   7000    1.0
    ## 115 1762600090  4/24/2015 1210000        4      2.50   3430  35120    2.0
    ## 116 1822039138  2/27/2015  600000        2      2.25   2320  18919    2.0
    ## 117 1826049426   1/5/2015  445000        4      2.75   2320  12368    1.0
    ## 118 1827200265  9/11/2014 1900000        2      2.75   3690  32044    2.0
    ## 119 1843100580   9/3/2014  360000        4      2.50   2340  13445    2.0
    ## 120 1921059310  3/20/2015  193000        2      1.75   1280   6774    1.0
    ## 121 1922039062  4/20/2015  480000        2      1.50   1008  26487    1.0
    ## 122 1922059027  1/23/2015  282510        4      1.00   1450  32234    1.5
    ## 123 1924059029  6/17/2014 4670000        5      6.75   9640  13068    1.0
    ## 124 1925069066  6/23/2014 1700000        3      2.75   2810  18731    2.0
    ## 125 1925069082  5/11/2015 2200000        5      4.25   4640  22703    2.0
    ## 126 1925069121  3/30/2015  960000        3      2.50   1730   4102    3.0
    ## 127 1926069137   7/7/2014  775000        4      3.25   4100 241322    2.0
    ## 128 1928300350 12/22/2014  570000        3      1.75   1370   3300    2.0
    ## 129 1931300665  10/9/2014  850000        3      3.00   1910   4800    1.5
    ## 130 1951820070  8/22/2014  491500        3      2.25   2230  13100    1.0
    ## 131 1954430190   8/8/2014  528000        4      2.75   2050   7171    1.0
    ## 132 1954700615 10/22/2014  825000        4      1.50   2040   6900    2.0
    ## 133 1959701890  7/29/2014  865000        4      1.75   1800   4180    2.0
    ## 134 1972200259   5/7/2014  425000        2      2.50   1150   1027    3.0
    ## 135 2011000010   5/2/2014  257950        3      1.75   1370   5858    1.0
    ## 136 2013802030  9/11/2014  357000        3      2.00   2460  53882    1.0
    ## 137 2013802060  9/27/2014  500000        2      1.00   1760  27332    1.0
    ## 138 2024069008  6/19/2014 2200000        5      4.75   5990  10450    2.0
    ## 139 2025059150   7/2/2014 1040000        4      1.75   1440  13296    1.0
    ## 140 2025069065  9/29/2014 2400000        4      2.50   3650   8354    1.0
    ## 141 2025069140  3/17/2015 1900000        3      2.50   2830   4334    3.0
    ## 142 2028701000  5/29/2014  635200        4      1.75   1640   4240    1.0
    ## 143 2095500120  7/18/2014  350000        4      2.50   2380   6124    2.0
    ## 144 2113700060 10/14/2014  400000        4      2.50   2350   3904    2.5
    ## 145 2115720130  8/21/2014  289950        3      2.50   2070   5013    2.0
    ## 146 2122039094 11/26/2014  705000        3      3.00   1970  20978    2.0
    ## 147 2123039032 10/27/2014  369900        1      0.75    760  10079    1.0
    ## 148 2124089028  7/14/2014  279000        3      1.75   1430  39160    1.0
    ## 149 2125059013  4/20/2015 1670000        5      3.50   4320  40816    2.0
    ## 150 2138700141   7/2/2014  736000        2      1.00   1500   4000    1.0
    ## 151 2159900120  8/22/2014  419000        2      2.50   1470   2034    2.0
    ## 152 2291401115 11/25/2014  349950        1      1.00   1230   9300    1.5
    ## 153 2310050110 12/15/2014  364950        3      2.25   2520   6170    2.0
    ## 154 2322029048 11/19/2014  999000        3      2.75   2830 505166    1.0
    ## 155 2325300037   9/2/2014  358000        3      3.25   1410   1442    3.0
    ## 156 2402100575  6/13/2014 1130000        6      3.75   3010   4360    2.0
    ## 157 2408800120  7/16/2014  360000        4      1.75   2140  49658    1.0
    ## 158 2413300240 11/10/2014  280000        4      2.25   1990   7350    1.0
    ## 159 2423029009  6/17/2014  465000        2      2.00   1494  19271    2.0
    ## 160 2424410110  6/11/2014  325000        3      1.75   1790  27427    1.0
    ## 161 2425049063  9/11/2014 3640000        4      3.25   4830  22257    2.0
    ## 162 2450000290 12/15/2014 1250000        5      2.50   3370   8113    2.0
    ## 163 2460700260  2/18/2015  300000        3      2.00   1480   6698    1.0
    ## 164 2469000010  3/23/2015 1080000        4      2.25   2100  12172    1.0
    ## 165 2473002080  3/17/2015  500000        3      2.75   3410   9360    1.5
    ## 166 2473250400  9/24/2014  325000        4      2.00   1780  10622    1.0
    ## 167 2473370750  2/24/2015  430000        3      1.75   3440  10428    1.5
    ## 168 2473400570  5/16/2014  317000        3      2.00   1760  11410    1.0
    ## 169 2475901105  7/15/2014  291000        3      1.00   1280  10500    1.5
    ## 170 2517010230  2/26/2015  286000        3      2.50   1800   3980    2.0
    ## 171 2523400205  4/21/2015  510000        2      1.50   1860   5100    1.0
    ## 172 2524049215   5/1/2015 1560000        4      3.75   3730  17000    2.0
    ## 173 2568200140  6/25/2014  739900        5      2.50   2980   5377    2.0
    ## 174 2579500110   7/1/2014 2370000        3      2.25   3530  17450    1.0
    ## 175 2597490410   4/2/2015  740000        3      2.50   2350   3798    2.0
    ## 176 2600010070  4/14/2015  998000        3      2.25   3370  11757    2.0
    ## 177 2619920170  10/1/2014  772500        4      2.50   3230   4290    2.0
    ## 178 2621700010   5/8/2014  569000        4      2.25   2250  41688    2.0
    ## 179 2623029003 12/16/2014  635000        3      1.75   1940 167125    1.0
    ## 180 2623039018 10/27/2014  685000        4      1.00   1550  15239    1.5
    ## 181 2623039019   5/8/2014  988500        3      2.75   2015  16807    2.0
    ## 182 2623039082  2/18/2015  770000        3      3.50   2050  21744    2.0
    ## 183 2624049091  3/13/2015 2900000        5      2.50   3750  91681    2.0
    ## 184 2658000335 10/27/2014  275000        3      1.25   1230   4500    1.5
    ## 185 2722059075  8/11/2014  455000        3      2.75   2720  31314    3.0
    ## 186 2723069147   9/2/2014  635000        3      2.25   2680 175982    1.0
    ## 187 2724079061 10/10/2014  610000        3      1.75   1650 221720    1.0
    ## 188 2725069164   8/5/2014  785000        4      2.50   2990   9374    2.0
    ## 189 2726049034 11/10/2014 2000000        3      3.25   2610  16387    2.0
    ## 190 2726079103  7/22/2014  475000        3      2.50   2630 185130    2.0
    ## 191 2738640040   4/9/2015  644000        4      2.50   3310   4839    2.0
    ## 192 2767602645 11/10/2014  507000        4      2.00   1360   2746    1.5
    ## 193 2767704756 10/20/2014  470000        3      3.50   1280   1257    2.0
    ## 194 2767704777  9/19/2014  436000        3      2.50   1460   1238    2.0
    ## 195 2781230020  12/9/2014  398500        4      2.50   2820   6666    2.0
    ## 196 2781600195 11/17/2014  285000        1      1.00   1060  54846    1.0
    ## 197 2804600155   5/7/2015 1350000        4      1.75   2000   3728    1.5
    ## 198 2856100935  9/23/2014 1080000        5      3.50   3740   5610    2.0
    ## 199 2868900020   4/8/2015  215000        3      1.00   1010  10125    1.0
    ## 200 2877104196  12/6/2014  760000        3      2.00   1780   1750    1.0
    ## 201 2887970040   4/8/2015  234950        2      2.50   1720   3132    2.0
    ## 202 2909300150  7/14/2014  675000        4      2.50   2900   5505    2.0
    ## 203 2919201335  7/31/2014  912000        4      3.75   1980   4095    2.0
    ## 204 2923039243 11/13/2014  340000        4      1.00   1200  11834    1.0
    ## 205 2923500230 12/16/2014 2600000        4      4.50   5270  12195    2.0
    ## 206 2946000285   3/2/2015  200000        3      2.00   1170  10051    1.0
    ## 207 2954400520  4/30/2015 1240000        4      3.25   5180  49936    2.0
    ## 208 3022039071  5/30/2014  800000        2      2.25   1730  31491    2.0
    ## 209 3024059014  3/25/2015 1900000        4      2.25   3020  11489    1.5
    ## 210 3026059361  4/17/2015  479000        2      2.50   1741   1439    2.0
    ## 211 3034200933  6/19/2014  399888        4      2.25   1820   8255    1.5
    ## 212 3102700160  9/25/2014  344900        4      1.75   1820   7700    1.0
    ## 213 3121500340  7/12/2014  690000        4      2.50   2900  23488    2.0
    ## 214 3124059023  2/13/2015 1960000        3      1.75   3330  12566    1.0
    ## 215 3126059023   3/3/2015 3400000        4      3.50   4730  47870    1.0
    ## 216 3211200140  7/10/2014  350000        4      2.00   1720   7210    1.0
    ## 217 3211240370  12/1/2014  460000        4      2.25   2690  36114    2.0
    ## 218 3222049044  6/12/2014  835000        3      3.00   2790  12523    2.0
    ## 219 3222049055  1/16/2015  650000        3      1.75   2800  19386    1.0
    ## 220 3222049087  4/22/2015  570000        1      1.00    720   7540    1.0
    ## 221 3222049151 10/30/2014  820000        3      2.50   2990  10711    1.0
    ## 222 3224079005  10/9/2014  255000        2      1.00    920  43560    1.0
    ## 223 3225069065  6/24/2014 3080000        4      5.00   4550  18641    1.0
    ## 224 3225069241  4/22/2015 2000000        3      2.50   3490  21064    1.0
    ## 225 3271801090  4/30/2015 1180000        4      2.00   2590   7220    2.0
    ## 226 3278600240  7/22/2014  372500        2      2.50   1400   2958    2.0
    ## 227 3278600710  7/14/2014  200000        1      1.50   1010   1157    2.0
    ## 228 3295700060   6/2/2014  500000        3      2.00   1720   5525    1.0
    ## 229 3298600850 12/16/2014  235000        3      1.75   1370  14030    1.0
    ## 230 3303860460  4/15/2015  499000        4      2.50   3100   5700    2.0
    ## 231 3304040020 12/26/2014  375500        4      2.50   2301   6452    2.0
    ## 232 3306300630  9/24/2014  212000        3      1.75   1100   9750    1.0
    ## 233 3320000212  10/6/2014  397500        3      2.25   1350    980    2.0
    ## 234 3325069064  3/26/2015 1050000        3      1.00   1860  44431    1.0
    ## 235 3327000140  6/17/2014  235000        3      1.75   1190   7280    1.0
    ## 236 3330500085  9/18/2014  366000        2      1.00   1210   3090    1.0
    ## 237 3342104046   7/8/2014 1570000        4      2.25   2890  18226    3.0
    ## 238 3343301910 10/20/2014 1000000        5      4.50   2120   8944    2.0
    ## 239 3343301920   3/2/2015 1650000        3      2.75   2690   8890    2.0
    ## 240 3343302110   3/6/2015 1800000        3      3.00   2790  13295    2.0
    ## 241 3421059049  6/10/2014  475000        2      1.75   1490 224334    1.0
    ## 242 3423049165  3/31/2015  240000        3      1.00   1270  12733    1.0
    ## 243 3423049209  3/18/2015  200450        3      1.00    970   9130    1.0
    ## 244 3426049284  8/19/2014 2300000        4      3.25   4110  15929    2.0
    ## 245 3432500210  3/26/2015  325000        2      1.00   1130   6908    1.5
    ## 246 3438503014 10/20/2014  230000        2      1.00    870   7020    1.0
    ## 247 3523029041  10/9/2014  290000        2      0.75    440   8313    1.0
    ## 248 3529000880   3/9/2015  610000        4      2.50   2110   6360    2.0
    ## 249 3558900430  4/14/2015  615000        3      2.25   2300   8067    1.0
    ## 250 3574770100  1/16/2015  550000        4      2.75   3650   4534    2.0
    ## 251 3621059043  5/27/2014  293000        4      2.50   3250 235063    1.0
    ## 252 3623500260  5/12/2014 1200000        3      1.75   1560   8078    1.5
    ## 253 3623500408  3/30/2015 2600000        3      3.00   3410  16015    2.0
    ## 254 3625059043   9/4/2014 3300000        5      4.75   6200  13873    2.0
    ## 255 3625059152 12/30/2014 3300000        3      3.25   4220  41300    1.0
    ## 256 3629970190  1/26/2015  769000        4      3.50   3010   6202    2.0
    ## 257 3630010040  5/23/2014  402000        3      2.00   1540   1827    2.0
    ## 258 3630080190   8/1/2014  405000        3      2.50   1500   2314    2.0
    ## 259 3649100276   6/9/2014  368000        3      1.75   1710  10800    1.0
    ## 260 3751600030  7/17/2014  100000        2      1.00    770  17334    1.0
    ## 261 3751606514  6/26/2014  270000        2      1.00   1780  81021    1.0
    ## 262 3760500116 11/20/2014 3070000        3      2.50   3930  55867    1.0
    ## 263 3761100045  6/18/2014 3000000        4      4.25   4850  12445    2.0
    ## 264 3761100180  9/17/2014 1600000        4      2.50   2980  13341    1.5
    ## 265 3761700053   1/5/2015 2150000        3      2.75   3470   9610    3.0
    ## 266 3790700070 11/21/2014  302500        4      2.50   1990   5511    2.0
    ## 267 3797000035  5/21/2014  430000        3      1.00   1150   3000    1.0
    ## 268 3818700185  9/25/2014  400000        4      1.50   2150  11026    1.0
    ## 269 3824100020   2/3/2015  335000        3      1.75   1510   9720    1.0
    ## 270 3824100166 11/22/2014  385000        4      1.75   1970  10358    1.0
    ## 271 3824100286  3/19/2015  565000        3      2.25   2440   8378    1.0
    ## 272 3831000010   8/6/2014  235000        4      1.50   1760   6150    1.5
    ## 273 3840700757  6/19/2014  585000        4      2.50   2840  11044    2.0
    ## 274 3885802136  7/23/2014  899000        4      2.50   2580   3943    2.0
    ## 275 3892500150  5/21/2014 1550000        3      2.50   4460  26027    2.0
    ## 276 3904930730  11/5/2014  496600        3      2.50   1910   5562    2.0
    ## 277 3905050240  6/24/2014  425000        3      2.50   1930   4500    2.0
    ## 278 3905080280   3/4/2015  529000        3      2.50   1880   4499    2.0
    ## 279 3905100840  7/23/2014  500000        3      2.25   1580   4379    2.0
    ## 280 3920000040 10/10/2014  280000        5      2.00   2110   7919    1.0
    ## 281 3946900010  3/23/2015  500007        2      1.75   1820   6050    1.0
    ## 282 4027701265   5/1/2015  480000        3      1.75   2920  21375    1.0
    ## 283 4030100005  12/9/2014 1800000        5      3.75   4320  39094    2.0
    ## 284 4030100290  10/1/2014 1680000        5      3.50   5170   7197    3.0
    ## 285 4031000520 11/25/2014  227000        1      2.00   1150   9812    1.0
    ## 286 4035900015  6/19/2014  659500        4      3.00   2620  18362    1.0
    ## 287 4037000470  3/16/2015  550000        3      1.75   1440   8957    1.0
    ## 288 4037000840   4/6/2015  554000        3      2.00   1910   9001    1.0
    ## 289 4039000050  7/14/2014  516130        3      1.75   1510   8250    1.0
    ## 290 4046601010 10/23/2014  399950        3      1.75   2450  15001    1.0
    ## 291 4046601420 12/30/2014  340000        3      2.00   1570  14992    1.0
    ## 292 4051110240  8/25/2014  225000        3      2.50   1750   7490    1.0
    ## 293 4053200285  8/11/2014  725000        3      2.50   3410  41022    2.0
    ## 294 4053200933  6/23/2014  249000        3      1.00   1000  19204    1.0
    ## 295 4055701200  4/21/2015 1960000        4      2.75   3120   7898    1.0
    ## 296 4058801225  12/2/2014  350000        4      1.75   1820   6930    1.0
    ## 297 4077800094   2/2/2015  675000        4      1.75   2220   7230    1.0
    ## 298 4077800438 12/31/2014  518000        4      1.75   1780   8768    1.0
    ## 299 4100000050 10/30/2014  813000        3      1.75   2080  11866    1.0
    ## 300 4109600306  2/18/2015  475000        2      1.00    920   5157    1.0
    ## 301 4114601570 11/18/2014 3600000        3      3.25   5020  12431    2.0
    ## 302 4114601580  7/24/2014 1900000        6      4.00   3020  13237    2.0
    ## 303 4123830480  6/10/2014  392000        4      2.75   1940   6555    2.0
    ## 304 4123840470  2/24/2015  408000        3      2.50   2620   8403    2.0
    ## 305 4131900066  8/25/2014 3100000        3      3.00   3920  13085    2.0
    ## 306 4134300175  4/17/2015 1850000        4      2.50   4120  14866    1.0
    ## 307 4139440610  5/12/2014  746000        3      2.50   2620   8950    2.0
    ## 308 4166600115 11/21/2014 1150000        3      2.75   3230  75889    2.0
    ## 309 4174600391  3/23/2015  393000        5      2.00   1820   5054    1.0
    ## 310 4178500440  1/28/2015  279900        3      2.00   1410   6600    1.0
    ## 311 4217401055   5/2/2014 1400000        4      2.50   2920   4000    1.5
    ## 312 4217402115  4/21/2015 3650000        6      4.75   5480  19401    1.5
    ## 313 4224100030   4/3/2015  372000        4      2.50   2520   9604    2.0
    ## 314 4279200060 12/30/2014  420000        4      2.50   2110   9825    2.0
    ## 315 4303200130  2/10/2015  277000        4      3.00   1960   5160    1.0
    ## 316 4305200070  5/19/2014  350000        3      2.25   1640   7200    2.0
    ## 317 4307330050  7/21/2014  439900        5      3.50   3390   7950    2.0
    ## 318 4318200090  6/18/2014  375000        2      1.00    940   9839    1.0
    ## 319 4331000400  2/20/2015  252000        3      1.50   1150  13200    1.0
    ## 320 4338800500 10/14/2014  262500        3      2.00   1130   7200    1.0
    ## 321 4356200210  3/18/2015  153500        3      1.00    890   4810    1.0
    ## 322 4376700570  4/27/2015  750000        6      1.75   2750   9563    2.0
    ## 323 4385700285   9/3/2014  690000        3      1.75   1600   4400    1.0
    ## 324 4397650080 10/15/2014  815000        3      3.75   2780   5002    2.0
    ## 325 4399210110  6/19/2014  232603        3      1.75   1750  11461    2.0
    ## 326 4458300190  4/24/2015  875000        3      2.50   1690  10592    1.0
    ## 327 4477000290  3/11/2015  474950        4      1.75   2030  15400    1.0
    ## 328 5014000215  8/18/2014  454000        2      1.00    880   6731    1.0
    ## 329 5019500215  1/15/2015  495000        2      1.75   1280   4000    1.0
    ## 330 5071401000  8/29/2014  779000        6      2.50   3250  12000    1.0
    ## 331 5104520610  7/14/2014  335000        4      2.50   1830   4500    2.0
    ## 332 5104531120  3/23/2015  775000        5      2.75   3750  12077    2.0
    ## 333 5119400075  6/20/2014  950000        3      3.25   3050  18892    1.0
    ## 334 5145100180  9/17/2014  325000        3      1.00   1150   7486    1.0
    ## 335 5145100300  9/18/2014  465000        3      2.00   1560   8509    1.0
    ## 336 5152100060  5/29/2014  472000        6      2.50   4410  14034    1.0
    ## 337 5154200015  4/14/2015 1710000        3      3.00   2490  27702    2.0
    ## 338 5154700060 10/15/2014 1660000        4      2.75   3520  19200    1.0
    ## 339 5215200010  6/26/2014  663000        3      2.50   2480  37843    1.5
    ## 340 5215200050  7/29/2014  750000        3      2.50   2960  69351    2.0
    ## 341 5216200090  6/16/2014  385000        2      1.00    830  26329    1.0
    ## 342 5256500025  8/27/2014  457000        4      1.75   2100  10358    1.0
    ## 343 5272200005  2/18/2015  175000        2      1.00   1160   6911    1.0
    ## 344 5347200070  4/27/2015  339000        3      1.00   1150   2496    1.0
    ## 345 5379804537  8/26/2014  270000        3      2.25   1760   8287    1.0
    ## 346 5416500660  4/30/2015  426500        4      2.50   2960   4640    2.0
    ## 347 5418500650  3/25/2015  586000        4      2.25   1930   8338    1.0
    ## 348 5419800220  6/10/2014  250000        3      1.75   1590   7560    1.0
    ## 349 5420300270  8/13/2014  231500        4      2.25   2080   7526    1.0
    ## 350 5422430320  7/21/2014  309950        4      2.50   1770   6666    2.0
    ## 351 5423030300  5/19/2014  525000        4      1.75   2420   7672    1.0
    ## 352 5437820020   8/7/2014  195000        3      1.75   1580   7875    1.0
    ## 353 5442300807  6/24/2014 2700000        5      2.75   3831  13800    2.0
    ## 354 5453700060  2/24/2015  875000        4      1.75   2180   9726    1.0
    ## 355 5454000010 12/10/2014  740000        3      1.75   2020   9478    1.0
    ## 356 5469300270   5/6/2014  234000        3      1.75   1490   8366    1.0
    ## 357 5560000640  6/19/2014  232500        3      1.00   1320   8450    1.0
    ## 358 5561300750  7/25/2014  518000        4      2.25   2640  34870    1.0
    ## 359 5561400610 10/19/2014  542500        5      3.50   2730  42500    1.0
    ## 360 5581400080  6/18/2014  770000        4      2.50   3210  14910    2.0
    ## 361 5608000630  11/3/2014 1520000        4      4.00   4500  11795    2.0
    ## 362 5612100065  5/29/2014  400000        4      2.00   1670  12056    1.0
    ## 363 5631501073  6/25/2014  374500        3      2.25   1400  11400    2.0
    ## 364 5632500110  7/16/2014  351000        3      1.00   1160  10518    1.0
    ## 365 5706201140 11/21/2014  533250        4      1.75   1520  15398    1.0
    ## 366 5728000060   8/1/2014  605000        3      1.75   1850   8823    1.0
    ## 367 6021501420  8/25/2014  571000        4      1.00   1350   4000    1.5
    ## 368 6021501685  4/22/2015  352000        2      1.00    940   5000    1.0
    ## 369 6055000430  3/27/2015  473000        4      3.50   4370  37193    2.0
    ## 370 6082400191  6/19/2014  287000        3      2.00   1300  11374    1.5
    ## 371 6102400166   9/5/2014  649000        3      2.00   1810  17006    2.0
    ## 372 6116500341  1/12/2015  419000        4      1.50   2150  23568    1.0
    ## 373 6117500460  6/30/2014 1310000        4      2.50   2680  12215    1.0
    ## 374 6117502220 11/17/2014 1580000        3      3.00   2610  22672    1.5
    ## 375 6117502230  12/1/2014 1640000        3      3.50   4660  21164    2.0
    ## 376 6143600555   6/9/2014  229950        4      1.75   1300  21000    1.0
    ## 377 6197200021  11/6/2014  144000        3      1.00    980   6800    1.0
    ## 378 6329000050  3/10/2015  641500        1      1.00   1000   9084    1.0
    ## 379 6329000185  3/29/2015  540000        3      2.50   2600  23361    1.5
    ## 380 6329000190  7/29/2014  750000        4      1.75   2520  21834    1.0
    ## 381 6329000380  6/19/2014  319950        2      1.00    920   8341    1.0
    ## 382 6352600210  6/11/2014  809950        4      2.50   3280   6181    2.0
    ## 383 6381501965  6/12/2014  430000        4      1.75   1890   6000    1.0
    ## 384 6383000690  3/25/2015  587100        3      2.25   1670   6414    1.0
    ## 385 6392001005  6/20/2014  511500        4      1.00   1360   6000    1.5
    ## 386 6412100092   1/5/2015  362500        3      1.00   1520   9507    1.0
    ## 387 6430000275   5/6/2014  485000        3      2.00   1420   4080    1.5
    ## 388 6431000015  5/30/2014  700000        4      2.50   2310   3570    1.5
    ## 389 6608500220 12/24/2014  410000        3      1.75   1340   9975    1.0
    ## 390 6613000930   9/2/2014 2950000        4      3.25   3890  25470    2.0
    ## 391 6623400356   7/2/2014  250000        3      1.75   1200  24805    1.0
    ## 392 6632300207   3/5/2015  385000        3      2.50   1520   1488    3.0
    ## 393 6664500090  1/15/2015  750000        5      4.00   4500   8130    2.0
    ## 394 6669020500  6/27/2014  330000        4      1.75   2440   7350    1.0
    ## 395 6728700075  5/20/2014  575000        4      1.75   1280   6060    1.0
    ## 396 6744701310  4/15/2015 1850000        4      2.50   3830  11972    1.0
    ## 397 6751300130  8/13/2014  510500        3      1.00   1270   8000    1.0
    ## 398 6752600050  8/12/2014  320000        4      2.50   2070   7007    2.0
    ## 399 6821102367  4/29/2015  547000        3      2.50   1570   1452    2.5
    ## 400 7003200120  6/27/2014  528000        2      0.75    840  40642    1.0
    ## 401 7010700660  4/28/2015  807000        3      2.50   1940   4000    2.0
    ## 402 7016300050  7/23/2014  420000        4      2.50   2030   8100    1.0
    ## 403 7110000068   7/3/2014  975000        6      2.75   2520  54160    2.0
    ## 404 7129303045  4/17/2015  949950        5      2.50   2340   1989    2.0
    ## 405 7129303070  8/20/2014  735000        4      2.75   3040   2415    2.0
    ## 406 7135520610  5/29/2014  950000        4      3.50   4140  13392    2.0
    ## 407 7140200450 12/31/2014  272000        4      2.75   1810   7350    1.0
    ## 408 7147400045  4/28/2015  355000        3      1.75   1870   8250    2.0
    ## 409 7167000020  6/16/2014  792500        4      2.50   4290 175421    2.0
    ## 410 7199330010  4/17/2015  525000        3      1.75   1720   7200    1.0
    ## 411 7203220050 11/18/2014  988830        5      3.25   4115   7910    2.0
    ## 412 7203600040  7/25/2014  625000        3      1.50   1990   5978    1.5
    ## 413 7204200025 10/28/2014 1230000        4      2.50   3120  49456    2.0
    ## 414 7205510370 11/24/2014  304500        3      2.25   1790   6930    1.0
    ## 415 7214700580   6/8/2014  510000        4      2.25   2450  62290    2.0
    ## 416 7214770020   4/9/2015  549950        5      2.50   2650  54380    2.0
    ## 417 7215400770  6/23/2014  260000        4      2.50   2000  37045    2.0
    ## 418 7215720680   2/3/2015  587000        4      2.75   2210   8430    2.0
    ## 419 7215721330 10/23/2014  485000        3      2.50   1650   4218    2.0
    ## 420 7278100515  8/21/2014 1300000        2      2.50   2910  19449    2.0
    ## 421 7281300010  8/22/2014 1200000        3      3.50   4310  10842    2.0
    ## 422 7297700055   3/5/2015  306000        3      1.00   1190  10350    1.0
    ## 423 7302000120  6/10/2014  695000        3      2.50   2550  45254    2.0
    ## 424 7334501440 10/21/2014  287000        3      1.50   1150  11475    1.0
    ## 425 7349660050 11/18/2014  268000        3      1.75   1600   7711    1.0
    ## 426 7351200050 12/18/2014 1340000        4      1.75   2300  13342    1.5
    ## 427 7351200295  1/14/2015 1150000        3      1.75   1760   6788    2.0
    ## 428 7352200025 10/13/2014 1190000        2      1.75   2080   8112    1.0
    ## 429 7352200100  2/24/2015 1360000        2      1.75   2620  14138    2.0
    ## 430 7352200450  1/15/2015 2050000        4      3.25   3580  19989    1.5
    ## 431 7403200050 11/13/2014 1600000        3      2.25   3370  23065    1.0
    ## 432 7417700664   4/8/2015  220000        4      2.00   1400   7140    1.0
    ## 433 7434500127  8/12/2014  527000        3      2.25   2240   6450    1.0
    ## 434 7452500190 10/16/2014  345000        3      1.75    710   5050    1.0
    ## 435 7518500855   5/4/2015  658600        4      2.00   1400   4690    1.5
    ## 436 7522500020  5/27/2014  730001        3      2.00   1840   4750    1.0
    ## 437 7524900003 12/10/2014 3280000        2      1.75   6840  10000    2.5
    ## 438 7558700030  4/13/2015 5300000        6      6.00   7390  24829    2.0
    ## 439 7558800620  8/22/2014  600000        2      1.75   1550   7764    1.0
    ## 440 7567600030  1/27/2015  750000        5      1.75   2640  13290    1.0
    ## 441 7567600045  8/27/2014  825000        2      1.00   1150  12775    1.0
    ## 442 7574910220  7/31/2014  795000        4      2.50   2920  32219    2.0
    ## 443 7574910450   2/3/2015  845000        4      2.50   3360  40471    2.0
    ## 444 7575610760   5/7/2015  290000        3      2.25   1620   7772    2.0
    ## 445 7575620120  4/22/2015  260000        3      3.00   2390   8993    2.0
    ## 446 7589200153   6/9/2014  559000        3      1.50   2070   5386    1.0
    ## 447 7625700305   6/5/2014  564000        3      1.75   1980   6250    1.0
    ## 448 7631200085  5/12/2014  947500        3      2.75   2980  27144    1.5
    ## 449 7631200310  11/6/2014  985000        2      2.50   2720  26761    2.0
    ## 450 7631800015   4/7/2015 2510000        3      3.25   5480  57990    2.0
    ## 451 7631800110  9/18/2014  380000        3      2.50   1980  17342    2.0
    ## 452 7636800041  6/25/2014  995000        3      4.50   4380  47044    2.0
    ## 453 7663700030   5/3/2015 1180000        2      2.50   1770   7155    2.0
    ## 454 7682200340  7/28/2014  182000        3      2.25   1960   8875    1.0
    ## 455 7686203195  3/23/2015  249950        3      1.50   1450   6875    1.0
    ## 456 7702010050  3/23/2015  590000        3      2.50   2830   5788    2.0
    ## 457 7715800430  11/4/2014  502000        3      2.50   1870   9135    1.0
    ## 458 7715801030  3/31/2015  510000        4      2.50   1620   8125    2.0
    ## 459 7738500731  8/15/2014 4500000        5      5.50   6640  40014    2.0
    ## 460 7781600025 10/23/2014 1160000        3      2.50   2490  24691    1.0
    ## 461 7781600100   9/5/2014 1340000        3      2.75   2730  38869    1.5
    ## 462 7796450340  12/5/2014  330000        4      2.50   2980   5674    2.0
    ## 463 7812801590 10/30/2014  219900        3      1.00    860   6664    1.0
    ## 464 7852030310  6/18/2014  440000        4      2.50   2410   4780    2.0
    ## 465 7852140040  8/25/2014  507250        3      2.50   2270   5536    2.0
    ## 466 7853240040  4/14/2015  700000        4      2.75   3350   7857    2.0
    ## 467 7853300280  2/13/2015  536000        4      2.50   2880   8833    2.0
    ## 468 7853301930  10/9/2014  405000        3      2.50   1960   6997    2.0
    ## 469 7853310150  7/22/2014  625000        5      1.00   3240   5324    2.0
    ## 470 7856400240  2/11/2015 1650000        4      3.00   3900   9750    1.0
    ## 471 7883605900 10/15/2014  315450        3      1.75   1130   7500    1.5
    ## 472 7889000125  3/19/2015  235000        3      1.00   1864   6978    1.0
    ## 473 7889600190  1/13/2015  229000        3      1.00   1590   6240    1.0
    ## 474 7905200315  4/16/2015  711777        4      1.75   2220   6731    1.0
    ## 475 7936500109  7/25/2014 2230000        3      3.00   3620  28064    2.0
    ## 476 7936500172  5/28/2014 1180000        3      2.50   1970  23180    1.0
    ## 477 7936500190 10/21/2014 1340000        4      3.75   2130  34689    1.5
    ## 478 7940700190  8/13/2014  380000        2      2.00   1370   5756    1.0
    ## 479 7940710070  8/22/2014  394000        3      2.50   1370   4400    1.0
    ## 480 7942601475  5/20/2014  345600        5      3.50   2800   5120    2.5
    ## 481 7977200945  3/10/2015  425000        3      1.00   1000   5100    1.0
    ## 482 7979900225  3/23/2015  360000        3      1.75   1900  11407    1.0
    ## 483 8029500100  2/26/2015  317000        3      2.50   2100   7587    2.0
    ## 484 8043700105  4/17/2015 2300000        4      4.00   4360   8175    2.5
    ## 485 8043700300   6/8/2014 2700000        4      3.25   4420   7850    2.0
    ## 486 8069000075 12/29/2014  790000        4      1.75   2460  10061    1.0
    ## 487 8073000480  7/22/2014  869000        2      1.75   1900  13122    1.0
    ## 488 8073000491 12/11/2014  700000        4      1.75   1950   7139    1.0
    ## 489 8073000495 10/10/2014  700000        2      1.00   1160  17635    1.0
    ## 490 8073000550  4/15/2015 1700000        4      3.75   3190  17186    2.0
    ## 491 8073000585  7/15/2014  840500        4      2.25   2290  12174    1.0
    ## 492 8091670030  5/12/2014  383000        4      2.50   2160   6223    2.0
    ## 493 8096000060  4/13/2015  655000        2      1.75   1450  15798    2.0
    ## 494 8106100105 11/14/2014 3850000        4      4.25   5770  21300    2.0
    ## 495 8121200460 11/19/2014  530000        3      2.50   2030  10958    2.0
    ## 496 8126300410  7/25/2014  650000        4      1.75   2390  12000    1.0
    ## 497 8161010060 12/18/2014  504750        3      2.50   2490  21937    2.0
    ## 498 8165500790 12/29/2014  336900        3      2.50   1690   1200    2.0
    ## 499 8550001515  10/1/2014  429592        2      2.75   1992  10946    1.5
    ## 500 8563030280  5/13/2014  700000        3      2.50   2030   8398    2.0
    ## 501 8572900135  5/23/2014  399500        3      1.75   2420  12676    2.0
    ## 502 8644000060 10/24/2014  237000        3      1.75   1270   8470    1.0
    ## 503 8645511500  4/20/2015  352750        4      2.75   2270  24237    1.0
    ## 504 8651510380  8/21/2014  310000        3      2.00   2070   9195    1.0
    ## 505 8682260470  6/19/2014  437000        2      1.75   1440   4225    1.0
    ## 506 8682261650  7/10/2014  554000        2      2.00   1670   4996    1.0
    ## 507 8691310070  6/18/2014  913000        4      2.50   3640  10576    2.0
    ## 508 8731800210  7/11/2014  235000        3      2.50   2350   9051    1.0
    ## 509 8731801190 12/23/2014  269000        3      2.25   1950   8661    1.0
    ## 510 8731951670   6/6/2014  270000        4      2.25   1900   8600    1.0
    ## 511 8731960540 12/15/2014  242000        4      2.50   1750  11400    2.0
    ## 512 8732130140 12/22/2014  285000        4      2.25   2150   8250    1.0
    ## 513 8805900430 12/29/2014 1150000        4      2.50   1940   4875    2.0
    ## 514 8819900270  5/20/2014  440000        2      1.75   1300   4000    2.0
    ## 515 8825900070  8/18/2014  705000        6      2.00   2570   4240    1.5
    ## 516 8835210130   8/8/2014  300000        2      1.50   1150   3927    2.0
    ## 517 8888000055 12/30/2014  530000        3      0.75    920  20412    1.0
    ## 518 8901001335  11/3/2014  637000        4      2.75   2850   7510    2.0
    ## 519 8917100020   6/6/2014 1150000        3      1.50   2170  16600    1.0
    ## 520 8923600185  8/29/2014  800000        3      2.50   2760   9471    1.0
    ## 521 8927600070  1/13/2015  630000        3      1.75   1540   6930    1.0
    ## 522 8929000090   7/2/2014  484998        4      2.50   1540   1870    2.0
    ## 523 8937500020  2/10/2015  325000        3      1.75   2420  14862    1.0
    ## 524 8937600080  1/26/2015  295000        3      1.75   1930  13350    1.0
    ## 525 9117000170   5/5/2015  268643        4      2.25   1810   9240    2.0
    ## 526 9141100070  1/26/2015  575000        5      2.50   1970  12375    1.0
    ## 527 9164100035  4/29/2015  655000        1      1.00   1660   5422    1.0
    ## 528 9197100263  8/19/2014  237000        3      1.75   2000  12208    1.0
    ## 529 9201300020  8/11/2014 1520000        3      2.25   2610   9409    1.0
    ## 530 9201300050  8/14/2014 1850000        5      2.25   2800   8442    2.0
    ## 531 9238450160  4/28/2015  389000        3      1.00   1280   9630    1.0
    ## 532 9238500190  12/2/2014  440000        4      2.25   2600  28600    1.0
    ## 533 9253900271   1/7/2015 3570000        5      4.50   4850  10584    2.0
    ## 534 9253900408   4/8/2015 1400000        3      2.75   3130  19530    1.0
    ## 535 9253900417  1/28/2015 1600000        3      2.50   2850  19593    1.5
    ## 536 9257900010  4/22/2015  499900        4      2.25   2360   7650    1.0
    ## 537 9262800002   7/8/2014  232000        3      1.50   1460  15000    1.0
    ## 538 9264920870 10/23/2014  300000        3      2.25   1730  10030    1.0
    ## 539 9265880040   5/9/2014  557000        4      2.50   2840   4500    2.0
    ## 540 9268850160   2/6/2015  293467        4      2.00   1590    942    3.0
    ## 541 9268851670  4/24/2015  645000        3      2.50   2170   1984    2.5
    ## 542 9285800345  6/26/2014  320000        2      1.00    950   5316    1.0
    ## 543 9297301055  12/9/2014  363000        2      1.00   1120   4800    1.0
    ## 544 9301300270  2/23/2015 1330000        3      3.00   3180   2758    2.0
    ## 545 9358000780  5/12/2015  275000        2      1.00    830   5610    1.0
    ## 546 9358002375   3/5/2015  420000        6      3.00   2290   6344    2.0
    ## 547 9362000040  6/23/2014 3400000        3      4.50   5230  17826    2.0
    ## 548 9406500530  9/12/2014  249000        2      2.00   1090   1357    2.0
    ## 549 9412400220  7/10/2014 1610000        4      2.75   5470  18200    2.0
    ## 550 9422400035  9/12/2014  477500        2      2.00   2090   6000    2.0
    ## 551 9432900560  2/12/2015  290000        3      2.50   2360   8764    2.0
    ## 552 9459200110  6/10/2014  315000        2      1.00   1740   3622    1.0
    ## 553 9465910190  5/29/2014  600000        3      1.75   2930  19876    1.0
    ## 554 9471201175   5/6/2014 1580000        4      3.25   3760  10920    1.5
    ## 555 9523102660  5/13/2014  560000        3      1.00   1440   5000    2.0
    ## 556 9536600010 12/23/2014  520000        4      0.75   1960   8277    1.0
    ## 557 9557300190  7/11/2014  440000        3      2.25   1900   7225    1.0
    ## 558 9808700762  6/11/2014 7060000        5      4.50  10040  37325    2.0
    ## 559 9828701741 10/21/2014  489000        2      2.75   1465    972    2.0
    ## 560 9828702895 10/22/2014  700000        4      1.75   2420    520    1.5
    ##     waterfront view condition above basement year zipcode living15  lot15
    ## 1           no    1     great   830      721 1942   98126     1330   6042
    ## 2           no    1   neutral  1980        1 1999   98055     1700  20580
    ## 3           no    1   neutral  3880        1 2006   98075     3280   7221
    ## 4           no    1      good  1950        1 1979   98023     1910   7280
    ## 5          yes    5     great  1159      876 1947   98070     1156  15961
    ## 6          yes    4   neutral  3130      481 1918   98023     2630  42126
    ## 7          yes    5      good  1120      341 1970   98023     1970  18893
    ## 8           no    1     great   980      901 1946   98178     1670  14350
    ## 9           no    1   neutral  3160        1 1995   98053     1890  24000
    ## 10          no    1   neutral  1400      641 1959   98133     1890   8580
    ## 11         yes    5   neutral  1550      541 1919   98070     1200 192268
    ## 12         yes    3     great   560      501 1947   98070      750  80201
    ## 13          no    1   neutral  2450        1 1994   98117     1060   5500
    ## 14          no    1   neutral  1330      761 1977   98072     2450  47178
    ## 15          no    1      good  1490        1 1967   98177     1640   7572
    ## 16          no    1   neutral   900        1 1998   98144      900   2728
    ## 17         yes    5     great  1580      601 1952   98146     2480  21503
    ## 18         yes    5      good  2160        1 1956   98146     2660  18530
    ## 19          no    1   neutral  2200        1 2005   98058     2310   5082
    ## 20          no    1   neutral  2790        1 1997   98074     2780   7779
    ## 21          no    1     great  1410        1 1960   98059     1610  24941
    ## 22         yes    5   neutral  4490        1 1993   98034     2710  51330
    ## 23          no    1   neutral  1610        1 1987   98052     1300   3823
    ## 24          no    3   neutral  1000      721 1954   98178     1690   6000
    ## 25          no    1   neutral   990        1 1977   98003     1560  11880
    ## 26          no    1      good  2100     1131 1964   98014     2760  62059
    ## 27          no    1   neutral  1480     1081 1959   98115     2600   7200
    ## 28          no    1   neutral  1980        1 1920   98103     1810   3245
    ## 29          no    1   neutral  1170        1 2003   98103     1170   1116
    ## 30         yes    5   neutral  2830      931 2014   98056     3560   5762
    ## 31         yes    5   neutral  3430      981 2014   98056     3550   5634
    ## 32          no    1   neutral  1720      831 2011   98056     2560   3828
    ## 33          no    4      good  2726      451 1985   98198     3470  12779
    ## 34          no    1      good  3200     1331 2003   98077     3430  83199
    ## 35          no    1   neutral  1370        1 1940   98117     1180   6694
    ## 36          no    1      good  1240      941 1977   98116     2000   5000
    ## 37         yes    4      good  2040        1 1919   98070     2040  35124
    ## 38         yes    5      good  5403        1 1976   98166     3980 104374
    ## 39         yes    5   neutral  2610      581 1956   98178     2240   8725
    ## 40         yes    5      good  2860     1271 1993   98075     2980   5575
    ## 41         yes    5      good  3500     3501 1991   98075     4913  14663
    ## 42         yes    5   neutral  3230     1001 1949   98033     2660  27295
    ## 43          no    1     great  1520     1101 1955   98133     1880   8960
    ## 44          no    1   neutral  4330        1 1997   98074     5110  26319
    ## 45          no    1   neutral  4340        1 1967   98004     2050   9100
    ## 46          no    1      good  1890        1 1967   98008     1920   7210
    ## 47          no    1   neutral  2550        1 2001   98027     1870  56628
    ## 48         yes    5      good  1230      581 1967   98075     2280   5070
    ## 49         yes    5   neutral  3000        1 2001   98075     3000  15959
    ## 50          no    1      good  1130      681 1954   98133     1810   8100
    ## 51          no    1   neutral  1370        1 1955   98004     1580   8671
    ## 52          no    1      good  1860        1 1955   98055     1560   8505
    ## 53          no    1   neutral  1200     1021 1966   98042     1300   9709
    ## 54         yes    5     great  1620     1131 1959   98074     2570   7009
    ## 55          no    1      good  1270      971 1914   98109     2240   4250
    ## 56          no    1   neutral  1180        1 1907   98109     1440   3400
    ## 57         yes    3     great  2165      589 1953   98070     2680  72513
    ## 58          no    1      good  2150        1 1955   98004     2370   9148
    ## 59         yes    5   neutral  2659     1817 1999   98033     4340   5250
    ## 60          no    1   neutral  2990        1 2000   98014     2590 218671
    ## 61          no    1   neutral  1650        1 2014   98105     1720   1960
    ## 62          no    1   neutral  1680     1201 1976   98058     2080   5586
    ## 63          no    1      good  2140        1 1983   98031     2310   8783
    ## 64          no    3   neutral   780      681 1946   98126     1560   4600
    ## 65          no    1   neutral  1120        1 1999   98103     1120   1087
    ## 66         yes    4   neutral  3280        1 1925   98199     3530  21074
    ## 67          no    1   neutral  2750        1 2014   98059     2310  11830
    ## 68          no    1   neutral  2448        1 2014   98030     2815   5446
    ## 69          no    1   neutral  2830        1 1993   98011     2970   9619
    ## 70          no    1   neutral   920        1 1919   98118     1510   5944
    ## 71          no    1   neutral   800      601 1953   98118     1620   6331
    ## 72          no    1      good  2340        1 1954   98112     3480   8526
    ## 73         yes    5   neutral  1510     1351 1948   98023     2720  59612
    ## 74          no    1   neutral  1480        1 2014   98107     1530   1321
    ## 75          no    1      good  2020      171 1956   98146     1920   8855
    ## 76         yes    5     great  2500        1 1942   98008     1880  18782
    ## 77          no    1   neutral   700      791 1913   98118     2180   4000
    ## 78          no    1   neutral  1920      921 1910   98118     1300   4900
    ## 79          no    1   neutral  3450      781 2008   98033     2650   8076
    ## 80         yes    5   neutral  5990     2021 1999   98033     3430  26788
    ## 81          no    3   neutral  2550        1 1981   98144     2030   3500
    ## 82          no    1   neutral  1010      501 1923   98103     1660   4080
    ## 83         yes    5   neutral  1934     1853 1978   98070     2850  33361
    ## 84          no    1      good  1080      501 1976   98092     1580   7470
    ## 85         yes    4     great  2260     1181 1931   98040     4230  18723
    ## 86          no    1   neutral  1430      421 1978   98052     2090  10599
    ## 87          no    1   neutral  1840        1 1989   98019     1490  10101
    ## 88          no    1     great  1630      601 1906   98112     2410   4800
    ## 89          no    3   neutral  2630     1331 1925   98112     3850   8640
    ## 90          no    1   neutral  1140      531 1929   98055     1010   4120
    ## 91          no    1   neutral  2360        1 1983   98058     2160   7733
    ## 92          no    1   neutral  2470        1 2002   98092     2100   5972
    ## 93          no    1   neutral   800        1 1958   98058     1090   9450
    ## 94          no    1      good  1050     1051 1968   98058     1600   8550
    ## 95          no    1   neutral  1250        1 1959   98058     1270   7615
    ## 96          no    1   neutral  2650        1 2014   98059     2650   5831
    ## 97          no    1   neutral  1120        1 1942   98168     1120   6794
    ## 98          no    1   neutral  1320        1 1997   98125     1430   2040
    ## 99         yes    5      good  2150     1341 1939   98125     2990  13104
    ## 100         no    4      good  1700      381 1940   98125     2560  10202
    ## 101         no    1      good  3070     1701 1973   98005     3530  38917
    ## 102         no    1   neutral  1020      721 1962   98168     1740   7573
    ## 103        yes    5   neutral  2170     1271 2012   98155     3170  11810
    ## 104         no    2   neutral  1150      681 1928   98040     3380  10854
    ## 105         no    1   neutral  3460      441 1967   98024     2460 217800
    ## 106         no    1     great  1270        1 1911   98117     1440   3090
    ## 107         no    3   neutral  2390        1 1920   98178     2850   6867
    ## 108         no    1   neutral   820      481 1945   98146     1250   6120
    ## 109        yes    5   neutral  4580        1 2004   98075     2710   4443
    ## 110        yes    4   neutral  1550        1 2006   98075     2710   3444
    ## 111        yes    4   neutral  2700        1 1990   98075     2710   3444
    ## 112        yes    4   neutral  1760      311 1988   98075     2740   3568
    ## 113         no    1   neutral  1640        1 1989   98014     2280 200811
    ## 114         no    1     great  1770        1 1972   98034     1780   6500
    ## 115         no    1   neutral  3430        1 1984   98033     3920  35230
    ## 116        yes    5      good  2320        1 1976   98070     1610  18919
    ## 117         no    1   neutral  1670      651 1968   98133     2070   9575
    ## 118        yes    5   neutral  3690        1 1989   98166     2310  26988
    ## 119         no    1   neutral  2340        1 1990   98042     2340  11188
    ## 120         no    1   neutral  1280        1 1991   98002     1450   7810
    ## 121        yes    5      good  1008        1 1943   98070     1132  24079
    ## 122         no    1      good  1450        1 1932   98030     1530  10125
    ## 123        yes    5   neutral  4820     4821 1983   98040     3270  10454
    ## 124        yes    5      good  2810        1 1974   98052     3120  14810
    ## 125        yes    5     great  2860     1781 1952   98052     3140  14200
    ## 126        yes    5   neutral  1730        1 1996   98074     2340  16994
    ## 127         no    1   neutral  2500     1601 1981   98072     3770  87821
    ## 128         no    1     great  1370        1 1927   98105     1560   3927
    ## 129         no    1   neutral  1910        1 1900   98103     1280   1310
    ## 130         no    1      good  1510      721 1974   98006     2010  10650
    ## 131         no    1   neutral  1540      511 1988   98074     1960   7110
    ## 132         no    1   neutral  2040        1 1903   98112     3150   8220
    ## 133         no    4      good  1800        1 1921   98102     2180   4620
    ## 134         no    1   neutral  1150        1 2008   98103     1360   1210
    ## 135         no    1   neutral  1370        1 1987   98198     1400   7500
    ## 136        yes    5   neutral  2460        1 1955   98198     2660  32625
    ## 137        yes    5      good  1300      461 1951   98198     2590  16630
    ## 138        yes    5   neutral  4050     1941 2002   98027     3330  14810
    ## 139         no    1      good  1440        1 1967   98004     3520  10802
    ## 140        yes    5   neutral  1830     1821 2000   98074     3120  18841
    ## 141        yes    5   neutral  2830        1 2006   98074     2830  38211
    ## 142         no    1     great   920      721 1921   98117     1300   4240
    ## 143         no    1   neutral  2380        1 1997   98030     2170   6097
    ## 144         no    1   neutral  2350        1 1999   98106     1120   4000
    ## 145         no    1   neutral  2070        1 1987   98023     1670   5013
    ## 146        yes    4      good  1770      201 1980   98070     2280  75396
    ## 147        yes    5     great   760        1 1936   98070     1230  14267
    ## 148         no    1   neutral   900      531 1925   98065     1430  40860
    ## 149         no    1      good  4320        1 1997   98004     4320  44584
    ## 150         no    1   neutral  1100      401 1933   98109     1980   4000
    ## 151         no    1      good  1470        1 1985   98007     1510   2055
    ## 152         no    1      good  1230        1 1918   98133     1190   6820
    ## 153         no    1   neutral  1850      671 2003   98038     2260   6967
    ## 154        yes    4      good  1830     1001 1962   98070     2120  21988
    ## 155         no    1   neutral  1360       51 2006   98125     1500   1200
    ## 156         no    1   neutral  2000     1011 2014   98103     1600   5160
    ## 157         no    1     great  2140        1 1959   98010     1720  99316
    ## 158         no    1      good  1180      811 1978   98003     2030   7210
    ## 159        yes    5   neutral  1494        1 1943   98070     1494  43583
    ## 160         no    1   neutral  1130      661 1978   98065     1610  16684
    ## 161        yes    5      good  4830        1 1990   98039     3820  25582
    ## 162         no    1   neutral  3370        1 2005   98004     2470   8113
    ## 163         no    1      good  1080      401 1979   98058     1850   7348
    ## 164         no    1     great  2100        1 1961   98040     2400  10713
    ## 165         no    1      good  3410        1 1967   98058     2260  10128
    ## 166         no    1      good   900      881 1976   98058     1550   8900
    ## 167         no    1     great  3440        1 1974   98058     2160   8400
    ## 168         no    1     great  1060      701 1977   98058     1680   9165
    ## 169         no    1      good  1280        1 1941   98024     1410  10500
    ## 170         no    1   neutral  1800        1 2006   98042     2580   4307
    ## 171         no    1   neutral  1060      801 1940   98136     1710   5100
    ## 172         no    4      good  2820      911 1986   98040     3880  15550
    ## 173         no    1   neutral  2980        1 2006   98052     3150   6593
    ## 174        yes    4   neutral  1840     1691 1930   98040     3530  17310
    ## 175         no    1   neutral  2350        1 2013   98029     2020   3532
    ## 176         no    3      good  3370        1 1980   98006     2690  10500
    ## 177         no    1   neutral  3230        1 2004   98033     3220   5083
    ## 178         no    1   neutral  2250        1 1980   98053     2350  37920
    ## 179        yes    2      good  1480      461 1955   98070     1910 127195
    ## 180        yes    5   neutral  1370      181 1930   98166     1790  22047
    ## 181        yes    5   neutral  2015        1 2007   98166     1780  12310
    ## 182        yes    5      good  1750      301 1930   98166     2300  12200
    ## 183        yes    5   neutral  3750        1 1925   98118     3540  24293
    ## 184         no    1      good  1230        1 1913   98118     1310   5000
    ## 185         no    3   neutral  2720        1 1986   98042     2290  15188
    ## 186         no    1   neutral  2680        1 2004   98038     3170 215186
    ## 187         no    1   neutral  1650        1 1992   98024     2520 221284
    ## 188         no    1   neutral  2990        1 2003   98074     2440   8711
    ## 189        yes    5   neutral  2610        1 2006   98125     2590  12958
    ## 190         no    1   neutral  2630        1 1991   98014     2630 210394
    ## 191         no    1   neutral  3310        1 2007   98072     3240   5280
    ## 192         no    1   neutral  1360        1 1945   98107     1960   2746
    ## 193         no    1   neutral  1040      241 2000   98107     1280   1249
    ## 194         no    1   neutral  1200      261 2008   98107     1280   1257
    ## 195         no    1   neutral  2820        1 2007   98038     1880   7200
    ## 196        yes    5   neutral  1060        1 1935   98070     2258  31762
    ## 197         no    1      good  1820      181 1926   98112     1950   3728
    ## 198         no    1   neutral  2860      881 2014   98117     1520   4590
    ## 199         no    1      good  1010        1 1972   98042     1230  10125
    ## 200         no    3   neutral  1400      381 1927   98103     1780   3750
    ## 201         no    1   neutral  1720        1 1999   98042     1740   4220
    ## 202         no    1   neutral  2900        1 2002   98074     2970   5251
    ## 203         no    1   neutral  1980        1 2009   98103     1480   3840
    ## 204        yes    4   neutral  1200        1 1972   98070     1670  47462
    ## 205        yes    5   neutral  3400     1871 1979   98027     3390   9905
    ## 206         no    1      good  1170        1 1957   98198     1440   9800
    ## 207         no    1      good  5180        1 1991   98053     4240  35363
    ## 208        yes    3      good  1730        1 1947   98070     1400  12410
    ## 209        yes    4     great  2110      911 1916   98040     3890  11489
    ## 210         no    1   neutral  1446      296 2007   98034     2090  10454
    ## 211         no    1      good  1320      501 1930   98133     1550   7628
    ## 212         no    1      good  1100      721 1955   98177     1700   8500
    ## 213         no    1   neutral  2900        1 1992   98053     2900  34589
    ## 214        yes    5      good  1940     1391 1960   98040     3730  16560
    ## 215        yes    5   neutral  2940     1791 1954   98033     3250  49346
    ## 216         no    1   neutral   860      861 1971   98034     1250   7210
    ## 217         no    1      good  2690        1 1986   98092     2570  35091
    ## 218        yes    5      good  1600     1191 1977   98198     2990  11476
    ## 219        yes    5   neutral  1400     1401 1965   98198     3270  31450
    ## 220        yes    5      good   720        1 1905   98198     1120   9736
    ## 221        yes    5   neutral  1560     1431 1976   98198     2870  11476
    ## 222         no    1      good   920        1 1923   98024     1530  11875
    ## 223        yes    5   neutral  2600     1951 2002   98074     4550  19508
    ## 224        yes    5   neutral  2290     1201 1968   98074     1780  15244
    ## 225         no    3      good  2590        1 1930   98199     2530   6380
    ## 226         no    1   neutral  1400        1 2007   98126     1540   2385
    ## 227         no    1   neutral   950       61 2007   98126     1360   1688
    ## 228         no    3     great   960      761 1941   98108     1760   5525
    ## 229         no    1      good  1370        1 1977   98092     2100  15260
    ## 230         no    1   neutral  3100        1 2011   98038     3060   6000
    ## 231         no    1   neutral  2301        1 2010   98001     2650   6054
    ## 232         no    1      good  1100        1 1967   98023     1100   9900
    ## 233         no    1   neutral  1050      301 2007   98144     1350   1245
    ## 234         no    1      good  1860        1 1947   98074     2000  44431
    ## 235         no    1      good  1190        1 1968   98092     1250   7800
    ## 236         no    1   neutral   860      351 1926   98118     1210   3348
    ## 237        yes    5   neutral  2890        1 1984   98056     2870  11151
    ## 238        yes    5     great  2120        1 1939   98006     2870   8944
    ## 239        yes    5      good  2690        1 1975   98006     2940   8890
    ## 240        yes    5      good  2370      421 1933   98006     3140  11949
    ## 241         no    3   neutral  1490        1 1983   98092     2350 213879
    ## 242         no    1   neutral  1270        1 1955   98188     1660  11536
    ## 243         no    1   neutral   970        1 1957   98188     1000   8886
    ## 244        yes    5   neutral  2720     1391 2001   98115     2640  15929
    ## 245         no    1   neutral  1130        1 1945   98155     1150   6908
    ## 246         no    1      good   570      301 1942   98106     1730   7020
    ## 247        yes    4      good   440        1 1943   98070      880  26289
    ## 248         no    1   neutral  2110        1 1988   98029     2050   7000
    ## 249         no    1      good  1300     1001 1979   98034     2120   9524
    ## 250         no    1   neutral  2940      711 2014   98028     2400   7682
    ## 251         no    3   neutral  3250        1 1973   98092     1600  44287
    ## 252        yes    5      good  1560        1 1928   98040     2890  16710
    ## 253        yes    5      good  2220     1191 1973   98040     3760  16572
    ## 254        yes    5      good  4440     1761 1989   98008     2940  13525
    ## 255        yes    5      good  2460     1761 1958   98008     3810  30401
    ## 256         no    1   neutral  3010        1 2005   98029     2520   5001
    ## 257         no    1   neutral  1540        1 2005   98029     1540   1827
    ## 258         no    1   neutral  1500        1 2005   98029     1440   2170
    ## 259         no    1      good  1710        1 1958   98028     1890  10800
    ## 260         no    1   neutral   770        1 1978   98001     1480  17334
    ## 261         no    4      good  1780        1 1954   98001     1780  26723
    ## 262        yes    5      good  2330     1601 1957   98034     2730  26324
    ## 263        yes    5     great  3850     1001 1989   98034     3350  12210
    ## 264        yes    5     great  1800     1181 1928   98034     2340  19810
    ## 265        yes    5   neutral  3470        1 1989   98034     4130  11875
    ## 266         no    1   neutral  1990        1 1994   98030     1850   6031
    ## 267         no    1     great  1150        1 1906   98103     1460   3200
    ## 268         no    1      good  2150        1 1952   98028     1760  10283
    ## 269         no    1   neutral  1510        1 1948   98028     1520  10037
    ## 270         no    1   neutral  1540      431 1977   98028     1900  10358
    ## 271         no    1   neutral  1480      961 1962   98028     2510   9602
    ## 272         no    1   neutral  1760        1 1951   98031     1760   8276
    ## 273         no    1   neutral  2840        1 2001   98034     1934   9605
    ## 274         no    1   neutral  2580        1 2013   98033     1700   5772
    ## 275         no    1   neutral  4460        1 1992   98033     3770  26027
    ## 276         no    1   neutral  1910        1 1988   98029     1940   4647
    ## 277         no    1   neutral  1930        1 1990   98029     1770   4500
    ## 278         no    1   neutral  1880        1 1993   98029     2130   5114
    ## 279         no    1      good  1580        1 1994   98029     1770   4187
    ## 280         no    1   neutral  1110     1001 1966   98118     1590   5250
    ## 281         no    1   neutral   910      911 1950   98115     1730   6050
    ## 282         no    1   neutral  1850     1071 1961   98028     1540   8482
    ## 283        yes    5   neutral  4320        1 1938   98155     1920   7750
    ## 284        yes    5   neutral  3520     1651 1998   98155     3020  12880
    ## 285         no    1      good  1150        1 1962   98001     1200   9812
    ## 286         no    1      good  1870      751 1956   98006     2630  16792
    ## 287         no    1      good  1440        1 1957   98008     1340   8780
    ## 288         no    1      good  1910        1 1957   98008     2040   8700
    ## 289         no    1      good  1510        1 1962   98008     1770   8250
    ## 290         no    1   neutral  1980      471 1989   98014     1790  15323
    ## 291         no    1   neutral  1570        1 2001   98014     1640  15000
    ## 292         no    1      good  1180      571 1979   98042     1570   7490
    ## 293         no    1   neutral  3410        1 1990   98042     2150  21429
    ## 294         no    1   neutral  1000        1 1968   98042     2450  25927
    ## 295        yes    5      good  1560     1561 1963   98034     2630  13868
    ## 296         no    3      good  1320      501 1952   98178     1820   6825
    ## 297         no    2   neutral  1280      941 1950   98125     2210   7230
    ## 298         no    1      good  1050      731 1951   98125     1590   8100
    ## 299         no    1   neutral  2080        1 1960   98005     2240  10696
    ## 300         no    1   neutral   920        1 1909   98118     1700   5150
    ## 301        yes    5   neutral  3420     1601 1941   98144     3680  12620
    ## 302        yes    5   neutral  2840      181 1942   98144     3680  12620
    ## 303         no    1   neutral  1940        1 1990   98038     1840   6912
    ## 304         no    1   neutral  2620        1 1991   98038     2190   7842
    ## 305        yes    5      good  3920        1 1996   98040     3450  13287
    ## 306        yes    5   neutral  2070     2051 1965   98006     3620  19729
    ## 307         no    1   neutral  2620        1 1992   98006     2850   8809
    ## 308        yes    5   neutral  3230        1 1925   98023     2560  72229
    ## 309         no    1      good   910      911 1970   98108     1180   5628
    ## 310         no    1      good  1410        1 1990   98042     1750   7150
    ## 311         no    1     great  1910     1011 1909   98105     2470   4000
    ## 312        yes    5     great  3910     1571 1936   98105     3510  15810
    ## 313         no    1   neutral  2520        1 1990   98031     2540   9793
    ## 314         no    1   neutral  2110        1 2000   98059     1650   9900
    ## 315         no    1   neutral  1170      791 2001   98106     1960   5160
    ## 316         no    1      good  1640        1 1985   98007     1830   8372
    ## 317         no    3   neutral  3390        1 2003   98056     2580   6900
    ## 318         no    1   neutral   940        1 1910   98136     1330   8740
    ## 319         no    1   neutral  1150        1 1956   98166     1220  13066
    ## 320         no    1      good  1130        1 1944   98166     1270   7500
    ## 321         no    1   neutral   890        1 1910   98118     1230   6057
    ## 322         no    1      good  2750        1 1973   98052     2040   9563
    ## 323         no    1   neutral  1030      571 1941   98112     2150   4000
    ## 324         no    1   neutral  2780        1 1999   98007     3110   5717
    ## 325         no    1      good  1750        1 1976   98002     2140  11276
    ## 326         no    1   neutral  1690        1 1973   98040     2260   9945
    ## 327         no    2   neutral  1130      901 1975   98166     2040  12425
    ## 328         no    1      good   880        1 1950   98116     1240   6731
    ## 329         no    1      good   730      551 1929   98116     2250   5382
    ## 330         no    2   neutral  1800     1451 1966   98115     3490  10320
    ## 331         no    1   neutral  1830        1 2004   98038     2080   5100
    ## 332         no    5   neutral  3750        1 2005   98038     3120   7255
    ## 333        yes    5      good  1650     1401 1962   98198     1170  70973
    ## 334         no    1   neutral  1150        1 1970   98034     1510   7486
    ## 335         no    1   neutral   790      771 1969   98034     1410   7428
    ## 336         no    3      good  2350     2061 1965   98003     2600  13988
    ## 337        yes    5   neutral  2490        1 2000   98116     2580  12119
    ## 338        yes    5      good  1950     1571 1951   98136     2450   7000
    ## 339        yes    4      good  2480        1 1974   98070     2350  42122
    ## 340        yes    4      good  2960        1 1990   98070     2350  41433
    ## 341        yes    4      good   830        1 1928   98070     2030  27338
    ## 342         no    1     great  1280      821 1959   98133     2080   9000
    ## 343         no    1   neutral  1160        1 1947   98125     1120   6948
    ## 344         no    1   neutral  1010      141 1947   98126     1340   1203
    ## 345         no    1   neutral  1160      601 1986   98188     1290   9587
    ## 346         no    1   neutral  2960        1 2005   98038     2750   4623
    ## 347         no    1   neutral  1930        1 1968   98125     2280   7616
    ## 348         no    1   neutral  1130      461 1984   98031     1500   7560
    ## 349         no    1      good  1280      801 1985   98030     1200   7500
    ## 350         no    1   neutral  1770        1 1989   98023     1780   6666
    ## 351         no    1   neutral  1480      941 1979   98027     2370   7699
    ## 352         no    1   neutral  1580        1 1979   98022     1560   8314
    ## 353        yes    5   neutral  3831        1 1959   98040     3850  36563
    ## 354         no    1      good  2180        1 1966   98040     2560  10244
    ## 355         no    1      good  2020        1 1961   98040     3050  15594
    ## 356         no    1      good  1010      481 1975   98042     1490   7469
    ## 357         no    1   neutral   880      441 1961   98023     1320   8450
    ## 358         no    1   neutral  1770      871 1977   98027     2500  35580
    ## 359         no    1   neutral  1530     1201 1989   98027     3180  37970
    ## 360         no    1   neutral  3210        1 1995   98074     3280  14910
    ## 361         no    1   neutral  4500        1 1991   98027     3930  11576
    ## 362         no    1   neutral  1670        1 1955   98028     1860  12056
    ## 363         no    1   neutral  1400        1 1984   98028     2180   9248
    ## 364         no    1   neutral  1160        1 1960   98028     1670   9380
    ## 365         no    1      good  1370      151 1960   98027     1840  12500
    ## 366         no    1      good  1370      481 1973   98008     1880   7580
    ## 367         no    1   neutral  1350        1 1930   98117     1520   4000
    ## 368         no    1      good   940        1 1937   98117     1560   4500
    ## 369         no    4   neutral  2780     1591 1996   98022     2860  39356
    ## 370         no    1     great  1300        1 1933   98168     1480   9670
    ## 371        yes    5   neutral  1810        1 1913   98166     2180  24911
    ## 372         no    1      good  2150        1 1950   98166     2150  10125
    ## 373        yes    5   neutral  1590     1091 1956   98166     2960  19964
    ## 374        yes    5      good  2610        1 1952   98166     2810  22672
    ## 375        yes    5   neutral  4660        1 1975   98166     3140  24274
    ## 376         no    1      good  1300        1 1969   98001     2120   9920
    ## 377         no    1   neutral   980        1 1946   98058     1140   9975
    ## 378        yes    4   neutral  1000        1 1950   98146     1090   6536
    ## 379        yes    5   neutral  2150      451 1912   98146     1700  14700
    ## 380        yes    5   neutral  1420     1101 1960   98146     1700   8100
    ## 381         no    1   neutral   920        1 1939   98146     2330   9792
    ## 382         no    1   neutral  3280        1 2001   98074     3110   7570
    ## 383         no    1      good  1110      781 1947   98125     1560   6356
    ## 384         no    1      good  1670        1 1961   98117     2130   7035
    ## 385         no    1   neutral  1360        1 1917   98115     1710   6000
    ## 386         no    1   neutral  1520        1 1954   98125     1360   7219
    ## 387         no    1   neutral  1420        1 1905   98103     1420   4590
    ## 388         no    1   neutral  1490      821 1927   98103     1580   3060
    ## 389         no    1      good  1340        1 1961   98033     1340  10050
    ## 390        yes    4     great  3030      861 1923   98105     4140  19281
    ## 391         no    1   neutral  1200        1 1984   98031     2150   4339
    ## 392         no    1   neutral  1520        1 2006   98125     1520   1497
    ## 393         no    1   neutral  4500        1 2007   98059     2840   8402
    ## 394         no    1   neutral  1610      831 1978   98032     2180   7680
    ## 395         no    1   neutral   860      421 1926   98117     1490   4680
    ## 396        yes    5   neutral  2370     1461 1981   98155     3080  12297
    ## 397         no    1      good  1270        1 1957   98007     1470   8000
    ## 398         no    1   neutral  2070        1 1996   98031     2130   8100
    ## 399         no    1   neutral  1240      331 2007   98199     1670   1596
    ## 400        yes    5      good   840        1 1937   98070     1850  64069
    ## 401         no    1      good  1940        1 2000   98199     1410   4000
    ## 402         no    1   neutral  1150      881 1973   98034     1770   8071
    ## 403        yes    5   neutral  2520        1 1954   98146     2790  26809
    ## 404        yes    5   neutral  2340        1 1959   98118     2200   3230
    ## 405        yes    5   neutral  3040        1 1966   98118     2620   2433
    ## 406         no    1   neutral  4140        1 2000   98059     4140  11529
    ## 407         no    1      good  1200      611 1980   98030     1750   7350
    ## 408         no    1   neutral  1870        1 1956   98188     1350   8714
    ## 409         no    1   neutral  4290        1 2004   98010     3370  63162
    ## 410         no    1   neutral  1140      581 1977   98052     1700   8400
    ## 411         no    1   neutral  4115        1 2014   98053     3950   6765
    ## 412        yes    5      good  1990        1 1926   98198     2100   6221
    ## 413        yes    5      good  2590      531 1974   98198     2030  32181
    ## 414         no    1   neutral  1390      401 1974   98003     1810   7420
    ## 415         no    1   neutral  2450        1 1976   98077     2450  41181
    ## 416         no    1   neutral  2650        1 1984   98077     2560  49044
    ## 417         no    1   neutral  2000        1 1989   98042     2390  36868
    ## 418         no    1   neutral  2210        1 1999   98075     2460   8069
    ## 419         no    1   neutral  1650        1 2000   98075     1650   4559
    ## 420        yes    5     great  1940      971 1985   98177     2540  23598
    ## 421         no    3   neutral  3140     1171 1988   98177     2280  11106
    ## 422         no    1      good  1190        1 1959   98028     1850  10500
    ## 423         no    1   neutral  2550        1 2001   98053     2190  49222
    ## 424         no    1   neutral  1150        1 1971   98045     1640  11475
    ## 425         no    1   neutral  1600        1 1999   98002     2100   7711
    ## 426        yes    5   neutral  2300        1 1934   98125     2500  13342
    ## 427        yes    5   neutral  1760        1 1940   98125     1630   7588
    ## 428        yes    5      good  1040     1041 1939   98125     2030   8408
    ## 429        yes    5   neutral  2120      501 1931   98125     1830   8279
    ## 430        yes    5      good  3480      101 1915   98125     2410   6389
    ## 431        yes    5   neutral  1920     1451 1980   98028     3410  19688
    ## 432         no    1   neutral  1400        1 1969   98155     1610  10500
    ## 433         no    1   neutral  1440      801 1979   98125     1390   6450
    ## 434         no    1      good   710        1 1950   98126      900   5050
    ## 435         no    1      good  1400        1 1945   98117     1400   4690
    ## 436         no    1     great  1010      831 1951   98117     1760   5510
    ## 437        yes    5   neutral  4350     2491 2001   98008     3120  12300
    ## 438        yes    5      good  5000     2391 1991   98040     4320  24619
    ## 439        yes    5      good  1550        1 1965   98070     1690  11620
    ## 440        yes    5      good  1400     1241 1954   98178     2400  11942
    ## 441        yes    5      good  1150        1 1908   98178     2440  11852
    ## 442         no    1     great  2920        1 1995   98077     3420  37206
    ## 443         no    1      good  3360        1 1994   98077     3150  36823
    ## 444         no    1      good  1620        1 1988   98003     1710   6455
    ## 445         no    1   neutral  1680      711 1988   98003     1820  10362
    ## 446         no    1      good  1140      931 1948   98117     1770   5386
    ## 447         no    2     great  1090      891 1910   98136     1980   6250
    ## 448        yes    3     great  2180      801 1917   98166     1890  12514
    ## 449        yes    5   neutral  2720        1 1990   98166     1870  12396
    ## 450        yes    5   neutral  5480        1 1991   98166     2500  22954
    ## 451        yes    5   neutral  1580      401 1984   98166     2060  17313
    ## 452        yes    4   neutral  3720      661 1968   98166     2460  18512
    ## 453        yes    5   neutral  1770        1 1957   98155     2410  10476
    ## 454         no    1   neutral  1290      671 1965   98003     1890   8700
    ## 455         no    1      good  1450        1 1961   98198     1270   8000
    ## 456         no    1   neutral  2830        1 2001   98028     2500   5802
    ## 457         no    1   neutral  1250      621 1984   98074     1550   9100
    ## 458         no    1      good  1620        1 1983   98074     1480   8120
    ## 459        yes    5   neutral  6350      291 2004   98155     3030  23408
    ## 460        yes    5      good  1580      911 1961   98146     2800  24121
    ## 461        yes    5   neutral  1940      791 1963   98146     2630  28188
    ## 462         no    1   neutral  2980        1 2003   98023     2610   5495
    ## 463         no    1   neutral   860        1 1944   98178     1150   6857
    ## 464         no    1   neutral  2410        1 2000   98065     2410   4025
    ## 465         no    1   neutral  2270        1 2003   98065     2270   5731
    ## 466         no    3   neutral  3350        1 2004   98065     3870   7886
    ## 467         no    1   neutral  2880        1 2006   98065     2570   5234
    ## 468         no    1   neutral  1960        1 2006   98065     2320   5178
    ## 469         no    1   neutral  3240        1 2007   98065     3240   6036
    ## 470         no    5     great  2520     1381 1972   98006     3410   9450
    ## 471         no    1      good  1130        1 1908   98108     1240   6000
    ## 472         no    1      good  1864        1 1958   98002      990   8000
    ## 473         no    1   neutral  1060      531 1956   98146     1410   6240
    ## 474         no    1      good  1110     1111 1953   98116     1600   6350
    ## 475        yes    5     great  2370     1251 1977   98136     2550  34713
    ## 476        yes    5   neutral  1100      871 1937   98136     3030  34689
    ## 477        yes    5   neutral  2130        1 1955   98136     3030  28598
    ## 478         no    1   neutral  1370        1 1986   98034     1380   5444
    ## 479         no    1   neutral  1370        1 1988   98034     1630   4400
    ## 480         no    1   neutral  2800        1 1903   98122     1780   5120
    ## 481         no    1   neutral   860      141 1946   98115     1000   5100
    ## 482         no    1   neutral  1900        1 1963   98155     1710  11407
    ## 483         no    1   neutral  2100        1 1990   98023     2330   8119
    ## 484        yes    5   neutral  3940      421 2007   98008     2670   8525
    ## 485        yes    5   neutral  3150     1271 2001   98008     2760   8525
    ## 486        yes    5   neutral  1410     1051 1961   98178     2300  10061
    ## 487        yes    5   neutral  1100      801 1954   98178     1650  13160
    ## 488        yes    5   neutral  1150      801 1957   98178     1600  13122
    ## 489        yes    5   neutral  1160        1 1945   98178     1510  13122
    ## 490        yes    5   neutral  3190        1 1999   98178     2290  13496
    ## 491        yes    5   neutral  1490      801 1948   98178     2290   9379
    ## 492         no    1   neutral  2160        1 2010   98038     2160   5555
    ## 493        yes    5   neutral  1230      221 1915   98166     2030  13193
    ## 494        yes    5      good  5770        1 1980   98040     4620  22748
    ## 495         no    1   neutral  2030        1 1983   98052     1960  10282
    ## 496         no    1   neutral  1470      921 1979   98052     2110  12000
    ## 497         no    1   neutral  2490        1 1993   98014     2450  21937
    ## 498         no    1   neutral  1410      281 2014   98106     1740   1664
    ## 499        yes    5     great  1288      705 1903   98070     1110   8328
    ## 500         no    1      good  2030        1 1975   98008     2450   8104
    ## 501         no    1   neutral  2420        1 1911   98045     1210   6769
    ## 502         no    1      good  1270        1 1960   98198     1600   8470
    ## 503         no    1      good  1360      911 1977   98058     2050   8016
    ## 504         no    1   neutral  1220      851 1982   98074     2080   9551
    ## 505         no    1   neutral  1440        1 2005   98053     1680   6200
    ## 506         no    1   neutral  1670        1 2004   98053     1670   4996
    ## 507         no    1   neutral  3640        1 1999   98075     3370  10351
    ## 508         no    1      good  1570      781 1966   98023     2270   8748
    ## 509         no    1      good  1950        1 1966   98023     1950   8800
    ## 510         no    1      good  1900        1 1975   98023     2120   8000
    ## 511         no    1      good  1750        1 1975   98023     1890   9024
    ## 512         no    1      good  1240      911 1978   98023     2050   7875
    ## 513         no    1      good  1940        1 1925   98112     1790   4875
    ## 514         no    1   neutral  1300        1 1948   98105     1350   4013
    ## 515         no    1      good  1970      601 1911   98115     2030   4240
    ## 516         no    1   neutral  1150        1 1982   98034     1400   3425
    ## 517        yes    3     great   920        1 1950   98070     1162  54705
    ## 518         no    1   neutral  2850        1 2008   98125     1510   8833
    ## 519        yes    3   neutral  1130     1041 1979   98052     3130  13875
    ## 520         no    3   neutral  1760     1001 1956   98115     3040   6765
    ## 521         no    1   neutral  1250      291 1944   98115     1760   6930
    ## 522         no    1   neutral  1540        1 2014   98029     1540   1619
    ## 523         no    1   neutral  1380     1041 1977   98023     2550  14675
    ## 524         no    1   neutral  1930        1 1967   98023     2270  13350
    ## 525         no    1   neutral  1810        1 1961   98055     1660   9240
    ## 526         no    1   neutral  1570      401 1959   98133     1970   8941
    ## 527         no    1      good   830      831 1908   98117     1100   5356
    ## 528         no    1   neutral  1140      861 1979   98032     1060   8194
    ## 529        yes    5      good  2610        1 1963   98075     2970   9156
    ## 530        yes    5   neutral  2800        1 1963   98075     3220   9156
    ## 531         no    1   neutral  1280        1 1968   98072     1300   9453
    ## 532         no    1   neutral  1810      791 1968   98072     2580  26950
    ## 533        yes    5   neutral  3540     1311 2007   98008     3470  18270
    ## 534        yes    5   neutral  1690     1441 1947   98008     2980  18782
    ## 535        yes    5   neutral  1790     1061 1978   98008     2850  18782
    ## 536         no    1   neutral  1640      721 1963   98155     2320  11060
    ## 537         no    1   neutral  1460        1 1966   98001     1510  15000
    ## 538         no    1      good  1730        1 1985   98023     2090   8823
    ## 539         no    1   neutral  2840        1 2012   98028     2840   4939
    ## 540         no    1   neutral  1590        1 2008   98027     1390    942
    ## 541         no    1   neutral  2170        1 2008   98027     2150   1984
    ## 542         no    3   neutral   950        1 1948   98126     1620   6085
    ## 543         no    1   neutral   770      351 1926   98126     1510   4800
    ## 544         no    3   neutral  2240      941 2008   98109     2420   2758
    ## 545         no    1   neutral   830        1 1922   98126     1310   2793
    ## 546         no    1   neutral  2290        1 1980   98126     1360   3202
    ## 547        yes    5   neutral  3740     1491 2005   98040     3670  17826
    ## 548         no    1   neutral  1090        1 1990   98028     1078   1318
    ## 549        yes    5   neutral  3730     1741 1992   98118     3620  15100
    ## 550         no    2   neutral  2090        1 1918   98116     1600   5400
    ## 551         no    1   neutral  2360        1 1991   98022     2360   8746
    ## 552         no    1      good   950      791 1924   98118     1270   3800
    ## 553         no    1   neutral  2030      901 1993   98072     2740  11499
    ## 554         no    1     great  2400     1361 1950   98105     3430  11050
    ## 555         no    1   neutral  1440        1 1910   98103     1850   4500
    ## 556        yes    5      good  1320      641 1923   98198     1940   8402
    ## 557         no    1   neutral  1220      681 1970   98008     1900   7399
    ## 558        yes    3   neutral  7680     2361 1940   98004     3930  25449
    ## 559         no    1   neutral  1050      416 2006   98112     1480   1430
    ## 560         no    1   neutral  2420        1 1900   98112     1200   1170
    ##     renovated      zipcodeArea
    ## 1          no Downtown Seattle
    ## 2          no            Other
    ## 3          no            Other
    ## 4          no            Other
    ## 5          no            Other
    ## 6          no            Other
    ## 7          no            Other
    ## 8          no Downtown Seattle
    ## 9          no            Other
    ## 10         no Downtown Seattle
    ## 11         no            Other
    ## 12         no            Other
    ## 13         no Downtown Seattle
    ## 14         no            Other
    ## 15         no Downtown Seattle
    ## 16         no Downtown Seattle
    ## 17         no Downtown Seattle
    ## 18         no Downtown Seattle
    ## 19         no            Other
    ## 20         no            Other
    ## 21         no            Other
    ## 22         no            Other
    ## 23         no            Other
    ## 24         no Downtown Seattle
    ## 25         no            Other
    ## 26         no            Other
    ## 27         no Downtown Seattle
    ## 28         no Downtown Seattle
    ## 29         no Downtown Seattle
    ## 30         no            Other
    ## 31         no            Other
    ## 32         no            Other
    ## 33         no            Other
    ## 34         no            Other
    ## 35        yes Downtown Seattle
    ## 36         no Downtown Seattle
    ## 37         no            Other
    ## 38         no            Other
    ## 39         no Downtown Seattle
    ## 40         no            Other
    ## 41         no            Other
    ## 42         no            Other
    ## 43         no Downtown Seattle
    ## 44         no            Other
    ## 45         no            Other
    ## 46         no            Other
    ## 47         no            Other
    ## 48         no            Other
    ## 49         no            Other
    ## 50         no Downtown Seattle
    ## 51         no            Other
    ## 52         no            Other
    ## 53         no            Other
    ## 54         no            Other
    ## 55         no Downtown Seattle
    ## 56         no Downtown Seattle
    ## 57         no            Other
    ## 58         no            Other
    ## 59         no            Other
    ## 60         no            Other
    ## 61         no Downtown Seattle
    ## 62        yes            Other
    ## 63         no            Other
    ## 64         no Downtown Seattle
    ## 65         no Downtown Seattle
    ## 66         no Downtown Seattle
    ## 67         no            Other
    ## 68         no            Other
    ## 69         no            Other
    ## 70        yes Downtown Seattle
    ## 71         no Downtown Seattle
    ## 72         no Downtown Seattle
    ## 73        yes            Other
    ## 74         no Downtown Seattle
    ## 75         no Downtown Seattle
    ## 76         no            Other
    ## 77         no Downtown Seattle
    ## 78        yes Downtown Seattle
    ## 79         no            Other
    ## 80         no            Other
    ## 81        yes Downtown Seattle
    ## 82         no Downtown Seattle
    ## 83        yes            Other
    ## 84         no            Other
    ## 85         no            Other
    ## 86         no            Other
    ## 87         no            Other
    ## 88         no Downtown Seattle
    ## 89         no Downtown Seattle
    ## 90        yes            Other
    ## 91         no            Other
    ## 92         no            Other
    ## 93         no            Other
    ## 94         no            Other
    ## 95         no            Other
    ## 96         no            Other
    ## 97         no Downtown Seattle
    ## 98         no Downtown Seattle
    ## 99         no Downtown Seattle
    ## 100        no Downtown Seattle
    ## 101        no            Other
    ## 102        no Downtown Seattle
    ## 103        no Downtown Seattle
    ## 104        no            Other
    ## 105       yes            Other
    ## 106        no Downtown Seattle
    ## 107        no Downtown Seattle
    ## 108        no Downtown Seattle
    ## 109        no            Other
    ## 110        no            Other
    ## 111        no            Other
    ## 112        no            Other
    ## 113        no            Other
    ## 114        no            Other
    ## 115        no            Other
    ## 116        no            Other
    ## 117        no Downtown Seattle
    ## 118        no            Other
    ## 119        no            Other
    ## 120        no            Other
    ## 121       yes            Other
    ## 122        no            Other
    ## 123       yes            Other
    ## 124        no            Other
    ## 125        no            Other
    ## 126        no            Other
    ## 127        no            Other
    ## 128        no Downtown Seattle
    ## 129        no Downtown Seattle
    ## 130        no            Other
    ## 131        no            Other
    ## 132        no Downtown Seattle
    ## 133        no Downtown Seattle
    ## 134        no Downtown Seattle
    ## 135        no            Other
    ## 136        no            Other
    ## 137        no            Other
    ## 138        no            Other
    ## 139        no            Other
    ## 140        no            Other
    ## 141        no            Other
    ## 142        no Downtown Seattle
    ## 143        no            Other
    ## 144        no Downtown Seattle
    ## 145        no            Other
    ## 146        no            Other
    ## 147        no            Other
    ## 148        no            Other
    ## 149        no            Other
    ## 150        no Downtown Seattle
    ## 151        no            Other
    ## 152        no Downtown Seattle
    ## 153        no            Other
    ## 154        no            Other
    ## 155        no Downtown Seattle
    ## 156        no Downtown Seattle
    ## 157        no            Other
    ## 158        no            Other
    ## 159       yes            Other
    ## 160        no            Other
    ## 161        no            Other
    ## 162        no            Other
    ## 163        no            Other
    ## 164        no            Other
    ## 165        no            Other
    ## 166        no            Other
    ## 167        no            Other
    ## 168        no            Other
    ## 169        no            Other
    ## 170        no            Other
    ## 171        no Downtown Seattle
    ## 172        no            Other
    ## 173        no            Other
    ## 174        no            Other
    ## 175        no            Other
    ## 176        no            Other
    ## 177        no            Other
    ## 178        no            Other
    ## 179        no            Other
    ## 180        no            Other
    ## 181        no            Other
    ## 182        no            Other
    ## 183        no Downtown Seattle
    ## 184        no Downtown Seattle
    ## 185        no            Other
    ## 186        no            Other
    ## 187        no            Other
    ## 188        no            Other
    ## 189        no Downtown Seattle
    ## 190        no            Other
    ## 191        no            Other
    ## 192       yes Downtown Seattle
    ## 193        no Downtown Seattle
    ## 194        no Downtown Seattle
    ## 195        no            Other
    ## 196        no            Other
    ## 197        no Downtown Seattle
    ## 198        no Downtown Seattle
    ## 199        no            Other
    ## 200       yes Downtown Seattle
    ## 201        no            Other
    ## 202        no            Other
    ## 203        no Downtown Seattle
    ## 204        no            Other
    ## 205        no            Other
    ## 206        no            Other
    ## 207        no            Other
    ## 208        no            Other
    ## 209        no            Other
    ## 210        no            Other
    ## 211        no Downtown Seattle
    ## 212        no Downtown Seattle
    ## 213        no            Other
    ## 214        no            Other
    ## 215        no            Other
    ## 216        no            Other
    ## 217        no            Other
    ## 218        no            Other
    ## 219        no            Other
    ## 220        no            Other
    ## 221        no            Other
    ## 222        no            Other
    ## 223        no            Other
    ## 224        no            Other
    ## 225        no Downtown Seattle
    ## 226        no Downtown Seattle
    ## 227        no Downtown Seattle
    ## 228        no Downtown Seattle
    ## 229        no            Other
    ## 230        no            Other
    ## 231        no            Other
    ## 232        no            Other
    ## 233        no Downtown Seattle
    ## 234        no            Other
    ## 235        no            Other
    ## 236        no Downtown Seattle
    ## 237        no            Other
    ## 238        no            Other
    ## 239        no            Other
    ## 240        no            Other
    ## 241        no            Other
    ## 242        no            Other
    ## 243        no            Other
    ## 244        no Downtown Seattle
    ## 245        no Downtown Seattle
    ## 246        no Downtown Seattle
    ## 247        no            Other
    ## 248        no            Other
    ## 249        no            Other
    ## 250        no            Other
    ## 251        no            Other
    ## 252        no            Other
    ## 253        no            Other
    ## 254        no            Other
    ## 255        no            Other
    ## 256        no            Other
    ## 257        no            Other
    ## 258        no            Other
    ## 259        no            Other
    ## 260        no            Other
    ## 261        no            Other
    ## 262        no            Other
    ## 263        no            Other
    ## 264        no            Other
    ## 265       yes            Other
    ## 266        no            Other
    ## 267        no Downtown Seattle
    ## 268        no            Other
    ## 269        no            Other
    ## 270        no            Other
    ## 271        no            Other
    ## 272        no            Other
    ## 273        no            Other
    ## 274        no            Other
    ## 275        no            Other
    ## 276        no            Other
    ## 277        no            Other
    ## 278        no            Other
    ## 279        no            Other
    ## 280        no Downtown Seattle
    ## 281        no Downtown Seattle
    ## 282        no            Other
    ## 283        no Downtown Seattle
    ## 284        no Downtown Seattle
    ## 285        no            Other
    ## 286        no            Other
    ## 287        no            Other
    ## 288        no            Other
    ## 289        no            Other
    ## 290        no            Other
    ## 291        no            Other
    ## 292        no            Other
    ## 293        no            Other
    ## 294       yes            Other
    ## 295        no            Other
    ## 296        no Downtown Seattle
    ## 297        no Downtown Seattle
    ## 298        no Downtown Seattle
    ## 299        no            Other
    ## 300        no Downtown Seattle
    ## 301       yes Downtown Seattle
    ## 302        no Downtown Seattle
    ## 303        no            Other
    ## 304        no            Other
    ## 305        no            Other
    ## 306        no            Other
    ## 307        no            Other
    ## 308        no            Other
    ## 309        no Downtown Seattle
    ## 310        no            Other
    ## 311        no Downtown Seattle
    ## 312        no Downtown Seattle
    ## 313        no            Other
    ## 314        no            Other
    ## 315        no Downtown Seattle
    ## 316        no            Other
    ## 317        no            Other
    ## 318        no Downtown Seattle
    ## 319        no            Other
    ## 320        no            Other
    ## 321        no Downtown Seattle
    ## 322        no            Other
    ## 323        no Downtown Seattle
    ## 324        no            Other
    ## 325        no            Other
    ## 326       yes            Other
    ## 327        no            Other
    ## 328        no Downtown Seattle
    ## 329        no Downtown Seattle
    ## 330        no Downtown Seattle
    ## 331        no            Other
    ## 332        no            Other
    ## 333        no            Other
    ## 334        no            Other
    ## 335        no            Other
    ## 336        no            Other
    ## 337        no Downtown Seattle
    ## 338        no Downtown Seattle
    ## 339        no            Other
    ## 340        no            Other
    ## 341        no            Other
    ## 342        no Downtown Seattle
    ## 343        no Downtown Seattle
    ## 344        no Downtown Seattle
    ## 345        no            Other
    ## 346        no            Other
    ## 347        no Downtown Seattle
    ## 348        no            Other
    ## 349        no            Other
    ## 350        no            Other
    ## 351        no            Other
    ## 352        no            Other
    ## 353        no            Other
    ## 354        no            Other
    ## 355        no            Other
    ## 356        no            Other
    ## 357        no            Other
    ## 358        no            Other
    ## 359        no            Other
    ## 360        no            Other
    ## 361        no            Other
    ## 362        no            Other
    ## 363        no            Other
    ## 364        no            Other
    ## 365        no            Other
    ## 366        no            Other
    ## 367        no Downtown Seattle
    ## 368        no Downtown Seattle
    ## 369        no            Other
    ## 370        no Downtown Seattle
    ## 371        no            Other
    ## 372        no            Other
    ## 373        no            Other
    ## 374        no            Other
    ## 375        no            Other
    ## 376        no            Other
    ## 377        no            Other
    ## 378        no Downtown Seattle
    ## 379        no Downtown Seattle
    ## 380        no Downtown Seattle
    ## 381        no Downtown Seattle
    ## 382        no            Other
    ## 383        no Downtown Seattle
    ## 384        no Downtown Seattle
    ## 385        no Downtown Seattle
    ## 386        no Downtown Seattle
    ## 387       yes Downtown Seattle
    ## 388       yes Downtown Seattle
    ## 389        no            Other
    ## 390        no Downtown Seattle
    ## 391        no            Other
    ## 392        no Downtown Seattle
    ## 393        no            Other
    ## 394        no            Other
    ## 395        no Downtown Seattle
    ## 396        no Downtown Seattle
    ## 397        no            Other
    ## 398        no            Other
    ## 399        no Downtown Seattle
    ## 400        no            Other
    ## 401        no Downtown Seattle
    ## 402        no            Other
    ## 403        no Downtown Seattle
    ## 404        no Downtown Seattle
    ## 405        no Downtown Seattle
    ## 406        no            Other
    ## 407        no            Other
    ## 408        no            Other
    ## 409        no            Other
    ## 410        no            Other
    ## 411        no            Other
    ## 412        no            Other
    ## 413        no            Other
    ## 414        no            Other
    ## 415        no            Other
    ## 416        no            Other
    ## 417        no            Other
    ## 418        no            Other
    ## 419        no            Other
    ## 420        no Downtown Seattle
    ## 421        no Downtown Seattle
    ## 422        no            Other
    ## 423        no            Other
    ## 424        no            Other
    ## 425        no            Other
    ## 426        no Downtown Seattle
    ## 427        no Downtown Seattle
    ## 428        no Downtown Seattle
    ## 429        no Downtown Seattle
    ## 430        no Downtown Seattle
    ## 431        no            Other
    ## 432        no Downtown Seattle
    ## 433        no Downtown Seattle
    ## 434        no Downtown Seattle
    ## 435        no Downtown Seattle
    ## 436        no Downtown Seattle
    ## 437        no            Other
    ## 438        no            Other
    ## 439        no            Other
    ## 440        no Downtown Seattle
    ## 441        no Downtown Seattle
    ## 442        no            Other
    ## 443        no            Other
    ## 444        no            Other
    ## 445        no            Other
    ## 446        no Downtown Seattle
    ## 447        no Downtown Seattle
    ## 448        no            Other
    ## 449        no            Other
    ## 450        no            Other
    ## 451        no            Other
    ## 452        no            Other
    ## 453       yes Downtown Seattle
    ## 454        no            Other
    ## 455        no            Other
    ## 456        no            Other
    ## 457        no            Other
    ## 458        no            Other
    ## 459        no Downtown Seattle
    ## 460        no Downtown Seattle
    ## 461       yes Downtown Seattle
    ## 462        no            Other
    ## 463        no Downtown Seattle
    ## 464        no            Other
    ## 465        no            Other
    ## 466        no            Other
    ## 467        no            Other
    ## 468        no            Other
    ## 469        no            Other
    ## 470        no            Other
    ## 471        no Downtown Seattle
    ## 472        no            Other
    ## 473        no Downtown Seattle
    ## 474        no Downtown Seattle
    ## 475        no Downtown Seattle
    ## 476       yes Downtown Seattle
    ## 477        no Downtown Seattle
    ## 478        no            Other
    ## 479        no            Other
    ## 480       yes Downtown Seattle
    ## 481        no Downtown Seattle
    ## 482        no Downtown Seattle
    ## 483        no            Other
    ## 484        no            Other
    ## 485        no            Other
    ## 486        no Downtown Seattle
    ## 487        no Downtown Seattle
    ## 488        no Downtown Seattle
    ## 489        no Downtown Seattle
    ## 490        no Downtown Seattle
    ## 491        no Downtown Seattle
    ## 492        no            Other
    ## 493        no            Other
    ## 494        no            Other
    ## 495        no            Other
    ## 496        no            Other
    ## 497        no            Other
    ## 498        no Downtown Seattle
    ## 499        no            Other
    ## 500        no            Other
    ## 501        no            Other
    ## 502        no            Other
    ## 503        no            Other
    ## 504        no            Other
    ## 505        no            Other
    ## 506        no            Other
    ## 507        no            Other
    ## 508        no            Other
    ## 509        no            Other
    ## 510        no            Other
    ## 511        no            Other
    ## 512        no            Other
    ## 513        no Downtown Seattle
    ## 514        no Downtown Seattle
    ## 515        no Downtown Seattle
    ## 516        no            Other
    ## 517        no            Other
    ## 518        no Downtown Seattle
    ## 519        no            Other
    ## 520        no Downtown Seattle
    ## 521        no Downtown Seattle
    ## 522        no            Other
    ## 523        no            Other
    ## 524        no            Other
    ## 525        no            Other
    ## 526        no Downtown Seattle
    ## 527        no Downtown Seattle
    ## 528        no            Other
    ## 529        no            Other
    ## 530       yes            Other
    ## 531        no            Other
    ## 532        no            Other
    ## 533        no            Other
    ## 534        no            Other
    ## 535        no            Other
    ## 536        no Downtown Seattle
    ## 537        no            Other
    ## 538        no            Other
    ## 539        no            Other
    ## 540        no            Other
    ## 541        no            Other
    ## 542        no Downtown Seattle
    ## 543        no Downtown Seattle
    ## 544        no Downtown Seattle
    ## 545        no Downtown Seattle
    ## 546        no Downtown Seattle
    ## 547        no            Other
    ## 548        no            Other
    ## 549        no Downtown Seattle
    ## 550        no Downtown Seattle
    ## 551        no            Other
    ## 552        no Downtown Seattle
    ## 553        no            Other
    ## 554        no Downtown Seattle
    ## 555        no Downtown Seattle
    ## 556        no            Other
    ## 557        no            Other
    ## 558       yes            Other
    ## 559        no Downtown Seattle
    ## 560        no Downtown Seattle

``` r
custom_colors <- c("mediumslateblue", "darkslateblue")

ggplot(data = housing3, aes(x = basement, fill = zipcodeArea)) +
  geom_histogram(binwidth = 300, color = "black") +
  labs(title = "",
       x = "Basement Sizes in Square Feet",
       y = "Number of Basements",
       fill = NULL) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) + 
  theme(axis.title = element_text())
```

![](HousingPricesProject_files/figure-gfm/Basement%20Size%20+%20Zipcode%20Area-1.png)<!-- -->

``` r
model1 <-lm(price ~ living + waterfront + basement*zipcodeArea + log(lot) + I(living15^2) + year + bathrooms*bedrooms, data = housing3)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = price ~ living + waterfront + basement * zipcodeArea + 
    ##     log(lot) + I(living15^2) + year + bathrooms * bedrooms, data = housing3)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1248300  -185577     1363   180795  2110220 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                5.274e+06  1.422e+06   3.710 0.000229 ***
    ## living                     4.152e+02  2.947e+01  14.089  < 2e-16 ***
    ## waterfrontyes              5.833e+05  4.299e+04  13.570  < 2e-16 ***
    ## basement                  -1.656e+02  6.094e+01  -2.717 0.006790 ** 
    ## zipcodeAreaOther          -1.900e+05  4.993e+04  -3.806 0.000157 ***
    ## log(lot)                  -9.699e+04  1.999e+04  -4.851 1.60e-06 ***
    ## I(living15^2)              3.250e-02  5.944e-03   5.467 6.97e-08 ***
    ## year                      -2.131e+03  7.182e+02  -2.967 0.003139 ** 
    ## bathrooms                 -1.636e+05  6.052e+04  -2.703 0.007079 ** 
    ## bedrooms                  -1.993e+05  3.775e+04  -5.278 1.88e-07 ***
    ## basement:zipcodeAreaOther  1.889e+02  6.604e+01   2.861 0.004384 ** 
    ## bathrooms:bedrooms         6.419e+04  1.359e+04   4.724 2.95e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 367400 on 548 degrees of freedom
    ## Multiple R-squared:  0.8085, Adjusted R-squared:  0.8047 
    ## F-statistic: 210.4 on 11 and 548 DF,  p-value: < 2.2e-16
