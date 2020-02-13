# PackNi

PackNi package provides functions for performing some statistical analysis, plotting and downloading online databes. 

### Installation

```R
#install the R package 'devtools', then install the package PackNi from Github
devtools::install_github('unimi-dse/bf98dcbb')
```

### Dataset

The package contains two in-built datasets from https://vincentarelbundock.github.io/Rdatasets/datasets.html

```R
# datasets documentation
?PackNi::canada_ab
?PaclNi::chicken_weight
```

### Usage

```R
# load PackNi package
require(PackNi)
```

##### absrel_freq

The function absrel_freq() computes the absolute ot relative frequency of elements in a vector

##### regr_signif

The function regr_signif() perform a regression by taking a y and x argument, then return if the estimated coefficient are significant or not. It computes again the regression whether the intercept is non-significant, taking it into account

##### online_dataset

The function online_dataset() downloads and saves into the global environment datasets in csv format from 'https://vincentarelbundock.github.io/Rdatasets/datasets.html'. Look at the documentation ?online_dataset for more information on the arguments taken

```R
online_dataset('https://vincentarelbundock.github.io/Rdatasets/csv/boot/amis.csv','car_speedwarn')
```

##### plot_ds

The function plot_ds() plots the data depending on the type of data used as arguments, returning a scatterplot or barplot. Look at the documentation ?plot_ds for more information









