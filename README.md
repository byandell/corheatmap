R/corheatmap

Package to show heatmap of matrix of data.
Clusters rows and columns using distance as `((1-cor)/2)^beta`.
That is positive correlation is near 0 distance,
and negative correlation is near 1 distance.
The `beta` attenuates the correlation near 1. 
Use by the following sequence (once package is installed):

```
corheatmap::heatmapApp()
```

### Installing the Package

To install the package, first install `devtools`:

```
install.packages("devtools")
```

On Windows, you also need [Rtools](http://cran.r-project.org/bin/windows/Rtools/). See [https://www.rstudio.com/products/rpackages/devtools/](https://www.rstudio.com/products/rpackages/devtools/) for details.

To install the package `corheatmap`, type the following:

```
library(devtools)
install_github("byandell/corheatmap")
```