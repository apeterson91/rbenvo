## <img src = "docs/figures/rbenvo_hex.png" align="right" width=190 height=220> `rbenvo`: Built Environment Objects for Point Pattern Data in R
<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Travis build status](https://travis-ci.org/apeterson91/rbenvo.svg?branch=master)](https://travis-ci.org/apeterson91/rbenvo)
[![R build status](https://github.com/apeterson91/rbenvo/workflows/R-CMD-check/badge.svg)](https://github.com/apeterson91/rbenvo/actions)
<!-- badges: end -->

## About

`rbenvo` is package of class (benvo) and methods for built environment data structures. 
They are designed to ease the use of working with these nonstandard data  structures and 
improve interoperability with other R packages developing methods in this same space (see eg. [rstapDP](https://apeterson91.github.io/rstapDP/)).


## Installation

### Development Version

As this package is under active development, it is currently only available via Github. In order to install `rbenvo`  use the following 
 lines of R code

 ```r
 if(!require(devtools)){
	install.packages("devtools")
	library(devtools)
 }

install_github("apeterson91/rbenvo",dependencies = TRUE)
 ```

## Contributing

 Examples and code contributions are welcome. Feel free to start/address a feature in the issue tracker and I'll be in touch shortly. 

#### Code of Conduct

Please note that `rbenvo` is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/). By contributing to this project, you agree to abide by its terms.


## How to cite this package

 A citation is in progress. Check back soon.

## Acknowledgments

This work was developed with support from NIH grant R01-HL131610 (PI: Sanchez).


