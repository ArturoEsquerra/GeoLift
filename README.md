![GeoLiftLogo](doc/GeoLift_IconText.png)

## Important: GeoLift has moved!
**The GeoLift package has moved to [facebookincubator/GeoLift](https://github.com/facebookincubator/GeoLift). Please refer to this new location to access the most up-to-date version of the GeoLift code.**

# GeoLift
[![Project Status: Moved to https://github.com/facebookincubator/GeoLift – The project has been moved to a new location, and the version at that location should be considered authoritative.](https://www.repostatus.org/badges/latest/moved.svg)](https://www.repostatus.org/#moved) to [facebookincubator/GeoLift](https://github.com/facebookincubator/GeoLift) 

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


## Overview
GeoLift is an end-to-end solution to measure Lift at a Geo-level using the latest developments in Synthetic Control Methods. Through this package it is possible to do a data-driven market selection for a geo-test using a variety of Power Calculators. Moreover, GeoLift features easy-to-use inference and plotting functionalities to analyze the results of a test.

Check out the vignette for a more detailed description of the main functionalities of the package:
- [GeoLift Walkthrough](https://github.com/ArturoEsquerra/GeoLift/blob/master/vignettes/GeoLift_Walkthrough.md)

## Requirements
GeoLift requires or works with:
- R version 4.0.0 or newer.

## Installing GeoLift
To install the package, first make sure that `devtools` and `augsynth` are installed.

```
install.packages("devtools", repos='http://cran.us.r-project.org')
devtools::install_github("ebenmichael/augsynth")
```

Then, install the package from GitHub:

```
devtools::install_github("facebookincubator/GeoLift")
```

## Contacts
- aesquerra@fb.com, Arturo Esquerra, Facebook Marketing Science Partner
- kpanchal@fb.com, Kanishka Panchal, Facebook Marketing Science Partner
- nicocru@fb.com, Nicolas Cruces, Facebook Marketing Science Partner

## Join the GeoLift community
- [Facebook page](www.facebook.com/groups/fbgeolift/)

## License
GeoLift is MIT licensed, as found in the LICENSE file.
