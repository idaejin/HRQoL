# Health Related Quality of Life regression analysis for the SF-36 short form survey

**HRQoL R package**

This `R` package implements the use of Beta-binomial regression (with logistic link) for Health Related Quality of Life outcomes from SF-36 survey.

In addition, it includes some graphical options and re-codification of SF-36 scores as in Arostegui *et al.* (2006).


**Creator(s):** Josu Nájera (<jnajera@bcamath.org>) & Dae-Jin Lee (<dlee@bcamath.org>)


## Installation of  `HRQoL` using `devtools`

`R` version >3.2.3 required

```
install.packages("devtools")
library(devtools)
dev_mode(on=TRUE)
install_github("idaejin/HRQoL")
```



## References
* Arostegui I., Nuñez-Antón V. & Quintana J. M. (2006): *Analysis of short-form-36 (SF-36): The beta-binomial distribution approach*, Statistics in Medicine, 26, 1318-1342.
* Arostegui I., Nunez-Antón V. & Quintana J. M. (2010): *Statistical approches to analyse patient-reported outcome as response variables: An application to health-related quality of life*, Statistical Methods in Medical Research, 21, 189-214.
* Arostegui I., Nunez-Anton V. & Quintana J. M. (2013): *On the recoding of continuous and bounded indexes to a binomial form: an application to quality-of-life scores*, Journal of Applied Statistics, 40:3, 563-583
* Breslow N. E. & Clayton D. G. (1993): *Approximate Inference in Generalized Linear Mixed Models*, Journal of the American Statistical Association, 88, 9-25.
* Forcina A. & Franconi L. (1988): *Regression analysis with Beta-Binomial distribution*, Revista di Statistica Applicata, 21. 
* McCulloch C. E. & Searle S. R. (2001): *Generalized, Linear, and Mixed Models*, John Wiley & Sons.
* Pawitan Y. (2001): *In All Likelihood: Statistical Modelling and Inference Using Likelihood*, Oxford University Press.

