# A set of simulations for validating the permutated splitting algorithm of the [splithalfr R package](https://github.com/tpronk/splithalfr)
To cite this simulation use:

Pronk, T., Molenaar, D., Wiers, R. W., & Murre, J. (2020). _A set of simulations for validating the permutated splitting algorithm of the splithalfr R package_. https://github.com/tpronk/splithalfr_simulation

## Introduction
These simulations were aimed to numerically reproduce an equivalence that has been proven analytically in extant research. Namely, that for scores generated by an essentially tau-equivalent model (i.e. a single factor model with equal factor loadings), the mean Flanagan-Rulon coefficient of all possible splits of a test approaches Cronbach's alpha ([Novick & Lewis, 1967](https://doi.org/10.1007/BF02289400); [Warrens, 2015](https://doi.org/10.1007/978-3-319-19977-1)).

## Methods
Tests were simulated in which 1000 participants answered 50 items. Essentially tau-equivalent answers were generated as follows. Each participant had a trait score T, which was drawn from a standard normal distribution. Each item score was the sum of the participant’s trait score and a noise term E drawn from a normal distribution with a mean of zero and a standard deviation of Y. In nine simulations, Y was varied from 1 to 9, reflecting tests that were increasingly unreliable in measuring the trait T. For each simulation, Cronbach’s alpha was calculated via the [psyc](https://cran.r-project.org/package=psych) package, as well as the mean Flanagan-Rulon coefficient over 10,000 permutated splits via the [splithalfr package](https://github.com/tpronk/splithalfr). Because 10,000 permutated splits is an approximation of all possible splits, we expected that the mean Flanagan-Rulon coefficient of these splits was close to Cronbach's Alpha.

## Results
Per simulation, the table below shows Y, Cronbach's alpha, the Flanagan-Rulon coefficient, and the difference between Cronbach's alpha and the Flanagan-Rulon coefficient. Note that, across stimulations, Cronbach's alpha and the Flanagan-Rulon coeffficient differed at most by 0.00052.

| Y | Cronbach’s alpha | Flanagan-Rulon | difference |
|---|-----------------:|---------------:|-----------:|
| 1 | 0.98193          | 0.98192        | 0.00002    |
| 2 | 0.92716          | 0.92712        | 0.00003    |
| 3 | 0.84669          | 0.84686        | -0.00017   |
| 4 | 0.76279          | 0.76303        | -0.00024   |
| 5 | 0.65186          | 0.65161        | 0.00024    |
| 6 | 0.58218          | 0.58270        | -0.00052   |
| 7 | 0.50347          | 0.50365        | -0.00018   |
| 8 | 0.49133          | 0.49117        | 0.00016    |
| 9 | 0.40008          | 0.39993        | 0.00016    |

## Discussion
Since Cronbach's alpha and the Flanagan-Rulon coefficient were indeed close to eachother, we conclude that the permutated splitting algorithm of the [splithalfr R package](https://github.com/tpronk/splithalfr) functions correctly.
