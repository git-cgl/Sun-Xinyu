# Sun-Xinyu
#### I am a Ph.D student from PBCSF THU. Attached please find some code and slides of my assignments, maybe research later during the course, which is just for no-business intention. Feel free to contact with me for further discussion.
#### E-mail:sunxy.18@pbcsf.tsinghua.edu.cn
********************************
Fama and French(1992)：The Cross-Section Of Expected Stock Returns

* 1.Data & Methodology
>In June of each year, all NYSE stocks on CRSP are sorted by size (ME) to determine the NYSE decile breakpoints for ME

>Subdivide each size decile into 10 portfolios on the basis of pre-ranking βs for individual stocks

>Match the accounting data for all fiscal yearends in calendar year t-1 (1962-1989) with the returns for July of year t to June of t+1

>The pre-ranking βs are estimated on 24 to 60 monthly returns (as available) in the 5 years before July of year t(at least 24 of the 60 months preceding July of year t)

>Estimate βs using the full sample (330 months) of post-ranking returns on each of the 100 portfolios, with the CRSP value-weighted portfolio of NYSE, AMEX, and (after 1972) NASDAQ stocks used as the proxy for the market

>estimate β as the sum of the slopes in the regression of the return on a portfolio on the current and prior month’s market return to adjust for nonsynchronous trading

>Allocate the full-period post-ranking β of a size-β portfolio to each stock in the portfolio. These are the βs that will be used in the Fama-MacBeth cross-sectional regressions for individual stocks.

* 2.Result
>(a) β does not seem to help explain the cross-section of average stock returns(with control of size effect)

>(b) the combination of size and book-to-market equity seems to absorb the roles of leverage and E/P in average stock returns, at least during our 1963-1990 sample period.

i. β & size

![](http://www.jijitang.com/photos/4b8e06f0-eb45-11e8-90f9-77961b66a500.jpg)
>Properties of Portfolios Formed on Size or Pre-Ranking β:
按规模或Pre-Ranking β分组的组合性质

![](https://github.com/sunxy-pbcsf/Sun-Xinyu/blob/master/Table%20II.jpg)
> Average Returns, Post-Ranking βs and Average Size For Portfolios Formed on Size and then β
规模-贝塔组合的平均回报，Post-Ranking βs与平均规模

