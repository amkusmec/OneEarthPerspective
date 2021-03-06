# OneEarthPerspective

The code and data were used to produce Figure 1 in the paper "**Interdisciplinary strategies to enable data-driven plant breeding in a changing climate**" (in press).

The file `USDA_yields.csv` was generated by the [United States Department of Agriculture-National Agricultural Statistics Service](https://quickstats.nass.usda.gov/) (USDA-NASS) website on 20 August 2020. The following options were selected:
- **Select Commodity:** SURVEY > CROPS > FIELD CROPS > CORN > YIELD > CORN, GRAIN--YIELD, MEASURED IN BU / ACRE > TOTAL
- **Select Location:** NATIONAL > US TOTAL
- **Select Time:** 1866-2019 > ANNUAL > YEAR

Following Troyer [1], average yield for each year was assigned to one of four categories: "open-pollinated" [1866-1929], "double-cross" [1930-1959], "single cross" [1960-1994], and "biotech/GMO" [1995-2019]. Linear regression of yield on year within each category was used to estimate the average rate of gain in bushels per acre per year. Best fit lines and average rates of gain are reported in the figure.

The file `agresearchfunding2015.xls` was retrieved from the [USDA-Economic Research Service](https://www.ers.usda.gov/data-products/agricultural-research-funding-in-the-public-and-private-sectors/) (USDA-ERS), accessed on 20 August 2020.

**References**

[1] Troyer, A.F. (2006). Adaptedness and heterosis in corn and mule hybrids. *Crop Science*, **46**: 528-43.
