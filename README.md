# cryptocurrency-portfolio-garch-modeling
Time series analysis of cryptocurrency portfolio volatility using GARCH models and Value-at-Risk (VaR) estimation in R

# Cryptocurrency Portfolio Volatility Modeling with GARCH

Time series analysis of cryptocurrency portfolio volatility using GARCH models. Project for Advanced Time Series course at University of Warsaw.

## Overview

Analysis of volatility dynamics in a cryptocurrency portfolio consisting of:
- Elysia (EL)
- Render (RNDR)
- VeChain (VET)
- Voxies (VOXEL)

**Data**: Daily prices from May 28, 2023 to June 30, 2024 (400 observations)

## Methodology

- **Exploratory analysis**: ACF, ARCH tests, normality tests
- **Model comparison**: Tested 9 GARCH-family models with different specifications
- **Best model**: ARMA(1,1)-GARCH(0,1) with Student-t errors
- **Validation**: In-sample (70%) vs out-of-sample (30%)
- **Risk management**: Value-at-Risk (VaR) estimation at 1% level

## Key Findings

- Strong volatility clustering in cryptocurrency returns
- ARMA(1,1)-GARCH(0,1) provides best fit based on AIC/BIC
- Student-t distribution better captures fat tails than normal
- VaR estimates accurate at 1% confidence level
- Volatility persistence extremely high (near unit root)

## Tools

- **R** with `rugarch`, `fGarch`, `FinTS`, `xts`, `dygraphs`

**Course**: Advanced Time Series Analysis  
**Institution**: University of Warsaw, Faculty of Economic Sciences  
