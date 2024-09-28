# Efficiency Analysis of European Banks Using TOPSIS and Regression Modeling

---

## Table of Contents

- [Introduction](#introduction)
- [Data Description](#data-description)
- [Methodology](#methodology)
  - [Information Entropy Weights](#information-entropy-weights)
  - [TOPSIS Analysis](#topsis-analysis)
  - [Bootstrapped TOPSIS Analysis](#bootstrapped-topsis-analysis)
  - [Endogeneity Analysis](#endogeneity-analysis)
  - [Regression Analysis](#regression-analysis)
    - [Bootstrap Sampling](#bootstrap-sampling)
    - [Model Combination and Optimization](#model-combination-and-optimization)
- [Repository Structure](#repository-structure)

---

## Introduction

This project conducts an efficiency analysis of European banks using Multi-Criteria Decision-Making (MCDM) techniques and regression modeling. The main objectives are:

- To evaluate the performance of banks using the Technique for Order Preference by Similarity to Ideal Solution (TOPSIS).
- To determine the weights of different criteria using Information Entropy.
- To assess endogeneity among the criteria.
- To model the relationship between contextual variables and the performance scores using regression analysis.

**Note**: the findings and interpretations presented here are part of an ongoing academic research project. The results are preliminary and subject to further refinement and validation. The final conclusions will be presented in the forthcoming academic paper.

---

## Data Description

The analysis is based on data from European banks, focusing on various performance indicators and contextual variables. The data is divided into the following categories:

- **Contextual Variables**: External factors that may influence bank performance.
- **Utility to Citizens**: Measures reflecting the value provided to individual customers.
- **Utility to Firms**: Metrics indicating the support provided to business clients.
- **Banking Economic Leverage**: Indicators of the bank's financial leverage.
- **Banking Financial Health**: Metrics assessing the overall financial stability of the banks.

---

## Methodology

### Information Entropy Weights

Information entropy is used to determine the weights of criteria in the TOPSIS analysis. Entropy measures the uncertainty or randomness in the data. Lower entropy indicates more significant differences among values, leading to higher weights.

### TOPSIS Analysis

TOPSIS is employed to rank the banks based on their performance across multiple criteria. The steps include:

1. **Normalization**: The decision matrix is normalized.
2. **Weighted Normalization**: Applying entropy weights to the normalized matrix.
3. **Ideal Solutions**: Determining the positive and negative ideal solutions.
4. **Distance Measures**: Calculating the distance of each alternative from the ideal solutions.
5. **Performance Score**: Computing the relative closeness to the ideal solution.

### Bootstrapped TOPSIS Analysis

Bootstrap sampling is used to assess the robustness of the TOPSIS rankings. By resampling the data multiple times, we can estimate the variability in the TOPSIS scores.

### Endogeneity Analysis

Endogeneity among criteria is analyzed using mutual information and conditional entropy. This helps identify any potential feedback loops or causal relationships between criteria.

### Regression Analysis

#### Bootstrap Sampling

Regression models are built to relate the TOPSIS scores to contextual variables. Bootstrap sampling is used to estimate the distribution of coefficients and to assess model stability.

#### Model Combination and Optimization

To improve predictive performance, we combine different regression models (Tobit, Beta, and Simplex) using optimized weights. The optimization minimizes the variance-covariance of residuals.

---

## Repository Structure


- **Data**: Contains the datasets used in the analysis.
- **Plots**: Contains all generated plots.
- **Results**: Contains the output files such as TOPSIS scores, regression coefficients, and metrics.
- **R**: Contains the R scripts used for the analysis.
- **README.md**: This readme file.

---
