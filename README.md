# PPG Paint Color Predictive Modeling: Dual Machine Learning Tasks

## Overview

This project applies a dual machine learning approach (Regression and Classification) to analyze and predict attributes within **PPG Industries'** proprietary paint color library. This work was conducted as a sponsored academic project, providing hands-on experience with real-world, commercial data science problems.

The primary objectives were to:
1.  **Build a Regression Model:** Accurately predict a key continuous color metric.
2.  **Build a Classification Model:** Predict categorical color attributes (e.g., saturation) while systematically addressing significant data imbalance.

## Technical Stack

The entire pipeline—from data cleaning and Exploratory Data Analysis (EDA) to feature engineering, model training, and interpretation—was executed in the **R ecosystem**.

* **Core Libraries:** `caret`, `tidyverse`, `tidymodels`
* **Modeling:** `glmnet`, `xgboost`, `randomForest`, `earth` (for MARS)
* **Analysis:** `FactoMineR`, `recipes` (for subsampling)
* **EDA:** `ggplot2`, `corrplot`, `DataExplorer`

---

## Task 1: Regression (Predicting Continuous Color Value)

The goal was to find the model that most accurately predicted a continuous color metric, evaluated on **Root Mean Squared Error (RMSE)** and **Mean Absolute Error (MAE)**.

### Regression Model Performance

A comprehensive `caret` training process compared multiple models, including Linear Models (LM), Random Forest (RF), XGBoost (XGB), and Multivariate Adaptive Regression Splines (MARS). The analysis revealed a trade-off between MAE and RMSE.

| Model | Best Metric | Test Set Value |
| :--- | :--- | :--- |
| **`lm_cat_interaction`** | **RMSE** | **0.060453** |

**Conclusion: Selected Model**
The **`lm_cat_interaction`** model was selected as the best overall regression model. As interpreted in the analysis, this model (based on the interaction of categorical inputs with all continuous main effects) provided the most robust and reliable performance on unseen data, prioritizing the strong RMSE result.

---

## Task 2: Classification (Predicting Color Category)

This task focused on predicting a categorical color attribute. The primary challenge was the **severe class imbalance** in the dataset.

### Imbalanced Data Strategy

To combat imbalance, **subsampling techniques** were implemented via the `recipes` package. This artificially balanced the output classes during training, forcing the model to learn the features of minority classes. The key evaluation metrics were therefore **Sensitivity** and **F1 Score**, not just simple Accuracy.

### Classification Model Performance

Models were compared on their ability to correctly identify all classes, especially the rare ones. The tuned `XGBoost` model was the clear winner.

| Model | Key Metric (for Selection) | Test Set ROC AUC | Result |
| :--- | :--- | :--- | :--- |
| **`XGBoost (XGB)`** | Best Sensitivity & F1-Score | **0.88** |

**Conclusion: Selected Model**
The **`XGBoost`** model demonstrated the best overall performance. It achieved an excellent **ROC AUC of 0.88** on unseen test data, indicating strong general predictive power. Furthermore, the model comparison plot confirmed it achieved a superior balance of Sensitivity and F1-Score, making it the most effective and robust model for this imbalanced data problem.

---

## Actionable Insights from Analysis

The combined EDA and model interpretation revealed several key findings for PPG:

1.  **Feature Importance:** The primary color channels (**`R`**, **`G`**, **`B`**) consistently showed significantly higher predictive contribution than derived metrics like `Hue`.
2.  **Regression "Hardiness":** The **`light`** lightness category was identified as the most difficult category for the regression model to predict accurately, suggesting higher inherent variability in its formulation data.
3.  **Classification "Hardiness":** The **`shaded`** saturation category proved to be the most difficult for the classifier to predict, while the **`bright`** category was the easiest.

## Repository Structure & Setup

Due to data confidentiality, the original data and proprietary interpretation files are excluded. This repository contains the R project code files necessary to reproduce the EDA and modeling pipelines.

| File | Description |
| :--- | :--- |
| `CHAVA_SAI_VIVEK_FINAL_PROJECT_EDA.html` | Comprehensive data exploration, visualization, and feature contribution analysis. |
| `CHAVA_SAI_VIVEK_REGRESSION.html` | Code and results for the regression modeling pipeline, including model comparison. |
| `CHAVA_SAS_VIVEK_FINAL_PROJECT_CLASSIFICATION.html` | Code and results for the classification pipeline, including subsampling for imbalanced classes and model performance plots. |

### **Setup Instructions**

1.  **Clone the Repository:**
    ```bash
    git clone https://github.com/sai-vivek108/ML-Project---PPG-Paints
    ```
2.  **Environment:** Open the R project files in RStudio. Ensure you have the necessary R packages installed (e.g., `install.packages(c("caret", "tidyverse", "xgboost", "earth"))`).
3.  **Run:** Execute the R Markdown files sequentially to reproduce the analysis and modeling pipeline.
