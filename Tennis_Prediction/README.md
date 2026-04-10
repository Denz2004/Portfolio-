# Tennis Match Prediction with Machine Learning

## Overview

This project develops machine learning models to predict professional ATP tennis match outcomes and evaluates their profitability when applied to betting strategies. We use 21 years of ATP historical data (2003–2024) and engineer 123 features capturing player skill dynamics, then compare three model classes: logistic regression, XGBoost, and multilayer perceptrons.

## Research Question

Can machine learning models predict tennis match outcomes with sufficient accuracy to generate positive returns when applied to betting strategies against bookmaker odds?

## Dataset

- **Source:** [Jeff Sackmann's ATP Tennis Dataset](https://github.com/JeffSackmann/tennis_atp)
- **Betting Odds:** [Tennis-Data.co.uk](http://www.tennis-data.co.uk/)
- **Period:** 2003–2024 (58,833 ATP matches)
- **Train/Test Split:** Temporal split at 2024 US Open

## Features (123 total)

| Category | Description |
|----------|-------------|
| **Static attributes** | ATP ranking, ranking points, height, age differences |
| **Head-to-head** | Historical win/loss records overall and per-surface |
| **Form** | Win rates over rolling windows (3, 5, 10, 25, 50, 100 matches) |
| **Elo ratings** | Overall and surface-specific ratings with adaptive K-factors and inactivity adjustments |
| **Rolling statistics** | Ace rate, break points saved, return points won, etc. across multiple time windows |

## Models

1. **Logistic Regression** — L2-regularised baseline
2. **XGBoost** — Two variants tuned for log-loss and error rate
3. **Multilayer Perceptron** — Two variants tuned for log-loss and error rate

Hyperparameter optimisation performed using Optuna with TPE sampler and TimeSeriesSplit cross-validation.

## Betting Strategies

Evaluated on 126 matches from the 2024 US Open using Pinnacle Sports closing odds:

1. **Winner Prediction Strategy:** Fixed stake on the predicted winner
2. **Threshold-Hedged Strategy:** Proportional stakes based on model probabilities, betting only when the model-implied edge exceeds the bookmaker margin

## Results

| Model | Test Accuracy | Strategy 1 ROI | Strategy 2 ROI |
|-------|---------------|----------------|----------------|
| Logistic Regression | 65.3% | -5.07% | -2.88% |
| XGBoost (Log-Loss) | 67.2% | +0.48% | -4.55% |
| XGBoost (Error) | 66.9% | -2.67% | -4.08% |
| MLP (Log-Loss) | 66.4% | +1.06% | -5.27% |
| MLP (Error) | 66.1% | +0.88% | -4.80% |

## Repository Structure

### 1. Data Cleaning, Feature Engineering & EDA (`CODE/Data.ipynb`)

- Clean raw ATP match data (2003–2024)
- Engineer 123 features:
  - Surface-specific Elo ratings (Hard, Clay, Grass) with experience-adjusted K-factors and inactivity multipliers
  - Multi-scale rolling statistics (windows: 3–2,000 matches)
  - Head-to-head records overall and per-surface
  - Form indicators across multiple time horizons
- Exploratory data analysis
- Output: train/test split datasets

**Data files:**
- `CODE/DATA/` — Raw ATP match CSVs from Sackmann (2003–2024)
- `CODE/Final/test.csv` — Test set: 705 matches from 2024 US Open onwards
- `CODE/Final/usopen.csv` — 2024 US Open betting odds from Tennis-Data.co.uk
- `CODE/Final/order.csv`, `betting_order.csv` — Alignment files for test set to betting matches

**Note:** `train.csv` is not included due to file size constraints. Run `Data.ipynb` to generate it.

### 2. Model Development

**Logistic Regression Baseline (`CODE/Log_Model.ipynb`)**
- Standard L2-regularised logistic regression as benchmark
- No hyperparameter tuning required

**XGBoost (`CODE/XGBoost.ipynb`)**
- Strong tabular data baseline for comparison
- Two models optimised for log-loss and error-rate respectively
- Optuna hyperparameter tuning with TimeSeriesSplit cross-validation
- Evaluation metrics: log-loss, accuracy, Brier score, AUC

**Neural Network / MLP (`CODE/MLP.ipynb`)**
- Feedforward neural network with PyTorch
- Architecture: 123 (input) → 256 → 256 → 256 → 256 → 1 (output) with ReLU activations and dropout
- BCEWithLogitsLoss for numerically stable training
- Two models optimised for log-loss and error-rate respectively
- Optuna hyperparameter tuning (learning rate, weight decay, batch size, dropout rate, epochs) with TimeSeriesSplit cross-validation
- Evaluation metrics: log-loss, accuracy, Brier score, AUC

### 3. Betting Strategy Evaluation (`CODE/Betting.ipynb`)

- Retrain all models on full training set with optimal hyperparameters
- Evaluate prediction accuracy on 705-match test set
- Obtain Pinnacle Sports closing odds for 2024 US Open (126 matches)
- Test two strategies:
  1. **Winner Prediction:** Fixed stake on model's predicted winner
  2. **Threshold-Hedged:** Proportional stakes based on model probabilities, betting only when edge exceeds bookmaker margin
- Evaluate across 10 metrics: ROI, Sharpe ratio, win rate, profit factor, max drawdown, MACE, volatility, etc.
- Visualisations: equity curves, calibration plots, confusion matrices, profit distributions

## References

- Elo, A. E. (1978). *The Rating of Chessplayers, Past and Present*
- Wilkens, S. (2021). Sports prediction and betting models in the machine learning age: The case of tennis. *Journal of Sports Analytics*
- Niculescu-Mizil, A. & Caruana, R. (2005). Predicting good probabilities with supervised learning. *ICML 2005*. 625-632.
- Wilson, A., Roelofs, R., Stern, M., Srebro, N. & Recht, B. (2017). The Marginal Value of Adaptive Gradient Methods in Machine Learning. *arXiv:1705.08292*
- Hinton, G., Srivastava, N., Krizhevsky, A., Sutskever, I. & Salakhutdinov, R. (2014). Dropout: A Simple Way to Prevent Neural Networks from Overfitting. *JMLR* 15, 1929-1958.
- Guo, C., Pleiss, G., Sun, Y. & Weinberger, K. (2017). On Calibration of Modern Neural Networks. *ICML 2017*
- Gorishniy, Y., Rubachev, I., Khrulkov, V. & Babenko, A. (2021). Revisiting Deep Learning Models for Tabular Data. *arXiv:2106.11959*
- Shmuel, A., Glickman, O. & Lazebnik, T. (2025). A comprehensive benchmark of machine and deep learning models on structured data. *Neurocomputing* 655, 131337.
- Kingma, D. & Ba, J. (2014). Adam: A Method for Stochastic Optimization. *ICLR 2015*

## Acknowledgements

We thank Jeff Sackmann for providing the tennis match dataset and Tennis-Data.co.uk for historical betting odds data.

## License

MIT License
