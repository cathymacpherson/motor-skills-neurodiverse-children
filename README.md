# Movement Skills in Neurodiverse and Neurotypical Children

This project investigates interpersonal coordination dynamics during rhythmic tasks in neurotypical and neurodiverse children, and their relationship to motor skill classification.

## Project Structure

### Data
- `ips_data.xlsx` - Dataset containing participant demographics, motor assessments (MABC-2), and coordination metrics (rho values)

### Analysis Scripts
- [mixed-effects-models.R](mixed-effects-models.R) - **Primary analysis**: Linear mixed-effects models examining coordination dynamics across neurotype groups
- [machine-learning.ipynb](machine-learning.ipynb) - **Secondary analysis**: Machine learning classifier (HGBC) predicting motor skill classification from coordination features

### Output Directories
- `MEM_Figures/` - Visualisations from mixed-effects models
- `ML_Figures/` - Visualisations from machine learning analysis

## Setup Instructions

See [SETUP.md](SETUP.md) for detailed first-time setup instructions for both R and Python environments.

## Running the Analyses

### 1. Mixed-Effects Models (R)
1. Open `mixed-effects-models.R` in RStudio or VS Code with R extension
2. Ensure required packages are installed (see script header)
3. Run the script to generate model outputs and figures

### 2. Machine Learning (Python)
1. Open [machine-learning.ipynb](machine-learning.ipynb) in VS Code or Jupyter
2. Ensure the virtual environment is selected as the kernel
3. Run all cells sequentially

## Output

### Mixed-Effects Models
- Model summaries and estimated marginal means
- Pairwise contrasts between neurotype groups
- Interaction plots saved to `MEM_Figures/`

### Machine Learning
- Classification metrics (accuracy, balanced accuracy, AUC)
- ROC curve with bootstrapped confidence intervals
- Permutation feature importance
- Individual prediction scatter plots
- Figures saved to `ML_Figures/`

## Reference

This repository is designed to support the a publication that is currently under review. Once published, the citation will be provided here.

## License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.
