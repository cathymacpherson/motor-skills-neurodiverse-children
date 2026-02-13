# Motor Skills in Neurodiverse Children

This project uses machine learning to classify motor skill development in neurotypical and neurodiverse children based on coordination dynamics during rhythmic tasks.

## Project Structure

- [machine-learning.ipynb](machine-learning.ipynb) - Main analysis notebook containing the logistic regression classifier and visualizations
- `Participant_Data_ML.csv` - Dataset containing participant demographics, motor assessments, and coordination metrics
- `plots/` - Generated visualizations from the analysis

## Setup Instructions

- [SETUP.md](SETUP.md) - Detailed set up instructions are provided here to assist with the first-time set up of this repository

## Running the Analysis

1. Open [machine-learning.ipynb](machine-learning.ipynb) in VS Code or Jupyter
2. Ensure the virtual environment is selected as the kernel
3. Run all cells sequentially

## Output

The notebook generates:
- Classification metrics (accuracy, balanced accuracy, AUC)
- ROC curve visualizations
- Feature importance analysis
- Confusion matrix breakdown

Generated plots are saved to the `plots/` directory.

## License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.
