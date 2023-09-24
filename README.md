# SleepFC

## Introduction:
This page contains the necessary code to run the analyses from the paper "Sleep network functional connectivity, hyperexcitability, and cognition in Alzheimer’s disease".  
Link: TBD.  

## Files included:  
* "Preprocessing_Features_and_Matrices.ipynb": Preprocess raw EEG timeseries .mat files and .mat sleep staging files to obtain output features and functional connectivity matrices.  
* "Manuscript_Figures.ipynb": Plot the main figures from the paper including FC matrix statistical comparisons, regressions, and box plots.  
* "Cross_sectional_MoCA.ipynb": Cognitive score analysis for the cross-sectional regression.  
* "Longitudinal_decline.R: Longitudinal cognitive score decline analysis.  
* "Preprocess_2nd_night.ipynb": Obtain features and matrices for the 2nd night reproducibility analysis.  
* "ML_group_classification.ipynb": Machine Learning classifier for groups (AD-NoEp, AD-Ep, and HC).  
* "Classifier_MoCA_decline.ipynb": Machine Learning classifier for fast cognitive decline.  

## Environment:  
Python 3.9.7  
mne 1.2.1  
numpy 1.22.4  
pandas 1.5.3  
pickleshare 0.7.5  
plotly 5.14.1  
scikit-learn 0.24.2  
scipy 1.7.1  
seaborn 0.11.2  
statsmodels 0.12.2  
