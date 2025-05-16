# Bayesian Modeling of Ordinal Data

This repository contains the R files used for my master's thesis titled **"Bayesian Modeling of Ordinal Survey Data: A Live Music Census Case Study."**

The thesis focuses on the analysis of ordinal data using Bayesian methods, with particular emphasis on the cumulative logistic model. It provides an overview of the fundamental model, explores various extensions and alternatives, and introduces the Bayesian framework for estimation and inference.

A simulation study is included to compare the performance of classical and Bayesian approaches, followed by an application to real-world data in a form of live music census. Both the simulation and case study results suggest that the Bayesian approach offers improved performance over traditional methods.

> **Note**: The Live Music Census dataset was collected as part of OpenMuse Eu project and is not currently publicly available. This repository contains only the code used for simulation, model fitting, and analysis. If the data becomes public in the future, this repository will be updated accordingly.

The outline of the repository is the following:
```
├── README.md                     # repository overview
├── figures/                      # contains R files for the different plots used in the thesis
│   ├── example_figures.R         # code for the latent distribution example plots
│   ├── freq_vs_bayes_figuers.R   # code for comparison figures for the live music census questions
│   ├── simulation_boxplots.R     # code for the box plots used in the simulation study
│   └── whole_data_analysis.R     # code for the whole census analysis figures
├── live_music_census_analysis/   # contains R fiels used in the analysis of the live music census data
│   ├── freq_vs_bayes_comp/       # contains R files used in the comparisons of frequentist and Bayesian approaches
|       ├── night.R               # code for the "How was your night" question
|       └── scene.R               # code for the "How would you rate the live music scene in your city" question
│   └── whole_data_analysis/      # contains R files for the analysis of the whole census data
|       ├── night_analysis.R      # code for the analysis of "How was your night" question
|       └── scene_analysis.R      # code for the analyis of "How would you rate the live music scene in your city" question
└── simulations/                  # contains R files used in the simulations
|   └── simulation.R              # code used in the simulation study
```
