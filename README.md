
For information about the project see: https://neobiota.pensoft.net/article/1270/

Populations were sampled in plots (up to 10 per population) and within each plot, a new plant could have been sampled every 20cm for a total of 5 measurements within each plot, or 50 measurements per population. 

There are 3 folders containing data analyses: Basic Analysis, Climate and Weather Models and Model Selection. Scripts should be run in these folder with respect to their numeric naming system. 

Data is initially organized into a wide format in "CorrectedDataAll.csv". Data on individuals plants within plots are organized in columns in the following format: P<PLOT#><MEASURE><POS#> e.g. P4Adult20. If data exhibit a trailing number as above, then this is a plant-specific measurement of a plant. For example,P4Adult20 corresponds to a body size measurement of an adult located at 20cm into plot 4. The same is true of rosettes (e,g,P4Ros20) and fungal damage. For example, P5Fung20 is the number of leaves with fungal damage in plot 5, 20cm deep into the population.

If the column lacks a last number for position: P<PLOT#><MEASURE> e.g. P4Adult, P2Ros, then this data describes the abundance in the plot, not size. For example, P4Adult is the number of adults in the 4th plot. The same is true of fungal damage: P5AdultFung is the number of adults with fungal damage. The same is true of P5RosFung, which corresponds to the number of rosettes with fungal damage. 

After population means are summarized in the 1_Basic Analysis files, data are presented in a long format, where each row represents the averaged data corresponding to each population. 


