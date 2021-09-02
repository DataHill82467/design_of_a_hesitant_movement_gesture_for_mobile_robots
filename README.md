# design_of_a_hesitant_movement_gesture_for_mobile_robots

This repository documents the analysis connected to the PLoS ONE publication 'Design of a hesitant movement gesture for mobile robots'

https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0249081

-----------------
## Installation

The analysis is performed in R

-----------------
## File description

- Rrun01_einlesen+modell.R : Data Wrangling to get a DataFrame which contains all information necessary for statistical analysis. Statitstical analysis for the experimental design, linear mixed-models and explorative plots and plots for the publication.

- Rrun01_eingestellte_Back-Offs_analysieren.R : Additional explorative Analysis of Back-off motions, designed by the participants.

- 06_Rohdaten : contains all quantitative data from the experiment

- BotimeFromDistanceAndSpeed.csv : contains the result of the Analysis made in Rrun01_eingestellte_Back-Offs_analysieren.R and is input for Rrun01_einlesen+modell.R

- Demographic.csv :  Contains the participants' demographic data

- FragebogenTeil2 : Contains answers to the questionnaire

- Robots_LWH_ : Contains the measures of robots presented in each experimental condition

- Wahrnehmung_Groessen: Contains information about whether the robots' sizes were perceived correctly
