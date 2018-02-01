# Sensor_reliability_study
Data from inertial-sensor based gait analysis and code for statistical analysis

This repository contains data from joint angular motion variables from 14 subjects (9 women and 5 men) collected in two different days.
The objective is to use this data to investigate the agreement statistics of each parameter.

Subjects performed 5 bouts of a 10 meter walking test. Data from inertial sensors were acquired by using a commercial system (Technaid, S.L., Spain). The variables from each test were extracted by a trained researcher, averaged and stored in ".csv" tables. Each table contain data from different joints

The variables on the datasets are presented in tidy format, where each column is a variable and each line represents one participant. Each parameter is identified by a capital letter representing the joint from where it was exctracted:
H -> Hip joint
K -> Knee joint
A -> Ankle joint

Variables from the second day of testing are identified with an additional "_2" after the variable name.

H1 -> represents the H1 variable from day 01
H1_2 -> represent the H1 variable from day 02.

The code provided in this repository was written in R language and provides a fully reproducible analysis from this data.

Researchers are encouraged to download and run this code, check each step taken and verify the tables and figures. Improvements and suggestions are welcomed.

