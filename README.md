# 1433-short-project

### Inputs
`cleaned_OI_state_data.csv`: COVID-19 and the US Economy Dataset, provided with project materials\
`end_shelter.csv`: End date of shelter in place/stay at home orders for each state. Source: [COVID-19 US state policy database (CUSP)](https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1894978869), retrieved 9/20/20

### Code
`1433-short-project-script.R`: Cleans data, produces exploratory analysis, runs regressions and tests, and outputs results

### Outputs
`f1.png`: Figure 1: Mobility and Spending Trends for US States (January - August 2020)\
`t1.tex`: Table 1: OLS/Fixed Effects Regressions of Discretionary Spending on Time Away from Home\
`t2.tex`: Table 2: Instrumental Variable Regressions of Discretionary Spending on Time Away from Home\
`tests.txt`: Outputs of significance tests, relevant values included in regression tables
