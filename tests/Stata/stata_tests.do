clear 
set more off
import delimited c:/Users/alexa/Dropbox/rwolf/test.csv
set seed 1
rwolf y1 y2 y3 y4, vce(cluster group_id) cluster(group_id)  indepvar(x1) controls(x2) reps(1000) nodots
