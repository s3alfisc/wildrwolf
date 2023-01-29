clear 
set more off
import delimited C:/Users/alexa/Dropbox/wildrwolf/tests/data.csv
set seed 1

rwolf y1 y2 y3 y4, indepvar(x1) reps(9999)
