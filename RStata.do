set more off

capture noisily {
/*RSTATA: cut me here*/
clear 
set more off
import delimited C:/Users/alexa/Dropbox/wildrwolf/tests/data.csv
set seed 1

rwolf y1 y2 y3 y4 y5 y6 y7 y8 y9 y10, indepvar(treatment)  reps(9999) 
//rwolf y1 y2 y3 y4, vce(cluster group_id) cluster(group_id)  indepvar(x1) controls(x2) reps(1000) nodots
/*RSTATA: cut me here*/
} /* end capture noisily */
exit, clear STATA
