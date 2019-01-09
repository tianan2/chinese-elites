************************************************************************
* A program that exports a matrix of predicted margins after "emargins"*
************************************************************************
*Junyan Jiang
*last updated 01/19/2014
capture program drop expmat
program expmat
args name
capture drop  beta se lb95 ub95 lb90 ub90 rn
matrix temp=r(table)' //call the stored results from emargins
matrix K=temp[1...,1..2],temp[1...,5..6]
matrix Z= K, K[1...,1]-1.644854*K[1...,2], K[1...,1]+1.644854*K[1...,2]
matrix colnames Z= beta se lb95 ub95 lb90 ub90
matrix list Z
svmat2 Z, names(col) r(rn)
outsheet beta se lb95 ub95 lb90 ub90 rn using `name'.csv, comma replace
drop beta se lb95 ub95 lb90 ub90 rn
di ""
di ""
di "a csv file has been written to `name'.csv"
end
