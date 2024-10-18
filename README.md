# Litigation
This is the code associated with the paper "The Effect of litigation on firm value" published in Nature Sustainability. 
The excell file contains the filing dates, decision dates the firm identifier codes (gvkey) and event categories. 
The first .do file builds up the database in Stata. Building upt the database requires company returns, which are proprietary data which we cannot provide, you can get them yourself at crsp. The file factors.dta contains the risk-free rate, market return, smb and hml factors for both the US and EU and is open source from Ken French's website. 
The second .do file converts the data into the correct shape runs the statistical analysis, and builds the tables of the paper. 
