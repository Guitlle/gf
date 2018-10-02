from GT_load_data import munisGT, munisGT_2009
import numpy as np
# ----------Population-------------------
# After doing some math with the exponential growth equation, I have got these:
# P_0 = (A-B)/(d*A) ^ ( Y_A / (Y_A-1) )
# k = log((A - B) / P_0) / d
# Where P_0 and k are the population at Year 0 and the coefficient of exponential growth, respectively.
# A is the population at year Y_A and B is the population at year Y_B. We assume that Y_A > Y_B
# The following functions add parameters columns to a population by municipality database
def parameterizePopModel(munisGT):
    munisGT["P_10_12"] = np.power(np.power(munisGT.Poblacion2010,1/2010)\
                          .divide(np.power(munisGT.Poblacion2012,1/2012)), 2010*2012/(2012-2010)) 
    munisGT["P_12_15"] = np.power(np.power(munisGT.Poblacion2012,1/2012)\
                          .divide(np.power(munisGT.Poblacion2015,1/2015)), 2015*2012/(2015-2012)) 
    munisGT["k_10_12"] = np.log(munisGT.Poblacion2012.divide(munisGT.P_10_12))/2012 
    munisGT["k_12_15"] = np.log(munisGT.Poblacion2015.divide(munisGT.P_12_15))/2015

# Setup parameters for population projections
parameterizePopModel(munisGT)
parameterizePopModel(munisGT_2009)

# This function accepts a code (or list of codes), a year (or list of years) and a flag that
# indicates whether it uses the 2009 version of the municipalities or the latest.
def GTMuniPopulation(code, year, _2009 = False):
    if _2009:
        popData = munisGT_2009 
    else:
        popData = munisGT
    if year > 2012:
        temp = popData.P_12_15.multiply(np.exp(popData.k_12_15.multiply(year)))
    else:
        temp = popData.P_10_12.multiply(np.exp(popData.k_10_12.multiply(year)))
    return pd.DataFrame(index=munisGT.municode, data = {"population_projection": b.values}).loc[code]
        
# TEST:
# GTMuniPopulation([101,102,103], 2008)
#    municode population_projection	
#    101      897606.200200
#    102      87706.381533
#    103      67726.392444

