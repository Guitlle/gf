import pandas as pd
import numpy as np
import geopandas as gp

munisGT = pd.read_csv("../../Covariates and Other Data/Demographics/Guatemala_Municipios_IGN2017_worldpop2010-2012-2015.csv", index_col = None)
# There are two municipalities that are actually one. To handle this unique case I am making the
# following groupby.
munisGT = munisGT.groupby("COD_MUNI__").agg({
    "NOMBRE__": "first",
    "COD_DEPT__": "first",
    "DEPTO__": "first",
    "AREA_KM__": "sum",
    "Poblacion2010": "sum",
    "Poblacion2012": "sum",
    "Poblacion2015": "sum"
}).reset_index()

splitted_municipalities = pd.DataFrame(data = {
    "parent_code": [1322,507,1002,1705,1708,1901,1218],
    "new_code": [1333,514,1021,1713,1714,1911,1230],
    "year_of_split": [2015,2015,2014,2011,2014,2014,2014]
})

munisGT.rename(columns = {
    "NOMBRE__": "name",
    "COD_DEPT__": "deptocode",
    "DEPTO__": "depto",
    "AREA_KM__": "area",
    "COD_MUNI__": "municode"
}, inplace = True)
munisGT = munisGT.merge(splitted_municipalities, 
                        left_on = "municode", right_on = "new_code", how="outer")
munisGT["parent_code"] = munisGT.apply(lambda x: x.municode if np.isnan(x.parent_code) else x.parent_code, axis="columns")

munisGT_2009 = munisGT.groupby("parent_code").agg({
    "name": "first",
    "deptocode": "first",
    "depto": "first",
    "area": "sum",
    "Poblacion2010": "sum",
    "Poblacion2012": "sum",
    "Poblacion2015": "sum"
}).reset_index()
munisGT_2009.rename(columns = {
    "parent_code": "municode"
}, inplace = True)

deptosGTshp = gp.read_file("../../Covariates and Other Data/GIS/GT-IGN-cartografia_basica-Departamentos.geojson")
deptosGTshp = deptosGTshp[lambda x: x.CODIGO.astype(int) <= 2200]

munisGTshp = gp.read_file("../../Covariates and Other Data/GIS/GT-IGN-cartografia_basica-Division politica Administrativa (Municipios).geojson")
