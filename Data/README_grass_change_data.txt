README describes the data layout for the data used in "Large contribution of woody plant expansion to recent vegetative greening of the Northern Great Plains"

Data authors: Bryce Currey, David B. McWethy, Nicholas R. Fox, E.N. Jack Brookshire


## ------------------------------------- ##
## DATA LAYOUT FOR Grass_change_data.csv ##
## ------------------------------------- ##

All climate variables are for the recent period 2000-2019.
All climate change varaibles are differenced between recent (2000-2019) and historical (1980-1999).
The numerical range of all variables can be found in the Supplemental Table 1 in "Large contribution of woody plant expansion to recent vegetative greening of the Northern Great Plains"


Variable 		Description				Units			Notes
--------		-----------				-----			-----
1. x			Longitude				Degrees			
2. y			Latitude				Degrees
3. lai			Proportional Leaf Area Index Change	Percent			
4. ndvi			Proportioanl Peak NDVI Change		Percent
5. ndvi_m		Proportional Mean NDVI Change		Percent
6. tc			Proportioanl Percent Tree Cover Change	Percent
7. tc19			2019 Percent Tree Cover			Percent
8. tc00			2000 Percent Tree Cover			Percent
9. north		Northern Aspect Ratio			-
10. east		Easter Aspect Ratio			-
11. slp			Slope					Degrees
12. elv			Elevation				Meters
13. rough		Roughness				-
14. soil		Soil Order				-
15. veg			Vegetation Classification		-			Irrelevant variable. All cells grassland 
17. pr_sp		Spring Precipitation			Milimeters
18. pr_sp_dif		Spring Precipitation Change		Milimeters
19. pr_su		Summer Precipitation			Milimeters
20. pr_su_dif		Summer Precipitation Change		Milimeters
21. pr_w		Winter Precipitation			Milimeters
22. pr_w_dif		Winter Precipitation Change		Milimeters
23. tmp_su		Summer Temperature			Celcius
24. tmp_su_dif		Summer Temperature Change		Celcius
25. tmp_w		Winter Temperature			Celcius
26. tmp_w_dif		Winter Temperature Change		Celcius
27. ndep		Nitrogen Deposition			kg N ha-1 yr-1		Year 1993
28. agdd		Aggregated Growing Degree Days		-
29. agdd_dif		Aggregated Growing Degree Days Change	-
30. cow			Cattle Density				head km-2		Year 2010
31. cv			Precipitation Coefficient of Variation	-
32. map_dm		Mean Annual Precipitation		Milimeters
33. mat_dm		Mean Annual Temperature 		Celcius
34. map_dif		Mean Annual Precipitation Change	Milimeters
35. mat_dif		Mean Annual Temperature Change		Celcius




## -------------------------------------- ##
## DATA LAYOUT FOR Grass_change_data2.csv ##
## -------------------------------------- ##

All climate variables are for the historical period 1980-1999.


Variable                Description                             Units                   Notes
--------                -----------                             -----                   -----
1. x                    Longitude                               Degrees
2. y                    Latitude                                Degrees
3. tc00                 2000 Percent Tree Cover                 Percent
4. north                Northern Aspect Ratio                   -
5. east                 Easter Aspect Ratio                     -
6. slp                  Slope                                   Degrees
7. elv                  Elevation                               Meters
8. rough                Roughness                               -
9. soil                 Soil Order                              -
10. veg                 Vegetation Classification               -                       Irrelevant variable. All cells grassland
11. fire                Fire Presence/Absence                   -                       Irrelevant variable for this dataset. Not used in analysis.
12. pr_sp               Spring Precipitation                    Milimeters              Historical (1980-1999) Period
13. pr_su               Summer Precipitation                    Milimeters              Historical (1980-1999) Period
14. pr_w                Winter Precipitation                    Milimeters              Historical (1980-1999) Period
15. tmp_su              Summer Temperature                      Celcius                 Historical (1980-1999) Period
16. tmp_w               Winter Temperature                      Celcius                 Historical (1980-1999) Period
17. ndep                Nitrogen Deposition                     kg N ha-1 yr-1          Year 1993
18. agdd                Aggregated Growing Degree Days          -                       Historical (1980-1999) Period
19. cow                 Cattle Density                          head km-2               Year 2010
20. cv                  Precipitation Coefficient of Variation  -                       Historical (1980-1999) Period
21. map                 Mean Annual Precipitation               Milimeters              Historical (1980-1999) Period
22. mat                 Mean Annual Temperature                 Celcius                 Historical (1980-1999) Period


