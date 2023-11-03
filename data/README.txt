
READ ME File For 'Data_Hubot_et_al_2021.csv'

Dataset DOI: 10.5258/SOTON/D2475

Date that the file was created: Decembre, 2022

-------------------
GENERAL INFORMATION
-------------------

ReadMe Author: Nathan Hubot, University of Southampton [orcid.org/0000-0001-6917-2255]

Date of data collection: 2018 - 2020

Information about geographic location of data collection: 
Horsea Lake (UK), Rames peninsula (UK), Walvis Bay (Namibia), London Aquarium (UK)

Related projects:
Researcher Training Support Grant (RTSG number: 517191102).
COMICS project (Controls over Ocean Mesopelagic Interior Carbon Storage; NE/M020835/1) 
Newton Fund RCUK-NRF International Ph.D. Partnering Scheme
--------------------------
SHARING/ACCESS INFORMATION
-------------------------- 

Licenses/restrictions placed on the data, or limitations of reuse:
CC-BY

Recommended citation for the data:

Hubot, N. D., S. L. C. Giering, J. Füssel, J. Robidart, A. Birchill, M. Stinchcombe, C. Dumousseaud, and C. H. Lucas. 2021. Evidence of nitrification associated with globally distributed pelagic jellyfish. Limnology and Oceanography 66: 2159–2173. doi:10.1002/lno.11736

This dataset supports the publication:
AUTHORS: Hubot, N. D., S. L. C. Giering, J. Füssel, J. Robidart, A. Birchill, M. Stinchcombe, C. Dumousseaud, and C. H. Lucas
TITLE: Evidence of nitrification associated with globally distributed pelagic jellyfish
JOURNAL: Limnology and Oceanography
PAPER DOI IF KNOWN:
doi:10.1002/lno.11736

--------------------
DATA & FILE OVERVIEW
--------------------

This dataset contains:

Data_Hubot_et_al_2021.csv

--------------------------
METHODOLOGICAL INFORMATION
--------------------------

For more details see publication: Hubot et al. 2021 (https://doi.org/10.1002/lno.11736) 

The duplicate sample for ammonium was analyzed using the o-phthalaldehyde fluorometric method (Taylor et al. 2007). The ammonium measurements were performed the same day using a Turner design Trilogy fluorometer (model 7200, US) with a UV module (7200-047). The duplicate sample for nitrite, nitrate and phosphate was immediately frozen for later analysis. Frozen samples were thawed at room temperature and phosphate, nitrate and nitrite concentrations were measured using standard gas segmented continuous flow spectrophotometric techniques (QuAAtro, Seal Analytical). The baseline of the auto-analyzer was determined using the same ASW as used in the experiment (except for C. fulgida samples, for which we used ultra-high purity water as baseline; detailed descriptions of the calibrations and detection limits in Data S1).

Our hourly sampling regime, which provides a relatively low temporal resolution, was determined by the time it takes to collect the sample and the sample volume removed relative to the incubation volume. To determine the release rates at a higher temporal resolution, for one of the specimen of A. aurita incubations, nitrite and nitrate were measured at high-resolution (every 20 min) using a microfluidic lab-on-chip analyzer (Beaton et al. 2012). This novel application of lab-on-chip microfluidic analyzers allowed high-resolution measurements with small sample volumes and avoiding the need for sample storage. The nitrate and nitrite concentrations measured using the “manual” and lab-on-chip method agreed well (Data S1, Fig. V), as shown by a linear regression between the two methods (auto-analyzer = 1.04 ± 0.06; lab-on-chip = 0.15 ± 0.04; R2 = 0.98, p < 0.001, n = 8; Data S1, Fig. VI). For both techniques, gas segmented continuous flow spectrophotometric and lab-on-chip, the combined (random + systematic) analytical uncertainty associated with nitrate + nitrite and phosphate measurements was < 5% (details in Data S1; Birchill et al. 2019).

Contamination, wall effects and production/absorption by microorganisms were accounted for by subtracting the changes in concentrations observed in the ASW controls from the treatments. In order to account for the loss of liquid due to the collection of nutrient samples, the total number of moles of nutrient released at each time point was calculated using the equation: (see publication)
The rates of nutrient (ammonium, phosphate, nitrite, and nitrate) release per incubator (or per jellyfish for the jellyfish treatment) were calculated using linear regression for each replicate. The rates were then normalized by the wet weight of the jellyfish and their differences were investigated by an analysis of covariance (ANCOVA; results are presented in Data S1). The rates of nutrient release per species were calculated by averaging the rates of the replicates for each species. Finally, the differences in weight-specific rates of nutrient release caused by the differences in experimental temperatures were standardized using Q10 temperature coefficient factors from the literature. For ammonium and phosphate release, a Q10 of 3.1 was used for A. aurita (Møller and Riisgård 2007), and the general Q10 of 2.66 was used for the other jellyfish species (Ikeda 2014). For nitrite and nitrate release rates, a Q10 of 2.2 was used for all species (Zheng et al. 2017), corresponding to the temperature coefficient factor of nitrifying microorganisms. Rates were adjusted to the median temperature of the experimental conditions (16°C) and N : P ratios were calculated as the sum of ammonium, nitrite and nitrite over phosphate. The temperature-corrected nutrient production rates were plotted against the wet weight of the jellyfish, and a linear regression was fitted to investigate the allometric relationships between body weight and nutrient release rates. Finally, estimates of inorganic nitrogen release by jellyfish blooms were calculated using the allometric equations together with jellyfish densities from two case studies. The uncertainty range of these estimates were determined from the error on the allometric exponents and the temperature. All statistical analyses were carried out using R Statistical Software (R Core Team 2019).

--------------------------
DATA-SPECIFIC INFORMATION 
--------------------------

Number of variables: 11

Number of cases/rows: 334

Variable list, defining any abbreviations, units of measure, codes or symbols used:

Species (Aurelia aurita=aurita, Chrysaora hysoscella=hysoscella, Chrysaora pacifica=pacifica, Chrysaora fulgida=fumgida)
time (minutes)
WW (wet weight; grams)
BD (Bell diameter; cm)
Replicate (replicate number + time point: 1T1, 1T2, etc.)
ID (Species + Replicate)
Ammonium (µM)
Phosphate (µM)
Nitrite (µM)
Nitrate (µM)
Vol_incub (L)
   
Missing data codes: NA















