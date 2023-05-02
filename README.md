# Contents
This repo contains code I authored or heavily adapted to:
* extract vegetation plant area density, index, and other metrics reflecting vegetation structure from pre-processed point cloud data (mostly adapted from supervisors),
* process microclimate and macroclimate data according to the the,
* create publication ready figures,

as part of my unpublished Master's thesis *Assessing the Impact of TLS-Derived Vegetation Structure on Microclimatic Variability in Taita Hills, Kenya*. Code is commented and semi-reproducible. File dependencies are not provided due to file size limitations, licensing reasons, and time constraints.

## Temperature Data
- [TMS-4 sensor readings processing pipeline](microclimate/microclimate_summary.R)
- [Weather station readings processing pipeline](macroclimate/Weather_station_data_retrieval.R)
- [ERA-5 download and processing pipeline](macroclimate/ERA5_grib_processing.R)
- [Altitudinal lapse rate calculation pipeline](macroclimate/Lapse_rate_calculation.R)
- [ERA-5 lapse rate correction pipeline](macroclimate/ERA5_lapse_rate_correction.R)
## GIS
- [TMS-4 sensor location as .kmz to (for GIS import)](locations_kmz/)
## Figures
- [Figure displaying monthly temperature ranges](figures/temperature_range/Monthly_Ranges.R)
- [Violinplot displaying temperature ranges](figures/temperature_range/Range_Violinplots.R)
- [Figure comparing average plant area density of 4 sites](figures/vertical_pad/Mean_PAD_4Tiles_Overview.R)
- [Figure displaying vertically layered PAD for one site](figures/vertical_pad/Mean_PAD_Figures_Script.R)
- [Figure comparing mean PAD of all sites and some other metric](figures/vertical_pad/Mean_PAD_Overview_Figure_Script.R)
## Linear Models
- [Correlation matrix creation pipeline](modeling/corrmat.R)
- [Linear modeling pipeline](modeling/modeling.R)
## License
- [CC-BY-4.0](https://github.com/jon-terschan/microclimate-taitahills/blob/main/LICENSE:%20CC-BY-4.0)[^1]

[^1]: Please note that the license applies only to code provided by this repo but neither the thesis's text body or other output created for the thesis.
