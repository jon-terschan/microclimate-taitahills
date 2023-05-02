# Contents
This repo contains code I (re)-wrote to:
* extract structural metrics from pre-processed point cloud data (mostly adapted from supervisors),
* process microclimate and macroclimate data,
* create figures,
* 
as part of my unpublished Master's thesis *Assessing the Impact of TLS-Derived Vegetation Structure on Microclimatic Variability in Taita Hills, Kenya*. Code is commented and semi-reproducible. File dependencies are not provided due to file size limitations and time constraints.

Take care that the licensing agreement only refers to 

## Temperature Data
- [TMS-4 sensor readings processing pipeline](microclimate/microclimate_summary.R)
- [Weather station readings processing pipeline](macroclimate/Weather_station_data_retrieval.R)
- [ERA-5 download and processing pipeline](macroclimate/ERA5_grib_processing.R)
- [Altitudinal lapse rate calculation pipeline](macroclimate/Lapse_rate_calculation.R)
- [ERA-5 lapse rate correction pipeline](macroclimate/ERA5_lapse_rate_correction.R)
## GIS
- [TMS-4 sensor location as .kmz to (for GIS import)](locations_kmz/)
## License
- [TMS-4 sensor location as .kmz to (for GIS import)](locations_kmz/)[^1]


[^1]: Please note that the licensing agreement concerns only code provided in this repo and neither the thesis itself nor other output I created for my thesis.
