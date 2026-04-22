# GREENeR 1.0.2

* Memory optimization: refactored green_shares with OpenBLAS thread control to prevent memory allocation failures.
* Compatibility: updated visualization functions to comply with the new tmap v4 API (migrated to tm_scalebar).
* System requirements: set R dependency to >= 4.3 to ensure compatibility with updated spatial and numerical libraries.

# GREENeR 1.0.1

* Updated roxygen2 metadata for key functions (`create_lits_of_maps`, `evolution_plot_area`, `gr_density_plot`, and `simobs_annual_plot`) to provide clearer technical descriptions and parameter details.

# GREENeR 1.0.0

* Added some new functions, generally to summarize information
* Modified other functions to modularize code
* Parallelized the `simobs_annual_plot()` function to speed up the generation of 
the figure
* Fixed some minor bugs

# GREENeR 0.1.3

* The dependency on the hydroGOF package has been removed, since it has been 
archived in CRAN.

# GREENeR 0.1.2

* Solved warning messages in `calib_boxplot()` function.

# GREENeR 0.1.1

* Add three new functions: `read_NSdata()`, `read_geometry()` and `shreve()`.
* Change the name of the function `nut_balace()` to `region_nut_balance()`.
* Edit some typos in legend of plots.

# GREENeR 0.1.0

* a newer version may be available on https://github.com/calfarog/GREENeR
* to get started, see the package vignette "Geospatial Regression Equation for 
European Nutrient losses (GREEN)" and the help files
* if you have any questions or suggestions, please contact me (c.alfarog at 
gmail dot com)
