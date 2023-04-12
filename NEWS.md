# kim 0.5.222
* Added the following function(s): two_way_anova, lu
* Updated the following function(s) (and other functions that use them): 
levene_test

# kim 0.5.213
* Updated the following function(s) (and other functions that use them): 
chi_squared_test

# kim 0.5.212
* Added the following function(s): contingency_table, chi_squared_test,
geomean, mad_remove_outliers
* Updated the following function(s) (and other functions that use them): 
chi_squared_test_pairwise, odds_ratio

# kim 0.5.202
* Critical fix or improvement in the following functions (and other 
functions that use them): z_score*
* Highlights: Replaced the histogram function and replaced it
with the function histogram_from_hist. Now executing the histogram 
function will execute the histogram_from_hist function
* Added the following function(s): odds_ratio, log_odds_ratio, 
log_odds_ratio_to_d, cohen_d_to_r, weighted_mean_effect_size,
var_of_log_odds_ratio_to_var_of_d
* Updated the following function(s) (and other functions that use them): 
forest_plot, cohen_d_borenstein, score_scale_items, z_score
* Fixed bugs or errors in the following functions (and other functions
that use them): cohen_d_borenstein, floodlight_2_by_continuous
* Undocumented functions added (see the function `und`):

# kim 0.5.138
* Highlights: Fixed the logistic regression section for
spotlight_2_by_continuous
* Added the following function(s): z_score, standardize
* Updated the following function(s) (and other functions that use them): 
spotlight_2_by_continuous, compare_groups, desc_stats
* Fixed bugs or errors in the following functions (and other functions
that use them): spotlight_2_by_continuous
* Critical improvement in the following functions (and other functions
that use them): 
* Undocumented functions added (see the function `und`):

# kim 0.5.73
* Highlights:
* Added the following function(s): histogram_from_hist
* Updated the following function(s) (and other functions that use them): 
parallel_analysis, und, parallel_analysis, score_scale_items,
histogram_from_hist
* Fixed bugs or errors in the following functions (and other functions
that use them): parallel_analysis
compare_dependent_rs
* Critical improvement in the following functions (and other functions
that use them): 
* Undocumented functions added (see the function `und`):

# kim 0.5.55
* Highlights:
* The package title was changed from 
"Behavioral Scientists' Analysis Toolkit" to 
"A Toolkit for Behavioral Scientists"
* Added the following function(s): loglinear_analysis, identical_all,
akaike_weights, modes_of_objects, check_modes, overlapping_interval,
standardized_regression
* Updated the following function(s) (and other functions that use them): 
forest_plot, t_test_pairwise, compare_groups, regex_match,
install_all_dependencies, compare_datasets, read_csv,
floodlight_2_by_continuous, parallel_analysis, ggsave_quick,
akaike_weights, scatterplot, spotlight_2_by_continuous
* Fixed bugs or errors in the following functions (and other functions
that use them): repeated_measures_anova, mann_whitney,
compare_datasets, compare_groups
* Critical improvement in the following functions (and other functions
that use them): 
* Undocumented functions added (see the function `und`): 
compare_strings, list_functions, mad_rm

# kim 0.5.3
* Highlights: addition of und, repeated_measures_anova
* Added the following function(s): vlookup, convert_cols_to_numeric,
convert_to_excel_formula, spotlight_2_by_continuous,
compare_independent_rs, spotlight_overlaid, forest_plot,
noncentrality_parameter, compare_effect_sizes, bracket, und, p0,
cohen_d_over_n
* Updated the following function(s) (and other functions that use them): 
chi_squared_test_pairwise, compare_groups, desc_stats_by_group, 
histogram_by_group, id_across_datasets, multiple_regression,
replace_values_in_dt, tv, weighted_mean_r, spotlight_2_by_continuous,
cohen_d, scatterplot, regex_match, t_test_pairwise
* Fixed bugs or errors in the following functions (and other functions
that use them): round_flexibly, outlier, multiple_regression, 
pretty_round_p_value, logistic_regression, spotlight_2_by_continuous,
id_across_datasets
* Critical improvement in the following functions (and other functions
that use them): scatterplot
* Undocumented functions added (see the function `und`): outlier_rm,
round_t0

# kim 0.4.22
* Added function(s) including simple_effects_analysis.

# kim 0.4.21
* Fixed a critical error with the function merge_data_tables and
merge_data_table_list
* Plotting examples are now set to be skipped in the package testing 
phase (to avoid producing NOTEs that prevent the package from being 
published on CRAN)

# kim 0.3.99
* Added function(s) including pivot_table, outlier, 
logistic_reg_w_interaction, pm, compare_dependent_rs,
logistic_regression_table, logistic_regression, replace_values_in_dt,
pretty_round_r, fisher_z_transform, z_to_r_transform, 
weighted_z, weighted_mean_r, duplicated_values, tv, remove_from_vector,
and combine_data_across_cols.
* Fixed the error in function name(s): 
chi_square_test_pairwise -> chi_squared_test_pairwise
* Updated function(s) including clean_data_from_qualtrics,
su, find_duplicates, scatterplot, chi_square_test_pairwise, 
two_way_anova, pretty_round_p_value, combine_data_across_cols, 
floodlight_2_by_continuous, remove_from_vector, and prep.
* Fixed minor bugs

# kim 0.3.13
* In response to CRAN Team's review, the package title was edited.
* In response to a new error message from CRAN check 
(r-devel-windows-x86_64-gcc10-UCRT), most dependencies were removed.

# kim 0.3.11
* Removed dependencies on most packages. In previous versions of 
Package 'kim', all dependencies (packages) required to run all functions
in Package 'kim' were installed when Package 'kim' was installed for the 
first time. In contrast, with Version 0.3.11, Package 'kim' will now ask 
users to install dependencies themselves if the functions they are using 
require such dependencies.
* Fixed bugs and added new function(s) including install_all_dependencies.

# kim 0.2.207
* Fixed bugs and added new function(s) including su and round_flexibly.

# kim 0.2.204
* Fixed bugs and added new function(s) including change_var_names, 
check_req_pkg, skewness, and kurtosis.
* Continued to remove dependencies on other packages.

# kim 0.2.172
* Fixed bugs, updated function documentations, and added new function(s) 
including setwd_to_active_doc.
* Deleted “LazyData: true” from DESCRIPTION.
* Started removing dependencies on other packages.

# kim 0.2.133
* Fixed bugs, and added new function(s) including id_across_datasets.

# kim 0.2.96
* Fixed bugs, and added function(s) including floodlight_2_by_continuous,
merge_data_tables, and order_rows_specifically_in_dt.

# kim 0.2.71
* Fixed bugs, removed dependency for dplyr, and added 
function(s), including write_csv, print_loop_progress, 
histogram_w_outlier_bin, find_duplicates, and start_kim.

# kim 0.2.38
* Added functions including read_sole_csv, read_csv, ggsave_quick.

# kim 0.2.35
* Added functions including clean_data_from_qualtrics and compare_datasets.

# kim 0.2.25
* Added the histogram function and updated code using CodeFactor.

# kim 0.2.20
* Added crucial functions including prep and update_kim.

# kim 0.1.11
* Following the suggestion by Ms. Julia Haider, I wrapped the scatterplot
function's examples in "\donttest{}" as the execution time was greater 
than 5 seconds on linux.
* I also removed a reference that was not essential.

# kim 0.1.10
* Wrapped the scatterplot function's examples in "dontrun" to avoid going over
5 seconds when testing in a linux system.

# kim 0.1.9
* added the histogram_by_group function.

# kim 0.1.8
* added the scatterplot function.

# kim 0.1.7.9101
* debugged the plot_group_means function and added 
desc_stats_by_group function.

# kim 0.1.7
* Edited the code with lintr.

# kim 0.1.6
* Added citation.

# kim 0.1.5
* Added more functions including tabulate_vector.

# kim 0.1.4
* Added more functions including two_way_anova.

# kim 0.1.3
* Added a `NEWS.md` file to track changes to the package.
