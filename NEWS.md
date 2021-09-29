# kim 0.4.6
* Fixed a critical error with the function merge_data_tables and
merge_data_table_list

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
