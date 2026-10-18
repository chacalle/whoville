# whoville 0.2.0

* Updated `countries` object, mostly from the WHO xMart REFMART tables:
  * IDN WHO region changed from SEAR to WPR.
  * World Bank income groups up to FY27 (`wb_ig_2021` to `wb_ig_2025`).
  * GBD columns from the GBD 2023 location hierarchy.
  * Name updates (e.g. Nauru is now 'Naoero').
  * All changes are listed in `data-raw/countries_diff_against_v0.1.7.xlsx`.
* `make_countries()` makes an updated `countries` object from the current
  sources and logs the version of each input. All helper functions can use it
  with the `whoville.option.countries` option or `countries_manual` argument.
* `compare_countries()` saves the differences between two `countries` objects
  to an xlsx file.
* Breaking changes:
  * Alternative and former name columns removed, so old names (e.g. "Turkey",
    "Swaziland") no longer match exactly.
  * `gbd_high_income` column removed, `is_gbd_high_income()` now uses the GBD
    'High-income' region (same 36 countries).
  * Column and row order of `countries` changed.
* Bug fixes: `valid_codes()` ignored `type`, and exact name matching failed with
  stringdist 0.9.17.

# whoville 0.1.7

* IHME's GBD codes and regional classifications added to allow conversion between ISO3 and GBD codes, as well as use of
the GBD regions in analyses.

# whoville 0.1.6

* Minor change to accommodate new formatting for the World Bank's historical classification by income for countries.

# whoville 0.1.5

* Added UN intermediate regions, UN DESA regions and subregions, and SDG regions
and subregions.

# whoville 0.1.4

* Small island developing states (SIDS) and land-locked developing countries (LLDC)
    classifications added to `countries`.
* Additional alt names from official ISO3 short names added.

# whoville 0.1.3

* UN region and subregion names added in for every language, which can be accessed
    using `iso3_to_regions()`.
* OECD membership added, usable through `is_oecd_member()` and `oecd_member_states().
* GBD 2019 high-income countries added, usable through `is_gbd_high_income()` and
    `gbd_high_income_states()`.
* UN least-developed country status added, usable through `is_un_ldc()` and 
    `un_ldcs()`.

# whotilities 0.1.2

* Country data updated to use WHO xMart4 REF_MART data. This has created changes across all functions and in the package.
* WHO formal and short names now available in all languages
* Reduced the number of country codes available to ISO3, ISO2, ISO (numeric), WHO code, and M49
* Increased numbers of countries to 248
* Accessed World Bank Income Group data directly from World Bank website, now have income group classifications for 1987 - 2019 (corresponds to fiscal year 1989 - 2021)

# whotilities 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Added `who_member_states()` to quickly extract ISO3 codes for all WHO member states

# whotilities 0.1.0

* Initial release of package
* Publication of `countries` dataframe with codes, names, and regional categorizations
* Added functions to coerce names to codes (and vice-versa) released
* Added functions to get key country information using ISO3 codes
