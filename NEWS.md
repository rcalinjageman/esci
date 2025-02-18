esci version 1.0.7 (Release data: 02/18/2025)
===========
Changes:
* Improved analysis of ordinal data in jamovi
* Fixed error with odds ratio for estimate_pdiff_two and estimate_pdiff_ind_contrast
* Small tweaks to outputs in jamovi and R


esci version 1.0.6 (Release data: 12/18/2024)
===========

Changes:
* Updated estimate_mdiff_2x2_mixed to return smds and medians (if raw data passed)
* Updated jamovi 2x2 mixed for smds and medians
* Tests updated to use expect_snapshot (why did no one tell me?)
* Updates to deal with statpsych 1.7
* Changed dependency on ggh4x to replacement packaged legendry, thanks to Teun
* data_effronraj_badnews updated to v1.1, fixed invalid negative value in
in synthetic data for participant 46.


esci version 1.0.5 (Release data: September 2024)
===========

Changes:

* Fixed bug in estimate_mdiff_paired where negative
values where causing an error due to changes in statpsych ci.ratio.median.ps


esci version 1.0.4 (Release data: September 2024)
===========

Changes:

* Fixed bug in estimate_mdiff_two and estimate_mdiff_ind_contrast where negative
values where causing an error due to changes in statpsych ci.ratio.median2
* Fixed bug in chi square table for jamovi due to changes in R format function
and also with setting of table note in jamovi
* Removed vestigial marker_size control from describe feature in jamovi
* Fixed issue with difference axis symbols not rendering properly under newer
versions of ggplot
* Fixed issue with negative values flag in estimate_mdiff_two in jamovi


esci version 1.0.3 (Release data: July 2024)
===========

Changes:

* Tweaks to deal with changes in statpsych 1.6. No substantive updates.  



esci version 1.0.2 (Release data: March 2024)
===========

Changes:

* First release to CRAN. Module is now complete, documented, and has relatively
complete test coverage.  This is still a pretty rough attempt, though.  Expect 
breaking changes still to come, especially with graphing functions.
