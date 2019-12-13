# Measuring Implicit Motives With the Picture Story Exercise (PSE): Databases of Expert-Coded German Stories, Pictures, and Updated Picture Norms.

This repository shares two openly available databases and the fully reproducible R code for the paper Schönbrodt et al. (2018): **Measuring Implicit Motives With the Picture Story Exercise (PSE): Databases of Expert-Coded German Stories, Pictures, and Updated Picture Norms.** (for full citation, see below).

The publication comes along with two databases:

- A database of expert-coded German PSE stories. These stories have been coded with David Winter's 1994 "Manual for scoring motive imagery in running text" for the presence of implicit affiliation/intimacy, achievement, and power motives. For downloading the database, go to [PSE database releases](#pse-database-releases) and select the version you like to download as tsv (tab-separated values) or RData file. The folder [database_releases](/R/database_releases) contains all database files as tsv and as RData files.
- A database of classic and new PSE picture stimuli. These files are stored on Open Science Framework: ([https://osf.io/dj8g9/](https://osf.io/dj8g9/))

# License and citation of the databases

Both the database on coded PSE stories and the picture database ([https://osf.io/pqckn/](https://osf.io/pqckn/)) can be downloaded and reused freely under a CC-BY 4.0 license. 

Please cite this publication if you use either database in your work ([BibTex](CITATION.bib)): 

Schönbrodt, F. D., Hagemeyer, B., Brandstätter, V., Czikmantori, T., Gröpel, P., Hennecke, M., Israel, L. S. F., Janson, K., Kemper, N., Köllner, M., Kopp, P. M., Mojzisch, A., Müller-Hotop, R., Prüfer, J., Quirin, M., Scheidemann, B., Schiestel, L., Schulz-Hardt, S., Sust, L., Zygar, C., Schultheiss, O. C. (2019). Measuring implicit motives with the Picture Story Exercise (PSE): Databases of expert-coded german stories, pictures, and updated picture norms.

As we expect that the PSE database will grow over time, we put a version number on it and archive old versions. **We urge researchers to always refer to the specific version number when the database is used in order to ensure reproducibility.**

# PSE database releases

- Version 0.1: PSE database submitted for peer review (n = 152,908 sentences, nested in 21,941 stories provided by 3,832 participants) - this dataset version is not provided here
- Version 0.2: PSE database for first revision (n = 183,415 sentences, nested in 26,389 stories provided by 4,570 participants)
	- [Download TSV file](/R/database_releases/PSE_0.2_redacted_data.tsv) - MD5 checksum: 3cbca6ab485f4b45331237800d0c426b
	- [Download RData file](/R/database_releases/PSE_0.2_redacted_data.RData) - MD5 checksum: 717919a9886f0877a9c1605783b8582e


# Codebook of the PSE story database

|Variable name  |Data type |Comment                                                                                                                                                                                                                  |Values                                                          |
|:--------------|:---------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------|
|row_id         |numeric   |Unique row id                                                                                                                                                                                                            |                                                                |
|study_id       |factor    |Identifier for the original study/data set                                                                                                                                                                               |                                                                |
|coding_lab     |factor    |Lab where the coders were trained                                                                                                                                                                                        |Munich, Erlangen, Osnabrueck, Trier                             |
|scoring_type   |factor    |Second sentence rule applied?                                                                                                                                                                                            |eachSentence, 2nd_sentence_rule                                 |
|participant_id |factor    |Unique person identifier                                                                                                                                                                                                 |                                                                |
|gender         |factor    |Gender                                                                                                                                                                                                                   |m = male, f = female, NA = missing/other                        |
|age            |factor    |Age category                                                                                                                                                                                                             |age <= 25, 25 < age <= 35, 35 < age <= 45, 45 < age <= 55, age > 55 |
|USID           |factor    |Unique story identifier                                                                                                                                                                                                  |                                                                |
|UTID           |factor    |Unique text identifier (each sentence is one 'text')                                                                                                                                                                     |                                                                |
|pic_id         |factor    |Unique picture identifier                                                                                                                                                                                                |See https://osf.io/pqckn/                                       |
|pic_position   |numeric   |Position of picture in PSE task. The number encodes the picture position of valid stories, and not the position of the presented picture (e.g., if the first story was empty, the second picture gets the position `1'). |                                                                |
|pic_order      |factor    |Picture order in PSE task fixed for all participants, or variable?                                                                                                                                                       |fixed, variable                                                 |
|unit           |numeric   |Sentence number within each story                                                                                                                                                                                        |                                                                |
|wc             |numeric   |Word count (at sentence level)                                                                                                                                                                                           |                                                                |
|sc             |numeric   |Sentence count (at story level)                                                                                                                                                                                          |                                                                |
|pow            |numeric   |Presence of power imagery                                                                                                                                                                                                |0 (absent) or 1 (present)                                       |
|ach            |numeric   |Presence of achievement imagery                                                                                                                                                                                          |0 (absent) or 1 (present)                                       |
|aff            |numeric   |Presence of affiliation/intimacy imagery                                                                                                                                                                                 |0 (absent) or 1 (present)                                       |
|motclass       |factor    |Multiclass combination of aff, ach, and pow codings. All mixed codings are collapsed into the category 'mixed'.                                                                                                          |none, ach, aff, pow, mixed                                      |
|motclassfull   |factor    |Multiclass combination of aff, ach, and pow codings with all possible combinations.                                                                                                                                      |none, ach, aff, pow, achpow, affach, affpow, affachpow          |
|text           |character |The text of the sentence (spell-checked).                                                                                                                                                                                |                                                                |
|text_original  |character |The orginal text of the sentence, as provided by the participants.                                                                                                                                                       |                                                                |
|holdout        |logical   |Part of hold out set? (For future machine learning purposes)                                                                                                                                                             |TRUE / FALSE                                                    |

Note that in study *MK3* there was a longer break between pictures 1--4 and 5--8.

# How to reproduce the analyses for the paper

The R code in folder [/R](/R) accesses the PSE database and computes all descriptive and inferential statistics reported in the paper. The folder and data file structure follows the [Psych-DS](https://github.com/psych-ds/psych-DS) standard.

- subfolders [/raw_data](/R/raw_data) and [/Database%20Releases](/R/database_releases) contain the current PSE database, and the codebook table.
- subfolder [/processed_data](/R/cache) stores intermediate data objects generated by the scripts. These can safely be deleted; they are useful when you directly start with a later script without computing all previous steps. No cache invalidation check is performed, so take care with cached objects.
- subfolder [/export](/R/export) stores exported summary files (currently only the picture pull norm table as Excel file)
- Scripts are numbered in the order they should be executed. Source [1-start.R](/R/1-start.R) to load all necessary libraries and the data file. R scripts 2-... to 7-... reproduce all reported results in the paper. R scripts 8-... to 10-... have some additional exploratory analyses.

The file [AMC-Database.Rnw](/Manuscript/AMC-Database.Rnw) in folder [Manuscript](/Manuscript) computes the reproducible manuscript.

# Full Reference

Schönbrodt, F. D., Hagemeyer, B., Brandstätter, V., Czikmantori, T., Gröpel, P., Hennecke, M., Israel, L. S. F., Janson, K., Kemper, N., Köllner, M., Kopp, P. M., Mojzisch, A., Müller-Hotop, R., Prüfer, J., Quirin, M., Scheidemann, B., Schiestel, L., Schulz-Hardt, S., Sust, L., Zygar, C., Schultheiss, O. C. (2018). Measuring Implicit Motives With the Picture Story Exercise (PSE): Databases of Expert-Coded German Stories, Pictures, and Updated Picture Norms.

