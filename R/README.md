# Measuring Implicit Motives With the Picture Story Exercise (PSE): Databases of Expert-Coded German Stories, Pictures, and Updated Picture Norms.

This repository stores two openly available databases and the fully reproducible R code for the paper Schönbrodt et al. (2018): **Measuring Implicit Motives With the Picture Story Exercise (PSE): Databases of Expert-Coded German Stories, Pictures, and Updated Picture Norms.** (for full citation, see below).

The publication comes along with two databases:

- A database of expert-coded German PSE stories. These stories have been coded with David Winter's 1994 "Manual for scoring motive imagery in running text" for the presence of implicit affiliation/intimacy, achievement, and power motives. For downloading the database, go to [Version history](#version-history) and select the version you like to download as csv or RData file. The folder [Database releases](/Database%20Releases) contains all database files as csv and as RData files.
- A database of classic and new PSE picture stimuli. These files are stored on Open Science Framework: ([https://osf.io/dj8g9/](https://osf.io/dj8g9/))

# License and citation of the databases

Both the database on coded PSE stories and the picture database ([https://osf.io/pqckn/](https://osf.io/pqckn/)) can be downloaded and reused freely under a CC-BY 4.0 license. 

Please cite this publication if you use either database in your work (full citation and BibTex are given at the end of this document): 

Schönbrodt, F. D., Hagemeyer, B., Brandstätter, V., Czikmantori, T., Gröpel, P., Hennecke, M., ..., Schultheiss, O. C. (2018). Measuring Implicit Motives With the Picture Story Exercise (PSE): Databases of Expert-Coded German Stories, Pictures, and Updated Picture Norms.

As we expect that the PSE database will grow over time, we put a version number on it and archive old versions. **We urge researchers to always refer to the specific version number when the database is used in order to ensure reproducibility.**

# Version history of PSE database

- Version 0.1: PSE database submitted for peer review (n = 152,908 sentences, nested in 21,941 stories provided by 3,832 participants)
	- [Download CSV](/Database%20Releases/PSE_0.1.csv) - MD5 checksum: 68dc6b9f5a6b1577b2fa2fb95a88de4e
	- [Download RData](/Database%20Releases/PSE_0.1.RData) - MD5 checksum: 7b1d1ce62498402f9b176d7573ec53a3


# Codebook of the PSE story database

|Variable name |Data type |Comment                                                                                                                                                                                                                  |Values                                                 |
|:-------------|:---------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------------------------------------------------|
|study_ID      |factor    |Identifier for the original study/data set                                                                                                                                                                               |                                                       |
|coding_lab    |factor    |Lab where the coders were trained                                                                                                                                                                                        |Munich, Erlangen, Osnabrueck, Trier                    |
|scoring_type  |factor    |Second sentence rule applied?                                                                                                                                                                                            |eachSentence, 2nd_sentence_rule                        |
|person_ID     |factor    |Unique person identifier                                                                                                                                                                                                 |                                                       |
|USID          |factor    |Unique story identifier                                                                                                                                                                                                  |                                                       |
|UTID          |factor    |Unique text identifier (each sentence is one 'text')                                                                                                                                                                     |                                                       |
|pic_ID        |factor    |Unique picture identifier                                                                                                                                                                                                |See https://osf.io/pqckn/                              |
|pic_position  |numeric   |Position of picture in PSE task. The number encodes the picture position of valid stories, and not the position of the presented picture (e.g., if the first story was empty, the second picture gets the position `1'). |                                                       |
|pic_order     |factor    |Picture order in PSE task fixed for all participants, or variable?                                                                                                                                                       |fixed, variable                                        |
|unit          |numeric   |Sentence number within each story                                                                                                                                                                                        |                                                       |
|wc            |numeric   |Word count (at sentence level)                                                                                                                                                                                           |                                                       |
|sc            |numeric   |Sentence count (at story level)                                                                                                                                                                                          |                                                       |
|pow           |numeric   |Presence of power imagery                                                                                                                                                                                                |0 (absent) or 1 (present)                              |
|ach           |numeric   |Presence of achievement imagery                                                                                                                                                                                          |0 (absent) or 1 (present)                              |
|aff           |numeric   |Presence of affiliation/intimacy imagery                                                                                                                                                                                 |0 (absent) or 1 (present)                              |
|motclass      |factor    |Multiclass combination of aff, ach, and pow codings. All mixed codings are collapsed into the category 'mixed'.                                                                                                          |none, ach, aff, pow, mixed                             |
|motclassfull  |factor    |Multiclass combination of aff, ach, and pow codings with all possible combinations.                                                                                                                                      |none, ach, aff, pow, achpow, affach, affpow, affachpow |
|text          |character |The text of the sentence.                                                                                                                                                                                                |                                                       |

Note that in study *MK3* there was a longer break between pictures 1--4 and 5--8.

# How to reproduce the analyses for the paper

The R code accesses the PSE database and computes all descriptive and inferential statistics reported in the paper.

- subfolder [/data](/data) contains the current PSE database (without the version number suffix), and the codebook table. You can copy any database version in [/Database%20Releases](/Database Releases) into that subfolder, remove the version suffix, and compute the paper's results.
- subfolder [/cache](/cache) stores intermediate data objects generated by the scripts. These can safely be deleted; they are useful when you directly start with a later script without computing all previous steps. No cache invalidation check is performed, so take care with cached objects.
- subfolder [/export](/export) stores exported summary files (currently only the picture pull norm table as Excel file)
- Scripts are numbered in the order they should be executed. R scripts 0-... to 5-... reproduce all reported results in the paper. R scripts 6-... to 8-... have some additional exploratory analyses.


# Full Reference

Schönbrodt, F. D., Hagemeyer, B., Brandstätter, V., Czikmantori, T., Gröpel, P., Hennecke, M., Janson, K., Kemper, N., Köllner, M., Kopp, P. M., Mojzisch, A., Müller-Hotop, R., Prüfer, J., Quirin, M., Scheidemann, B., Schiestel, L., Schulz-Hardt, S., Sust, L., Zygar, C., Schultheiss, O. C. (2018). *Measuring Implicit Motives With the Picture Story Exercise (PSE): Databases of Expert-Coded German Stories, Pictures, and Updated Picture Norms.*

TODO: Add BibTex