# Map information from article and run code supplement

Currently we almost exclusively consider maps for tables that are shown in the article or supplement PDF. Regressions are treated in more detail but also other sort of tables, e.g. descriptive statistics can be mapped.

## Matching vs Map

For better structure, I try to distinguish *matchings* and *maps*:

**Matchings**: Table elements from the article like a cell or a regression can typically have several matchings. E.g. `match_reg` describes for each table column that can represent a regression, the regression commands whose coefficients and standarrd errors decently match the numbers shown on the table. But such a matching is seldom unique, there might be several regressions from the code that provide a halfway decent match to the numbers in the table. For regressions, we compute a `match_score` that measures how well the numbers related to the regression match in total.

Yet, just by matching numbers alone, we may not know exactly which match is the correct one. Sometimes there are multiple matches with the same match score, or given transcription errors it could also be the case that a regression with a lower match score is actually the correct map. It may also be the case that no matching is correct and the table actually shows descriptive statistics that happen to be shown in a similar format than regressions.   

**Maps** A map uses information from matchings and additional information from tables' or code files' layout to generate 1-1 mappings. The main maps map 1-1 table cells to outputs from the code. But we may also generate maps from a table column to a regression methodology extracted from the article's text and so on. 

A map will essentially defined for a table. A table-to-code map will partition the table's cells into several **blocks**. Cells of a block are expected to come from the same command (`runid`) or at least the same command type (`cmd`).


## Regressions

We have a special focus on regressions. When parsing the article tables, we assign a `regid` to every block that potentially could correspond to a regression.

The potential regressions are specified in `art_reg` and `art_small_reg`. 

Sometimes each row of such a regression block consists of multiple regressions (e.g. varying by their dependent variable). For an example consider aejapp_3_2_2 Table 2. Here the different rows do not correspond to different explanatory variables of a single regression but to different regressions. The table `art_small_reg` contains these *small regression* interpretations of a regression block with more than one control variables.

Which (small) regression candidates will actually be considered as regressions depends on the map. There will be some natural constraints that maps will satisfy like:

- A table has either big regressions or small regressions, but not a mix.
