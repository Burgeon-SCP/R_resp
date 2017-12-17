## `R::resp` To Be Developed file

- Hide and redirect warning messages after `lmer()` calculation returned by `mod.resp()`:
    - could be printed on calculation due to model inner fails
    - actual `suppressWarnings()` hides everything
    - should be added as message on "`[+] Modeling response variables..`" 

- Use `mresp.check()` to analyse `mresp` object structure:
    - Has `UserDataFrame` and `ChosenModelResponse` ?
    - Is a list of lists except the two previous dataframes ?
    - Each variable is a list of models ?
    - Has a `check_out` dataframe if checked ?
    - Has `mod.choose()` results ?
    - Has errors ?

- Create `tell()` function to improve clarity and debugging of package code.

### Abstract `glist()` creation from `factor_stats.R` and `anova.nested.data.R`:

Must be using same data structures based on tables of lists of data groups

 1. Creation of `out` list
 1. Creation of `params` list
 1. Insert `for ()` in result calculation
 1. Use each necessary function with `do.call()`
    1. may include also:
       - nested anovas
       - mixed models
       - basic factor analysis
       - peer-reviewed-type tables
       - plot images generation
