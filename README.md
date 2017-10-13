# R_resp


####This package is meant to use experimental results in the form of a `data.frame` to analyze the whole subset at the same time.
 
Once dependent and random factors are given, `lmer` objects are created recursively to allow
 model comparision for each other factor, understant as response variable

Returns a `mresp` object with sets of models and summary objects that allows `print` method to give user the selected model
 for each factor, *i.e.* the significant effect of the fixed factors on the response variables.

`resp::` Team <resp@burgeon.cat>