## resp to be developed file

- hide and redirect warning messages after lmer() calculation returned by mod.resp():
    - could be printed on calculation due to model inner fails
    - actual suppressWarnings() hides everything
    - should be added as message on "[+] Modeling response variables.." 

- mresp.check() to analyse mresp object structure:
    - has UserDataFrame and ChosenModelResponse
    - is a list of list execpt the two previous dataframes
    - each variable is a list of models [with a 'check_out' dataframe if checked]
    - has mod.choose() results?
    - has errors?

- create tell() function to improve clarity and debugging of package code.