### MRFInference API

#### Specifying a Markov Random Field

To initialize an MRF you need to specify the number of variables, the domain of
the variables (all variables are on the same domain), and the factors of the
distribution.

##### Specifying Variables

When you initialize an MRF with the number of variables n, the variables are
given ids 0 through n-1.

##### Specifying factors

A factor needs to have a scope, which is the variables that contribute to it,
and a feature, which is a function from the variables to a double that should
be nonnegative. The scope of a factor is specified as a vector with 