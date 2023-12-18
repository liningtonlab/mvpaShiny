# mvpaShiny app

This is the corresponding shiny app to the [mvpa R package](https://github.com/liningtonlab/mvpa).
For how to use the R shiny app or the underlying R package, please refer to the [online documentation](https://liningtonlab.github.io/mvpaShiny_documentation/).

# How to install and run mvpaShiny
Please install [R Studio](https://posit.co/downloads/) and [R](https://www.r-project.org/) (>= 4.2, lower versions have not been tested).
Open R Studio and type the following code into the console:

```R
if (!require("devtools", quietly = TRUE)) {
  install.packages("devtools") 
} else {
  print("Devtools has been already installed.")
}

# The mvpa R package will be installed automatically  
devtools::install_github("liningtonlab/mvpaShiny")
```

This will install the underlying mvpa package and the shiny app.

To start the app, run the following code:

```R
library(mvpaShiny)
mvpaApp()
```

# Contact
For feedback, questions or comments please contact [Roger G. Linington](mailto:rliningt@sfu.ca) or [Olav M. Kvalheim](mailto:olav.kvalheim@uib.no)

