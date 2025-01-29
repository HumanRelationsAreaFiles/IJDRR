# **Supplement to: "An ethnographic approach to the global study of the ecological dimensions of hazards"**
#     **King et al. 2025, *International Journal of Disaster Risk Reduction* (under review)**


## **Contents**
  ### **Data**
  - **DT-hz-event-level-expanded-FA.csv:** this is the main data file, containing all coded variables necessary to run all scripts
  - **DT-meta-hz-clean.csv:** the meta data file, containing the SCCS ID, OWC term and eHRAF name for all societies in the sample
  - **merged_coords.csv:** contains the coordinates for each society; necessary for making the map in Figure 1
  ### **Scripts**
  - **Descriptive_masterscript.R:** principal R script, containing code and user instructions to re-create all analyses and figures
  - **hz.initialize.R:** required to initialize certain objects used in Descriptive_masterscript.R
  - **hz_msp_figure.R:** to recreate the minimum spanning tree in Figure 2

## **Requirements**
To run the R scripts, you will need the following:
- **R ([minimum version]):** 4.4.1
- **Required R packages:**
    - dplyr
    - ggplot2
    - rnaturalearth
    - rnaturalearthdata
    - Hmisc
    - viridis
    - scales
    - igraph
    - tidyr
    - readxl
    - tidyverse
    - reshape2
    - corrplot
    - fauxnaif
    - naniar
    - ggpubr
    - gridExtra
    - cowplot
    - sf
    - janitor
    - DSL
    - pastecs
  
**You can install the required packages using:**
```r
install.packages(c("dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata", "Hmisc", "viridis", "scales", "igraph", "tidyr", "readxl", "tidyverse", "reshape2", "corrplot", "fauxnaif", "naniar", "ggpubr", "gridExtra", "cowplot", "sf", "janitor", "DSL", "pastecs"))
```

## **Contact**
Contact samantha.king@yale.edu or seb.wanggaouette@yale.edu with questions. 
