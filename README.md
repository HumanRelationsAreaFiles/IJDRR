# **Datasets and R scripts for: "An ethnographic approach to the global study of the ecological dimensions of hazards"**
### **King et al., *Submitted to the International Journal of Disaster Risk Reduction***


## **Contents**
  ### **Data**
  - **DT-hz-event-level-expanded-FA.csv:** main dataset that contains all coded variables
  - **DT-meta-hz-clean.csv:** meta data file that contains SCCS ID, OWC, and eHRAF name for all societies in the sample
  - **merged_coords.csv:** geographic coordinates for each society; necessary for making the map in Figure 1
  ### **Scripts**
  - **Descriptive_masterscript.R:** principal R script that contains code and user instructions to re-create most analyses and figures
  - **hz.initialize.R:** required to initialize certain objects used in Descriptive_masterscript.R
  - **hz_msp_figure.R:** script to recreate Figure 2 (minimum spanning tree)

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
