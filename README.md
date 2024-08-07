# Beaver and Nome Creeks Arctic grayling radiotelemetry, 2021--2022

## Abstract and Operational Plan

This project plan outlines a 2-year radiotelemetry study for Arctic grayling Thymallus arcticus in the Beaver Creek drainage. Information on life history, migration timing, and habitat use is needed to better understand the distribution of Arctic grayling during critical time periods and to inform a future population assessment. Radio transmitters with a 2-year operational life will be surgically implanted into 150 Arctic grayling in parts of the drainage that are most susceptible to sport fishing pressure. Aerial tracking flights to locate radiotagged Arctic grayling will be conducted during winter, spring, summer, and fall in order to identify, document, and characterize overwintering, spring spawning, and summer feeding areas. Location data will also be used to examine seasonal fidelity to these areas. Seasonal migration timing between Beaver Creek and its Nome Creek tributary will be determined with a stationary tracking station located near the confluence. Results from the radiotelemetry study will be used to select an appropriate index area and time for a mark-recapture experiment that will be conducted during summer 2023.

The Operational Plan can be accessed here: <https://www.adfg.alaska.gov/FedAidPDFs/ROP.SF.3F.2021.07.pdf>

## Repository Contents:

### R_code

All R code for analyses. Further information is provided within each file.

-   **1_beaver_cr_data.R**: Reads external data and performs most necessary data manipulation and restructuring. This script is sourced at the beginning of almost all others.

-   **2_beaver_cr_distancetables.R**: Performs all distance-related computation, and produces associated plots and tables.

-   **3_beaver_cr_DiscreteMovementPlots.R**: Produces plots and tables, considering movement between river Sections (Lower, Middle, Upper, Headwaters) and between river Designations (Mainstem, Tributaries, Headwaters), both defined in Script 1.

-   **4_beaver_cr_ByIndividual.R**: Does additional data manipulation to create a dataset organized by individual; that is, each row represents an individual fish and each row represents a different variable, all of which are aggregate measures over the duration of the study (length, homerange, etc.)

-   **5_beaver_cr_ByIndividual_analyses.R**: Visualizes the dataset created in Script 4, and producing automated plots of reasonable pairwise relationships. Also produces some plots of seasonal fidelity, as expressed in travel distance and homerange fraction.

-   **6_beaver_cr_kerneldensity.R**: Produces kernel density plots by season, as well as kernel density anomaly (difference between season and average.) Also makes a visualization that shows movement between sequential seasons as colored arrows depending on net direction of movement.

### R_data

Flat Excel files (.csv) copied from data files provided by the project biologist:

-   **all_locations.csv**: Long-format file with observations from all tracking flights

-   **seasonal_consolidation.csv**: Long-format file with an observation for each study season

-   **tagging_data.csv**: Information from tagging

R workspaces:

-   **beaver_cr_rivernetwork.Rdata**: The study area formatted as a rivernetwork for riverdist

-   **beaver_cr_rivernetwork_op.Rdata**: The study area formatted as a rivernetwork for riverdist, with some additional edits for consistency with the Operation Plan

### R_output

#### R_output/Tables

All R-generated tables, exported as flat .csv files. Note that no formatting has been done yet, but all relevant information should be present.

-   **ByIndividual.csv**: The dataset created by Script 4, containing many columns for study-long aggregate variables. Each row represents an individual fish. Columns include:

    -   Fish: Tag ID (formatted as number)
    -   Length_mm: Length measured at tagging
    -   n_surveys: Number of aerial surveys detected
    -   homerange_km: Minimum observed homerange (river km)
    -   cumuldist_km: Total cumulative distance (river km)
    -   winterwinter_km: Distance between winter locations in 2022 and 2023 (river km)
    -   springspring_km: Distance between spring locations in 2022 and 2023 (river km)
    -   summersummer_km: Distance between summer locations in 2022 and 2023 (river km)
    -   wintersection: River sections observed for winter surveys
    -   springsection: River sections observed for spring surveys
    -   summersection: River sections observed for summer surveys
    -   winterdesignation: River designation (mainstem/trib/etc) observed for winter surveys
    -   springdesignation: River designation (mainstem/trib/etc) observed for spring surveys
    -   summerdesignation: River designation (mainstem/trib/etc) observed for summer surveys
    -   mn_upstream_km: Mean distance from Yukon river confluence (river km) for all observations
    -   mn_winter_upstream_km: Mean distance from Yukon river confluence (river km) for winter
    -   mn_spring_upstream_km: Mean distance from Yukon river confluence (river km) for spring
    -   mn_summer_upstream_km: Mean distance from Yukon river confluence (river km) for summer

-   **Designation_bySeason.csv** and **Designation_bySurvey.csv**: Counts, proportion, and SE(proportion) of fish located in each river Designation (Mainstem, Tributary, Headwaters) for each Season, and for each Survey.

-   **directional_dist_byseason.csv** and **directional_dist_bysurvey.csv**: Mean, SD, and SE of Directional Distance (travel distance, defined as positive for net upstream movement and negative for net downstream movement), as well as counts and proportions of fish traveling Upstream and Downstream, for each sequential pair of Seasons, and for each sequential pair of Surveys.

-   **Fidelity_tab.csv**: A simple summary of seasonal fidelity measures, for Winter, Spring, and Summer:

    -   Median Distance (rkm) between locations observed for each seasonal pair. These are quite small, indicating strong seasonal fidelity, particularly Summer. Spring is a bit larger, suggesting greater straying during spring spawning.

    -   Proportion less than 3 rkm. 3 rkm was chosen as a very rough threshold for strong fidelity, but this can be refined.

    -   Median distance (fraction of homerange). Again, very strong fidelity in summer (median was 1.2% of homerange) and less in spring (median was 17% of homerange).

    -   Proportion with 5% of homerange. Similarly, 5% was chosen as a threshold for strong fidelity, but this can also be refined.

-   **Section_bySeason.csv** and **Section_bySurvey.csv**: Counts, proportion, and SE(proportion) of fish located in each river Section (Lower, Middle, Upper, Headwaters) for each Season, and for each Survey.

And Plots.

-   **Designation_DiscreteTS.png** and **Designation_Sankey.png**: Plots visualizing the movement of fish between river Designations (Mainstem, Tributary, Headwaters) for each Season. These could be considered companion plots to tables **Designation_bySeason.csv** and **Designation_bySurvey.csv**.

-   **Direction_Arrows.png**: A sequence of plots visualizing travel between sequential pairs of surveys. Net upstream movement is expressed as blue lines, and net downstream movement is expressed as red lines.

-   **directional_dist_byseason.png** and **directional_dist_bysurvey.png**: Boxplots visualizing directional travel distance (upstream is positive, downstream is negative) between sequential pairs of Seasons, and between sequential pairs of Surveys. These could be considered companion plots to tables **directional_dist_byseason.csv** and **directional_dist_bysurvey.csv**.

-   **EDA_1.png** through **EDA_12.png**: A sequence of exploratory data analysis plots, visualizing reasonable pairwise relationships between variables. THESE SHOULD NOT BE CONSIDERED BIOMETRICALLY ROBUST YET, and should be interpreted with a few caveats:

    -   All plots have an automatically-generated p-value in their titles, which are calculated from a simple linear regression, Anova, or $\chi^2$ test.  The assumptions of these tests have not been verified, and in some cases are obviously not met.  
    -   In many cases, X-Y scatterplots are presented that do not have a strong associated p-value, but the point distributions do show interesting stories regardless.  For example, the lengths of fish are relatively constant in the lower reaches, but exhibit a much larger range in the upper reaches.
    -   In many cases, numeric variables are displayed on the log-scale, which must be kept in mind in interpretation.  In these cases, statistical tests (the pvals in the titles) were also conducted on the log-scale.
    -   A few additional variables were created to show seasonal fidelity as a binned categorical variable.  Variable winterFid is defined by winter-winter distance, and variable winterFid2 is defined by winter-winter distance as a fraction of homerange.  These breakpoints correspond with vertical lines on **FidelityCDF.png** and **FidelityCDF_2.png**.

-   **FidelityCDF.png** and **FidelityCDF_2.png**: Visualizations of seasonal fidelity, expressed as the empirical cumulative density of seasonal distances (winter-winter, spring-spring, summer-summer), and seasonal distances as a fraction of homerange.  These figures could be considered companion to **Fidelity_tab.csv**.

-   **Kernel_density.png**: Spatial density of locations for each Season, visualized as a Gaussian kernel density.

-   **Kernel_density_NegAnomaly.png** and **Kernel_density_PosAnomaly.png**:  Alternate (and interesting!) visualizations, defined as differences between each season's kernel density and the average across seasons.  The positive anomaly is defined as positive differences (higher density than average), and the negative anomaly is defined as negative differences (lower density than average).  Negative might not be as useful, but positive really highlights areas of higher seasonal density.

-   **Section_DiscreteTS.png** and **Section_Sankey.png**: Plots visualizing the movement of fish between river Sections (Lower, Middle, Upper, Headwaters) for each Season. These could be considered companion plots to tables **Section_bySeason.csv** and **Section_bySurvey.csv**.

-   **Section_labels**: A simple plot describing how Section and Designation were defined, so these definitions can be refined if desired.
