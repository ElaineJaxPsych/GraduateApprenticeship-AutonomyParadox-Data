# Graduate Apprenticeship Autonomy Paradox Analysis

Analysis code for "The Autonomy Paradox: Discretionary Learning in Graduate Apprenticeship Programmes as a Catalyst for Business Resilience and Regional Growth" by Jackson, Gillon, Barr, and Andrei.

## Quick Start

```bash
git clone [https://github.com/[your-username]/graduate-apprenticeship-analysis.git
cd graduate-apprenticeship-analysis](https://github.com/ElaineJaxPsych/GraduateApprenticeship-AutonomyParadox-Data/new/Main-Repository?filename=README.md)
```

Open R/RStudio and run:
```r
source("analysis.R")
```

## Key Findings

- **Autonomy Paradox Confirmed**: r=0.106 between perceived and practiced autonomy
- **University Support Critical**: r=0.765 strongest predictor of effective autonomy  
- **Developmental Tri-Sphere Model**: Different optimal combinations across learning stages
- **Exceptional Alumni Impact**: r=0.871 GA impact ↔ ongoing learning correlation
- **100% Regional Retention**: All alumni remained in Scottish region

## Required Packages

```r
install.packages(c("dplyr", "ggplot2", "psych", "gridExtra", "tidyr", "scales"))
```

## Repository Structure

```
├── README.md              # This file
├── analysis.R             # Main analysis script  
├── data/                  # Survey data (anonymized)
│   ├── UWS_current.csv    # Current students (n=25)
│   ├── Glasgow_current.csv # Current students (n=5)  
│   ├── UWS_alumni.csv     # Alumni (n=15)
│   └── Glasgow_alumni.csv # Alumni (n=5)
└── output/                # Generated figures and results
```

## Sample Description

- **Total participants**: 50
- **Current students**: 30 (UWS n=25, Glasgow n=5)
- **Alumni**: 20 (UWS n=15, Glasgow n=5)
- **Disciplines**: Engineering, IT, Business Management
- **Data collection**: Qualtrics survey platform
- **Ethics approval**: Both universities provided ethical clearance

## Key Results Summary

### Autonomy Paradox
- 61.5% report high autonomy (levels 4-5)
- Near-zero correlation (r=0.106) with autonomy in practice
- Paradox confirmed: high perceived autonomy ≠ better learning outcomes

### Tri-Sphere Model Validation
**Current Students (during programme):**
- Academia + Apprentice: 71.9% effectiveness (optimal)
- Academia + Workplace: 0% effectiveness  
- All Three Domains: 62.5% effectiveness

**Alumni (post-graduation):**
- All Three Domains: 94.6% effectiveness (optimal)
- Academia + Workplace: 81.2% effectiveness (+81.2% improvement!)
- Academia + Apprentice: 75% effectiveness

### Support Structure Analysis
- University support → Autonomy in practice: **r=0.765***
- Employer support → Self-directed learning: **r=-0.233**
- Team development strongest workplace predictor: **r=0.608**

### Developmental Trajectory
- Current students learning: M=2.85
- Alumni ongoing learning: M=4.28  
- **Improvement: +1.44 points** (dramatic developmental progression)

## Data Information

Survey data from Graduate Apprenticeship programmes at University of West Scotland and University of Glasgow. All data anonymized with identifying information removed.

**Key Variables:**
- **Current Students**: Autonomy (Q9, Q10, Q14, Q16), Support (Q12, Q15), Learning (Q18)
- **Alumni**: Programme impact (Q13, Q14), Workplace autonomy (Q21), Ongoing learning (Q18)
- **Composite Scales**: Created using rowMeans, α=0.84-0.94 reliability

## Statistical Approach

- **Mixed-methods design**: Quantitative analysis with qualitative contextualization
- **Tri-sphere model**: Median splits for balanced domain combinations
- **Missing data**: Pairwise deletion for correlations, listwise for ANOVA
- **Effect sizes**: Emphasized alongside significance testing
- **Reproducibility**: set.seed(12345) used throughout

## Citation

```
Jackson, E., Gillon, G., Barr, M., & Andrei, O. (2025). 
The Autonomy Paradox: Discretionary Learning in Graduate Apprenticeship 
Programmes as a Catalyst for Business Resilience and Regional Growth. 
[Journal details pending]
```

## Contact

**Corresponding Author**: Elaine Jackson  
**Email**: Elaine.Jackson@uws.ac.uk  
**Institution**: School of Business and Creative Industries, University of the West of Scotland

**Co-authors**:
- Gary Gillon (Gary.Gillon@uws.ac.uk) - University of the West of Scotland
- Matthew Barr (Matthew.Barr@glasgow.ac.uk) - University of Glasgow  
- Oana Andrei (Oana.Andrei@glasgow.ac.uk) - University of Glasgow

- # Graduate Apprenticeship Research: R1 Revision

## Repository Overview

This repository contains the analysis code, data processing scripts, and supplementary materials for the manuscript:

**"The Autonomy Paradox in Graduate Apprenticeships: Autonomy Beliefs, Workplace Constraints, and the Emergence of Discretionary Learning"**

Submitted to: *International Journal of Entrepreneurial Behavior & Research*

**Version:** R1 Revision (February 2026)

---

## Revision Summary

This revision addresses reviewer feedback through the following key changes:

### Major Revisions (Reviewer 1)

| Comment | Change Made | Location |
|---------|-------------|----------|
| 1.1 Sample size | Added power analysis; explicit acknowledgment of limitations | Section 4.2, Section 8 |
| 1.2 International literature | New Section 2.4 on comparative perspectives | Section 2.4 |
| 1.3 Streamline figures | Removed redundant Figure 2; renumbered figures | Throughout |
| 1.4 Temporal tensions | New Section 6.7 with Figure 5 (ROI evolution) | Section 6.7 |
| 1.5 Productive struggle | New Section 6.6 with empirical grounding | Section 6.6 |
| 1.6 Future research | Comprehensive Section 8 with 5-priority agenda | Section 8 |

### Minor Revisions (Reviewer 2)

| Comment | Change Made | Location |
|---------|-------------|----------|
| 2.1 Figure alignment | All figures centered at 6" width | Throughout |
| 2.2 Theory vs Framework | Changed to "Discretionary Learning framework" | Throughout |

### Major Revisions (Reviewer 3)

| Comment | Change Made | Location |
|---------|-------------|----------|
| 3.5 Statistical reporting | Added F-tests, CIs, effect sizes, assumption checks | Section 5 |
| 3.6 Regression analysis | New hierarchical regression (Section 5.4) | Section 5.4 |

---

## Key Statistical Findings

### Hierarchical Regression Results (n=26)

**Final Model:** R² = .378, F(4,21) = 3.19, p = .034

| Predictor | β | t | p | 95% CI | Interpretation |
|-----------|---|---|---|--------|----------------|
| Perceived Autonomy | .33 | 2.50 | .021* | [.06, .61] | Autonomy beliefs predict learning |
| Autonomy in Practice | .19 | 0.49 | .628 | [-.63, 1.01] | Practice doesn't add unique variance |
| Employer Support | **-.45** | -2.42 | .025* | [-.84, -.06] | **Negative coefficient** |
| University Support | .24 | 0.67 | .511 | [-.50, .98] | Operates through enabling autonomy |

**Critical Finding:** The negative employer support coefficient (β = -.45) reveals that workplace demands, even supportive ones, interfere with learning when continuously present during development.

### Autonomy Paradox Validation

- Perceived ↔ Practice correlation: r = .106, p = .606, 95% CI [-.29, .47]
- Interpretation: Near-zero correlation confirms beliefs don't translate to practice

### Developmental Trajectory

- Current Students: M = 2.85, SD = 0.94
- Alumni: M = 4.29, SD = 0.71
- Improvement: +1.44 points, t(48) = 5.89, p < .001, Cohen's d = 1.45

---

## File Structure

```
GA-Research/
├── README.md                          # This file
├── CHANGELOG.md                       # Version history
│
├── data/
│   ├── raw/
│   │   ├── UWS current.csv           # UWS current students (anonymised)
│   │   ├── UWS alumni.csv            # UWS alumni (anonymised)
│   │   ├── Glasgow current.csv       # Glasgow current students (anonymised)
│   │   └── Glasgow alumni.csv        # Glasgow alumni (anonymised)
│   └── processed/
│       └── combined_analysis.RData   # Processed R workspace
│
├── scripts/
│   └── GA_Analysis_R1_Revision.R     # Main analysis script (this revision)
│
├── publication_figures/
│   ├── Figure1_TriSphere_Model.png   # Conceptual model (created separately)
│   ├── Figure2_Developmental_Trajectory.png
│   ├── Figure3_Learning_Outcomes.png
│   ├── Figure4_Workplace_Dimensions.png
│   └── Figure5_ROI_Evolution.png
│
├── manuscript/
│   ├── IJEBR_R1_Revision.docx        # Revised manuscript
│   └── Reviewer_Response_R1.docx     # Response to reviewers
│
└── supplementary/
    └── Statistical_Output.txt         # Full R output log
```

---

## Figure Mapping (R1 Revision)

| Figure | Title | Description | Script Output |
|--------|-------|-------------|---------------|
| 1 | Tri-Sphere Model | Conceptual model diagram | Created separately |
| 2 | Developmental Trajectory | Dumbbell plot showing 0%→81.2% transformation | `Figure2_Developmental_Trajectory.png` |
| 3 | Learning Outcomes | Bar chart comparing current vs alumni | `Figure3_Learning_Outcomes.png` |
| 4 | Workplace Dimensions | Q21 correlations with ongoing learning | `Figure4_Workplace_Dimensions.png` |
| 5 | ROI Evolution | 5-year return on investment trajectory | `Figure5_ROI_Evolution.png` |

**Note:** Original Figure 2 (current students only) was removed as redundant with Table III per Reviewer 1.3.

---

## Running the Analysis

### Prerequisites

```r
# Required packages
install.packages(c("dplyr", "ggplot2", "psych", "gridExtra", 
                   "grid", "tidyr", "scales", "car"))
```

### Execution

```r
# Set working directory to data folder
setwd("path/to/GA-Research/data/raw")

# Run analysis
source("../scripts/GA_Analysis_R1_Revision.R")

# Outputs saved to:
# - publication_figures/ (PNG files)
# - Global environment (R objects for further analysis)
```

### Key Output Objects

| Object | Description |
|--------|-------------|
| `current_students_data` | Processed current student data (n=30) |
| `alumni_students_data` | Processed alumni data (n=20) |
| `hierarchical_regression` | Full regression model results |
| `table_iii_results` | Tri-sphere comparative analysis |
| `developmental_trajectory` | t-test and effect size results |

---

## Reproducibility Notes

- **R Version:** 4.3.0 or higher recommended
- **Random seed:** Not required (no stochastic elements)
- **Data anonymisation:** All participant identifiers removed; only aggregate data shared


---

## Citation

If using this code or methodology, please cite:

```
[Authors]. (2026). The Autonomy Paradox in Graduate Apprenticeships: 
Autonomy Beliefs, Workplace Constraints, and the Emergence of 
Discretionary Learning. International Journal of Entrepreneurial 
Behavior & Research. [DOI pending]
```

---

## Contact

For questions regarding this analysis:
- **Corresponding Author:** [Email]
- **Data Access:** Available upon reasonable request following ethics approval

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | Oct 2025 | Initial submission |
| R1 | Feb 2026 | Reviewer revisions (hierarchical regression, figure consolidation) |

---

*Last updated: February 2026*

## Acknowledgments

- Graduate Apprenticeship participants from both universities
- Programme coordinators and academic supervisors
- Research ethics committees at UWS and University of Glasgow
- Scottish Government Graduate Apprenticeship policy framework
