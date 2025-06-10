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

## Acknowledgments

- Graduate Apprenticeship participants from both universities
- Programme coordinators and academic supervisors
- Research ethics committees at UWS and University of Glasgow
- Scottish Government Graduate Apprenticeship policy framework
