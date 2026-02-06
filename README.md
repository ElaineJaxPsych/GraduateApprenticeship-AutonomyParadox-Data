# Graduate Apprenticeship Autonomy Paradox Analysis

Analysis code for **"The Autonomy Paradox in Graduate Apprenticeships: Autonomy Beliefs, Workplace Constraints, and the Emergence of Discretionary Learning"**

**Authors:** Jackson, E., Gillon, G., Barr, M., & Andrei, O.

**Submitted to:** International Journal of Entrepreneurial Behavior & Research

**Version:** R1 Revision (February 2026)

---

## Quick Start

```bash
git clone https://github.com/ElaineJaxPsych/GraduateApprenticeship-AutonomyParadox-Data.git
cd GraduateApprenticeship-AutonomyParadox-Data
```

Open R/RStudio and run:

```r
source("scripts/GA_Analysis_R1_Revision.R")
```

---

## Key Findings

| Finding | Result | Interpretation |
|---------|--------|----------------|
| **Autonomy Paradox** | r = 0.106 (perceived ↔ practiced) | Beliefs don't translate to practice |
| **Employer Support** | β = -0.45, p = .025 | Negative effect — workplace demands interfere |
| **Perceived Autonomy** | β = 0.33, p = .021 | Beliefs predict learning success |
| **Developmental Trajectory** | +1.44 points, d = 1.45 | Large effect from current → alumni |
| **Alumni GA Impact** | r = 0.871 | Exceptional long-term programme effect |
| **Regional Retention** | 100% | All alumni remained in Scottish region |

### Hierarchical Regression Results (n=26)

**Final Model:** R² = .378, F(4,21) = 3.19, p = .034

| Predictor | β | p | 95% CI | VIF |
|-----------|---|---|--------|-----|
| Perceived Autonomy | .33 | .021* | [.06, .61] | 1.89 |
| Autonomy in Practice | .19 | .628 | [-.63, 1.01] | 1.45 |
| Employer Support | **-.45** | .025* | [-.84, -.06] | 2.31 |
| University Support | .24 | .511 | [-.50, .98] | 2.62 |

**Critical Finding:** The negative employer support coefficient reveals that workplace demands, even supportive ones, interfere with learning when continuously present during development.

---

## Sample Description

| Group | UWS | Glasgow | Total |
|-------|-----|---------|-------|
| Current Students | 25 | 5 | 30 |
| Alumni | 15 | 5 | 20 |
| **Total** | 40 | 10 | **50** |

- **Disciplines:** Engineering, IT, Business Management
- **Data collection:** Qualtrics survey platform
- **Ethics approval:** Both universities provided ethical clearance

---

## Repository Structure

```
├── README.md                              # This file
├── CHANGELOG.md                           # Version history
│
├── data/
│   ├── raw/
│   │   ├── UWS current.csv               # UWS current students (n=25)
│   │   ├── UWS alumni.csv                # UWS alumni (n=15)
│   │   ├── Glasgow current.csv           # Glasgow current students (n=5)
│   │   └── Glasgow alumni.csv            # Glasgow alumni (n=5)
│   └── processed/
│       └── combined_analysis.RData       # Processed R workspace
│
├── scripts/
│   └── GA_Analysis_R1_Revision.R         # Main analysis script
│
├── publication_figures/
│   ├── Figure1_TriSphere_Model.png       # Conceptual model
│   ├── Figure2_Developmental_Trajectory.png
│   ├── Figure3_Learning_Outcomes.png
│   ├── Figure4_Workplace_Dimensions.png
│   └── Figure5_ROI_Evolution.png
│
├── manuscript/
│   ├── IJEBR_R1_Revision.docx            # Revised manuscript
│   └── Reviewer_Response_R1.docx         # Response to reviewers
│
└── supplementary/
    └── Statistical_Output.txt            # Full R output log
```

---

## Required Packages

```r
install.packages(c("dplyr", "ggplot2", "psych", "gridExtra", 
                   "grid", "tidyr", "scales", "car"))
```

---

## Figure Mapping (R1 Revision)

| Figure | Title | Description |
|--------|-------|-------------|
| 1 | Tri-Sphere Model | Conceptual model diagram |
| 2 | Developmental Trajectory | Dumbbell plot: 0%→81.2% transformation |
| 3 | Learning Outcomes | Bar chart: current vs alumni comparison |
| 4 | Workplace Dimensions | Q21 correlations with ongoing learning |
| 5 | ROI Evolution | 5-year return on investment trajectory |

**Note:** Original Figure 2 (current students only) removed as redundant with Table III per Reviewer 1.3.

---

## Tri-Sphere Model Results

### Current Students (during programme)

| Domain Combination | Effectiveness |
|--------------------|---------------|
| Academia + Apprentice | 71.9% (optimal) |
| All Three Domains | 62.5% |
| Academia + Workplace | 0% |

### Alumni (post-graduation)

| Domain Combination | Effectiveness | Improvement |
|--------------------|---------------|-------------|
| All Three Domains | 94.6% (optimal) | +32.1% |
| Academia + Workplace | 81.2% | **+81.2%** |
| Academia + Apprentice | 75.0% | +3.1% |

---

## R1 Revision Changes

### Reviewer 1 (Major)
| Comment | Change | Location |
|---------|--------|----------|
| 1.1 Sample size | Power analysis added | Section 4.2, 8 |
| 1.2 International lit | Section 2.4 added | Section 2.4 |
| 1.3 Streamline figures | Removed Figure 2, renumbered | Throughout |
| 1.4 Temporal tensions | Section 6.7, Figure 5 | Section 6.7 |
| 1.5 Productive struggle | Section 6.6 | Section 6.6 |
| 1.6 Future research | 5-priority agenda | Section 8 |

### Reviewer 2 (Minor)
| Comment | Change | Location |
|---------|--------|----------|
| 2.1 Figure alignment | All centered, 6" width | Throughout |
| 2.2 Theory → Framework | Language changed | Throughout |

### Reviewer 3 (Major)
| Comment | Change | Location |
|---------|--------|----------|
| 3.5 Statistics | Full reporting with CIs | Section 5 |
| 3.6 Regression | Hierarchical regression | Section 5.4 |

---

## Citation

```
Jackson, E., Gillon, G., Barr, M., & Andrei, O. (2026). 
The Autonomy Paradox in Graduate Apprenticeships: Autonomy Beliefs, 
Workplace Constraints, and the Emergence of Discretionary Learning. 
International Journal of Entrepreneurial Behavior & Research. 
https://doi.org/[DOI pending]
```

---

## Contact

**Corresponding Author:** Elaine Jackson  
**Email:** Elaine.Jackson@uws.ac.uk  
**Institution:** School of Business and Creative Industries, University of the West of Scotland

**Co-authors:**
- Gary Gillon (Gary.Gillon@uws.ac.uk) - University of the West of Scotland
- Matthew Barr (Matthew.Barr@glasgow.ac.uk) - University of Glasgow
- Oana Andrei (Oana.Andrei@glasgow.ac.uk) - University of Glasgow

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | Oct 2025 | Initial submission |
| R1 | Feb 2026 | Hierarchical regression, figure consolidation, power analysis |

---

## Acknowledgments

- Graduate Apprenticeship participants from both universities
- Programme coordinators and academic supervisors
- Research ethics committees at UWS and University of Glasgow
- Scottish Government Graduate Apprenticeship policy framework

---

*Last updated: February 2026*
