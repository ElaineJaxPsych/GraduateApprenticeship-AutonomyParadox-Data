# The Autonomy Paradox: Graduate Apprenticeship Research

## Complete Reproducible Analysis Pipeline

**"The Autonomy Paradox: Discretionary Learning in Graduate Apprenticeship Programmes as a Catalyst for Business Resilience and Regional Growth"**

### Authors
- **Elaine Jackson** (University of the West of Scotland) - *Corresponding Author*
- **Gary Gillon** (University of the West of Scotland)  
- **Matthew Barr** (University of Glasgow)
- **Oana Andrei** (University of Glasgow)

---

## 🎯 Key Findings

- **Autonomy Paradox**: 69.6% report high autonomy, yet self-directed learning remains moderate (M=3.46/5.0)
- **Support Supremacy**: Employer support (r=0.713) far outweighs autonomy (r=0.221) in predicting learning outcomes
- **Business Impact**: Alumni demonstrate substantial workplace impact (79.5% mean across domains)
- **Tri-Sphere Model**: Integration across Academia, Workplace, and Apprentice spheres yields optimal outcomes

---

## 🚀 Complete Reproducible Workflow

### Prerequisites

**Software Requirements:**
- R 4.0.0 or later
- RStudio (recommended but not required)

**Required R Packages** (automatically installed by script):
```r
tidyverse, readr, ggplot2, dplyr, tidyr, gridExtra, scales, 
grid, corrplot, psych, car, broom, knitr
```

### Step 1: Prepare Your Data Files

Place your **raw Qualtrics CSV exports** in the same directory as the analysis script:

**Required Files (exact names):**
1. `Graduate Apprenticeship Programme Survey  Current Students_April 28 2025_15.48.csv`
2. `Graduate Apprenticeship Programme Survey  Alumni_April 28 2025_16.01.csv`
3. `Current_Glasgow_31 May 2025_14.27.csv`
4. `Alumni_Glasgow_31 May 2025_14.27.csv`

**Note:** These are the exact standardized CSV files from your Qualtrics exports. The script is designed to work with your specific data structure.

### Step 2: Run Complete Analysis

**Option A: Single Command (Recommended)**
```r
# Download and run the complete analysis with your exact data files
source("complete_autonomy_analysis.R")
```

This will:
- ✅ Read your 4 standardized CSV files
- ✅ Process UWS Current (n≈14) and Alumni (n≈15) data 
- ✅ Process Glasgow Current (n≈13) and Alumni (n≈2) data
- ✅ Generate all manuscript figures and tables
- ✅ Produce complete statistical analysis
- ✅ Create validation report

**Option B: Step-by-Step**
```r
# 1. Load the script
source("complete_autonomy_analysis.R")

# 2. Run individual steps if needed
# (All steps are included in the main script)
```

### Step 3: Review Outputs

The script automatically creates:

```
📁 Project Directory/
├── 📄 complete_autonomy_analysis.R        # Main analysis script
├── 📁 figures/                            # Publication-ready figures
│   ├── 🖼️ Figure1A_Autonomy_Distribution.png
│   ├── 🖼️ Figure1B_Learning_by_Autonomy.png
│   ├── 🖼️ Figure2_Correlation_Forest.png
│   └── 🖼️ Figure3_Business_Impact.png      # (if alumni data available)
├── 📁 tables/                             # Statistical tables (CSV format)
│   ├── 📊 Table1_Sample_Characteristics.csv
│   ├── 📊 Table2_Descriptive_Statistics.csv
│   ├── 📊 Table3_Correlation_Matrix.csv
│   └── 📊 Table4_Regression_Results.csv
└── 📁 analysis_output/                    # Analysis documentation
    └── 📄 Analysis_Report.txt             # Comprehensive analysis report
```

---

## 📊 What the Analysis Does

### Data Processing Pipeline

1. **Auto-Detection**: Automatically finds and identifies your CSV files
2. **Data Cleaning**: Removes Qualtrics metadata rows and filters completed responses
3. **Standardization**: Converts all Likert scales to consistent 1-5 numeric format
4. **Scale Creation**: Builds composite scales from question series (Q10-Q15)
5. **Integration**: Combines UWS and Glasgow datasets with common variables

### Statistical Analysis

- **Descriptive Statistics**: Sample characteristics and key variable distributions
- **Correlation Analysis**: Relationships between autonomy, support, and learning outcomes
- **Multiple Regression**: Predicting self-directed learning from key predictors
- **Validation**: Quality checks against published manuscript values

### Figure Generation

- **Figure 1**: The Autonomy Paradox (distribution + violin plots)
- **Figure 2**: Correlation forest plot showing support supremacy
- **Figure 3**: Business impact analysis (if alumni data available)
- All figures saved as high-resolution PNG files (300 DPI)

---

## 🔬 Manuscript Integration

### Key Statistics for Papers

The analysis generates your exact manuscript-ready statistics:

```r
# Autonomy Paradox Quote (validated with your data)
"[X]% of apprentices report high autonomy, yet self-directed learning 
remains moderate (M=[X]/5.0)"

# Support Supremacy Quote (from your correlations)
"Employer support demonstrates substantially stronger relationships (r=[X]) 
compared to autonomy alone (r=[X])"

# Sample Composition (your actual data)
"Mixed-methods study (n=[X]) across UWS (n=[X]) and Glasgow (n=[X])"

# Statistical Model (your regression results)
"R² = [X], F([X],[X]) = [X], p < 0.001"
```

### Table Integration

All tables are saved as CSV files that can be directly imported into:
- Microsoft Word (for manuscript submission)
- LaTeX (for academic formatting)
- Excel (for further analysis)

---

## 🛠️ Troubleshooting

### Common Issues

**"File not found" errors:**
- Ensure CSV files are in the same directory as the script
- Check that filenames contain keywords: "current", "alumni", "glasgow"
- Verify files are actual CSV exports from Qualtrics

**Missing packages:**
- The script automatically installs missing packages
- If installation fails, manually install: `install.packages("tidyverse")`

**Low sample sizes:**
- Script includes validation checks against expected manuscript values
- Warnings will appear if sample sizes are too small for statistical power

### Data File Requirements

Your CSV files should:
- ✅ Be your exact standardized exports (4 files as listed above)
- ✅ Include the standard Qualtrics header rows (script removes these automatically)
- ✅ Have 'Finished' column indicating completed responses
- ✅ Use your Q10-Q21 question structure for scales
- ✅ Be in the same directory as the analysis script

---

## 📈 Validation

The script includes comprehensive validation:

- **Data Quality**: Checks for missing data and completeness
- **Statistical Validation**: Compares results to manuscript values
- **File Output**: Verifies all figures and tables are generated
- **Reproducibility**: Sets random seed for consistent results

Expected validation ranges (based on your actual data):
- Sample size: ~40-45 total participants
- UWS vs Glasgow ratio: ~70/30 split
- Current vs Alumni ratio: ~60/40 split
- High autonomy: Variable by institution
- Correlation patterns: Support > Autonomy for learning outcomes

---

## 🎓 Academic Use

### Citation

If you use this analysis pipeline, please cite:

```bibtex
@article{jackson2025autonomy,
  title={The Autonomy Paradox: Discretionary Learning in Graduate Apprenticeship 
         Programmes as a Catalyst for Business Resilience and Regional Growth},
  author={Jackson, Elaine and Gillon, Gary and Barr, Matthew and Andrei, Oana},
  journal={International Journal of Entrepreneurial Behavior \& Research},
  year={2025},
  note={Analysis code available at: https://github.com/ElaineJaxPsych/GraduateApprenticeship-AutonomyParadox-Data}
}
```

### Reproducibility Standards

This analysis follows enhanced reproducible research principles:
- ✅ **Complete workflow**: From raw data to publication figures
- ✅ **Automated pipeline**: Single script execution
- ✅ **Version control**: All code documented and tracked
- ✅ **Quality validation**: Built-in checks and balances
- ✅ **Open science**: Full methodology transparent

---

## 🤝 Contributing

### For Collaborators

1. **Fork the repository**
2. **Test with your data**: Run script with your CSV files
3. **Report issues**: Use GitHub Issues for bugs or questions
4. **Submit improvements**: Pull requests welcome

### For Other Researchers

1. **Adapt the pipeline**: Modify for your survey structure
2. **Extend analysis**: Add new statistical methods
3. **Share modifications**: Contribute back to the community

---

## 📞 Support

### Getting Help

- **Technical Issues**: Create a GitHub Issue with error details
- **Data Questions**: Email Elaine.Jackson@uws.ac.uk
- **Collaboration**: Contact corresponding author for research partnerships

### Quick Start Checklist

- [ ] R 4.0+ installed
- [ ] Your 4 standardized CSV files in project directory:
  - [ ] `Graduate Apprenticeship Programme Survey  Current Students_April 28 2025_15.48.csv`
  - [ ] `Graduate Apprenticeship Programme Survey  Alumni_April 28 2025_16.01.csv`
  - [ ] `Current_Glasgow_31 May 2025_14.27.csv`
  - [ ] `Alumni_Glasgow_31 May 2025_14.27.csv`
- [ ] Download `complete_autonomy_analysis.R`
- [ ] Run `source("complete_autonomy_analysis.R")`
- [ ] Check `analysis_output/Complete_Analysis_Report.txt` for results
- [ ] Review figures in `figures/` directory
- [ ] Import tables from `tables/` directory

---

## 📄 License

**MIT License** - See LICENSE file for details

This research is conducted under ethical approval from participating universities. All data is anonymized and used in accordance with GDPR and institutional ethics policies.

---

## 🔄 Version History

- **v2.0** - Complete reproducible pipeline with auto-detection
- **v1.5** - Enhanced figures with publication-quality formatting  
- **v1.0** - Initial analysis scripts and basic visualizations

---

**🎯 Ready to replicate the Autonomy Paradox findings? Run the script and generate publication-ready results in minutes!**

---

*Last updated: June 2025 | Questions? Contact Elaine.Jackson@uws.ac.uk*
