# The Autonomy Paradox: Graduate Apprenticeship Research

## Complete Reproducible Analysis Repository

**"The Autonomy Paradox: Discretionary Learning in Graduate Apprenticeship Programmes as a Catalyst for Business Resilience and Regional Growth"**

### Authors
- **Elaine Jackson** (University of the West of Scotland) - *Corresponding Author*
- **Gary Gillon** (University of the West of Scotland)
- **Matthew Barr** (University of Glasgow)  
- **Oana Andrei** (University of Glasgow)

---

## 🎯 Key Research Findings

- **Autonomy Paradox**: High autonomy reported but moderate self-directed learning outcomes
- **Support Supremacy**: Employer support outweighs autonomy in predicting learning success
- **Business Impact**: Alumni demonstrate substantial workplace contributions
- **Tri-Sphere Model**: Integration across Academia, Workplace, and Apprentice spheres optimal

---

## 🚀 Complete Reproducible Workflow

### One-Command Analysis

```r
# Download complete_autonomy_analysis.R and run:
source("complete_autonomy_analysis.R")
```

**This single command:**
- ✅ Reads all 4 CSV data files from this repository
- ✅ Processes and standardizes all survey responses  
- ✅ Calculates all manuscript statistics
- ✅ Generates all publication figures
- ✅ Creates all statistical tables
- ✅ Produces comprehensive analysis report

### Prerequisites

**Software:** R 4.0+ (packages installed automatically)

**Data:** All required CSV files are already in this repository

---

## 📁 Repository Contents

### Raw Data Files
- `Graduate Apprenticeship Programme Survey  Current Students_April 28 2025_15.48.csv` - UWS current students (n≈14)
- `Graduate Apprenticeship Programme Survey  Alumni_April 28 2025_16.01.csv` - UWS alumni (n≈15)
- `Current_Glasgow_31 May 2025_14.27.csv` - Glasgow current students (n≈13)
- `Alumni_Glasgow_31 May 2025_14.27.csv` - Glasgow alumni (n≈2)

### Analysis Script
- `complete_autonomy_analysis.R` - **Single script that reproduces ALL manuscript results**

### Generated Outputs (Examples)
- `Figure_1A_Autonomy_Distribution.png` - Autonomy paradox visualization
- `Figure_2_Support_Correlation_Forest.png` - Support vs autonomy correlations
- `Figure_3_Business_Impact.png` - Alumni workplace contributions
- `Table_1_Sample_Characteristics.csv` - Sample demographics
- `Table_2_Descriptive_Statistics.csv` - Variable summaries
- `Table_3_Correlation_Matrix.csv` - Correlation results
- `Table_4_Regression_Results.csv` - Multiple regression analysis

---

## 📊 What the Analysis Produces

### Manuscript Statistics
```
"[X]% of apprentices report high autonomy, yet self-directed learning 
remains moderate (M=[X]/5.0)"

"Employer support demonstrates substantially stronger relationships (r=[X]) 
compared to autonomy alone (r=[X])"

"Model R² = [X], F([X],[X]) = [X], p < 0.001"
```

### Publication Figures
- **Figure 1**: The Autonomy Paradox (distribution + learning outcomes)
- **Figure 2**: Support Supremacy (correlation forest plot)
- **Figure 3**: Business Impact Analysis (alumni contributions)

### Statistical Tables
- **Table 1**: Sample characteristics by institution and status
- **Table 2**: Descriptive statistics for all key variables
- **Table 3**: Correlation matrix for main constructs
- **Table 4**: Multiple regression predicting self-directed learning

---

## 🔬 For Researchers

### Perfect for:
- **Replicating findings** with identical methodology
- **Adapting analysis** for similar apprenticeship research
- **Understanding methodology** from raw data to conclusions
- **Teaching reproducible research** practices
- **Building upon** this theoretical framework

### Technical Features:
- **Complete transparency**: Every analytical step documented
- **Quality validation**: Built-in checks against manuscript values  
- **Professional outputs**: Publication-ready figures and tables
- **Error handling**: Robust processing of survey response variations
- **Scalable design**: Easily adaptable to other datasets

---

## 🛠️ Usage Instructions

### Step 1: Clone Repository
```bash
git clone https://github.com/ElaineJaxPsych/GraduateApprenticeship-AutonomyParadox-Data
cd GraduateApprenticeship-AutonomyParadox-Data
```

### Step 2: Run Analysis
```r
# In R or RStudio:
source("complete_autonomy_analysis.R")
```

### Step 3: Review Results
- Check `outputs/ANALYSIS_REPORT.txt` for complete results
- Find figures in `outputs/figures/` directory
- Import tables from `outputs/tables/` directory

### Expected Runtime
- **Data processing**: ~30 seconds
- **Statistical analysis**: ~10 seconds  
- **Figure generation**: ~20 seconds
- **Total time**: ~1 minute

---

## 📈 Validation & Quality

### Built-in Checks
- ✅ **Data integrity**: Validates all CSV files load correctly
- ✅ **Sample sizes**: Confirms sufficient data for analysis
- ✅ **Statistical assumptions**: Checks for multicollinearity and outliers
- ✅ **Output validation**: Ensures all figures and tables generate successfully
- ✅ **Reproducibility**: Sets random seed for consistent results

### Expected Results
- **Sample size**: ~40-45 total participants
- **Institution ratio**: ~70% UWS, 30% Glasgow
- **Status ratio**: ~60% current students, 40% alumni
- **High autonomy**: Variable by institution and methodology
- **Support patterns**: Employer support > University support > Autonomy

---

## 📚 Academic Integration

### Citation
```bibtex
@article{jackson2025autonomy,
  title={The Autonomy Paradox: Discretionary Learning in Graduate Apprenticeship 
         Programmes as a Catalyst for Business Resilience and Regional Growth},
  author={Jackson, Elaine and Gillon, Gary and Barr, Matthew and Andrei, Oana},
  journal={International Journal of Entrepreneurial Behavior \& Research},
  year={2025},
  note={Data and code: https://github.com/ElaineJaxPsych/GraduateApprenticeship-AutonomyParadox-Data}
}
```

### Theoretical Contributions
1. **Autonomy Paradox**: New construct explaining autonomy-performance gaps
2. **Tri-Sphere Model**: Framework for work-based learning integration
3. **Support Supremacy**: Empirical evidence for support over autonomy
4. **Discretionary Learning**: Novel approach to structured autonomy

---

## 🤝 Contributing & Support

### For Collaborators
- **Fork repository** and submit pull requests for improvements
- **Report issues** using GitHub Issues for bugs or questions
- **Extend analysis** with additional statistical methods or visualizations
- **Share adaptations** for other educational contexts

### For Other Researchers  
- **Adapt methodology** for your apprenticeship or work-based learning research
- **Use framework** as foundation for similar studies
- **Modify questionnaires** based on our validated scales
- **Apply Tri-Sphere Model** to different educational contexts

### Getting Help
- **Technical issues**: Create GitHub Issue with detailed error messages
- **Research questions**: Email Elaine.Jackson@uws.ac.uk
- **Collaboration**: Contact corresponding author for partnerships
- **Data questions**: Reference CODEBOOK.md and analysis script comments

---

## 🔒 Ethics & Privacy

### Data Protection
- ✅ **Anonymized data**: All personally identifiable information removed
- ✅ **Ethical approval**: Research conducted under institutional ethics approval
- ✅ **GDPR compliant**: Data processing follows EU data protection regulations
- ✅ **Participant consent**: All participants provided informed consent for data sharing

### Usage Guidelines
- **Academic use**: Freely available for research and educational purposes
- **Commercial use**: Contact authors for licensing arrangements
- **Attribution**: Please cite original paper when using data or methodology
- **Modifications**: Document any changes made to analysis or interpretation

---

## 🏆 Impact & Recognition

### Research Excellence
- **Reproducible science**: Gold standard for open research practices
- **Methodological rigor**: Comprehensive validation and quality checks
- **Theoretical innovation**: Novel constructs and frameworks
- **Practical relevance**: Direct implications for educational policy

### Educational Value
- **Teaching tool**: Perfect for graduate research methods courses
- **Best practices**: Demonstrates complete reproducible workflow
- **Code quality**: Well-documented, professional R programming
- **Open science**: Exemplifies transparent research practices

---

## 📄 License & Terms

**Creative Commons Attribution 4.0 International License**

You are free to:
- **Share**: Copy and redistribute in any medium or format
- **Adapt**: Remix, transform, and build upon the material
- **Commercial**: Use for any purpose, even commercially

Under these terms:
- **Attribution**: Must give appropriate credit and indicate if changes were made
- **No additional restrictions**: Cannot apply legal terms that restrict others

---

## 🔄 Version History

- **v2.0** (Current) - Complete reproducible pipeline with single-script execution
- **v1.5** - Enhanced figures and comprehensive statistical analysis
- **v1.0** - Initial data release with basic analysis scripts

---

## 📞 Contact Information

### Primary Contact
**Dr. Elaine Jackson**  
University of the West of Scotland  
📧 Elaine.Jackson@uws.ac.uk  
🔗 [ORCID Profile](https://orcid.org/[ORCID-ID])

### Co-Authors
- **Gary Gillon** - Gary.Gillon@uws.ac.uk
- **Matthew Barr** - Matthew.Barr@glasgow.ac.uk  
- **Oana Andrei** - Oana.Andrei@glasgow.ac.uk

### Repository
🌐 **GitHub**: https://github.com/ElaineJaxPsych/GraduateApprenticeship-AutonomyParadox-Data  
📊 **DOI**: [To be assigned upon publication]

---

## ⭐ Quick Start Checklist

- [ ] **Clone repository** or download ZIP file
- [ ] **Install R 4.0+** (RStudio recommended but not required)
- [ ] **Verify data files** (4 CSV files should be present)
- [ ] **Run analysis**: `source("complete_autonomy_analysis.R")`
- [ ] **Check outputs** in newly created `outputs/` directory
- [ ] **Review report**: Read `outputs/ANALYSIS_REPORT.txt`
- [ ] **Use results**: Import figures and tables for your work

---

**🎯 Ready to explore the Autonomy Paradox? One command reproduces all findings!**

---

*Repository maintained by the Graduate Apprenticeship Research Team*  
*Last updated: June 2025*  
*Status: ✅ Active & Maintained*
