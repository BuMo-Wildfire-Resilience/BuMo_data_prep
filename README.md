### Bulkley Morice Wildfire Resilience Pilot 

BuMo_data_prep
============================
The Bulkley-Morice Pilot project (BuMo) is focused on summarizing existing knowledge of forest and fuel management practices and assessing how practices could be revised to improve wildfire resilience . The project is being designed so that decision-makers and experts can develop specific management scenarios that integrate the three streams of the project; 1) wildfire mitigation, 2) conservation design, and 3) carbon management. 

Five sets of BuMo R scripts are being developed to support the project.  
1) BuMo_data_prep- this repository which organizes project data for the other repositories as well as identifying the project area of interest (AOI);  
2) Conservation_Assessment;  
3) TEF_model_support;  
4) Fuel_Management; and  
5) Carbon.  

### Usage

There are a set of scripts that help prepare data:     
Control scripts - set up the analysis environment;  
Load scripts - loads base data and field data;    
Clean scripts - cleans data; and    
Analysis scripts.

#Control Scripts:   
run_all.R	Sets local variables and directories used by scripts, presents script order.  
header.R	loads R packages, sets global directories, and attributes.

#Load Scripts:	
01_load.R	load script sourcing all the various pre-processed layers required - typically only required for first run.  

#Clean Scripts:   
02_clean.R

#Analysis_Scripts:   
03_analysis.R

### Project Status

The set of R BuMo scripts are continually being modified and improved.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/BCWF-Wetlands/WESP_data_prep/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

