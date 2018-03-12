Disease Models Using SBML Arrays
================================

This repository implements several simple disease models using and extension to the Systems Biology Markup Language (SBML) called [SBML Arrays](http://sbml.org/Documents/Specifications/SBML_Level_3/Packages/arrays)

The purpose of this work is to show that it is possible to generate reproducible disease models using the SBML arrays package.

The models are implemented using two paths: 
1. iBioSim using SBML files generated by a python script 
2. The MIcroSimlation Tool (MIST) as a reference for comparison

These examples are supporting the paper titled: "Disease Models using the System Biology Markup Language (SBML)".

This work extends previous work on [sharing disease models](https://github.com/Jacob-Barhak/SharingDiseaseModels) to support micro-simulation.


FILES:
------
### General
* README.md - is the file you are reading now
* License.txt - GNU Lesser General Public License version 3 license file

### iBioSim directory: contains example generation code and iBioSim processing
* simulation_results.zip - an archive containing simulation runs for all examples
* Disease.omex - an archive containing all of the information to reproduce the results.

#### iBioSim/SBML sub directory - containing SBML files and the python script that generated them that were passed to iBioSim 
* SBMLExamplesForPaper.py - The python file that generates the SBML examples
* Example*.xml - Generated SBML code representing example *

#### iBioSim/stat_analysis sub directory: contains statistical analysis of the results in simulation_results.zip
* Example*_n=*_*.csv - Includes the min, max, mean, and std. dev. for each variable in each example, for 1, 10, and 100 runs.

#### iBioSim/projects sub directory - includes iBioSim project files
* BioSim.prj - An iBioSim project file.
* disease.sbol - Associated SBOL from the iBioSim project (not used for disease models).
* disease.sedml - SED-ML file that describes the simulation experiments for the iBioSim project.

#### iBioSim/projects/Example* sub directory - analysis view to setup simulation for each Example within iBioSim

#### iBioSim/scripts sub directory - includes results and scripts to plot them
* plot0*.R - R script to generate the plots for example*
* plot.R - R script to generate all plots
* util.R - R script that has a function to export statistics to a csv file.
* parser.py - a python script to convert tsd to csv files.

### MIST directory: contains the reference implementation using MIST
* Example*Random.zip - MIST model file before execution of example *
* Example*_short_RepOpt - Report options file for generating report within MIST for example *
* RunSimulations.bat - A batch file running all simulations using MIST commands
* Results.zip - archive containing results from executing the RunSimulations.bat script

#### MIST/Results.zip archive contents
* Example*Random_90000??.zip - MIST model file after execution with results for example * repetition ??
* Example*Random_90000??_TraceBack.txt - MIST RAndom state traceback file to allow reproducing results for example * repetition ??
* Example*Random_90000??.csv - statistics of simulation run executed for example * repetition ??
* Example*Random_9000000*.csv - statistics of 100 repetitions for example * repetition ?? using Min,Max,Mean,Median,STD
* Example*Random_9000090*.csv - statistics of 10 repetitions for example * repetition ?? using Min,Max,Mean,Median,STD

### Comparison directory: contains code to compare MIST and iBioSim results and generate statistics and graphics
* CompareResults.py - a python script generating the comparison plots between MIST and iBioSim

#### Comparison/Results Sub Directory - Statistics comparison results generated by CompareResults.py
* Example*Comparison.csv - spreadsheet file comparing all statistics for all repetitions for example *
* Example*DiffPlot.html - Plot comparing Example * MIST and iBioSim results for all repetitions for all variables and stratifications
* Repetitions*.html - Plot comparing all results for * repetitions

#### Comparison/Figures Sub Directory - generated plots
* Example*.pdf - pdf version of the html file comparing results for example *
* Run*.pdf - pdf version of the html file comparing results for all examples for * repetitions


USAGE:
------
Each file uses a different system and format. To run some of the files to reproduce paper results specific tools are required:
The following tools and versions were used to generate the results in this paper:
* MIST Version 0.92.2.0 - Install from: https://github.com/Jacob-Barhak/MIST executed on a Windows 10 (64 bit) machine using Python 2.7.14 | Anaconda2-5.0.1 (64-bit) 
* SBMLExamplesForPaper.py required libsbml experimental version 5.16.0 that can be obtained from https://sourceforge.net/projects/sbml/files/libsbml/5.16.0/ for this work it was executed on a Windows 10 (64 bit) machine using Python 2.7.14 | Anaconda2-5.0.1 (64-bit) 
* iBioSim Version 3.0.0 - Freely available for download at: http://www.async.ece.utah.edu/ibiosim

### For MIST Files
1. Install MIST
2. Copy MIST to a working directory of choice
3. Copy the files Example*Random.zip, Example*_short_RepOpt, RunSimulations.bat to the working directory
4. Open Anaconda prompt and change to the working directory using the command CD DIrectoryYouChose
5. Execute the script RunSimulations.bat


### To Regenerate iBioSim Results
0. Optional: You can regenerate the SBML files already provided in this repository using the command: python SBMLExamplesForPaper.py
1. Install iBioSim using the instructions in: http://www.async.ece.utah.edu/ibiosim
2. Run iBioSim
3. Select from the menu: File->New->Project.
4. Import the Disease.omex archive and the results will be generated.
5. Run python parser.py <directory> where directory is where the tsd files were generated inside the project directory.
6. Create an Example* directory for each example where the R scripts are located. Copy the converted csv files to their corresponding Example* directory. Execute plot.R. 

### To Compare results and generate plots
1. Copy all iBioSim results to a sub-directory called results in the same directory MIST results reside
2. Copy the file CompareResults.py to the MIST directory where MIST results exist
3. Type the command "python CompareResults.py"


VERSION HISTORY:
----------------
Development started on April 2017 following this discussion on the SBML mailing list:
https://groups.google.com/forum/#!topic/sbml-discuss/gQsLHmP5i9U

First upload to Github on 9-Sep-2017 - no version number assigned
Major update on 12-MArch-2018 


DEVELOPER CONTACT INFO:
-----------------------

Please pass questions according to implementation to:

### SBML Arrays
Chris Myers

Email: myers@ece.utah.edu

http://www.async.ece.utah.edu/Myers

Leandro Watanabe

Email: l.watanabe@utah.edu

https://leandrohw.github.io/

### MIST
Jacob Barhak

Email: jacob.barhak@gmail.com

http://sites.google.com/site/jacobbarhak/



ACKNOWLEDGEMENTS:
-----------------
Many thanks to the SBML discussion groups that helped with this work by participating in the discussion or providing solutions:  Lucian Smith, Brett Olivier, Nicolas Le Novere, Fengkai Zhang, Sarah Keating.

This work used the MIcro Simulation Tool (MIST) that is based on IEST. The IEST GPL disease modeling framework was initially supported by the Biostatistics and Economic Modeling Core of the MDRTC (P60DK020572) and by the Methods and Measurement Core of the MCDTR (P30DK092926), both funded by the National Institute of Diabetes and Digestive and Kidney Diseases. The modeling framework was initially defined as GPL and was funded by Chronic Disease Modeling for Clinical Research Innovations grant (R21DK075077) from the same institute. MIST, however, was developed without financial support.

Developments for arrays within iBioSim has been supported by the National Science Foundation under Grants CCF-1218095 and CCF-1748200. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

The arrays package support and its development within the JSBML API was supported by Google Summer of Code 2014. 

LICENSE
-------
This file is part of the SBML Arrays Examples. The SBML Arrays Examples is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. The SBML Arrays is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

ADDITIONAL CLARIFICATION

The SBML Arrays Examples is distributed in the hope that it will be useful, but "as is" and WITHOUT ANY WARRANTY of any kind, including any warranty that it will not infringe on any property rights of another party or the IMPLIED WARRANTIES OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. THE AUTHORS assume no responsibilities with respect to the use of the SBML Arrays Examples.
See the GNU Lesser General Public License for more details.
