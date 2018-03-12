# Copyright (C) 2018 Jacob Barhak, Leandro Watanabe, Chris Myres 
# This file is part of the SBML MIST Examples. 
# The SBML MIST Examples is free software: you can redistribute it and/or 
# modify it under the terms of the GNU General Public License as published by 
# the Free Software Foundation, either version 3 of the License, or 
# (at your option) any later version.
# The SBML MIST is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
# See the GNU General Public License for more details.
# ADDITIONAL CLARIFICATION
# The SBML MIST Examples is distributed in the hope that it will be 
# useful, but "as is" and WITHOUT ANY WARRANTY of any kind, including any 
# warranty that it will not infringe on any property rights of another party 
# or the IMPLIED WARRANTIES OF MERCHANTABILITY or FITNESS FOR A PARTICULAR 
# PURPOSE. THE AUTHORS assume no responsibilities with respect to the use of 
# the SBML MIST Examples.

# This specific file was written to compare iBioSim results to MIST results
# It generates the summary statistical comparison files

from __future__ import division
import csv
import os
import pandas
import math
from bokeh.plotting import figure
from bokeh.resources import CDN
from bokeh.embed import file_html
from bokeh.layouts import row, column
from bokeh.models import Range1d
from bokeh.palettes import Category20


ResultsDirectory = 'results' + os.sep 

ParametersForExamples = [ [('alive','Alive'), ('dead', 'Dead')],
                          [('health','Healthy'),('sick','Sick'),('dead','Dead')],
                          [('health','Healthy'),('sick','Sick'),('dead','Dead')],
                          [('health','Healthy'),('sick','Sick'),('dead','Dead'),('age', 'Age')],
                          [('health','Healthy'),('sick','Sick'),('dead','Dead'),('age', 'Age'),('bp','BP'),('cost_year','CostThisYear'),('cost','Cost')],
        ]



Statifications = [ [('_','')],
                   [('_','')],
                   [('_','_female'),('_','_male')],
                   [('_','_female'),('_','_male')],
                   [('_young_','_female'),('_old_','_female'),('_young_','_male'),('_old_','_male')],
                   ]
                   
                  
StatCategories = ('','Min','Max','Mean','STD')
StatCategoriesToPlotInStatistics = ('','Mean','STD')
StatCategoriesToPlotInSideBeSide = ('','Mean')
RepetitionCategories = [(1,'Random_9000000'),(10,'Random_900009'),(100,'Random_90000')]

def FloatEmpty(Text,NullReturnValue = 0):
    "converts text to float or returns zero for NA or blank"
    if Text in ['','NA']:
        return NullReturnValue
    else:
        return(float(Text))
    
ResDict = {}
for ExampleNumber in range(1,6):
    PlotColumn = []                 
    OutArray = []
    for (RepetitionCategory,FilePrefixMIST) in RepetitionCategories:
        for (Parameter,ParameterMIST) in ParametersForExamples[ExampleNumber-1]:
            for (StartificationIndex,(StartificationPrefix,StartificationPostfix)) in enumerate(Statifications[ExampleNumber-1]):
                FileNameToLoad1 = ResultsDirectory+'Example'+str(ExampleNumber)+'_n='+str(RepetitionCategory)+StartificationPrefix+Parameter+StartificationPostfix+'.csv'
                TheInputFile1 = open(FileNameToLoad1)
                ReadCSV = map(None,csv.reader(TheInputFile1))
                # Remove title line and year column
                # the data will be organized as [min,max,mean,sd] in rows
                Results1 = map (None,*(ReadCSV[1:]))[1:]
                TheInputFile1.close()
                for (StatCategoryIndex, StatCategory) in enumerate(StatCategories):
                    if RepetitionCategory != 1 and StatCategory !='' or RepetitionCategory == 1 and StatCategory =='':
                        FileNameToLoad2 = 'Example'+str(ExampleNumber)+FilePrefixMIST+StatCategory+'.csv'
                        TheInputFile2 = open(FileNameToLoad2)
                        Results2 = map(None,csv.reader(TheInputFile2))
                        Results2Transposed = map(None,*Results2)
                        TheInputFile2.close()
                        #Collect the proper row to compare from iBioSim results
                        #in case of raw results, collect the min column
                        DataToCompare1 = Results1[max(0,StatCategoryIndex-1)]
                        # Locate the correct stratification and line
                        StratificationsLocated = 0
                        for (ColumnIndex,YearLocated) in enumerate(Results2[3]):
                            if YearLocated == '0':
                                if StratificationsLocated == StartificationIndex:
                                    # found the start record the index
                                    StartColumnIndex=ColumnIndex
                                    break
                                else:
                                    StratificationsLocated += 1
                        # Locate the Row
                        ChosenRowIndex = Results2Transposed[1].index(ParameterMIST)
                        DataToCompare2 = Results2[ChosenRowIndex][StartColumnIndex:StartColumnIndex+len(DataToCompare1)]
                        DiffVector = [ abs(FloatEmpty(Entry1)-FloatEmpty(Entry2)) for (Entry1,Entry2) in zip(DataToCompare1,DataToCompare2)]
                        OutArray.append(['Stat']+[StatCategory]+['Repetitions']+[RepetitionCategory]+['Stratification']+[StartificationPrefix+StartificationPostfix]+['']*(len(DataToCompare1)-6))
                        OutArray.append([ParameterMIST]+range(len(DataToCompare1)))
                        OutArray.append(['iBioSim']+list(DataToCompare1))
                        OutArray.append(['MIST']+list(DataToCompare2))                
                        OutArray.append(['Diff']+DiffVector)
                        OutArray.append(['']*(len(DataToCompare1)+1))
                        # Organize results in a dictionary
                        ResDict[('Diff',ExampleNumber,RepetitionCategory,Parameter,StartificationPrefix,StartificationPostfix,StatCategory)]=DiffVector[1:]
                        ResDict[('iBioSim',ExampleNumber,RepetitionCategory,Parameter,StartificationPrefix,StartificationPostfix,StatCategory)] = [FloatEmpty(Entry,None) for Entry in DataToCompare1]  
                        ResDict[('MIST',ExampleNumber,RepetitionCategory,Parameter,StartificationPrefix,StartificationPostfix,StatCategory)] = [FloatEmpty(Entry,None) for Entry in DataToCompare2] 
    DataFrame = pandas.DataFrame(OutArray)
    DataFrame.to_csv('Example'+str(ExampleNumber)+'Comparison.csv', header = False, index = False)

    for (Parameter,ParameterMIST) in ParametersForExamples[ExampleNumber-1]:
        for (StartificationIndex,(StartificationPrefix,StartificationPostfix)) in enumerate(Statifications[ExampleNumber-1]):
            Plot1 = figure(title = ('Difference Example '+str(ExampleNumber)+' '+ ParameterMIST +' '+ StartificationPrefix.replace('_','')+' '+StartificationPostfix.replace('_','')).replace('  ',' ') , x_axis_label = 'Time', y_axis_label = 'Difference', tools=['save'], x_range = Range1d(0, 11))
            CategoricalRange = [str(RepetitionCategory) for (RepetitionCategory,FilePrefixMIST) in RepetitionCategories]
            Plot2 = figure(title = ('Average Difference Example '+str(ExampleNumber) +' '+ ParameterMIST+' '+StartificationPrefix.replace('_','')+' '+StartificationPostfix.replace('_','')).replace('  ',' ') , x_axis_label = 'Repetitions', y_axis_label = 'Difference', tools=['save'], x_range = CategoricalRange)
            for (StatCategoryIndex, StatCategory) in enumerate(StatCategories):
                if StatCategory in StatCategoriesToPlotInStatistics:
                    for (RepetitionCategory,FilePrefixMIST) in RepetitionCategories:
                        if RepetitionCategory != 1 and StatCategory !='' or RepetitionCategory == 1 and StatCategory =='':
                            MarkerTypes = ['circle','inverted_triangle','triangle','circle','square']
                            Marker = MarkerTypes[StatCategoryIndex]
                            Color = ['red','yellow','green'] [int(math.log10(RepetitionCategory))]
                            Difference = ResDict[('Diff',ExampleNumber,RepetitionCategory,Parameter,StartificationPrefix,StartificationPostfix,StatCategory)]
                            if RepetitionCategory == 1:
                                CombinedLegendToUse = 'Single Run'
                            else:
                                CombinedLegendToUse =  StatCategory + ' of ' + str(RepetitionCategory)                      
                            Plot1.scatter(legend = CombinedLegendToUse , x=range(1,11), y=Difference, marker = Marker, fill_color = Color, fill_alpha = 0.4, size = 10)
                            Plot2.scatter(legend = CombinedLegendToUse, x=[str(RepetitionCategory)], y=[sum(Difference)/10], marker = Marker, fill_color = Color, fill_alpha = 0.4, size = 10)
                            PlotRow = row([Plot1,Plot2])
                            Plot1.legend.location = 'top_center'
                            Plot1.legend.level = 'underlay'                           
                            Plot2.legend.location = 'center_right'
                            Plot2.legend.level = 'underlay'
            PlotColumn.append(PlotRow)

    SendToPlot = column(PlotColumn)
    Html = file_html(SendToPlot, CDN, 'Results for Example'+str(ExampleNumber))
    OutFile = open('Example'+ str(ExampleNumber) +'DiffPlot.html','w')
    OutFile.write(Html)
    OutFile.close()
                       

def DefineNewPlot(RepetitionCategory,ExampleNumber):
    "Define New Plots"                 
    Plot1 = figure(title = 'MIST Results ' + str(RepetitionCategory) + ' Repetitions for Example '+str(ExampleNumber) , x_axis_label = 'Time', y_axis_label = 'Result', tools=['save'], x_range = Range1d(0, 11))
    Plot2 = figure(title = 'iBioSim Results ' + str(RepetitionCategory) + ' Repetitions for Example '+str(ExampleNumber) , x_axis_label = 'Time', y_axis_label = 'Result', tools=['save'], x_range = Range1d(0, 11))
    return (Plot1,Plot2)

                
for (RepetitionCategory,FilePrefixMIST) in RepetitionCategories:
    PlotColumn = []
    for ExampleNumber in range(1,6):
        (Plot1,Plot2) = DefineNewPlot(RepetitionCategory,ExampleNumber)   
        for (ParamEnum,(Parameter,ParameterMIST)) in enumerate(ParametersForExamples[ExampleNumber-1]):
            if ParamEnum in [3,5]:
                # Dont put too many parameters in one plot
                PlotRow = row([Plot1,Plot2])
                PlotColumn.append(PlotRow)
                (Plot1,Plot2) = DefineNewPlot(RepetitionCategory,ExampleNumber)                   
            for (StartificationIndex,(StartificationPrefix,StartificationPostfix)) in enumerate(Statifications[ExampleNumber-1]):
                for (StatCategoryIndex, StatCategory) in enumerate(StatCategories):
                    if StatCategory in StatCategoriesToPlotInSideBeSide and  (RepetitionCategory != 1 and StatCategory !='' or RepetitionCategory == 1 and StatCategory ==''): 
                        Data1 = ResDict[('MIST',ExampleNumber,RepetitionCategory,Parameter,StartificationPrefix,StartificationPostfix,StatCategory)]
                        Data2 = ResDict[('iBioSim',ExampleNumber,RepetitionCategory,Parameter,StartificationPrefix,StartificationPostfix,StatCategory)]
                        Color = Category20[19][ParamEnum]
                        LineDash = [[1], [4,4], [6,2], [1,2]] [StartificationIndex]
                        CombinedLegendToUse = (ParameterMIST +' '+ StartificationPrefix.replace('_','')+' ' + StartificationPostfix.replace('_','')).replace('  ',' ')
                        Plot1.legend.level = 'underlay'
                        Plot2.legend.level = 'underlay'     
                        Plot1.line(x=range(0,11), y = Data1, line_dash=LineDash, line_color = Color, legend = CombinedLegendToUse, line_width = 3 )
                        Plot2.line(x=range(0,11), y = Data2, line_dash=LineDash, line_color = Color, legend = CombinedLegendToUse, line_width = 3 )
                      
        PlotRow = row([Plot1,Plot2])
        PlotColumn.append(PlotRow)
    SendToPlot = column(PlotColumn)
    Html = file_html(SendToPlot, CDN, 'Repetitions'+ str(RepetitionCategory))
    OutFile = open('Repetitions'+ str(RepetitionCategory) + '.html','w')
    OutFile.write(Html)
    OutFile.close()
        
                       

               


