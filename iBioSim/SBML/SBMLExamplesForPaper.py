# Copyright (C) 2017-2018 Jacob Barhak, Leandro Watanabe, Chris Myres 
# Copyright (C) 2013-2015 Jacob Barhak 
# Copyright (C) 2009-2012 The Regents of the University of Michigan
# This file is part of the SBML Array Examples. 
# The SBML Array Examples is free software: you can redistribute it and/or 
# modify it under the terms of the Lesser GNU General Public License as 
# published by the Free Software Foundation, either version 3 of the 
# License, or (at your option) any later version.
# The SBML Array is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
# See the Lesser GNU General Public License for more details.
# ADDITIONAL CLARIFICATION
# The SBML Array Examples is distributed in the hope that it will be 
# useful, but "as is" and WITHOUT ANY WARRANTY of any kind, including any 
# warranty that it will not infringe on any property rights of another party 
# or the IMPLIED WARRANTIES OF MERCHANTABILITY or FITNESS FOR A PARTICULAR 
# PURPOSE. THE AUTHORS assume no responsibilities with respect to the use 
# of the SBML ARRAY Examples.
# The SBML Array Examples initially used examples derived from the 
# MIcroSimulation Tool (MIST) that was derived from the 
# Indirect Estimation and Simulation Tool (IEST) and used code distributed 
# under the IEST name. 
# MIST and IEST were originally provided under GPL license, yet after
# interaction with the University of Michigan Tech Transfer Office, 
# permission was provided to release this code under the more permissive  
# LGPL license.


import libsbml 
import sys


def Check(Value, Message):
    "Check Code"
    # If 'value' is None, prints an error message constructed using   
    # 'message' and then exits with status code 1.
    # If 'value' is an integer, it assumes it is a libSBML return status code.  
    # If the code value is LIBSBML_OPERATION_SUCCESS, 
    # returns without further action; if it is not,
    # prints an error message constructed using 'message' along with text from   
    #libSBML explaining the meaning of the code, and exits with status code 1.     
    if Value == None:
        raise SystemExit('LibSBML returned a null value trying to ' + Message + '.')   
    elif type(Value) is int:
        if Value == libsbml.LIBSBML_OPERATION_SUCCESS:
            return
        else:
            ErrorMessage = 'Error encountered trying to ' + Message + '.' + 'LibSBML returned error code ' + str(Value) + ': "' + libsbml.OperationReturnValue_toString(Value).strip() + '"'       
            raise SystemExit(ErrorMessage)
    else:
        return
        


def DefineSBMLParam(Model, ID , IsConstant = None, Value = None, Units = None):
    "defines a parameter within a model "
    Param = Model.createParameter()
    Check(Param, 'create parameter ' + ID)
    Check(Param.setId(ID), 'set parameter ' + ID + ' id')
    if IsConstant != None:
        Check(Param.setConstant(IsConstant), 'set parameter ' + ID + ' constant to ' + str(IsConstant))
    if Value != None:
        Check(Param.setValue(Value), 'set parameter ' + ID + ' value to ' + str(Value))
    if Units != None:
        Check(Param.setUnits(Units), 'set parameter ' + ID + ' units to ' + str(Units))    
    return Param
    
def DefineDimension(Container, Size, DimEnum, ID=None):
    "Create a dimension to a container"
    Dim = Container.getPlugin("arrays").createDimension()
    if ID != None:
        Dim.setId(ID)
    Dim.setSize(Size.getId())
    Dim.setArrayDimension(DimEnum)
    return Dim

def DefineIndex(Container, DimEnum, ReferencedAttribute, MathExpr):
    "Create an index to a container"
    Index = Container.getPlugin("arrays").createIndex()
    Index.setArrayDimension(DimEnum)
    Index.setReferencedAttribute(ReferencedAttribute)
    Index1Math = libsbml.parseL3Formula(MathExpr)
    Check(Index1Math,'Check Math') 
    Index.setMath(Index1Math)
    return Index

    
def DefineInitialAssignment(Container, Symbol, Size, MathExpression):
    RetInit = Container.createInitialAssignment()
    RetInit.setSymbol(Symbol)
    Init1Math = libsbml.parseL3FormulaWithModel(MathExpression,Container)
    Check(Init1Math,'Check Math') 
    RetInit.setMath(Init1Math)
    DefineDimension(Container=RetInit, Size = Size, DimEnum=0, ID='d0')
    DefineIndex(Container=RetInit, DimEnum=0, ReferencedAttribute='symbol', MathExpr='d0')
    return RetInit
    

def DefineEvent(Container, Name, TriggerFormula, PriorityFormula, DelayFormula, Assignments):
    "Create Event"
    
    RetEvent = Container.createEvent()
    RetEvent.setName(Name)
    RetEvent.setUseValuesFromTriggerTime(False)

    RetTrigger = RetEvent.createTrigger()
    # Since event state trigger needs to be rechecked events are persistant
    RetTrigger.setPersistent(False)
    RetTrigger.setInitialValue(False)
    TriggerMath = libsbml.parseL3FormulaWithModel(TriggerFormula,Container)
    Check(TriggerMath,'Check Trigger Math') 
    RetTrigger.setMath(TriggerMath)

    RetPriority = RetEvent.createPriority()
    PriorityMath = libsbml.parseL3FormulaWithModel(PriorityFormula,Container)
    Check(PriorityMath,'Check Priority Math') 
    RetPriority.setMath(PriorityMath)
    
    RetDelay = RetEvent.createDelay()
    DelayyMath = libsbml.parseL3FormulaWithModel(DelayFormula,Container)
    Check(DelayyMath,'Check Delay Math') 
    RetDelay.setMath(DelayyMath)

    

    for (AssignmentVariable, AssignmentFormula, ArrayData) in (Assignments):
        Assignment = RetEvent.createEventAssignment()
        Assignment.setVariable(AssignmentVariable)
        AssignmentMath = libsbml.parseL3FormulaWithModel(AssignmentFormula,Container)        
        Check(AssignmentMath,'Assignment Math') 
        Assignment.setMath(AssignmentMath)
        if ArrayData != None:
            DimEnum, ReferencedAttribute, MathExpr = ArrayData
            DefineIndex(Assignment, DimEnum, ReferencedAttribute, MathExpr)
   
    return RetEvent
    


def DefineDualEventArrays(Container, Size, Name, InstructionNumber, TriggerFormulas, LeadPriority, DelayFormula, Assignments, NextLine=None):
    "Create Dual Events with consecutive line numbers and trigger probability"
    # The lead event creates a random variable to be used in the second event
    # Line numbers will be consecutive 
    # First Add line number assignment to the 
    # The name Random means an array of Random numbers to be generated if the 
    # first event to be potentially checked in the second event
    # The second event can have multiple duplicates and defined in an array
    # To create a terminal event, just create a trigger without a complementary
    # trigger to advance the instruction number if does not happen

    EventList = []
    if NextLine == None:
        NextLineToUse = InstructionNumber+1
    else:
        NextLineToUse = NextLine

    Assignments1 = [ ('Random', 'uniform(0,1)', (0,'variable','d0' ) ) ,
                     ('InstructionNumber', str(InstructionNumber+0.5), (0,'variable','d0' ))
                    ]
    
    Event1 = DefineEvent(Container = Container, Name = Name + '_Lead', TriggerFormula = 'selector(InstructionNumber,d0) == '+str(InstructionNumber), PriorityFormula = str(LeadPriority), DelayFormula=DelayFormula, Assignments = Assignments1)
    DefineDimension(Event1, Size = Size, DimEnum=0, ID='d0')
    EventList.append(Event1)

    for (TriggerEnum,CurrentTriggerFormula) in enumerate(TriggerFormulas):
        CurrentAssignment = Assignments[TriggerEnum]
        
        NextLineAssignment = [ ('InstructionNumber', str(NextLineToUse), (0,'variable','d0' )) ]
        Assignments2 = CurrentAssignment + NextLineAssignment
        
        Event2 = DefineEvent(Container = Container, Name = Name+'_'+str(TriggerEnum), TriggerFormula= 'selector(InstructionNumber,d0) == ' + str(InstructionNumber+0.5) + ' && ' + CurrentTriggerFormula, PriorityFormula = str(LeadPriority+1), DelayFormula=DelayFormula, Assignments = Assignments2)
        DefineDimension(Event2, Size = Size, DimEnum=0, ID='d0')
        EventList.append(Event2)
        
    return EventList

    

def DefineFunctions (Model):
    # Define all functions to be used
    
    # Distribution Function
    Func1 = Model.createFunctionDefinition()
    Func1.setId('uniform')
    Func1Math = libsbml.parseL3Formula('lambda(a,b,(a+b)/2)')
    Check(Func1Math,'Check Math') 
    Func1.setMath(Func1Math)

    UniformDistrAnnotation = """<annotation>
          <distribution xmlns="http://sbml.org/annotations/distribution" definition="http://en.wikipedia.org/wiki/Uniform_distribution_(continuous)"/>
        </annotation>"""
    Func1.appendAnnotation(UniformDistrAnnotation)
    
    DistribPlugin1 = Func1.getPlugin("distrib")
    DrawFromDistrib = DistribPlugin1.createDrawFromDistribution()
    InputA = DrawFromDistrib.createDistribInput()
    InputA.setId('a')
    InputA.setIndex(0)
    
    InputB = DrawFromDistrib.createDistribInput()
    InputB.setId('b')
    InputB.setIndex(1)

    Uncert = libsbml.UncertMLNode_createDistributionNode("UniformDistribution", "minimum, maximum", "a,b");
    DrawFromDistrib.setUncertML(Uncert)
    
    Func2 = Model.createFunctionDefinition()
    Func2.setId('min')
    Func2Math = libsbml.parseL3Formula('lambda(a,b,piecewise(a, a < b, b))')
    Check(Func2Math,'Check Math') 
    Func2.setMath(Func2Math)    
    
    return Func1, Func2
    
def DefineParamArray(Model, ParamID, Size, DeFineState = False, IsConstant = False, Value = 0, Units = ''):
    "Define Parameter array"
    Entities = []
    # initialize to default values
    Entity = DefineSBMLParam(Model, ID = ParamID , IsConstant = IsConstant, Value = Value, Units = Units)
    DefineDimension(Entity, Size = Size, DimEnum=0, ID='d0')
    Entities.append(Entity)
    return Entities


def InitExample(NumberToSimulate):
    "Initialize parameters including simulation size and repetitions "
    
    NameSpace = libsbml.SBMLNamespaces (3, 2, "arrays", 1)    
    NameSpace.addPackageNamespace("distrib",1)

    try:
        Document = libsbml.SBMLDocument(NameSpace)
    except ValueError:
        raise SystemExit('Could not create SBMLDocumention object')
    Model = Document.createModel()   
    Check(Model, 'create model')
    Document.setPackageRequired("arrays", True)

    # Define Individual unit
    IndividualsUnitDefinition = Model.createUnitDefinition()   
    Check(IndividualsUnitDefinition,                     'create unit definition')
    Check(IndividualsUnitDefinition.setId('Individual'), 'set unit definition id')
    IndividualsUnit = IndividualsUnitDefinition.createUnit()
    Check(IndividualsUnit,                               'create unit on Individual')
    Check(IndividualsUnit.setKind(libsbml.UNIT_KIND_ITEM),       'set unit kind')
    Check(IndividualsUnit.setExponent(0),                'set unit exponent')
    Check(IndividualsUnit.setScale(0),                   'set unit scale')
    Check(IndividualsUnit.setMultiplier(1),              'set unit multiplier')
    
    # Parameters
    SimulationSize = DefineSBMLParam(Model, ID = 'SimulationSize' , IsConstant = True, Value = NumberToSimulate, Units = 'Individual')

    InstructionNumber = DefineSBMLParam(Model, ID = 'InstructionNumber' , IsConstant = False, Value = 0, Units = '')
    DefineDimension(InstructionNumber, Size = SimulationSize, DimEnum=0, ID='d0')
    
    # Define Random parameter
    Random = DefineParamArray(Model=Model, ParamID='Random', Size=SimulationSize, Value=-1)    

    # Define the Time parameter
    Time = DefineParamArray(Model=Model, ParamID='Time', Size=SimulationSize, Value=-1)    

    DefineFunctions(Model)    

    return (NameSpace,Document,Model,IndividualsUnit,SimulationSize,InstructionNumber,DefineDimension,Random,Time)



def TransitionFormula(Model, FromStates, ToStates, Probabilities, Size, InstructionNumber, NextLine = None):
    "Define transition formula"

    Assignments = []
    for (ToStateEnum,ToState) in enumerate(ToStates):
        FromStateAssignments = [ (FromState, '0', (0,'variable','d0' ) ) for FromState in FromStates ]
        ToStateAssignments = [ (ToState, '1', (0,'variable','d0' ) ) ] 
        Assignments.append( FromStateAssignments + ToStateAssignments)
    # Append the final null assignment
    Assignments.append([])
    
    
    if NextLine==InstructionNumber:
        # This indicates an event state since instruction does not advance
        DelyFormula = '0'
    else:
        DelyFormula = '1'
    EventList = []
    for (FromEnum,FromState) in enumerate(FromStates):
        FromStateClause = 'selector( ' + FromState + ',d0)==1'

        AccumulatedProbability = '0'
        TriggerFormulas = []
        for (ToEnum,ToState) in enumerate(ToStates+[None]):
            if ToState != None:
                Probability = Probabilities[FromEnum][ToEnum]
                NextAccumulatedProbability = AccumulatedProbability + ' + ' + Probability
            else:
                NextAccumulatedProbability = '1'
            TriggerFormula = FromStateClause + ' && selector(Random,d0)>=(' + AccumulatedProbability + ') && selector(Random,d0)<('+ NextAccumulatedProbability + ')' 
            TriggerFormulas.append(TriggerFormula)
            AccumulatedProbability = NextAccumulatedProbability

        NewEvents = DefineDualEventArrays(Container=Model, Size = Size, Name = FromState + '_Transition', InstructionNumber=InstructionNumber, TriggerFormulas = TriggerFormulas, LeadPriority = 0, DelayFormula=DelyFormula, Assignments=Assignments, NextLine=NextLine)
        EventList.append(NewEvents)
    return EventList



def DefineTerminalEvent(Model, TerminalStates, Size, TimeLimit):
    "Define the termination event"
    
    TimeTriggerFormula = '(selector(Time,d0) < ' + str(TimeLimit) + ')'
    if TerminalStates != []:
        TerminalTriggerFormula = '(' + (' && '.join(['selector('+TerminalState+',d0)==0' for TerminalState in TerminalStates])) + ')'
    else:
        TerminalTriggerFormula = 'true'

    # First advance Time
    Assignments1 = [ 
                            ('Time', 'selector(Time,d0)+1', (0,'variable','d0' ) ) ,
                            ('InstructionNumber', '0.1', (0,'variable','d0' ))
                   ] 
                                       
    # The second event will advance InstructionNumber unless terminated
    Assignments2 = [ ('InstructionNumber', '0.2', (0,'variable','d0' )) ]


    # The second event will advance InstructionNumber unless terminated
    Assignments3 = [ ('InstructionNumber', '1', (0,'variable','d0' )) ]

    Event1 = DefineEvent(Container = Model, Name = 'TimeIncrease', TriggerFormula = 'selector(InstructionNumber,d0) == 0', PriorityFormula = '0', DelayFormula = '1', Assignments = Assignments1)
    DefineDimension(Event1, Size = Size, DimEnum=0, ID='d0')
    Event2 = DefineEvent(Container = Model, Name = 'ResultSamplePoint', TriggerFormula = 'selector(InstructionNumber,d0) == 0.1', PriorityFormula = '0', DelayFormula = '1', Assignments = Assignments2)
    DefineDimension(Event2, Size = Size, DimEnum=0, ID='d0')
    Event3 = DefineEvent(Container = Model, Name = 'Terminate', TriggerFormula = 'selector(InstructionNumber,d0) == 0.2 && ' + TerminalTriggerFormula + ' && ' + TimeTriggerFormula, PriorityFormula = '0', DelayFormula = '1', Assignments = Assignments3)
    DefineDimension(Event3, Size = Size, DimEnum=0, ID='d0')
    EventList = [Event1,Event2,Event3]

    return EventList


def DefineStates(Model, States, Size):
    "Define all states in one bundle"
    RetList = []
    for State in States:
        RetVal = DefineParamArray(Model,State,Size,True)        
        RetList.append(RetVal)
    return RetList



        
def CreateExample1(Repetitions = 1):
    "Create SBML Array simulation example 1"
    PopulationSize = 100
    TimeLimit = 10
    (NameSpace,Document,Model,IndividualsUnit,SimulationSize,InstructionNumber,DefineDimension,Random,Time) = InitExample(PopulationSize*Repetitions)

    StateList = ['Alive','Dead']
    DefineStates(Model, StateList, SimulationSize)  
    
    DefineInitialAssignment(Container = Model, Symbol = 'Alive', Size = SimulationSize, MathExpression = 'selector(vector(' + str([1]*PopulationSize*Repetitions)[1:-1] + '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Dead', Size = SimulationSize, MathExpression = 'selector(vector(' + str([0]*PopulationSize*Repetitions)[1:-1] + '),d0)')

    # terminate event at start of loop
    DefineTerminalEvent (Model = Model, TerminalStates = ['Dead'], Size = SimulationSize, TimeLimit=TimeLimit)

    TransitionFormula(Model = Model, FromStates = [ 'Alive' ], ToStates = [ 'Dead'] , Probabilities = [ [ '0.05' ] ] , Size = SimulationSize, InstructionNumber = 1, NextLine = 0)
    
    return libsbml.writeSBMLToString(Document)
    
    
    
def CreateExample2(Repetitions = 1):
    "Create SBML Array simulation example 2"
    PopulationSize = 100
    TimeLimit = 10
    (NameSpace,Document,Model,IndividualsUnit,SimulationSize,InstructionNumber,DefineDimension,Random,Time) = InitExample(PopulationSize*Repetitions)

    StateList = ['Healthy','Sick','Dead']
    DefineStates(Model, StateList, SimulationSize)  


    DefineInitialAssignment(Container = Model, Symbol = 'Healthy', Size = SimulationSize, MathExpression = 'selector(vector('+str([1]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Sick', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Dead', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')

    # terminate event at start of loop
    DefineTerminalEvent (Model = Model, TerminalStates = ['Dead'], Size = SimulationSize, TimeLimit=TimeLimit)


    TransitionFormula(Model = Model, FromStates = [ 'Healthy' ], ToStates = [ 'Dead', 'Sick'] , Probabilities = [ [ '0.01' ,'0.2'] ] , Size = SimulationSize, InstructionNumber = 1, NextLine = 0)
    TransitionFormula(Model = Model, FromStates = [ 'Sick' ], ToStates = [ 'Healthy','Dead'] , Probabilities = [ [ '0.1','0.3' ] ] , Size = SimulationSize, InstructionNumber = 1, NextLine = 0)

    return libsbml.writeSBMLToString(Document)
    





def CreateExample3(Repetitions = 1):
    "Create SBML Array simulation example 3"
    PopulationSize = 100
    TimeLimit = 10
    (NameSpace,Document,Model,IndividualsUnit,SimulationSize,InstructionNumber,DefineDimension,Random,Time) = InitExample(PopulationSize*Repetitions)

    StateList = ['Healthy','Sick','Dead']
    DefineStates(Model, StateList, SimulationSize)  

    DefineParamArray(Model,'Male',SimulationSize)


    DefineInitialAssignment(Container = Model, Symbol = 'Healthy', Size = SimulationSize, MathExpression = 'selector(vector('+str([1]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Sick', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Dead', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Male', Size = SimulationSize, MathExpression = 'selector(vector('+str(([0]*(PopulationSize/2)+[1]*(PopulationSize/2))*Repetitions)[1:-1]+ '),d0)')



    # terminate event at start of loop
    DefineTerminalEvent (Model = Model, TerminalStates = ['Dead'], Size = SimulationSize, TimeLimit=TimeLimit)

    TransitionFormula(Model = Model, FromStates = [ 'Healthy' ], ToStates = [ 'Dead', 'Sick'] , Probabilities = [ [ '0.01' ,'0.1*(1+selector(Male,d0))'] ] , Size = SimulationSize, InstructionNumber = 1, NextLine = 0)
    TransitionFormula(Model = Model, FromStates = [ 'Sick' ], ToStates = [ 'Healthy','Dead'] , Probabilities = [ [ '0.1','0.3' ] ] , Size = SimulationSize, InstructionNumber = 1, NextLine = 0)

    return libsbml.writeSBMLToString(Document)



def CreateExample4(Repetitions = 1):
    "Create SBML Array simulation example 4"
    PopulationSize = 100
    TimeLimit = 10
    (NameSpace,Document,Model,IndividualsUnit,SimulationSize,InstructionNumber,DefineDimension,Random,Time) = InitExample(PopulationSize*Repetitions)

    StateList = ['Healthy','Sick','Dead']
    DefineStates(Model, StateList, SimulationSize)  

    DefineParamArray(Model,'Male',SimulationSize)
    DefineParamArray(Model,'Age',SimulationSize)


    DefineInitialAssignment(Container = Model, Symbol = 'Healthy', Size = SimulationSize, MathExpression = 'selector(vector('+str([1]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Sick', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Dead', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Male', Size = SimulationSize, MathExpression = 'selector(vector('+str(([0]*(PopulationSize/2)+[1]*(PopulationSize/2))*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Age', Size = SimulationSize, MathExpression = 'selector(vector('+str(list(range(1,(PopulationSize/2)+1))*2*Repetitions)[1:-1]+ '),d0)')

    # terminate event at start of loop
    DefineTerminalEvent (Model = Model, TerminalStates = ['Dead'], Size = SimulationSize, TimeLimit=TimeLimit)
    
    Assignments =   (
                        [ 
                            ('Age', 'selector(Age,d0)+1', (0,'variable','d0' ) ) ,
                        ]
                        ,
                       []
                    )                    

    DefineDualEventArrays(Container=Model, Size = SimulationSize, Name='Age Increase', InstructionNumber=1, TriggerFormulas = ('true','false'), LeadPriority = 0, DelayFormula='1', Assignments=Assignments)

    
    TransitionFormula(Model = Model, FromStates = [ 'Healthy' ], ToStates = [ 'Dead', 'Sick'] , Probabilities = [ [ 'selector(Age,d0)/1000' ,'min(0.8,0.1*(1+selector(Male,d0))+0.01*selector(Age,d0))'] ] , Size = SimulationSize, InstructionNumber = 2, NextLine = 0)
    TransitionFormula(Model = Model, FromStates = [ 'Sick' ], ToStates = [ 'Healthy','Dead'] , Probabilities = [ [ '0.1','min(0.9, 0.01*selector(Age,d0) + 0.2*selector(Male,d0))' ] ] , Size = SimulationSize, InstructionNumber = 2, NextLine = 0)


    return libsbml.writeSBMLToString(Document)


def CreateExample5(Repetitions = 1):
    "Create SBML Array simulation example 5"
    PopulationSize = 100
    TimeLimit = 10
    (NameSpace,Document,Model,IndividualsUnit,SimulationSize,InstructionNumber,DefineDimension,Random,Time) = InitExample(PopulationSize*Repetitions)

    StateList = ['Healthy','Sick','Dead']
    DefineStates(Model, StateList, SimulationSize)  

    DefineParamArray(Model,'Male',SimulationSize)
    DefineParamArray(Model,'Age',SimulationSize)
    DefineParamArray(Model,'BP',SimulationSize)
    DefineParamArray(Model,'Treatment',SimulationSize)
    DefineParamArray(Model,'CostThisYear',SimulationSize)
    DefineParamArray(Model,'Cost',SimulationSize)


    DefineInitialAssignment(Container = Model, Symbol = 'Healthy', Size = SimulationSize, MathExpression = 'selector(vector('+str([1]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Sick', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Dead', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Male', Size = SimulationSize, MathExpression = 'selector(vector('+str(([0]*(PopulationSize/2)+[1]*(PopulationSize/2))*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Age', Size = SimulationSize, MathExpression = 'selector(vector('+str(list(range(1,(PopulationSize/2)+1))*2*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'BP', Size = SimulationSize, MathExpression = 'selector(vector('+str([120]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Treatment', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'CostThisYear', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')
    DefineInitialAssignment(Container = Model, Symbol = 'Cost', Size = SimulationSize, MathExpression = 'selector(vector('+str([0]*PopulationSize*Repetitions)[1:-1]+ '),d0)')

    # terminate event at start of loop
    DefineTerminalEvent (Model = Model, TerminalStates = ['Dead'], Size = SimulationSize, TimeLimit=TimeLimit)
    
    Assignments =   (
                        [ 
                            ('Age', 'selector(Age,d0)+1', (0,'variable','d0' ) ) ,
                        ]
                        ,
                       []
                    )                    

    DefineDualEventArrays(Container=Model, Size = SimulationSize, Name='Age Increase', InstructionNumber=1, TriggerFormulas = ('true','false'), LeadPriority = 0, DelayFormula='1', Assignments=Assignments)

    Assignments =   (
                        [ 
                            ('BP', 'selector(BP,d0) + selector(Age,d0)/10', (0,'variable','d0' ) ) ,
                        ]
                        ,
                       []
                    )                    

    DefineDualEventArrays(Container=Model, Size = SimulationSize, Name='BP Increase', InstructionNumber=2, TriggerFormulas = ('true','false'), LeadPriority = 0, DelayFormula='1', Assignments=Assignments)



    TransitionFormula(Model = Model, FromStates = [ 'Healthy' ], ToStates = [ 'Dead', 'Sick'] , Probabilities = [ [ 'selector(Age,d0)/1000' ,'min(0.8,0.1*(1+selector(Male,d0))+0.01*selector(Age,d0) + ((selector(BP,d0)-120)/100)^2 )'] ] , Size = SimulationSize, InstructionNumber = 3, NextLine = 4)
    TransitionFormula(Model = Model, FromStates = [ 'Sick' ], ToStates = [ 'Healthy','Dead'] , Probabilities = [ [ '0.1','min(0.9, 0.01*selector(Age,d0) + 0.2*selector(Male,d0))' ] ] , Size = SimulationSize, InstructionNumber = 3, NextLine = 4)


    Assignments =   (
                        [ 
                            ('Treatment', 'selector(BP,d0)>140', (0,'variable','d0' ) ) ,
                        ]
                        ,
                       []
                    )                    

    DefineDualEventArrays(Container=Model, Size = SimulationSize, Name='Treatment Trigger', InstructionNumber=4, TriggerFormulas = ('true','false'), LeadPriority = 0, DelayFormula='1', Assignments=Assignments)

    Assignments =   (
                        [ 
                            ('BP', 'selector(BP,d0) - selector(Treatment,d0)*10', (0,'variable','d0' ) ) ,
                        ]
                        ,
                       []
                    )                    

    DefineDualEventArrays(Container=Model, Size = SimulationSize, Name='BP Treatment Effect', InstructionNumber=5, TriggerFormulas = ('true','false'), LeadPriority = 0, DelayFormula='1', Assignments=Assignments)

    Assignments =   (
                        [ 
                            ('CostThisYear', ' selector(Age,d0) + selector(Treatment,d0)*10', (0,'variable','d0' ) ) ,
                        ]
                        ,
                       []
                    )                    

    DefineDualEventArrays(Container=Model, Size = SimulationSize, Name='Calculate Cost', InstructionNumber=6, TriggerFormulas = ('true','false'), LeadPriority = 0, DelayFormula='1', Assignments=Assignments)

    Assignments =   (
                        [ 
                            ('Cost', 'selector(Cost,d0) + selector(CostThisYear,d0)', (0,'variable','d0' ) ) ,
                        ]
                        ,
                       []
                    )                    

    DefineDualEventArrays(Container=Model, Size = SimulationSize, Name='Accumulate Cost', InstructionNumber=7, TriggerFormulas = ('true','false'), LeadPriority = 0, DelayFormula='1', Assignments=Assignments, NextLine=0)

    return libsbml.writeSBMLToString(Document)


   

    
if __name__ == '__main__':
    RepetitionText = '()'
    if len(sys.argv)>1:
        Repetitions = int(sys.argv[1])
        if Repetitions > 0:
            RepetitionText = '(%i)'% Repetitions
    if len(sys.argv)<3:
        Examples = ['1','2','3','4','5']
    else:
        Examples = [sys.argv[2]]
        
    for Example in Examples: 
        FuncName = 'CreateExample'+Example+RepetitionText
        OutText = eval(FuncName)
        File = open('Example'+Example+'.xml','w')
        File.write(OutText)
    
    
    
