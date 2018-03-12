#*******************************************************************************
#
# Copyright (C) 2017 Jacob Barhak, Leandro Watanabe, Chris Myers 
# This file is part of the SBML Arrays Examples.
#
# This script is free software: you can redistribute it and/or 
# modify it under the terms of the GNU Lesser General Public License as 
# published by the Free Software Foundation, either version 3 of the 
# License, or (at your option) any later version.
#
# ADDITIONAL CLARIFICATION
# This script is distributed in the hope that it will be 
# useful, but "as is" and WITHOUT ANY WARRANTY of any kind, including any 
# warranty that it will not infringe on any property rights of another party 
# or the IMPLIED WARRANTIES OF MERCHANTABILITY or FITNESS FOR A PARTICULAR 
# PURPOSE. THE AUTHORS assume no responsibilities with respect to the use 
# of it.
#  
#*******************************************************************************

import sys
import os

def convert_to_csv(input):
    output = input.replace(".tsd", ".csv")
    file = open(input,"r")
    out = open(output,"w")

    text = file.read()
    text = text[1:len(text)-1]
    text = text.replace('"', '')
    while len(text) > 0:
        parse = text[text.find("(")+1:text.find(")")]
        text = text[len(parse)+3:len(text)]
        if len(parse) > 0:
            out.write(parse + "\n")


        file.close()
    out.close()

if __name__ == '__main__':
    directory = sys.argv[1]
    for tsd in os.listdir(directory):
        if tsd.endswith('.tsd'):
            path = os.path.join(directory, tsd)
            convert_to_csv(path)
