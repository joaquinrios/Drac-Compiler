# Drac compiler, version 0.1

This program is free software. You may redistribute it under the terms of the GNU General Public License version 3 or later. See `license.txt` for details.

This Drac compiler is built upon a Buttercup compiler developed by Ariel Ortiz. 

Included in this release:

* Lexical analysis
* Syntactic analysis

## How to Build

At the terminal type:

    make

## How to Run

At the terminal type:

    mono drac.exe <drac_source_file>

Where `<drac_source_file>` is the name of a Drac source file. You can try with these files:

* `prog1.drac`
* `prog2.drac`

## How to Clean

To delete all the files that get created automatically, at the terminal type:

    make clean
