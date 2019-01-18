# Latin Hyper-rectangular Designs

R script containing functions to create a latin hyper-rectangle design (LHrD) of experiments. Default function arguments will produce an example design if outputLHrD() is run from R prompt. This repository was created to provide these functions as a reference for Johnson and Cox, 2017, given below.

List of functions:

makeLHRfromList():  Creates a latin hyper-rectangle corresponding to a
                    named list of factor levels, and samples a design
                    from that latin hyper-rectangle

outputLHrD():       Takes the output from makeLHRfromList() and produces
                    a tabulated experimental design as a data.frame and
                    as ./LHrD.csv


#### Authors: Samuel D. N. Johnson and Sean P. Cox

#### Date: 30 August, 2017

## References:

Johnson, S.D.N. and  S.P. Cox, Evaluating the role of data quality when sharing information in hierarchical multi-stock assessment models, with an application to dover sole. CJFAS, 2019


J. P. Kleijnen. Design and analysis of simulation experiments, volume 20. 
    Springer, 2008.
