### About
  
**MFIaggr** is used for a fast multiple comparison of the cycle dependent 
variance of the fluorescence. It is a web-based implementation of function from package [chipPCR](http://github.com/michbur/chipPCR). 

**Authors**: [Stefan Roediger](http://www.hs-lausitz.de/groups/multiplex-assays/bildbasierte-assays-imagebased-assays/members.html), [Michal Burdukiewicz](https://github.com/michbur).  

### Readme

1. *Choose file* - a .csv file with a fluorescence data and cycle number.  
2. *Header* - must be checked if input table has header.  
3. *Type of .csv file* - choose type of .csv file (depending on decimal separator).  
4. *Column containing the cycle data* - number of column containing the cycle number.  
5. *Relative standard deviation* - show relative standard deviation in percent.  
6. *Median and MAD* - must be checked if median and MAD should be used instead of mean and standard deviation.  
7. *Lower and upper border of the region of interest* - two cycle numbers defining upper and lower limit of the region of interest (ROI) used for the density and quantile plot.