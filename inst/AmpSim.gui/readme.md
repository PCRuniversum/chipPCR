### About
  
**AmpSim** is a simple simulator for amplification reaction. Parameters: b.eff and Cq are most connected. Changing one of them will change both values. Cq can be used to define an approximate  Cq value. The expression 'approximate Cq value' is used here because the actual Cq value is dependent on  the users preferred method (e.g., Cy0 method, Second Derivative Maximum (SDM) method, threshold method). The funtion can be used to see how an experimental system compares to a predicted model. Moreover it can be used to simulate data with noise, missing values (NA), ignal-to-noise ratios, photo-bleaching and other influences on a PCR reaction.  
This is a web-based implementation of function from package [chipPCR](http://github.com/michbur/chipPCR). 

**Authors**: [Stefan Roediger](https://www.researchgate.net/profile/Stefan_Roediger), [Michal Burdukiewicz](https://github.com/michbur).  

### Readme

1. *Cycles* - the number of cycles.  
2. *Efficiency* - amplification efficiency adjustment.  
3. *Baseline* -  base level (minimum) of the background range.  
4. *Amplitude* -  the plateau (maximum) of the amplification reaction.  
5. *Cq value* - defines approximately the quantification point (Cq) of the amplification reaction.  
6. *Use noise in simulation* - should noise be added to the simulation?  
7. *Noise level* - level of noise during the amplification reaction.  
8. *Variable* - trend of noise level during the amplification reaction. "constant" uses same noise of amplification, "decreasing" leads to less noise at the end of the amplification reaction, and "increasing" leads to more noise at the end of the amplification reaction.  
9. *Fluorescence threshold value* - a fluorescence value which defines the threshold.  
10. *Automatic estimation of the threshold* -  indicates if an automatic estimation of the threshold should be used (Note: Experimental, not save to use).  
11. *Linear regression instead of quadratic* -  indicates if a linear regression should be used instead of quadratic regression for the calculation. 
