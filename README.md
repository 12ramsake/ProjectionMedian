# ProjectionMedian
<br /> <br />
Depreciated! Simply install the R package via the line: install_github('12ramsake/DurocherProjectionMedian') and use the dpm function to compute the projection median. <br /> <br />
Compute the Projection Median in any Dimension <br /> <br />
For 2 Dimensions run 2 Dimensions script. <br /> <br />
For 3 Dimensions:
1. Download all the c++ files (including the headers (.cpp and .h) into your working directoty in R
2. Source the calcMedLevel.cpp file in R with rcpp
2. Run the projection median 3d script and use the projectionMedian3D() function to calculate the projection median of your data <br />
<br />
For the approximation in d use the Monte Carlo file and source the script
All functions take an nxd matrix  <br /> <br />
To run the large sample test example, run the prostate example script after downloading prostmat and placing it into your R working directory
