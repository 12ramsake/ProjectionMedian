# ProjectionMedian
Compute the Projection Median in any Dimension
Files related to computing the projection median in Any Dimension.
For 2 Dimensions run the script 2d efficient for odd data and efficient even for even data, use the calcMedEff function to calculate the projection median of your data.
For 3 Dimensions:
1. download all the c++ files (including the headers) and source the cpp file in R with rcpp
2. Run the calcmed3d script and use the projMed() function to calculate the projection median of your data
For the approximation in d use the projmed Monte Carlo file and source the script and run the projMC function with your data
All functions take an nxd matrix
