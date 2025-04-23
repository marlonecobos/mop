# mop 0.1.3

Column names of the `data.frame` used to define `levels` in categorical 
`SpatRaster` objects were changed to ("id" and "category") to prevent issues 
when reading such objects after writing them as raster files in local 
directories.

# mop 0.1.2

A new argument in `mop` and `mop_distance` (`tol = NULL`) allow users to 
play with the a tolerance value when calculating Mahalanobis distances.


# mop 0.1.1

Euclidean distance calculation uses our c++ function to avoid non-needed 
dependencies.

# mop 0.1.0

This is the first complete version of mop.
