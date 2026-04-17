## V 0.1.4 first submission

In this version the following changes were made:
-   The way the values to identify variable combinations outside reference ranges
was changed to improve scalability. This is, values were previously assigned using
10^(1:nvar). If the number of variables was large (e.g., 22) the output exceeded 
the precision of a 64-bit float. Now values are assigned using 2^(1:nvar)

## Test environments

-   Ubuntu 24.04 LTS, R 4.5.3 (local)
-   MacOS 15.7.4, R release (GitHub Actions) 
-   Microsoft Windows Server 2022, R release (GitHub Actions) 
-   Ubuntu 24.04 LTS, R release (GitHub Actions)  
-   Ubuntu 24.04 LTS, R devel (GitHub Actions) 
-   Ubuntu 24.04 LTS, R oldrel-1 (GitHub Actions)  

## R CMD check results

There were no ERRORs:

There were no WARNINGs:

There were no NOTEs:

## Downstream dependencies

kuenm2



## V 0.1.3 first submission

In this version the following changes were made:
-   Column names of the data.frame used to define levels in categorical 
SpatRaster objects were changed to (id and category) to prevent issues when 
reading such objects after writing them as raster files in local directories.

## Test environments

-   Ubuntu 24.04 LTS, R 4.3.3 (local)
-   MacOS 14.7.4, R release (GitHub Actions) 
-   Microsoft Windows Server 2022 10.0.20348, R release (GitHub Actions) 
-   Ubuntu 24.04 LTS, R release (GitHub Actions)  
-   Ubuntu 24.04 LTS, R devel (GitHub Actions) 
-   Ubuntu 24.04 LTS, R oldrel-1 (GitHub Actions)  

## R CMD check results

There were no ERRORs:

There were no WARNINGs:

There were no NOTEs:

## Downstream dependencies

There are currently no downstream dependencies for this package.



## V 0.1.2 first submission

In this version the following changes were made:
-   Documentation details via roxygen2 were changed to avoid warnings during
checks.
-   A new argument was added to mop, and mop_distance functions to allow 
changing tolerance levels when calculating Mahalanobis distances.

## Test environments

-   Ubuntu 22.04 LTS, R 4.3.3 (local)
-   MacOS 12.7.3, R release (GitHub Actions)
-   Windows 10.0.20348, R release (GitHub Actions)
-   Ubuntu 22.04 LTS, R release (GitHub Actions)
-   Ubuntu 22.04 LTS, R devel (GitHub Actions)
-   Ubuntu 22.04 LTS, R oldrel-1 (GitHub Actions)

## R CMD check results

There were no ERRORs:

There were no WARNINGs:

There were no NOTEs:

## Downstream dependencies

There are currently no downstream dependencies for this package.



## V 0.1.1 first submission

This is a new release.

## Test environments

-   Ubuntu 22.04 LTS, R 4.3.0 (local)
-   MacOS 12.6.5, R release (GitHub Actions)
-   Windows 10.0.20348, R release (GitHub Actions)
-   Ubuntu 22.04 LTS, R release (GitHub Actions)
-   Ubuntu 22.04 LTS, R devel (GitHub Actions)
-   Ubuntu 22.04 LTS, R oldrel-1 (GitHub Actions)

## R CMD check results

There were no ERRORs:

There were no WARNINGs:

There were no NOTEs:

## Downstream dependencies

There are currently no downstream dependencies for this package.
