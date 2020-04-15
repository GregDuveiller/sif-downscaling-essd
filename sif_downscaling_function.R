#!/usr/bin/Rscript
# ##############################################################################
# Title         : SIF downscaling function
#
# Description   : Downscales SIF original GOME-2 sun induced fluorescence (SIF)
#     from 0.5 dd to 0.05 dd using a semi-empirical regionalized model based on
#     light-use effieciency calibrated with explanatory variables derived from 
#     the MODIS instrument. It works over space and requires as input a netcdf  
#     for SIF values (--input_SIF), and a set of explanatory variables in netcdf
#     including a vegetation index (--input_NDVI), an indicator of water stress 
#     (--input_ET) and an indicator of thermic stress (--input_LST). For details
#     please refer to the paper.
#
#     Duveiller G., Filipponi F., Walther S., Köhler P., Frankenberg C., Guanter
#     L., and Cescatti A. 2020. A spatially downscaled sun-induced fluorescence
#     global product for enhanced monitoring of vegetation productivity. Earth
#     System Science Data. https://doi.org/10.5194/essd-2019-121
#
# Date          : Apr 2020
# Licence       : GPL v3
# Authors       : Gregory Duveiller, Federico Filipponi
# ##############################################################################

# Usage example
# sif_downscaling_function.R --input_SIF=MODIS_4_SIF___SIF/GOME_SIFv26JJ_05deg_16dfromsinglemeasurements_2009001.nc 
# --input_NDVI=/MODIS_4_SIF/MCD43C4.A2009005.006.2016224024647.hdf_NDVIqualsnowfiltered_005deg.nc --input_ET=/space/filipfe/SIF/data/MODIS_4_SIF/MCD43C4.A2009005.006.2016224024647.hdf_NDWIqualsnowfiltered_005deg.nc 
# --input_LST=/MODIS_4_SIF/MOD11C2.A2009001.006.2016006051722.hdf_LSTqualfiltered_005deg.nc --output=/space/filipfe/SIF/test/SIF_test_102.nc -q 10 --filter --verbose

# load optparse library for argument parsing
options("repos"="http://cran.at.r-project.org/")
invisible(tryCatch(find.package("optparse"), error=function(e) install.packages("optparse", dependencies=TRUE)))
require(optparse, quietly=TRUE)

# read arguments
option_list <- list(
  optparse::make_option(c("--input_SIF"), type="character", default=NULL, 
                        help="Input 'Solar Induced chlorophyll Fluorescence (SIF)' file path", metavar="file"),
  optparse::make_option(c("--input_NDVI"), type="character", default=NULL, 
                        help="Input 'Normalized Difference Vegetation Index (NDVI)' file path", metavar="file"),
  optparse::make_option(c("--input_ET"), type="character", default=NULL, 
                        help="Input 'Evapotransiration (ET)' file path", metavar="file"),
  optparse::make_option(c("--input_LST"), type="character", default=NULL, 
                        help="Input 'Land Surface Temperature (LST)' file path", metavar="file"),
  optparse::make_option(c("-o", "--output"), type="character", default=NULL, 
                        help="Output NetCDF-4 file path", metavar="file"),
  optparse::make_option(c("--output_coefficients"), type="character", default=NULL, 
                        help="Output NetCDF-4 file path for parameters coefficients", metavar="file"),
  optparse::make_option(c("--output_variables"), type="character", default=NULL, 
                        help="Output NetCDF-4 file path for parameters variables", metavar="file"),
  optparse::make_option(c("-w", "--winsize"), type="integer", default=11, 
                        help="Size of the moving window (in pixels) [default = %default]", metavar="integer"),
  optparse::make_option(c("-p", "--percentage"), type="integer", default=50, 
                        help="Percentage of minumum pixels available for coarse resolution aggregation [default = %default]", metavar="integer"),
  optparse::make_option(c("-m", "--minimum_samples"), type="integer", default=40, 
                        help="Minumum number of pixel in the moving window used for parameter optimization [default = %default]", metavar="integer"),
  optparse::make_option(c("--input_SIF_obs"), type="character", default=NULL, 
                        help="Input file containing 'Solar Induced chlorophyll Fluorescence (SIF)' number of temporal observations", metavar="file"),
  optparse::make_option(c("-x", "--min_obs"), type="integer", default=5, 
                        help="Threshold value for minimum number of temporal observation [default = %default]", metavar="integer"),
  optparse::make_option(c("--input_SIF_std"), type="character", default=NULL, 
                        help="Input file containing 'Solar Induced chlorophyll Fluorescence (SIF)' temporal standard deviation", metavar="file"),
  optparse::make_option(c("-s", "--max_std"), type="double", default=2.0, 
                        help="Threshold value for maximim temporal standard deviation [default = %default]", metavar="double"),
  optparse::make_option(c("--sif_min"), type="double", default=0.0, 
                        help="Minimum value allowed for SIF [default = %default]", metavar="double"),
  optparse::make_option(c("--sif_max"), type="double", default=6.0, 
                        help="Maximum value allowed for SIF [default = %default]", metavar="double"),
  optparse::make_option(c("-d", "--deflate"), type="integer", default=9, 
                        help="Set the deflate compression level for output NetCDF-4 file (interval 1-9) [default = %default]", metavar="integer"),
  optparse::make_option(c("-q", "--q_parallelism"), type="integer", default=-1, 
                        help="Set the maximum parallelism used for the computation. If not set all available cores are used for the analysis", metavar="integer"),
  optparse::make_option(c("-f","--filter"), type="logical", default=FALSE, action="store_true",
                        help="Apply local spatial weighted filter"),
  optparse::make_option(c("--quiet"), type="logical", default=FALSE, action="store_true",
                        help="Quiet mode"),
  optparse::make_option(c("--verbose"), type="logical", default=FALSE, action="store_true",
                        help="Verbose mode")
)

# ###################################
# get arguments

opt_parser <- optparse::OptionParser(option_list=option_list)
opt <- optparse::parse_args(opt_parser)

# check if required arguments are supplied and point to existing files
if(is.null(opt$input_SIF)){
  optparse::print_help(opt_parser)
  stop("At least one argument must be supplied (input_SIF).", call.=FALSE)
}
if(is.null(opt$input_NDVI)){
  optparse::print_help(opt_parser)
  stop("At least one argument must be supplied (input_NDVI).", call.=FALSE)
}
if(is.null(opt$input_ET)){
  optparse::print_help(opt_parser)
  stop("At least one argument must be supplied (input_ET).", call.=FALSE)
}
if(is.null(opt$input_LST)){
  optparse::print_help(opt_parser)
  stop("At least one argument must be supplied (input_LST).", call.=FALSE)
}
if(is.null(opt$output)){
  optparse::print_help(opt_parser)
  stop("At least one argument must be supplied (output).", call.=FALSE)
}

# define R objects from options
input_SIF <- normalizePath(path=opt$input_SIF, winslash="/", mustWork=TRUE)
input_NDVI <- normalizePath(path=opt$input_NDVI, winslash="/", mustWork=TRUE)
input_ET <- normalizePath(path=opt$input_ET, winslash="/", mustWork=TRUE)
input_LST <- normalizePath(path=opt$input_LST, winslash="/", mustWork=TRUE)
output_file <- normalizePath(path=opt$output, winslash="/", mustWork=FALSE)
winsize <- as.integer(opt$winsize)
minsamples <- as.integer(opt$minimum_sample)
minpixagr_perc <- as.integer(opt$percentage)
i_output_coefficients <- opt$output_coefficients
if(!is.null(i_output_coefficients)){
  output_coefficients <- normalizePath(path=i_output_coefficients, winslash="/", mustWork=FALSE)
}
i_output_variables <- opt$output_variables
if(!is.null(i_output_variables)){
  output_variables <- normalizePath(path=i_output_variables, winslash="/", mustWork=FALSE)
}
i_SIF_obs <- opt$input_SIF_obs
if(!is.null(i_SIF_obs)){
  input_SIF_obs <- normalizePath(path=i_SIF_obs, winslash="/", mustWork=TRUE)
}
i_SIF_std <- opt$input_SIF_std
if(!is.null(i_SIF_std)){
  input_SIF_std <- normalizePath(path=i_SIF_std, winslash="/", mustWork=TRUE)
}
min_obs <- as.integer(opt$min_obs)
max_std <- as.double(opt$max_std)
sif_min <- as.double(opt$sif_min)
sif_max <- as.double(opt$sif_max)
nc_deflate <- as.integer(opt$deflate)
cores <- as.integer(opt$q_parallelism)
LocalAverage <- opt$filter
verbose <- opt$verbose
quiet <- opt$quiet
filter_type <- "gaussian"



# ###################################
# load required libraries
invisible(tryCatch(find.package("raster"), error=function(e) install.packages("raster", dependencies=TRUE)))
invisible(tryCatch(find.package("ncdf4"), error=function(e) install.packages("ncdf4", dependencies=TRUE)))
invisible(tryCatch(find.package("prettyGraphs"), error=function(e) install.packages("prettyGraphs", dependencies=TRUE)))
invisible(tryCatch(find.package("parallel"), error=function(e) install.packages("parallel", dependencies=TRUE)))
suppressPackageStartupMessages(require(raster, quietly=TRUE))
suppressPackageStartupMessages(require(ncdf4, quietly=TRUE))
suppressPackageStartupMessages(require(prettyGraphs, quietly=TRUE))
suppressPackageStartupMessages(require(parallel, quietly=TRUE))

if(quiet){
  verbose <- FALSE
} else {
  message("Running SIF downscaling routine ...")
}

# ###################################
# Define required functions

# define off check function
is.odd <- function(x){
  x %% 2 != 0
}

# define function to generate the weighted focal filter
focalWeightFilter <- function(x){
  scale_fact <- x
  mWm <- scale_fact * 3
  xCent <- matrix(data=c(rep( -scale_fact, scale_fact), rep(0, scale_fact), rep(scale_fact, scale_fact)), ncol = mWm, nrow = mWm, byrow = TRUE)
  yCent <- matrix(data=c(rep(scale_fact, scale_fact), rep(0, scale_fact), rep( -scale_fact, scale_fact)), ncol = mWm, nrow = mWm, byrow = FALSE)
  vPos <- as.vector((1:scale_fact) - (scale_fact/2 + 0.5))
  xPos <- matrix(data=rep(vPos, scale_fact), ncol = mWm, nrow = mWm, byrow = TRUE)
  yPos <- matrix(data=rep(sort(vPos, decreasing = TRUE), scale_fact), ncol = mWm, nrow = mWm, byrow = FALSE)
  dista <- -(sqrt((xPos - xCent)^2 + (yPos - yCent)^2)/scale_fact)
  dista[which(is.infinite(dista))] <- 0
  #dista <- (dista - (min(dista) - 0.000001))^2 # apply a square function to maximize central pixel
  dista <- (dista - (min(dista) - 0.000001))
  dista <- dista + 0.1 # add factor to reduce differences between min and max and make the weights comparable with the gaussian filter
  weights_matrix <- matrix(data=as.double(NA), ncol = mWm, nrow = mWm)
  # normalize weights
  for(n in 1:scale_fact){
    j <- (n-(as.integer((n-1)/scale_fact)*scale_fact))
    k <- (scale_fact*(0:2))+j
    for(m in 1:scale_fact){
      o <- (scale_fact*(0:2))+m
      distaSum <- sum(dista[o,k])
      weights_matrix[o,k] <- (dista[o,k] / distaSum)
    }
  }
  return(weights_matrix)
}

# define function to generate the weighted focal filter (gaussian)
gaussianWeightFilter <- function(x){
  scale_fact <- x
  mWm <- scale_fact * 3
  yM <- matrix(rep(1:mWm,times=mWm),ncol=mWm)
  xM <- matrix(rep(1:mWm,each=mWm),nrow=mWm)
  dista <- exp(-(((xM-(scale_fact*1.5))^2)+((yM-(scale_fact*1.5))^2))/(2*scale_fact^2))
  #dista <- exp(-(((xM-15.5)^2)+((yM-15.5)^2))/(2*sigma^2))#*(1/(2*pi*sigma^2))
  weights_matrix <- matrix(data=as.double(NA), ncol = mWm, nrow = mWm)
  # normalize weights
  for(n in 1:scale_fact){
    j <- (n-(as.integer((n-1)/scale_fact)*scale_fact))
    k <- (scale_fact*(0:2))+j
    for(m in 1:scale_fact){
      o <- (scale_fact*(0:2))+m
      distaSum <- sum(dista[o,k])
      weights_matrix[o,k] <- (dista[o,k] / distaSum)
    }
  }
  return(weights_matrix)
}

# define function to aggregate a Raster object to a lower resolution 
sprep.aggreg <- function(x,scl,minpixagr){
  x.agr <- raster::aggregate(x,scl)
  z <- raster::raster(x.agr)
  z[] <- raster::cellsFromExtent(object=x.agr,extent=extent(x.agr))
  dum <- raster::zonal(x,raster::disaggregate(z,scl),fun=function(x,...){sum(!is.na(x))},na.rm=F)
  r <- raster::raster(x.agr)
  r[] <- dum[,2]
  x.agr[r<minpixagr] <- NA
  return(x.agr)
}

# define cost function
COSTFUN <- function(df,b){
  with(df, sum((SIFest(NDVI,EVTR,LSTD,b)-FLUO)^2))
}

# define functions to process explanatory variables
Eps_NDVI <- function(NDVI,b1,b2){
  b2*(NDVI^b1)
}
Eps_ET <- function(EVTR,b1,b2){
  1/(1+exp(b1*(-EVTR+b2)))
}
Eps_LST <- function(LSTD,b1,b2){
  exp(-0.5*((LSTD+b1)/b2)^2)
}

# define function to compute SIF from explanatory variables
SIFest <- function(NDVI,EVTR,LSTD,b){
  Eps_NDVI(NDVI,b[1],b[2])*Eps_ET(EVTR,b[3],b[4])*Eps_LST(LSTD,b[5],b[6])
}

# define function for parameters optimization
optimFUN <- function(iCell,Xvar,Y,pad,minsamples){
  cRow <- raster::rowFromCell(Y, iCell)
  cCol <- raster::colFromCell(Y, iCell)
  
  ext <- raster::extent(Y,cRow-pad,cRow+pad,cCol-pad,cCol+pad)
  locsif <- raster::crop(Y,ext)
  locdis <- raster::distanceFromPoints(locsif, xy=xyFromCell(object = Y,cell = iCell))
  #locdis <- raster::distanceFromPoints(locsif, xy=xyFromCell(object = Y,cell = iCell))/1000 # distance from centre cell in km
  Xvar_sub <- raster::extract(Xvar,ext)
  
  # create a data frame with values
  df <- data.frame(cell=rep(iCell,times=winsize^2),
                   #time=rep(as.Date(sTime,format='%Y%j'),times=winsize^2),
                   ncno=cellsFromExtent(Y,ext),
                   dist=as.vector(locdis),
                   FLUO=as.vector(extract(Y,ext)),
                   NDVI=as.vector(Xvar_sub[,1]),
                   EVTR=as.vector(Xvar_sub[,2]), 
                   LSTD=as.vector(Xvar_sub[,3]))
  
  df <- df[complete.cases(df),]
  df <- df[with(df, order(dist)),] # order samples by distance from central cell
  
  # skip cell if there are not complete cases
  if(nrow(df)<minsamples){
    #message(paste('Pt:',as.character(iCell),' Had to stop b/c not enough points...'))
    return(NULL)
  }
  
  df <- df[1:minsamples,] # to remain with always the same num of samples
  
  # QuasiNewton Optimisation
  outList_QN=tryCatch({
    outQNewt=optim(par=param_ini,fn=COSTFUN,df=df,method="L-BFGS-B",lower=param_min,upper=param_max)
    df$FLUOest=SIFest(df$NDVI,df$EVTR,df$LSTD,outQNewt$par)
    
    outList_QN=list(cellID=iCell, coeff=outQNewt$par, minCostVal=outQNewt$value,convergence=outQNewt$convergence,
                    MAE=mean(abs(df$FLUO-df$FLUOest)), RMSE=sqrt(mean((df$FLUO-df$FLUOest)^2)))
  },
  error=function(cond){
    #message(paste("Optim seems to fail for:", as.character(iCell)))
    # Choose a return value in case of error
    return(NULL)}
  )
  
  # Linear regression
  outList_LinReg=tryCatch({
    fit=with(df,lm(FLUO~NDVI+EVTR+LSTD))
    lmg=calc.relimp(fit)
    
    outList=list(cellID=iCell, coeff=fit$coefficients, lmg=lmg@lmg,
                 MAE=mean(abs(fit$residuals)), RMSE=sqrt(mean((fit$residuals)^2)))
  },
  error=function(cond){
    #message(paste("Fit seems to fail for:", as.character(iCell)))
    # Choose a return value in case of error
    return(NULL)}
  )
  
  locSIF <- list(cellID=iCell,mu=mean(df$FLUO,na.rm=T),s2=var(df$FLUO,na.rm=T),CV=sd(df$FLUO,na.rm=T)/abs(mean(df$FLUO,na.rm=T)))
  outList <- list(LinReg=outList_LinReg,QN=outList_QN,locSIF=locSIF)
  return(outList)
}

# define funciton to place optimization results in a raster stack object
bStackFUN <- function(data2extract,dataname,pcells,ref_raster){
  dumStack <- raster::stack()
  dumTable <- do.call("rbind",lapply(1:length(outList), function(x) {
    if(is.list(outList[[x]][1])){eval(parse(text=paste('outList[[x]]$',data2extract,sep='')))}}))
  for(i in 1:dim(dumTable)[2]){
    r <- raster::raster(dumRaster)
    r[] < -NA
    r[pcells] <- dumTable[,i]
    dumStack <- addLayer(dumStack, r)
  }
  return(dumStack)
}

# ###################################
# check input arguments

# check if winsize is at least of size 5
if(winsize < 5)
  stop("Input 'winsize' parameter should be a odd number equal or greater '5'")

# check if local window size is odd
if(!is.odd(winsize))
  stop("Input 'winsize' parameter is not odd")

# check percentage and set number of pixel with different compositions
if(minpixagr_perc < 0 | minpixagr_perc > 100)
  stop("Percentage of minimum number of pixels in the moving window required for the aggregation should be in the interval 30-100 %")

# check if minumum number of pixel in the moving window used for parameter optimization is equal or greater 25
if(minsamples < 25)
  stop("Minumum number of pixel in the moving window used for parameter optimization should be at least '25'")
if(minsamples > winsize^2)
  stop(paste("Minumum number of pixel in the moving window set for parameter optimization '", minsamples,"' is greater than number of pixels in selected window size '", winsize^2, "'", sep=""))

# check low resolution
sFLUO_LR <- raster::raster(input_SIF)
LR_res <- raster::res(sFLUO_LR)
if(round(LR_res[1], digits=4) != round(LR_res[2], digits=4))
  stop("File 'input_SIF' should have the same spatial resolution in Latitude and Longitude dimensions")
LRpixsiz <- round(LR_res[1], digits=4)

# check high resolution
NDVI_HR <- raster::raster(input_NDVI)
HR_res <- raster::res(NDVI_HR)
if(round(HR_res[1], digits=4) != round(HR_res[2], digits=4))
  stop("File 'input_NDVI' should have the same spatial resolution in Latitude and Longitude dimensions")
HRpixsiz <- round(HR_res[1], digits=4)

# check if input HR raster has same extent and crs than LR raster
LR_HR_compare <- suppressWarnings(raster::compareRaster(sFLUO_LR, NDVI_HR, extent=TRUE, crs=TRUE, rowcol=FALSE, res=FALSE))
if(!LR_HR_compare)
  stop("Lower Resolution and Higher Resolution input rasters should have be the same spatial extent and CRS")

# check if Lower Resolution is integermultiple of Higher Resolution
if(HRpixsiz >= LRpixsiz)
  stop("Target resolution set using 'resolution' argument is equal or greater 'input' file resolution")
scl <- LRpixsiz/HRpixsiz
if(round((LRpixsiz/HRpixsiz) - round(LRpixsiz/HRpixsiz), digits = 4) != 0)
  stop(paste("Lower Resolution '", LRpixsiz, "' is not multiple of Higher Resolution '", HRpixsiz, "'\nThis is currently not supported, use input files at Higher Resolution with a integer multiple resolution of Lower Resolution file", sep=""))

# check if input HR rasters have same extent and resolution
EVTR_HR <- raster::raster(input_ET)
LSTD_HR <- raster::raster(input_LST)
HR_HR_compare <- suppressWarnings(raster::compareRaster(NDVI_HR, EVTR_HR, LSTD_HR, extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE))
if(!HR_HR_compare)
  stop("Lower Resolution and Higher Resolution input rasters should have be the same spatial extent and CRS")

# check output file name
if(file.exists(output_file)){
  stop(paste("Output file path '", output_file,"' already exists. Set another output file name", sep=""))
}
# create output folder
dir.create(dirname(output_file), showWarnings=FALSE, recursive=TRUE)

# check output coefficients
if(!is.null(i_output_coefficients)){
  # check output file name
  if(file.exists(output_coefficients)){
    stop(paste("Output file path '", output_coefficients,"' already exists. Set another output file name for parameters coefficients", sep=""))
  }
  # create output folder
  dir.create(dirname(output_coefficients), showWarnings=FALSE, recursive=TRUE)
}

# check output variables
if(!is.null(i_output_variables)){
  # check output file name
  if(file.exists(output_variables)){
    stop(paste("Output file path '", output_variables,"' already exists. Set another output file name for parameters variables", sep=""))
  }
  # create output folder
  dir.create(dirname(output_variables), showWarnings=FALSE, recursive=TRUE)
}


# #############################
# set parameters

# set pad size
pad <- as.integer((winsize-1)/2)
# # set minumum numbre of HR pixels to be found in function aggregate
# minpixagr <- 50
minpixagr <- as.integer(round(minpixagr_perc * scl^2 / 100, digits=0))
# # set minimum number of samples needed for optimization function
# minsamples <- 40

# define initial parameters for the explanatory variables
# param_ini=c(   1,  2,  0.1, 20,-295, 10)
# param_min=c( 0.5,0.1, 0.05,  1,-310,  1)
# param_max=c( 1.5,  5,  0.5,200,-290, 50)
# edit coefficient b4 for NDWI
param_ini=c(   1,  2,  50.0,  0,-295, 10)
param_min=c( 0.5,0.1,   0.0, -1,-310,  1)
param_max=c( 1.5,  5, 500.0,  1,-290, 50)

# print parameters
if(verbose){
  message("")
  message(paste(c("Processing file: "), input_SIF, sep=""))
  message(paste("Spatial downscaling from ", LRpixsiz, " to ", HRpixsiz, " (scale factor of ", scl, ")", sep=""))
  message(paste("Using a moving window of size: ", winsize, " x ", winsize, sep=""))
  if(LocalAverage){
    message(c("Applying weighted filter"))
  }
  message(paste("Results are saved to file: ", output_file, sep=""))
  if(!is.null(i_output_coefficients)){
    message(paste("Coefficients are saved to file: ", output_coefficients, sep=""))
  }
  if(!is.null(i_output_variables)){
    message(paste("Variables are saved to file: ", output_variables, sep=""))
  }
  message(paste(c("Explanatory variables files: "), sep=""))
  message(c(input_NDVI))
  message(c(input_ET))
  message(c(input_LST))
  if(!is.null(i_SIF_obs)){
    message(paste("Using auxiliary SIF file: ", input_SIF_obs, sep=""))
  }
  if(!is.null(i_SIF_std)){
    message(paste("Using auxiliary SIF file: ", input_SIF_std, sep=""))
  }
  message("")
}

# #############################
# clean input SIF

if(verbose){
  message(paste(c("SIF downscaling started at: "), Sys.time(), sep=""))
}
ptm <- proc.time()

# some filtering out of undesirable (unreliable) data
sFLUO_LR[sFLUO_LR > sif_max] <- NA 
sFLUO_LR[sFLUO_LR <= sif_min] <- NA

# clean using number of temporal observation
if(!is.null(i_SIF_obs)){
  sFLUOnobs_LR <- raster::raster(input_SIF_obs)
  if(suppressWarnings(raster::compareRaster(sFLUO_LR, sFLUOnobs_LR, extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE))){
    sFLUO_LR[sFLUOnobs_LR < min_obs] <- NA
  } else {
    warning("Input raster of 'SIF number of temporal observation' has not the same extent and resolution than input SIF raster")
  }
}
# clean using temporal standard deviation
if(!is.null(i_SIF_std)){
  sFLUOstd_LR <- raster::raster(input_SIF_std)
  if(suppressWarnings(raster::compareRaster(sFLUO_LR, sFLUOstd_LR, extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE))){
    sFLUO_LR[sFLUOstd_LR > max_std] <- NA
  } else {
    warning("Input raster of 'SIF temporal standard deviation' has not the same extent and resolution than input SIF raster")
  }
}

# create list of SIF LR cells to be processed
cells2process <- which(getValues(!is.na(sFLUO_LR))==T)

# check if input file contain data
if(length(cells2process) == 0)
  stop("Input SIF file only contains NULL cells")

# refine list of cells 2 process by removing the image border (according to the size of moving window)
LR_nc <- raster::ncol(sFLUO_LR)
LR_nr <- raster::nrow(sFLUO_LR)
border_cells <- c(cellFromRow(sFLUO_LR, 1:pad), cellFromCol(sFLUO_LR, 1:pad), cellFromRow(sFLUO_LR, c((LR_nr-pad+1):(LR_nr))), cellFromCol(sFLUO_LR, c((LR_nc-pad+1):(LR_nc))))
border_cells <- sort(unique(border_cells))
sFLUO_LR[border_cells] <- NA ### should be set to NA ?
cells2process <- setdiff(cells2process, border_cells)

# #############################
# aggregate input parameters

if(verbose){
  message("Aggregating explanatory variables ...")
}

# mask variables for missing pixels (for variable consistency in aggregation)
values(NDVI_HR) <- raster::getValues(NDVI_HR)
values(EVTR_HR) <- raster::getValues(EVTR_HR)
values(LSTD_HR) <- raster::getValues(LSTD_HR)

NDVI_HR_missing <- which(is.na(raster::getValues(NDVI_HR)))
EVTR_HR_missing <- which(is.na(raster::getValues(EVTR_HR)))
LSTD_HR_missing <- which(is.na(raster::getValues(LSTD_HR)))
mask_pix <- sort(unique(NDVI_HR_missing, EVTR_HR_missing, LSTD_HR_missing))

# check if input explanatory variables will contain data after masking
if(length(mask_pix) == raster::ncell(NDVI_HR))
  stop("No valid cells found in explanatory variable files")

# mask HR pixels
NDVI_HR[mask_pix] <- NA
EVTR_HR[mask_pix] <- NA
LSTD_HR[mask_pix] <- NA

# aggregate input raster to lower resolution
sNDVI_LR <- sprep.aggreg(NDVI_HR, scl, minpixagr)
sEVTR_LR <- sprep.aggreg(EVTR_HR, scl, minpixagr)
sLSTD_LR <- sprep.aggreg(LSTD_HR, scl, minpixagr)
# assemble results in a raster stack
Xvar <- stack(sNDVI_LR,sEVTR_LR,sLSTD_LR)

# #############################
# optimize downscaling function parameters

if(verbose){
  message("Optimizing downscaling function parameters ...")
}

# set and check number of cores
if(cores == -1){
  cores <- parallel::detectCores()
  if(verbose){
    message(paste("Number of cores used for the optimization procedure is set to '", cores, "' according to local hardware", sep=""))
  }
}
# check if machine has enough available cores
if(cores > parallel::detectCores()){
  cores <- parallel::detectCores()
  if(verbose){
    warning(paste("Number of cores used for the optimization procedure is reduced to '", cores, "' according to local hardware", sep=""))
  }
} else {
  if(verbose){
    message(paste("Number of cores used for the optimization procedure is set to '", cores, "'", sep=""))
  }
}

# optimize 'b' coefficients
outList <- mclapply(X=cells2process,FUN=optimFUN,Xvar=stack(sNDVI_LR,sEVTR_LR,sLSTD_LR),Y=sFLUO_LR,pad=pad,minsamples=minsamples,mc.cores=cores)

# place 'b' coefficients in a raster stack
# duplicate input SIF raster
dumRaster <- sFLUO_LR
raster::values(dumRaster) <- NaN

pcells_local=do.call("rbind",lapply(1:length(outList), function(x) {if(is.list(outList[[x]][1])){outList[[x]]$locSIF$cellID}}))
pcells_LN=do.call("rbind",lapply(1:length(outList), function(x) {if(is.list(outList[[x]][1])){outList[[x]]$LinReg$cellID}}))
pcells_QN=do.call("rbind",lapply(1:length(outList), function(x) {if(is.list(outList[[x]][1])){outList[[x]]$QN$cellID}}))
data2extract='LinReg$coeff'

B <- bStackFUN('QN$coeff','SIF_QN_coeffs',pcells_QN, ref_raster=dumRaster)
names(B) <- raster::validNames(paste("b", c(1:nlayers(B)), sep=""))

### optionally export b coefficients (and other results) to output NetCDF file
### these are ancillary rasters generated in the optimization procedure
# s <- bStackFUN('LinReg$coeff','SIF_LN_coeffs',pcells_LN)
# s <- bStackFUN('LinReg$RMSE','SIF_LN_rmse',pcells_LN)
# s <- bStackFUN('LinReg$MAE','SIF_LN_mae',pcells_LN)
# s <- bStackFUN('LinReg$lmg','SIF_LN_lmg',pcells_LN)
# s <- bStackFUN('QN$coeff','SIF_QN_coeffs',pcells_QN)
# s <- bStackFUN('QN$RMSE','SIF_QN_rmse',pcells_QN)
# s <- bStackFUN('QN$MAE','SIF_QN_mae',pcells_QN)
# s <- bStackFUN('locSIF$s2','SIF_local_var',pcells_local)
# s <- bStackFUN('locSIF$CV','SIF_local_cv',pcells_local)

# clean workspace
invisible(gc(verbose=FALSE))

# #############################
# compute HR SIF

if(verbose){
  message("Computing High Resolution SIF ...")
}

B5 <- raster::disaggregate(B,fact=scl)
FLUO_HR5 <- (B5[[2]]*(NDVI_HR^B5[[1]]))*(1/(1+exp(B5[[3]]*(-1*EVTR_HR+B5[[4]]))))*(exp(-0.5*((LSTD_HR+B5[[5]])/B5[[6]])^2))

if(LocalAverage){
  
  if(verbose){
    message("Applying spatial filter ...")
  }
  
  # get param for each neighbourhood
  dumB <- raster::raster(B)
  dumB[] <- 0
  Bext <- raster::extent(B)
  #nList <- c(1,2,3,4,6,7,8,9)
  nList <- c(1:9)
  shift_list <- data.frame(x=c(-LRpixsiz,0,LRpixsiz,-LRpixsiz,0,LRpixsiz,-LRpixsiz,0,LRpixsiz), y=c(LRpixsiz,LRpixsiz,LRpixsiz,0,0,0,-LRpixsiz,-LRpixsiz,-LRpixsiz))
  
  FLS <- raster::stack()
  for(l in nList){
    #message(paste("Processing layer ", l, "/9", sep=""))
    if(l == 5){
      FLS <- raster::addLayer(FLS, FLUO_HR5)
    } else {
      # shift and disaggregate coefficients
      Bl <- raster::disaggregate(raster::merge(raster::crop(raster::shift(B,x=shift_list$x[l],y=shift_list$y[l]),Bext),dumB,overlap=F),fact=scl)
      # compute HR SIF
      FLUO_HR <- (Bl[[2]]*(NDVI_HR^Bl[[1]]))*(1/(1+exp(Bl[[3]]*(-1*EVTR_HR+Bl[[4]]))))*(exp(-0.5*((LSTD_HR+Bl[[5]])/Bl[[6]])^2))
      FLS <- raster::addLayer(FLS, FLUO_HR)
      rm(Bl,FLUO_HR)
    }
    invisible(gc(verbose=FALSE))
  }
  
  # create filter
  
  # filter_type <- "hat" ### uncomment to use the focal 'hat' filter
  
  if(filter_type == "hat"){
    Wfilter <- focalWeightFilter(scl)
  }
  if(filter_type == "gaussian"){
    Wfilter <- gaussianWeightFilter(scl)
  }
  
  # spatially replicate the filter
  weights <- raster::stack()
  Fi <- data.frame(nS=c(1,1,1,scl+1,scl+1,scl+1,2*scl+1,2*scl+1,2*scl+1),nE=c(scl,scl,scl,scl*2,scl*2,scl*2,scl*3,scl*3,scl*3),mS=c(1,scl+1,2*scl+1,1,scl+1,2*scl+1,1,scl+1,2*scl+1),mE=c(scl,scl*2,scl*3,scl,scl*2,scl*3,scl,scl*2,scl*3))
  for(f in 1:9){
    Wf <- raster::raster(nrows=raster::nrow(FLUO_HR5), ncols=raster::ncol(FLUO_HR5), ext=raster::extent(FLUO_HR5), vals=prettyGraphs::repmat(a=Wfilter[(Fi$nS[f]:Fi$nE[f]),(Fi$mS[f]:Fi$mE[f])], n=LR_nr, m=LR_nc))
    weights <- raster::addLayer(weights, Wf)
    rm(Wf)
    invisible(gc(verbose=FALSE))
  }
  
  # apply filter
  weights <- raster::mask(weights,is.na(FLS),maskvalue=1)
  wSum <- sum(weights,na.rm=T)
  FLUO_HR <- sum(FLS*weights,na.rm=T)/wSum
  FLUO_HR[which(getValues(FLUO_HR) == 0)] <- NA
  suppressWarnings(rm(Wfilter,wSum,weights))
  invisible(gc(verbose=FALSE))
}

if(LocalAverage){
  SIF <- round(10000*FLUO_HR)
} else {
  SIF <- round(10000*FLUO_HR5)
}
SIF[which(getValues(SIF) <= 0)] <- NA
SIF[which(getValues(SIF) > 65534)] <- NA
# apply offset to deal with NetCDF storage
SIF <- SIF - 32767

# get spatial extent and resolution of final SIF product
spE <- raster::extent(SIF)

# get CRS information
p4s <- sp::proj4string(SIF)
epsg_code <- as.integer(rgdal::showEPSG(p4s))
wkt_def <- rgdal::showWKT(p4s)
crs_def <- as.character(p4s)

# ###########################
# create output NetCDF-4 file to store results

if(verbose){
  message("Exporting results ...")
}

# read input NDVI as NetCDF object
nc_in <- ncdf4::nc_open(filename=input_NDVI, write=FALSE, readunlim=FALSE)
# get dimensions position
nc_lon_pos <- which(names(nc_in$dim) %in% c("lon","longitude","Longitude"))[1]
nc_lat_pos <- which(names(nc_in$dim) %in% c("lat","latitude","Latitude"))[1]
#nc_time_pos <- which(names(nc_in$dim) %in% c("time","Time"))[1]

# get dimension values
nc_lon_dim <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_lon_pos])
nc_lat_dim <- sort(ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_lat_pos]), decreasing=TRUE)
#nc_time_dim <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_time_pos])
acq_date <- as.integer(rev(unlist(strsplit(unlist(strsplit(basename(input_SIF), split=".nc")), split="_")))[1]) ### fix
nc_time_dim <- as.integer(as.Date(strptime(acq_date, format="%Y%j"))) ### fix
# get dimension units
nc_lon_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lon_pos], "units")$value
nc_lat_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lat_pos], "units")$value
#nc_time_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_time_pos], "units")$value
nc_time_units <- c("days since 1970-01-01 00:00:00") ### fix
# get dimension longnames
nc_lon_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lon_pos], "long_name")$value
nc_lat_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lat_pos], "long_name")$value
#nc_time_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_time_pos], "long_name")$value
nc_time_lname <- "Time" ### fix
# define fill value
ncout_fill_value <- -32768

# define new dimensions
ncout_londim <- ncdf4::ncdim_def(name="lon", longname=nc_lon_lname, units=nc_lon_units, vals=as.double(nc_lon_dim), unlim=FALSE)
ncout_latdim <- ncdf4::ncdim_def(name="lat", longname=nc_lat_lname, units=nc_lat_units, vals=as.double(sort(nc_lat_dim, decreasing=TRUE)), unlim=FALSE)
ncout_timedim <- ncdf4::ncdim_def(name="time", longname=nc_time_lname, units=nc_time_units, vals=as.double(nc_time_dim), unlim=TRUE)
# define variables
nc_var_crs <- ncdf4::ncvar_def(name="crs", longname="CRS definition", units="dl", prec="char", dim=list())
nc_var_sif <- ncdf4::ncvar_def(name="SIF", longname="sun-induced chlorophyll fluorescence", units="dl", missval=ncout_fill_value, prec="short", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
# create list of variables for NetCDF-4 creation
nc_var_list <- list(nc_var_sif, nc_var_crs)

# create output NetCDF-4 file
ncout <- ncdf4::nc_create(output_file, vars=nc_var_list, force_v4=TRUE)

# write results to NetCDF file
ncdf4::ncvar_put(ncout, varid="SIF", vals=as.integer(raster::getValues(SIF)), start=c(1,1,1), count=c(-1,-1,-1))

# put additional attributes into dimension and data variables
ncdf4::ncatt_put(ncout,"lon","standard_name","longitude")
ncdf4::ncatt_put(ncout,"lon","axis","X")
ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
ncdf4::ncatt_put(ncout,"lat","axis","Y")
ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
ncdf4::ncatt_put(ncout,"time","standard_name","time")
ncdf4::ncatt_put(ncout,"time","calendar","standard")
ncdf4::ncatt_put(ncout, nc_var_sif, "add_offset", 3.2767, prec="float")
ncdf4::ncatt_put(ncout, nc_var_sif, "scale_factor", 0.0001, prec="float")
ncdf4::ncatt_put(ncout, nc_var_sif, "grid_mapping", "crs")

# set attributed for 'crs' variable
if(epsg_code == 4326){
  ncdf4::ncatt_put(ncout,"crs","grid_mapping_name","latitude_longitude")
  ncdf4::ncatt_put(ncout,"crs","longitude_of_prime_meridian", 0.0, prec="float")
  ncdf4::ncatt_put(ncout,"crs","semi_major_axis", 6378137, prec="float")
  ncdf4::ncatt_put(ncout,"crs","inverse_flattening", 298.257223563, prec="float")
}
ncdf4::ncatt_put(ncout,"crs","projection", crs_def)
ncdf4::ncatt_put(ncout,"crs","proj4_params", crs_def)
ncdf4::ncatt_put(ncout,"crs","EPSG_code", epsg_code, prec="short")
ncdf4::ncatt_put(ncout,"crs","spatial_ref", wkt_def)

# set global attributes to output NetCDF-4 file
ncdf4::ncatt_put(ncout,0,"title","SIF")
ncdf4::ncatt_put(ncout,0,"product_version", 2.0, prec="float")
ncdf4::ncatt_put(ncout,0,"Conventions", "CF-1.7")
ncdf4::ncatt_put(ncout,0,"summary","Product generated using ’SIF spatially downscaling algorithm’")
ncdf4::ncatt_put(ncout,0,"keywords","sun-induced chlorophyll fluorescence")
ncdf4::ncatt_put(ncout,0,"references","Duveiller, G., & Cescatti, A. (2016). Spatially downscaling sun-induced chlorophyll fluorescence leads to an improved temporal correlation with gross primary productivity. Remote Sensing of Environment, 182, 72-89.")
ncdf4::ncatt_put(ncout,0,"geospatial_lon_min", spE@xmin, prec="float")
ncdf4::ncatt_put(ncout,0,"geospatial_lon_max", spE@xmax, prec="float")
ncdf4::ncatt_put(ncout,0,"geospatial_lat_min", spE@ymin, prec="float")
ncdf4::ncatt_put(ncout,0,"geospatial_lat_max", spE@ymax, prec="float")
ncdf4::ncatt_put(ncout,0,"spatial_resolution", paste(raster::res(SIF)[1], "degrees", sep=" "))
ncdf4::ncatt_put(ncout,0,"cdm_data_type","Grid")
ncdf4::ncatt_put(ncout,0,"grid_type","GeoX GeoY")

# set product history
nc_hhist <- paste(paste(names(opt), opt, sep="="), collapse=" --")
nc_history <- paste(date(), ": ", "SIF_downscaling", " --", nc_hhist, sep="")
ncdf4::ncatt_put(ncout,0,"history",nc_history)
# set creation date
get_systime <- unlist(strsplit(as.character(Sys.time()), split=" "))
date_creation <- paste(get_systime[1], "T", get_systime[2], "+0000", sep="")
ncdf4::ncatt_put(ncout,0,"date_created",date_creation)

# # additional arguments related to input products
# #ncdf4::ncatt_put(ncout,0,"","")
ncdf4::ncatt_put(ncout,0,"time_coverage_start", as.character(as.Date(nc_time_dim-8, origin="1970-01-01")))
ncdf4::ncatt_put(ncout,0,"time_coverage_end", as.character(as.Date(nc_time_dim+7, origin="1970-01-01")))
# ncdf4::ncatt_put(ncout,0,"creator_name","")
# ncdf4::ncatt_put(ncout,0,"contact","") ### email
# ncdf4::ncatt_put(ncout,0,"processing_level","")
ncdf4::ncatt_put(ncout,0,"processor_name","SIF spatial donwscaling processor")
ncdf4::ncatt_put(ncout,0,"processor_version","2.0")
# ncdf4::ncatt_put(ncout,0,"source", "SIF products from ...") ### set name of products
ncdf4::ncatt_put(ncout,0,"platform", "MetOp")
ncdf4::ncatt_put(ncout,0,"sensor", "GOME-2")
# #ncdf4::ncatt_put(ncout,0,"contributor_name","") ###
# #ncdf4::ncatt_put(ncout,0,"contributor_role","") ###
# ncdf4::ncatt_put(ncout,0,"comment","")
# ncdf4::ncatt_put(ncout,0,"license","")
# ncdf4::ncatt_put(ncout,0,"terms_of_use","")
# ncdf4::ncatt_put(ncout,0,"disclaimer","")
# ncdf4::ncatt_put(ncout,0,"acknowledgment","")

# close the file, writing data to disk
ncdf4::nc_sync(ncout)
ncdf4::nc_close(ncout)

# write out variables coefficients
if(!is.null(i_output_variables)){
  
  # compute variables
  NDVI_VAR <- (B5[[2]]*(NDVI_HR^B5[[1]]))
  ET_VAR <- (1/(1+exp(B5[[3]]*(-1*EVTR_HR+B5[[4]]))))
  LST_VAR <- (exp(-0.5*((LSTD_HR+B5[[5]])/B5[[6]])^2))
  
  # clean data
  # NDVI_VAR[which(getValues(NDVI_VAR) <= 0)] <- NA
  # NDVI_VAR[which(getValues(NDVI_VAR) > 65534)] <- NA
  # NDVI_VAR[which(is.na(getValues(SIF)))] <- NA
  # ET_VAR[which(getValues(ET_VAR) <= 0)] <- NA
  # ET_VAR[which(getValues(ET_VAR) > 65534)] <- NA
  # ET_VAR[which(is.na(getValues(SIF)))] <- NA
  # LST_VAR[which(getValues(LST_VAR) <= 0)] <- NA
  # LST_VAR[which(getValues(LST_VAR) > 65534)] <- NA
  # LST_VAR[which(is.na(getValues(SIF)))] <- NA

  # # read input NDVI as NetCDF object
  # nc_in <- ncdf4::nc_open(filename=input_SIF, write=FALSE, readunlim=FALSE)
  # # get dimensions position
  # nc_lon_pos <- which(names(nc_in$dim) %in% c("lon","longitude","Longitude"))[1]
  # nc_lat_pos <- which(names(nc_in$dim) %in% c("lat","latitude","Latitude"))[1]
  # #nc_time_pos <- which(names(nc_in$dim) %in% c("time","Time"))[1]
  # 
  # # get dimension values
  # nc_lon_dim <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_lon_pos])
  # nc_lat_dim <- sort(ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_lat_pos]), decreasing=TRUE)
  # #nc_time_dim <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_time_pos])
  # acq_date <- as.integer(rev(unlist(strsplit(unlist(strsplit(basename(input_SIF), split=".nc")), split="_")))[1]) ### fix
  # nc_time_dim <- as.integer(as.Date(strptime(acq_date, format="%Y%j"))) ### fix
  # # get dimension units
  # nc_lon_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lon_pos], "units")$value
  # nc_lat_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lat_pos], "units")$value
  # #nc_time_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_time_pos], "units")$value
  # nc_time_units <- c("days since 1970-01-01 00:00:00") ### fix
  # # get dimension longnames
  # nc_lon_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lon_pos], "long_name")$value
  # nc_lat_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lat_pos], "long_name")$value
  # #nc_time_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_time_pos], "long_name")$value
  # nc_time_lname <- "Time" ### fix
  # # define fill value
  # #ncout_fill_value <- -32768
  # 
  # # define new dimensions
  # ncout_londim <- ncdf4::ncdim_def(name="lon", longname=nc_lon_lname, units=nc_lon_units, vals=as.double(nc_lon_dim), unlim=FALSE)
  # ncout_latdim <- ncdf4::ncdim_def(name="lat", longname=nc_lat_lname, units=nc_lat_units, vals=as.double(sort(nc_lat_dim, decreasing=TRUE)), unlim=FALSE)
  # ncout_timedim <- ncdf4::ncdim_def(name="time", longname=nc_time_lname, units=nc_time_units, vals=as.double(nc_time_dim), unlim=TRUE)
  
  # define variables
  nc_var_crs <- ncdf4::ncvar_def(name="crs", longname="CRS definition", units="dl", prec="char", dim=list())
  nc_var_ndvi <- ncdf4::ncvar_def(name="NDVI", longname="NDVI variable used for SIF downscaling", units="dl", prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
  nc_var_et <- ncdf4::ncvar_def(name="ET", longname="ET variable used for SIF downscaling", units="dl", prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
  nc_var_lst <- ncdf4::ncvar_def(name="LST", longname="NDWI variable used for SIF downscaling", units="dl", prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
  # create list of variables for NetCDF-4 creation
  nc_var_list <- list(nc_var_ndvi, nc_var_et, nc_var_lst, nc_var_crs)
  
  # create output NetCDF-4 file
  ncout <- ncdf4::nc_create(output_variables, vars=nc_var_list, force_v4=TRUE)
  
  # write results to NetCDF file
  ncdf4::ncvar_put(ncout, varid="NDVI", vals=raster::getValues(NDVI_VAR), start=c(1,1,1), count=c(-1,-1,-1))
  ncdf4::ncvar_put(ncout, varid="ET", vals=raster::getValues(ET_VAR), start=c(1,1,1), count=c(-1,-1,-1))
  ncdf4::ncvar_put(ncout, varid="LST", vals=raster::getValues(LST_VAR), start=c(1,1,1), count=c(-1,-1,-1))
  
  # put additional attributes into dimension and data variables
  ncdf4::ncatt_put(ncout,"lon","standard_name","longitude")
  ncdf4::ncatt_put(ncout,"lon","axis","X")
  ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
  ncdf4::ncatt_put(ncout,"lat","axis","Y")
  ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
  ncdf4::ncatt_put(ncout,"time","standard_name","time")
  ncdf4::ncatt_put(ncout,"time","calendar","standard")
  #ncdf4::ncatt_put(ncout, nc_var_ndvi, "add_offset", 0.0, prec="float")
  #ncdf4::ncatt_put(ncout, nc_var_ndvi, "scale_factor", 0.0001, prec="float")
  ncdf4::ncatt_put(ncout, nc_var_ndvi, "grid_mapping", "crs")
  #ncdf4::ncatt_put(ncout, nc_var_et, "add_offset", 0.0, prec="float")
  #ncdf4::ncatt_put(ncout, nc_var_et, "scale_factor", 0.0001, prec="float")
  ncdf4::ncatt_put(ncout, nc_var_et, "grid_mapping", "crs")
  #ncdf4::ncatt_put(ncout, nc_var_lst, "add_offset", 0.0, prec="float")
  #ncdf4::ncatt_put(ncout, nc_var_lst, "scale_factor", 0.0001, prec="float")
  ncdf4::ncatt_put(ncout, nc_var_lst, "grid_mapping", "crs")

  # set attributed for 'crs' variable
  if(epsg_code == 4326){
    ncdf4::ncatt_put(ncout,"crs","grid_mapping_name","latitude_longitude")
    ncdf4::ncatt_put(ncout,"crs","longitude_of_prime_meridian", 0.0, prec="float")
    ncdf4::ncatt_put(ncout,"crs","semi_major_axis", 6378137, prec="float")
    ncdf4::ncatt_put(ncout,"crs","inverse_flattening", 298.257223563, prec="float")
  }
  ncdf4::ncatt_put(ncout,"crs","projection", crs_def)
  ncdf4::ncatt_put(ncout,"crs","proj4_params", crs_def)
  ncdf4::ncatt_put(ncout,"crs","EPSG_code", epsg_code, prec="short")
  ncdf4::ncatt_put(ncout,"crs","spatial_ref", wkt_def)
  
  # set global attributes to output NetCDF-4 file
  ncdf4::ncatt_put(ncout,0,"title","SIF variables")
  ncdf4::ncatt_put(ncout,0,"product_version", "2.0", prec="char")
  ncdf4::ncatt_put(ncout,0,"Conventions", "CF-1.7")
  ncdf4::ncatt_put(ncout,0,"summary","Product generated using ’SIF spatially downscaling algorithm’")
  ncdf4::ncatt_put(ncout,0,"keywords","sun-induced chlorophyll fluorescence")
  ncdf4::ncatt_put(ncout,0,"references","Duveiller, G., & Cescatti, A. (2016). Spatially downscaling sun-induced chlorophyll fluorescence leads to an improved temporal correlation with gross primary productivity. Remote Sensing of Environment, 182, 72-89.")
  ncdf4::ncatt_put(ncout,0,"geospatial_lon_min", spE@xmin, prec="float")
  ncdf4::ncatt_put(ncout,0,"geospatial_lon_max", spE@xmax, prec="float")
  ncdf4::ncatt_put(ncout,0,"geospatial_lat_min", spE@ymin, prec="float")
  ncdf4::ncatt_put(ncout,0,"geospatial_lat_max", spE@ymax, prec="float")
  ncdf4::ncatt_put(ncout,0,"spatial_resolution", paste(raster::res(B)[1], "degrees", sep=" "))
  ncdf4::ncatt_put(ncout,0,"cdm_data_type","Grid")
  ncdf4::ncatt_put(ncout,0,"grid_type","GeoX GeoY")
  
  # set product history
  nc_hhist <- paste(paste(names(opt), opt, sep="="), collapse=" --")
  nc_history <- paste(date(), ": ", "SIF_downscaling", " --", nc_hhist, sep="")
  ncdf4::ncatt_put(ncout,0,"history",nc_history)
  # set creation date
  get_systime <- unlist(strsplit(as.character(Sys.time()), split=" "))
  date_creation <- paste(get_systime[1], "T", get_systime[2], "+0000", sep="")
  ncdf4::ncatt_put(ncout,0,"date_created",date_creation)
  
  # close the file, writing data to disk
  ncdf4::nc_sync(ncout)
  ncdf4::nc_close(ncout)
}

# write out variables coefficients
if(!is.null(i_output_coefficients)){
  
  # read input NDVI as NetCDF object
  nc_in <- ncdf4::nc_open(filename=input_SIF, write=FALSE, readunlim=FALSE)
  # get dimensions position
  nc_lon_pos <- which(names(nc_in$dim) %in% c("lon","longitude","Longitude"))[1]
  nc_lat_pos <- which(names(nc_in$dim) %in% c("lat","latitude","Latitude"))[1]
  #nc_time_pos <- which(names(nc_in$dim) %in% c("time","Time"))[1]
  
  # get dimension values
  nc_lon_dim <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_lon_pos])
  nc_lat_dim <- sort(ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_lat_pos]), decreasing=TRUE)
  #nc_time_dim <- ncdf4::ncvar_get(nc_in, names(nc_in$dim)[nc_time_pos])
  acq_date <- as.integer(rev(unlist(strsplit(unlist(strsplit(basename(input_SIF), split=".nc")), split="_")))[1]) ### fix
  nc_time_dim <- as.integer(as.Date(strptime(acq_date, format="%Y%j"))) ### fix
  # get dimension units
  nc_lon_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lon_pos], "units")$value
  nc_lat_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lat_pos], "units")$value
  #nc_time_units <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_time_pos], "units")$value
  nc_time_units <- c("days since 1970-01-01 00:00:00") ### fix
  # get dimension longnames
  nc_lon_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lon_pos], "long_name")$value
  nc_lat_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_lat_pos], "long_name")$value
  #nc_time_lname <- ncdf4::ncatt_get(nc_in, names(nc_in$dim)[nc_time_pos], "long_name")$value
  nc_time_lname <- "Time" ### fix
  # define fill value
  #ncout_fill_value <- -32768
  
  # define new dimensions
  ncout_londim <- ncdf4::ncdim_def(name="lon", longname=nc_lon_lname, units=nc_lon_units, vals=as.double(nc_lon_dim), unlim=FALSE)
  ncout_latdim <- ncdf4::ncdim_def(name="lat", longname=nc_lat_lname, units=nc_lat_units, vals=as.double(sort(nc_lat_dim, decreasing=TRUE)), unlim=FALSE)
  ncout_timedim <- ncdf4::ncdim_def(name="time", longname=nc_time_lname, units=nc_time_units, vals=as.double(nc_time_dim), unlim=TRUE)
  # define variables
  nc_var_crs <- ncdf4::ncvar_def(name="crs", longname="CRS definition", units="dl", prec="char", dim=list())
  nc_var_b1 <- ncdf4::ncvar_def(name="b1", longname="b1 parameter coefficients used for SIF downscaling", units="dl", prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
  nc_var_b2 <- ncdf4::ncvar_def(name="b2", longname="b2 parameter coefficients used for SIF downscaling", units="dl", prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
  nc_var_b3 <- ncdf4::ncvar_def(name="b3", longname="b3 parameter coefficients used for SIF downscaling", units="dl", prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
  nc_var_b4 <- ncdf4::ncvar_def(name="b4", longname="b4 parameter coefficients used for SIF downscaling", units="dl", prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
  nc_var_b5 <- ncdf4::ncvar_def(name="b5", longname="b5 parameter coefficients used for SIF downscaling", units="dl", prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
  nc_var_b6 <- ncdf4::ncvar_def(name="b6", longname="b6 parameter coefficients used for SIF downscaling", units="dl", prec="double", dim=list(ncout_londim,ncout_latdim,ncout_timedim), chunksizes=c(length(nc_lon_dim),length(nc_lat_dim),1), compression=nc_deflate)
  # create list of variables for NetCDF-4 creation
  nc_var_list <- list(nc_var_b1, nc_var_b2, nc_var_b3, nc_var_b4, nc_var_b5, nc_var_b6, nc_var_crs)
  
  # create output NetCDF-4 file
  ncout <- ncdf4::nc_create(output_coefficients, vars=nc_var_list, force_v4=TRUE)
  
  # write results to NetCDF file
  for(l in 1:raster::nlayers(B)){
    ncdf4::ncvar_put(ncout, varid=paste("b", l, sep=""), vals=raster::getValues(B[[l]]), start=c(1,1,1), count=c(-1,-1,-1))
  }
  
  # put additional attributes into dimension and data variables
  ncdf4::ncatt_put(ncout,"lon","standard_name","longitude")
  ncdf4::ncatt_put(ncout,"lon","axis","X")
  ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
  ncdf4::ncatt_put(ncout,"lat","axis","Y")
  ncdf4::ncatt_put(ncout,"lat","standard_name","latitude")
  ncdf4::ncatt_put(ncout,"time","standard_name","time")
  ncdf4::ncatt_put(ncout,"time","calendar","standard")
  #ncdf4::ncatt_put(ncout, nc_var_sif, "add_offset", 0.0, prec="float")
  #ncdf4::ncatt_put(ncout, nc_var_sif, "scale_factor", 0.0001, prec="float")
  ncdf4::ncatt_put(ncout, nc_var_b1, "grid_mapping", "crs")
  ncdf4::ncatt_put(ncout, nc_var_b2, "grid_mapping", "crs")
  ncdf4::ncatt_put(ncout, nc_var_b3, "grid_mapping", "crs")
  ncdf4::ncatt_put(ncout, nc_var_b4, "grid_mapping", "crs")
  ncdf4::ncatt_put(ncout, nc_var_b5, "grid_mapping", "crs")
  ncdf4::ncatt_put(ncout, nc_var_b6, "grid_mapping", "crs")
  # put attributes related to the variables used
  ncdf4::ncatt_put(ncout, nc_var_b1, "parameter", "NDVI")
  ncdf4::ncatt_put(ncout, nc_var_b2, "parameter", "NDVI")
  ncdf4::ncatt_put(ncout, nc_var_b3, "parameter", "NDWI")
  ncdf4::ncatt_put(ncout, nc_var_b4, "parameter", "NDWI")
  ncdf4::ncatt_put(ncout, nc_var_b5, "parameter", "LST")
  ncdf4::ncatt_put(ncout, nc_var_b6, "parameter", "LST")
  #ncdf4::ncatt_put(ncout, nc_var_coefficients, "coefficients_parameters", c("b1: NDVI; b2: NDVI; b3: NDWI; b4: NDWI; b5: LST; b6:LST"))
  
  # set attributed for 'crs' variable
  # get spatial extent and resolution of final SIF product
  spE <- raster::extent(B)
  if(epsg_code == 4326){
    ncdf4::ncatt_put(ncout,"crs","grid_mapping_name","latitude_longitude")
    ncdf4::ncatt_put(ncout,"crs","longitude_of_prime_meridian", 0.0, prec="float")
    ncdf4::ncatt_put(ncout,"crs","semi_major_axis", 6378137, prec="float")
    ncdf4::ncatt_put(ncout,"crs","inverse_flattening", 298.257223563, prec="float")
  }
  ncdf4::ncatt_put(ncout,"crs","projection", crs_def)
  ncdf4::ncatt_put(ncout,"crs","proj4_params", crs_def)
  ncdf4::ncatt_put(ncout,"crs","EPSG_code", epsg_code, prec="short")
  ncdf4::ncatt_put(ncout,"crs","spatial_ref", wkt_def)
  
  # set global attributes to output NetCDF-4 file
  ncdf4::ncatt_put(ncout,0,"title","SIF coefficients")
  ncdf4::ncatt_put(ncout,0,"product_version", "2.0", prec="char")
  ncdf4::ncatt_put(ncout,0,"Conventions", "CF-1.7")
  ncdf4::ncatt_put(ncout,0,"summary","Product generated using ’SIF spatially downscaling algorithm’")
  ncdf4::ncatt_put(ncout,0,"keywords","sun-induced chlorophyll fluorescence")
  ncdf4::ncatt_put(ncout,0,"references","Duveiller, G., & Cescatti, A. (2016). Spatially downscaling sun-induced chlorophyll fluorescence leads to an improved temporal correlation with gross primary productivity. Remote Sensing of Environment, 182, 72-89.")
  ncdf4::ncatt_put(ncout,0,"geospatial_lon_min", spE@xmin, prec="float")
  ncdf4::ncatt_put(ncout,0,"geospatial_lon_max", spE@xmax, prec="float")
  ncdf4::ncatt_put(ncout,0,"geospatial_lat_min", spE@ymin, prec="float")
  ncdf4::ncatt_put(ncout,0,"geospatial_lat_max", spE@ymax, prec="float")
  ncdf4::ncatt_put(ncout,0,"spatial_resolution", paste(raster::res(B)[1], "degrees", sep=" "))
  ncdf4::ncatt_put(ncout,0,"cdm_data_type","Grid")
  ncdf4::ncatt_put(ncout,0,"grid_type","GeoX GeoY")
  
  # set product history
  nc_hhist <- paste(paste(names(opt), opt, sep="="), collapse=" --")
  nc_history <- paste(date(), ": ", "SIF_downscaling", " --", nc_hhist, sep="")
  ncdf4::ncatt_put(ncout,0,"history",nc_history)
  # set creation date
  get_systime <- unlist(strsplit(as.character(Sys.time()), split=" "))
  date_creation <- paste(get_systime[1], "T", get_systime[2], "+0000", sep="")
  ncdf4::ncatt_put(ncout,0,"date_created",date_creation)
  
  # close the file, writing data to disk
  ncdf4::nc_sync(ncout)
  ncdf4::nc_close(ncout)
}

# print usage time
if(verbose){
  message(paste(c("SIF downscaling ended at: "), Sys.time(), sep=""))
}
if(!quiet){
  message(paste("Elapsed time ", as.character(paste(as.integer(as.numeric(proc.time() - ptm)[3]/3600), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/3600 - as.integer(as.numeric(proc.time() - ptm)[3]/3600)) * 60)), ":", sprintf("%02i", as.integer((as.numeric(proc.time() - ptm)[3]/60 - as.integer(as.numeric(proc.time() - ptm)[3]/60)) * 60)), sep="")), " hours", sep=""))
}

# clean workspace and exist
rm(list=ls())
invisible(gc(verbose=FALSE))
q("no")
