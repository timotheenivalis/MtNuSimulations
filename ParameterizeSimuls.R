init_folder <- function(homedir="Simulations", focaldir = "test/", 
                        exefile="AllForward_V022.exe")
{
  focaldir <- gsub(focaldir, pattern = "/", replacement = "")
  
  spldir <- unlist(strsplit(homedir, split = "/"))
  currentpath <- "."
  for (i in 1:length(spldir))
  {
    if (! spldir[i] %in% list.files(path = currentpath , all.files = TRUE))
      {
        system(command = paste0("mkdir ", paste0(currentpath, "/", spldir[i])))
    }
    currentpath <- paste0(currentpath, "/", spldir[i])
  }
  
  if(substr(x = homedir, start = nchar(homedir), stop=nchar(homedir))=="/")
    {
    homedir <- substr(x = homedir, start = 1, stop=nchar(homedir)-1)
  }
  
  if(! focaldir %in% list.files(homedir) )
  {
    commkdir <- paste0("mkdir ", homedir, "/", focaldir, "/")
    system(command = commkdir)
    commcpexe <- paste0("cp ", exefile, " ", homedir, "/", focaldir, "/.")
    system(command = commcpexe)
    
  }
  return(0)
}

write_full_param <- function(homedir="Simulations", focaldir = "test/",
                             demesize=6, dimx=10, dimy=10, Xlimit=5,
         HabitatSlideBegin=0, HabitatSlideEnd=0, HabitatSlideDepth=0, 
         generationnumber=10, AllopatryLast=8000, DemeSamplingRatio=1, IndMeanSample=10, 
         dispmax=5, mFemale=0.1, geomFemale=0.1, mMale=0.1, geomMale=0.1, EdgeEffects="true",
         Swamping = "false", fitnessynormal=1, fitnessmaladaptation=c(0.9, 1.0), 
         fitnesshybridfemale=c(0.9,1.0), fitnesshybridmale=c(0.9, 1.0), fitnessMaladaptMt=1.0, 
         RunNumber = 1, LowHybridBound = 10, HighHybridBound= 0, seed = NULL, 
         WriteIdMatrix="false", WriteIdentitiesProba="false", WriteFstHe="false", 
         WriteGenepopFile="false", WriteGenepopIntrog="false", WriteGenepopOrigin="false",
         WriteGenepopAlsoPreContact="false", WriteIntrogProfile="true", WriteIntrogStats="true", WritePeriod = -1)
{
  focaldir <- gsub(focaldir, pattern = "/", replacement = "")
  
  if(substr(x = homedir, start = nchar(homedir), stop=nchar(homedir))=="/")
  {
    homedir <- substr(x = homedir, start = 1, stop=nchar(homedir)-1)
  } 
  sink(file = paste0(homedir, "/", focaldir, "/AllForward.txt"), append = FALSE)
  
  cat("%%%Demographic parameters%%%", sep = "\n")
  cat(paste0("demesize=", demesize, " //number in individuals"), sep = "\n")
  cat(paste0("dimx=", dimx), sep = "\n")
  cat(paste0("dimy=", dimy), sep = "\n")
  cat(paste0("Xlimit=", Xlimit, "//doit etre egal a dimX si un seul habitat"), sep = "\n")
  cat(paste0("HabitatSlideBegin=", HabitatSlideBegin), sep = "\n")
  cat(paste0("HabitatSlideEnd=", HabitatSlideEnd, "// if ==0 no habitat change"), sep = "\n")
  cat(paste0("HabitatSlideDepth=", HabitatSlideDepth, "// if ==0 no habitat change"), sep = "\n")
  cat(paste0("generationnumber=", generationnumber), sep = "\n")
  cat(paste0("AllopatryLast=", AllopatryLast,"  //in generation"), sep = "\n")
    
  cat("\n%%%Sampling parameters%%%", sep = "\n")
  cat(paste0("DemeSamplingRatio=", ifelse(DemeSamplingRatio==1, "1.", DemeSamplingRatio)), sep = "\n")
  cat(paste0("IndMeanSample=", IndMeanSample), sep = "\n")
    
    cat("\n%%%Dispersion parameters%%%", sep = "\n")
    cat(paste0("dispmax=", dispmax), sep = "\n")
    cat(paste0("mFemale=", paste(mFemale, collapse = ",")), sep = "\n")
    cat(paste0("geomFemale=", paste(geomFemale, collapse = ",")), sep = "\n")
    cat(paste0("mMale=", paste(mMale, collapse = ",")), sep = "\n")
    cat(paste0("geomMale=", paste(geomMale, collapse = ",")), sep = "\n")
    cat(paste0("EdgeEffects=", EdgeEffects), sep = "\n")
    
    cat("\n%%%Reproduction parameters%%%", sep = "\n")
    cat(paste0("Swamping=", Swamping), sep = "\n")
    cat(paste0("fitnessynormal=", ifelse(fitnessynormal==1, "1.0", fitnessynormal)), sep = "\n")
    cat(paste0("fitnessmaladaptation=", ifelse(fitnessmaladaptation==1, "1.0", fitnessmaladaptation)), sep = "\n")
    cat(paste0("fitnesshybridfemale=", 
           paste0( ifelse( fitnesshybridfemale==1, "1.",fitnesshybridfemale), collapse = ", ")), sep = "\n")
    cat(paste0("fitnesshybridmale=", 
           paste0( ifelse( fitnesshybridmale==1, "1.",fitnesshybridmale), collapse = ", ")), sep = "\n")
    cat(paste0("fitnessMaladaptMt=", ifelse(fitnessMaladaptMt==1, "1.0", fitnessMaladaptMt),
               "//fitness of the bad strand (habitat 1)"), sep = "\n")
    cat(paste0("HybridNb=", -1, "//-1==Infini"), sep = "\n")
    cat(paste0("AcceptRates=", 1, ",",1, ",",1, ",",1, ",",1, ",",1, ",",1, ",",1, ",",1), sep = "\n")
    cat(paste0("HomogamyAllLoci=false"), sep = "\n")
    cat(paste0("ChoosyFemale=", 0.5, "//rate of females which starts the couple formation"), sep = "\n")
    
    cat("\n%%%Markers parameters%%%", sep = "\n")
    cat(paste0("MuRate=5e-004"), sep = "\n")
    cat(paste0("AutLociNumber=", 20), sep = "\n")
    cat(paste0("InterRecombiRate=", 0.5, " //must <= 0.5"), sep = "\n")
    cat(paste0("IntraRecombiRate=", 0.5, " //must <= 0.5"), sep = "\n")
    
    cat("\n%%%Simulation parameters%%%", sep = "\n")
    cat(paste0("RunNumber=", RunNumber), sep = "\n")
    cat(paste0("MigRatesCorrection=true"), sep = "\n")
    cat(paste0("Pause=Final"), sep = "\n")
    cat(paste0("LowHybridBound=", LowHybridBound, " //ATTENTION, en cas d'invasion, penser a regler"), sep = "\n")
    cat(paste0("HighHybridBound=", HighHybridBound), sep = "\n")
    cat(paste0("SamplingSeed=", ifelse(is.null(seed), sample(1:2^15, 1), seed)), sep = "\n")

    cat("\n%%%Writing parameters%%%", sep = "\n")
    cat(paste0("WriteIdMatrix=", WriteIdMatrix), sep = "\n")
    cat(paste0("WriteIdentitiesProba=", WriteIdentitiesProba), sep = "\n")
    cat(paste0("WriteFstHe=", WriteFstHe), sep = "\n")
    cat(paste0("WriteGenepopFile=", WriteGenepopFile, " //pop=demes"), sep = "\n")
    cat(paste0("WriteGenepopIntrog=", WriteGenepopIntrog, " //pop=taxa"), sep = "\n")
    cat(paste0("WriteGenepopOrigin=", WriteGenepopOrigin, " //pop= deme, alleles=taxa of origin"), sep = "\n")
    cat(paste0("WriteGenepopAlsoPreContact=", WriteGenepopAlsoPreContact), sep = "\n")
    cat(paste0("WriteIntrogProfile=", WriteIntrogProfile), sep = "\n")
    cat(paste0("WriteIntrogStats=", WriteIntrogStats), sep = "\n")
    cat(paste0("WritePeriod=", WritePeriod), sep = "\n")

  sink()
}


qsub_gadi <- function(jobdir = "Simulations/test/", gsubfile = "gadi_file")
{
  if(substr(x = jobdir, start = nchar(jobdir), stop=nchar(jobdir))!="/")
  {
    jobdir <- paste0(jobdir, "/")
  }
  
  fil <- paste0(jobdir, gsubfile)
  write(x = "#!/bin/bash", file = fil, append = FALSE)
  write(x = " #PBS -P fu17", file = fil, append = TRUE)
  write(x = " #PBS -q normal", file = fil, append = TRUE)
  write(x = " #PBS -l walltime=48:00:00", file = fil, append = TRUE)
  write(x = " #PBS -l mem=8GB", file = fil, append = TRUE)
  write(x = " #PBS -l ncpus=1", file = fil, append = TRUE)
  write(x = " #PBS -M timotheebonnetc@gmail.com", file = fil, append = TRUE)
  write(x = " #PBS -m ae", file = fil, append = TRUE)
  write(x = " #PBS -l wd", file = fil, append = TRUE)
  
  write(x = paste0("./AllForward_V022.exe"), file = fil, append = TRUE)
  
  system(command = paste0("chmod a+x ./", fil))
  system(command = paste0("cd ", jobdir, " ; qsub ./",gsubfile))

}
