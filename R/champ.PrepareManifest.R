# Function to preprocess manifest provided by illumina (or other valid format)
# into a data frame for IDAT data loading. More specifically, it will extract
# information like probe, CpG, colour channel, control .etc from manifest. They
# are important for IDAT file parsing.

library("data.table")
library("glue")

champ.PrepareManifest <- function(manifestPath,
                                  skip = "auto") {
    
    champ.paramCheck.PrepareManifest(manifestPath, skip)

    skip_lines <- NULL
    if(skip == "auto") {
        manifest <- fread(manifestPath, fill=TRUE, showProgress=FALSE)
        skip_lines$assay <- grep('\\[Assay\\]', as.data.frame(manifest[, 1])[,1])
        skip_lines$controls <- grep('\\[Controls\\]', as.data.frame(manifest[, 1])[,1])

        message("[Estimate Skipline] Success.")
    } else {
        skip_lines$assay <- skip[1]
        skip_lines$controls <- skip[2]
    }
    
    manifest <- fread(manifestPath, head = TRUE, skip=skip_lines$assay, nrows=skip_lines$controls - skip_lines$assay - 2)
    manifest <- manifest[, c("IlmnID", "AddressA_ID", "AddressB_ID", "Infinium_Design_Type", "Color_Channel")]

    message("[Process] Read Manifest (No Control Probes) Success.")
    message(glue("[Process] Totally there are {nrow(manifest)} probes in your manifest."))
    
    message("[Process] Separately processing Type-II, Type-I-Red and Type-I-Grn CpGs.")
    typeII <- manifest[Infinium_Design_Type == "II" | Infinium_Design_Type == 2, ]
    typeIRed <- manifest[(Infinium_Design_Type == "I" | Infinium_Design_Type == 1) & Color_Channel == "Red", ]
    typeIGrn <- manifest[(Infinium_Design_Type == "I" | Infinium_Design_Type == 1) & Color_Channel == "Grn", ]
    
    message("[Process] Create colour-probe information for each type of probes.")
    mName <- c(typeII$IlmnID, typeIRed$IlmnID, typeIGrn$IlmnID)
    mIndex <- c(paste("G", typeII$AddressA_ID, sep = "-"),
                paste("R", typeIRed$AddressB_ID, sep = "-"), 
                paste("G", typeIGrn$AddressB_ID, sep = "-"))
    
    uIndex <- c(paste("R", typeII$AddressA_ID, sep = "-"), 
                paste("R", typeIRed$AddressA_ID, sep = "-"), 
                paste("G", typeIGrn$AddressA_ID, sep = "-"))
                
    pChannal <- c(rep("g+r", nrow(typeII)), rep("r", nrow(typeIRed)), rep("g", nrow(typeIGrn)))
    probeInfo <- data.frame(CpG = mName, mIndex = mIndex, uIndex = uIndex, channel = pChannal)
    rownames(probeInfo) <- probeInfo$CpG

    message("[Process] probe information data frame created. Success.")
    
    return(probeInfo)
}


champ.paramCheck.PrepareManifest <- function(manifestPath, skip) {
  
    if(class(manifestPath) != "character") {
        stop("[Parameter Check] Error: parameter manifest_path must be a string character to manifest path.")
    }
    if(skip != "auto" & (class(skip) != "numeric" & length(skip) == 2)) {
        stop("[Parameter Check] Error: array_skip must be 'auto' or a two length integer vector.")
    }

    message("[Parameter Check] Success.")
}
