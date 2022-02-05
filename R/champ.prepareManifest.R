# Function to preprocess manifest provided by illumina (or other valid format)
# into a data frame for IDAT data loading. More specifically, it will extract
# information like probe, CpG, colour channel, control .etc from manifest. They
# are important for IDAT file parsing.

library("data.table")

champ.PrepareManifest <- function(manifestPath,
                                  skip = "auto") {
    
    champ.paramCheck.PrepareManifest(manifestPath, skip)

    skip_lines <- NULL
    if(skip == "auto") {
        manifest <- fread(manifestPath, fill=TRUE, showProgress=FALSE)
        skip_lines$assay <- grep('\\[Assay\\]', as.data.frame(manifest[, 1])[,1])
        skip_lines$controls <- grep('\\[Controls\\]', as.data.frame(manifest[, 1])[,1])
    } else {
        skip_lines$assay <- skip[1]
        skip_lines$controls <- skip[2]
    }

  manifest <- fread(manifestPath, head = TRUE, skip=skip_lines$assay, nrows=skip_lines$controls - skip_lines$assay - 2)
  manifest <- manifest[, c("IlmnID", "AddressA_ID", "AddressB_ID", "Infinium_Design_Type", "Color_Channel")]

  typeII <- manifest[Infinium_Design_Type == "II", ]
  typeIRed <- manifest[Infinium_Design_Type == "I" & Color_Channel == "Red", ]
  typeIGrn <- manifest[Infinium_Design_Type == "I" & Color_Channel == "Grn", ]

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

  Annotation$CpG <- as.character(Annotation$CpG)
  Annotation$M.index <- as.character(Annotation$M.index)
  Annotation$U.index <- as.character(Annotation$U.index)
  Annotation$Channel <- as.character(Annotation$Channel)

  Control <- read.csv("MethylationEPIC_v-1-0_B4_3.csv", head = F, sep = ",")
}


champ.paramCheck.PrepareManifest <- function(manifestPath, skip) {
    if(class(manifestPath) !== "character") {
        stop("[Parameter Error] parameter manifest_path must be a string character to manifest path.")
    }

    if(!file.exists(manifestPath)) {
        stop("[Parameter Error] manifest file not found in manifest_path. Please check your file path.")
    }

    if(skip != "auto" & (class(skip) != "numeric" & length(skip) == 2)) {
        stop("[Parameter Error] array_skip must be 'auto' or a two length integer vector.")
    }

    message("[Parameter Check Success]")
}
