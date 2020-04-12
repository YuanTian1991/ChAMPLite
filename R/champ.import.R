# if(getRversion() >= "3.1.0") utils::globalVariables(c("Anno","myImport","read.csv"))
champ.import <- function(directory = getwd(),
                         offset = 100,
                         arraytype="450K")
{
    champ.message(0, '[ChAMP IMPORT START]')

    champ.message(2,"Please make sure there is ONLY one csv file in your directory.")
    champ.message(2,"Please make sure AT LEAST Sample_Name, Sentrix_ID, Sentrix_Position contains in your table header.")
    champ.message(2,"Please make sure CSV corresponding IDAT files are exists in the directory, or sub-directories.")

    champ.message(1, '1: Read CSV Files')
    if(!file.exists(directory)) champ.message(1, "Your 'directory' parameter does not exists, please assign a correct directory.", 'stop')
    csvfile <- list.files(directory,recursive=TRUE,pattern="csv$",full.names=TRUE)
    if(length(csvfile) == 0) champ.message(2, paste("champ.import can not find any csv file in ",directory,"."), 'stop')
    if (length(csvfile) >=2) champ.message(2, paste("champ.import finds more than one csv file in ",directory,"."), 'stop')

    champ.message(2,"Reading CSV File")
    skipline <- which(substr(readLines(csvfile),1,6) == "[Data]")
    if(length(skipline)==0)
        suppressWarnings(pd <- read.csv(csvfile,stringsAsFactor=FALSE,header=TRUE, as.is=TRUE))
     else
         suppressWarnings(pd <- read.csv(csvfile,skip=skipline,stringsAsFactor=FALSE,header=TRUE, as.is=TRUE))

    if("Sentrix_Position" %in% colnames(pd))
    {
         colnames(pd)[which(colnames(pd)=="Sentrix_Position")] <- "Array"
         champ.message(2, "Replace Sentrix_Position into Array")
    } else {
         champ.message(2, "Your pd file contains NO Array(Sentrix_Position) information.")
     }

     if("Sentrix_ID" %in% colnames(pd))
     {
         colnames(pd)[which(colnames(pd)=="Sentrix_ID")] <- "Slide"
         champ.message(2,"Replace Sentrix_ID into Slide")
     } else
     {
         champ.message("Your pd file contains NO Slide(Sentrix_ID) information.")
     }

    champ.message(1, '2: Matching IDAT Files with CSV File')

     if(!all(c("Array","Slide") %in% colnames(pd))) champ.message(2,"No Array/Sentrix_Position and Slide/Sentrix_ID detected in your csv","stop")

     GrnPath <- unlist(sapply(paste(pd$Slide,pd$Array,"Grn.idat",sep="_"), function(x) grep(x,list.files(directory,recursive=T,full.names=TRUE), value = TRUE)))
     RedPath <- unlist(sapply(paste(pd$Slide,pd$Array,"Red.idat",sep="_"), function(x) grep(x,list.files(directory,recursive=T,full.names=TRUE), value = TRUE)))

     GrnFiles <- names(GrnPath)
     RedFiles <- names(RedPath)

     csvGrnName <- paste(pd$Slide,pd$Array,"Grn.idat",sep="_")
     csvRedName <- paste(pd$Slide,pd$Array,"Red.idat",sep="_")

     if(identical(csvGrnName,GrnFiles) & identical(csvRedName, RedFiles))
     {
         champ.message(2,"Both Green and Red Channels are Matched Success.")
     } else
     {
         champ.message(2, "Below are Green IDAT in csv file, but not in your folder.")
         print(setdiff(csvGrnName,GrnFiles))
         champ.message(2, "Below are Red IDAT in csv file, but not in your folder.")
         print(setdiff(csvRedName,RedFiles))

         champ.message(2,"IDAT File Match Failed.","stop")
     }

    champ.message(1, '3: Read IDAT Files')

    readFile <- function(file, fileList)
    {
        name <- tail(strsplit(file,split="/")[[1]],1)
        champ.message(2, paste("Loading: ", name," ---- (", which(fileList == file), "/",length(fileList),")", sep=""))
        return(readIDAT(file))
    }

     G.idats <- lapply(GrnPath, function(x) readFile(x, GrnPath))
     R.idats <- lapply(RedPath, function(x) readFile(x, RedPath))

    # champ.message(2, '3.1: Extract Green Channel')
    # champ.message(2, '3.2: Extract Red Channel')
    # champ.message(1, '4: Prepare Data Matrix')
    # champ.message(2, '4.1: Prepare Detected P Matrix')
    # champ.message(2, '4.2: Prepare Beta Matrix')
    # champ.message(1, '5: Return Result')

    return(list(pd=pd))
}
