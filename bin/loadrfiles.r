thefiles <- file.path("R", list.files("./R", pattern=".R"))
sapply(thefiles, source)