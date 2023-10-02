# Function to load fonts given a vector of names.

loadfonts <- function(fontvec = NULL, fontframe = NULL) {
  
  # Get all fonts if no fontframe
  if (is.null(fontframe)) {
    fontframe <- sysfonts::font_files()
  }
  
  # Load all fonts if no fontvec
  if (is.null(fontvec)) {
    fontvec <- unique(fontframe$family)
  }
  
  # Loop over the font families
  for (i in 1:length(fontvec)) {
    familyname <- fontvec[i]
    regfile <- fontframe[which(fontframe$family == familyname &
                                 fontframe$face == 'Regular'), 'file']
    
    italfile <- fontframe[which(fontframe$family == familyname &
                                  fontframe$face == 'Italic'), 'file']
    
    boldfile <- fontframe[which(fontframe$family == familyname &
                                  fontframe$face == 'Bold'), 'file']
    
    bifile <- fontframe[which(fontframe$family == familyname &
                                fontframe$face == 'Bold Italic'), 'file']
    
    ## TODO: THROW A TRYCATCH ON HERE TO BYPASS AND ALERT FOR FAILURES
    # For example, Berlin Sans FB Demi has no 'regular' and so fails. let's just skip those, this isn't supposed to be the most robust thing ever that handles all cases flawlessly.
    try(sysfonts::font_add(fontvec[i], 
                 regular = noface(regfile), 
                 italic = noface(italfile), 
                 bold = noface(boldfile), 
                 bolditalic = noface(bifile)))
    
    # To avoid unforeseen carryover through the loop
    rm(familyname, regfile, italfile, boldfile, bifile)
  }
  
}

# small helper
noface <- function(x) {ifelse(rlang::is_empty(x), return(NULL), return(x))}