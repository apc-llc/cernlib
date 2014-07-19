*
* $Id: fatbank%.inc,v 1.1.1.1 1996/03/07 15:17:35 mclareni Exp $
*
* $Log: fatbank%.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:35  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATBANK%_INC
#define CERNLIB_FATMEN_FATBANK%_INC
*
*
* fatbank%.inc
*
#if defined(CERNLIB__DOC)
#include "fatmen/fatbank.inc"
 
             fan-out bank offsets
 
    MNOPFA      Number of open files belonging to such "data base"
    MLUNFA      logical unit of the RZ-file
    MFLGFA
    MTOTFA
    MCRDFA
    MINPFA
    MOUTFA
    MTOPFA
    MNCHFA    number of character of the top directory name
    MCHRFA    top directory name
 
    NDUPFA    total number of word in the top directory
 
    NSLUFA    Number of "system links in "UPFA" bank
 
 
                Book-keeping bank   "BKFA"
    KLBKFA    link offset in UPFA
    NWBKFA    number of word for each "file"
 
    MSTAFA    file status
    MPNTFA    pointer in the generic names bank
    MCGNFA    number of characters used by the gen.name
 
                Generic names bank   "GNFA"
    KLGNFA    link offset in UPFA
 
    -5        number of word used
 
 
#endif

#endif
