*
* $Id: fmsort.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmsort.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSORT(FILES,KEYS,NFILES,JSORT,IRC)
#include "fatmen/faust.inc"
*
*     Subroutine to return in the array JSORT the indices
*     of FILES(I) and KEYS(LKEYFA,I) sorted in ascending order
*     according to contents of character array FILES
*
      CHARACTER*(*) FILES(NFILES)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA,NFILES)
      DIMENSION JSORT(NFILES)
 
      IRC    = 0
      NFSORT = NFSORT + 1
      CALL SORTCH(FILES,JSORT,NFILES)
 
      END
 
