*
* $Id: fort.inc,v 1.1.1.1 1996/02/15 17:53:55 mclareni Exp $
*
* $Log: fort.inc,v $
* Revision 1.1.1.1  1996/02/15 17:53:55  mclareni
* Kernlib
*
*
*
* fort.inc
*
#if defined(CERNLIB_GUYFORE)
FORE      EQU       1
FORT      EQU       0
FTN       EQU       0
#endif
*
* fort.inc
*
#if defined(CERNLIB_GUYFORT)
FORE      EQU       0
FORT      EQU       1
FTN       EQU       0
#endif
*
* fort.inc
*
#if defined(CERNLIB_GUYFTN)
FORE      EQU       0
FORT      EQU       0
FTN       EQU       1
.         ASCII .  PROC TO BE INCLUDED WHERE NEEDED ONLY
#endif
