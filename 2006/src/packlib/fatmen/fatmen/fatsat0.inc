*
* $Id: fatsat0.inc,v 1.1.1.1 1996/03/07 15:17:53 mclareni Exp $
*
* $Log: fatsat0.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:53  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATSAT0_INC
#define CERNLIB_FATMEN_FATSAT0_INC
*
*
* fatsat0.inc
*
      COMMON/FATSAT/LSRCST,LDSTST,CHSRCE,CHDEST
      CHARACTER*40  CHSRCE,CHDEST
*
*     CHEOPS sites
*
      PARAMETER   (NSITES=5)
      CHARACTER*8 CHSITE(NSITES)

#endif
