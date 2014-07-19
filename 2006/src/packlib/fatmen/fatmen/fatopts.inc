*
* $Id: fatopts.inc,v 1.1.1.1 1996/03/07 15:17:56 mclareni Exp $
*
* $Log: fatopts.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:56  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATOPTS_INC
#define CERNLIB_FATMEN_FATOPTS_INC
*
*
* fatopts.inc
*
      CHARACTER*(*)  CHOPT
#include "fatmen/fatoptd.inc"
      CALL FMOPTC(CHOPT,ALFNUM,IOPT)
*     CALL UOPTC(CHOPT,ALFNUM,IOPT)

#endif
