*
* $Id: fmrmlc.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmrmlc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRMLC
 
      CHARACTER*255 CHLINK,CHFILE
      CHARACTER*8   CHOPT
 
      CALL KUGETC(CHFILE,LLINK)
      CALL FMFIXF(CHFILE,CHLINK)
      LLINK = LENOCC(CHLINK)
      CALL KUGETI(LWRITE)
      CALL KUGETC(CHFILE,LFILE)
      CALL KUGETC(CHOPT,LOPT)
 
      CALL FMRMLN(CHLINK(1:LLINK),LWRITE,CHFILE(1:LFILE),
     +   CHOPT(1:LOPT),IRC)
 
      END
