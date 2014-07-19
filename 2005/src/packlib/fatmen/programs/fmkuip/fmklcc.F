*
* $Id: fmklcc.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmklcc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMKLCC
 
      CHARACTER*255 CHFILE
 
      CALL KUGETS(CHFILE,LFILE)
 
      IF(LFILE.EQ.0) RETURN
 
      CALL FMGLUN(LUNCOD,IRC)
      CALL FMLCOD(LUNCOD,CHFILE(1:LFILE),' ',IRC)
      CALL FMFLUN(LUNCOD,IRC)
 
      END
