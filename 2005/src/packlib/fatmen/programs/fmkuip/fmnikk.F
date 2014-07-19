*
* $Id: fmnikk.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmnikk.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMNIKK
 
      CHARACTER*255 CHNICK,CHNAME,CHDESC,CHFILE
      CHARACTER*8   CHOPT
      LOGICAL       IEXIST
#include "fatmen/fatbug.inc"
#include "fatmen/fatsys.inc"
 
      CALL KUGETC(CHNICK,LNICK)
      CALL KUGETC(CHFILE,LFILE)
      CALL KUGETC(CHOPT,LOPT)
 
      IF(LFILE.EQ.0) THEN
 
         CHFILE = ' '
         LFILE  = 1
      ENDIF
 
      CALL FMGLUN(LUN,IC)
      CALL FMNICK(LUN,CHFILE(1:LFILE),CHNICK(1:LNICK),
     +   CHNAME,CHDESC,CHOPT,IRC)
      CALL FMFLUN(LUN,IC)
 
      IF(IRC.EQ.32) THEN
         WRITE(LPRTFA,9001) CHNICK(1:LNICK),CHFILE(1:LFILE)
         RETURN
      ENDIF
 
      LNAME = LENOCC(CHNAME)
      LDESC = LENOCC(CHDESC)
 
      WRITE(LPRTFA,9002) CHNICK(1:LNICK),CHNAME(1:LNAME),CHDESC(1:LDESC)
 
9002  FORMAT(' FMNIKK. nickname: ',A,' = ',A/,'      description: ',A)
9001  FORMAT(' FMNIKK. nickname ',A,' not found in ',A)
 
      END
