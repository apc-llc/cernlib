*
* $Id: fmrslv.F,v 1.1.1.1 1996/03/07 15:18:21 mclareni Exp $
*
* $Log: fmrslv.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:21  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRSLV(DOMAIN,CHOPT,IRC)
*
*     Get current domain
*
      CHARACTER*(*)     DOMAIN
      CHARACTER*255     CHLINE
 
#include "fatmen/fatbug.inc"
#include "fatmen/fatopts.inc"
 
      IRC    = 0
      DOMAIN = ' '
 
#if defined(CERNLIB_UNIX)
*
*     Read /etc/resolv.conf and look for 'domain xxx'
*
      CALL CIOPEN(LUNRSL,'r','/etc/resolv.conf',IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) WRITE(LPRTFA,9001) IRC
 9001 FORMAT(' FMRSLV. error ',I10,' opening /etc/resolv.conf')
         GOTO 999
      ENDIF
 
   10 CONTINUE
      CALL FMCFGL(LUNRSL,CHLINE,LLINE,' ',ISTAT)
      IF(ISTAT.NE.0) GOTO 30
 
      IF(LLINE.EQ.0) GOTO 10
*
*     Convert non-printing characters (e.g. tabs) to blanks
*
      DO 20 I=1,LLINE
         IF(ICHAR(CHLINE(I:I)).LT.32.OR.
     +      ICHAR(CHLINE(I:I)).GT.126) CHLINE(I:I) = ' '
   20 CONTINUE
 
      IDOM = INDEX(CHLINE(1:LLINE),'domain ')
      IF(IDOM.EQ.0) GOTO 10
 
      DOMAIN = CHLINE(INDEXB(CHLINE(1:LLINE),' ')+1:LLINE)
      GOTO 30
 
   30 CONTINUE
      CALL FMCFGL(LUNRSL,CHLINE,LLINE,'F',ISTAT)
      CALL CICLOS(LUNRSL)
#endif
 
  999 END
