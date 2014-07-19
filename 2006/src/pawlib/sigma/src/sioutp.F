*
* $Id: sioutp.F,v 1.1.1.1 1995/12/12 14:36:19 mclareni Exp $
*
* $Log: sioutp.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:19  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/00 26/10/93  12.10.16  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIOUTP (LIT)
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      CALL SITRAX(' SIOUTP  ')
C
C CHECK IPOINT LE MAX1, MAX2
      IF(IPOINT.LT.210)GO TO 40
      CALL SINERR(20)

      PRINT *,IPOINT
      IPOINT=1
40    CONTINUE
      L=LIT
      IF(L.GT.1000) GO TO 1000
      IF(SITRAK(12).NE.0)WRITE(NPRINT,1)L
1     FORMAT(' * XUTPUT *   ',I8)
1001  I=IPOINT
      IPOINT=IPOINT+1
      IPROGR(I)=LIT
      IF(SITRAK(5).NE.0)PRINT 4,L
 4    FORMAT (' OUT= ',I6)
C
      RETURN
1000  IF(SITRAK(12).NE.0)WRITE(NPRINT,1002)L
1002  FORMAT(' * XUTPUT *   ',A4)
       GOTO 1001
      END
