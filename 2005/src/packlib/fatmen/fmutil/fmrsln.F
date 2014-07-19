*
* $Id: fmrsln.F,v 1.1.1.1 1996/03/07 15:18:20 mclareni Exp $
*
* $Log: fmrsln.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:20  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_UNIX)
      SUBROUTINE FMRSLN(CHLINK,CHFILE,CHOPT,IRC)
*
*     Return file name pointed to by CHLINK
*
#include "fatmen/slate.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*(*) CHLINK,CHFILE
      CHARACTER*255 CHTEMP,CHCOMM
      INTEGER       SYSTEMF
 
      LOGICAL       IEXIST
 
#include "fatmen/fatopts.inc"
 
      IRC    = 0
      CHFILE = ' '
      IS(1)  = 0
 
      LLINK  = LENOCC(CHLINK)
 
*     Get temporary file name
*
   10 CONTINUE
      CALL FMFNME(CHTEMP)
      LTEMP = LENOCC(CHTEMP)
      INQUIRE(FILE=CHTEMP(1:LTEMP),EXIST=IEXIST)
      IF(IEXIST) THEN
         IC = SLEEPF(1)
         GO TO 10
      ENDIF
 
      IF(IDEBFA.GE.1) PRINT 9001,CHTEMP(1:LTEMP)
 9001 FORMAT(' FMRSLN. using temporary file ',A)
 
      WRITE(CHCOMM,9002) CHLINK(1:LLINK),CHTEMP(1:LTEMP)
 9002 FORMAT('ls -l ',A,' > ',A)
      LCOMM = LENOCC(CHCOMM)
 
      IF(IDEBFA.GE.1) WRITE(LPRTFA,9003) CHCOMM(1:LCOMM)
 9003 FORMAT(' FMRSLN. issuing ',A)
      IRC = SYSTEMF(CHCOMM(1:LCOMM))
 
      CALL CIOPEN(LUNPTR,'r',CHTEMP(1:LTEMP),IRC)
      CALL CIGET(LUNPTR,CHCOMM,255,NREAD,IRC)
      CALL CICLOS(LUNPTR)
      IF(NREAD.EQ.0.OR.IRC.NE.0) GOTO 99
      LCOMM = LENOCC(CHCOMM)
 
      LNEWL = INDEX(CHCOMM,CHAR(10))
      IF(LNEWL.NE.0) LCOMM = LNEWL - 1
 
      LSTART = INDEXB(CHCOMM(1:LCOMM),' ') + 1
 
      CHFILE = CHCOMM(LSTART:LCOMM)
      IS(1)  = LCOMM - LSTART + 1
 
      IF(IDEBFA.GE.0) PRINT 9004,CHFILE(1:IS(1))
 9004 FORMAT(' FMRSLN. file name is ',A)
 
 99   CALL UNLINKF(CHTEMP(1:LTEMP))
 
      END
#endif
