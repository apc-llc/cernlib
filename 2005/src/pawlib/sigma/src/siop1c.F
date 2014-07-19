*
* $Id: siop1c.F,v 1.1.1.1 1995/12/12 14:36:19 mclareni Exp $
*
* $Log: siop1c.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:19  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/03 10/02/94  17.17.05  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIOP1C
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
#include "sigma/siclin.inc"
C
C
      DIMENSION IAD(3),JMIP(3)
      EQUIVALENCE(IAD,IZAD,IADA),(JMIP,IMODA,IZMOD)
      DIMENSION RCA(2),RCX(2)
      EQUIVALENCE(CPLA,RCA),(CPLX,RCX)
      EQUIVALENCE(RA,RCA(1)),(CA,RCA(2)),(RX,RCX(1)),(CX,RCX(2))
C
C
      RA=DYNA(LA2)
      IF(IMODA.EQ.2) CA=DYNA(LA2+1)
C
C
C   KLASS BOUNDS ARE ALREADY CHECKED BY SYSFUN
      GO TO(1,2,3,4,5,6,7,10,11,12),KLASS
 1    CONTINUE
C
C******** 01     ************
C 001    SIN
C
C
      CPLX=CSIN(CPLA)
      GOTO 111
C
 2    CONTINUE
C
C******** 02     ************
C 002
C
      CPLX=CCOS(CPLA)
      GOTO 111
C
 3    CONTINUE
C
C******** 03     ************
C 003    SQRT
      CPLX=CSQRT(CPLA)
      GOTO 111
C
 4    CONTINUE
C
C******** 04     ************
C 004    EXP
C
      CPLX=CEXP(CPLA)
      GOTO 111
C
 5    CONTINUE
C
C******** 05     ************
C 005    LOG
      IF(RA.EQ.0.0.AND.CA.EQ.0.0) GOTO 22222
      CPLX=CLOG(CPLA)
      GOTO 111
C
 6    CONTINUE
C
C******** 06     ************
C 006    ATAN
      CALL SINERR(32)
      RETURN
C
 7    CONTINUE
C******** 07     ************
C 007    ABS
C
      CPLX=CABS(CPLA)
      GOTO 111
C
 10   CONTINUE
C******** 10     ************
C 010    INT
      RX=AINT(RA)
      CX=AINT(CA)
      GOTO 111
C
 11   CONTINUE
C******** 11     ************
C 011    LOG10
      IF(RA.EQ.0.0.AND.CA.EQ.0.0) GOTO 22222
      CPLX=CLOG(CPLA)*(.43429448190325,0.0)
      GOTO 111
C
 12   CONTINUE
C******** 12     ************
C 012    TANH
      CALL SINERR(31)
      RETURN
C
C
22222 CONTINUE
C   ARBITRARILY RESET RESULT TO ZERO IF ARG NOT PERMISSIBLE
      RX=0.
      CX=0.
111   CONTINUE
      DYNA(LA3)=RX
      DYNA(LA3+1)=CX
C
      END
