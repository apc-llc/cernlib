*
* $Id: siustk.F,v 1.1.1.1 1995/12/12 14:36:21 mclareni Exp $
*
* $Log: siustk.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:21  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/02 06/08/93  17.17.00  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIUSTK
C    ....................................................
C
C
C         PURPOSE
C            DELETE STACK ENTRY AND SET: ISTAPO=ISTAPO-1
C
C         USAGE
C            CALL SIUSTK
C            CALL UNSTAC    ENTRY FOR DECREMENTING ISTAPO
C
C         COMM. BLOCKS USED
C            COM1
C
C         REMARKS
C            OVERFLOW CONTROLL IS IN FOR ISTAPO
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SINERR
C            MESSI
C            RELARE
C            SEE
C            TRACE
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      CHARACTER CH*1,CNAM*8,CWORD*8
      DIMENSION INAM(6),IWORD(6)
      EQUIVALENCE(CH,CWORD)
C
      CALL SITRAC (' SIUSTK  ')
C
      DO 33 J=1,ITAM
      IWORD(J)=LSTACK(J,ISTAPO)
  33  CONTINUE
      CWORD=CSTACK(ISTAPO)
      IF(CH.NE.'$') GO TO 1
C---  IF $NAME
      DO 34 J=1,ITAM
      INAM(J)=INAME(J)
  34  CONTINUE
      CNAM=CNAME
      KI=KIT
      DO 35 J=1,ITAM
      INAME(J)=IWORD(J)
  35  CONTINUE
      CNAME=CWORD
      CALL SIGSEE
      IF(KIT.GE.0)CALL SINERR(6)
C          UNST& RELEASED
        CALL KUVDEL(CNAME)
      KIT=KI
      DO 36 J=1,ITAM
      INAME(J)=INAM(J)
  36  CONTINUE
      CNAME=CNAM
  1   CONTINUE
C
C
      ENTRY SIUSTC
      CALL SITRAC( ' SIUSTC')
      ISTAPO=ISTAPO-1
C
C
      IF(ISTAPO.LT.0) CALL SINERR(6)
C
C
      IF(SITRAK(20).EQ.1) PRINT *,' ISTAPO=  ',ISTAPO
C
 999  END
