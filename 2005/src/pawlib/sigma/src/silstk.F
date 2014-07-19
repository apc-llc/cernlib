*
* $Id: silstk.F,v 1.1.1.1 1995/12/12 14:36:18 mclareni Exp $
*
* $Log: silstk.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:18  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SILSTK (LL,CL)
C
C
C
C    ....................................................
C
C
C         PURPOSE
C            STACK PARAMETERS IN LSTACK AND CSTACK
C            INCREMENT STACK POINTER ISTAPO BYE ONE
C
C         USAGE
C            CALL SILSTK(LL,CL)
C
C         DESCRIPTION OF PARAMETERS
C            LL: ARRAY TO BE PUT IN LSTACK
C            CL: NAME TO PUT IN CSTACK
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: ISTAPO,ITAM,LSTACK(,),CSTACK()
C
C         REMARKS
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SITRAC
C            MESSL
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
C
C
      DIMENSION LL(6)
      CHARACTER CL*8
C
      CALL SITRAC (' SILSTK  ')
C
C
      ISTAPO=ISTAPO+1
      IF (ISTAPO.LE.0) PRINT 1, ISTAPO
      IF (ISTAPO.GE.128) PRINT 1, ISTAPO
      DO 33 IL=1,ITAM
      LSTACK(IL,ISTAPO)=LL(IL)
 33   CONTINUE
      CSTACK(ISTAPO)=CL
      IF(SITRAK(20).EQ.1) PRINT 20,ISTAPO
  20  FORMAT(' STACKED AT ISTAPO = ',I5)
C
 1    FORMAT (' ISTAPO FROM ILSTAK = ',I5)
 999  END
