*
* $Id: silskk.F,v 1.1.1.1 1995/12/12 14:36:18 mclareni Exp $
*
* $Log: silskk.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:18  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SILSKK(L,LL,CL)
C
C
C
C    ....................................................
C
C
C         PURPOSE
C             RETURNS LL AND CL       OF POSITION -L OF LSTACK
C
C         USAGE
C            CALL SILSKK(L,LL,CL)
C
C         DESCRIPTION OF PARAMETERS
C            LL: STACKED ARRAY TO BE RETURNED
C            CL: STACKED NAME TO BE RETURNED
C            L : STACK POSISTION TO USE FOR RETURN
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: ISTAPO,CSTACK,LSTACK
C
C         REMARKS
C            NOTE THAT CURRENT POSITION OF STAK(TOP OF IT)
C            I S  A D D R E S S E D   B Y       0 (Z E R O )...
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C            MESSI
C            MESSL
C            MESSC
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
C
C
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
C
C
      DIMENSION LL(6)
      CHARACTER CL*8
C
C
      CALL SITRAC (' SILSKK   ')
C
      DO 33 I=1,ITAM
      LL(I)=LSTACK(I,ISTAPO-L)
  33  CONTINUE
      CL=CSTACK(ISTAPO-L)
      IF(SITRAK(20).EQ.1) PRINT 20,ISTAPO-L,L
  20  FORMAT(' STACKED AT ISTAPO-L = ',I5,' L = ',I5)
C
 999  END
