*
* $Id: sisyso.F,v 1.1.1.1 1995/12/12 14:36:20 mclareni Exp $
*
* $Log: sisyso.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.20  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SISYSO
C
C
C
C    ....................................................
C
C
C         PURPOSE
C            PROCESS SIGMAOPERATORS SYSFUN CODE 600
C
C         USAGE
C            CALL SISYSO
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: KLASS
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            BESCJ
C            SICONT
C            SIDIAG
C            DROP
C            EIGEN
C            EVAL
C            SUMN
C            SINVDE
C            MULT
C            ORDER
C            PROJ
C            QUAD
C            TP
C            TRACE
C            TRACOP
C            WHIT
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
C
C
C
#include "sigma/sigc.inc"
C
C
      CALL SITRAC(' SISYSO   ')
C
C???      IF (KLASS.LT.601.OR.KLASS.GT.617) CALL ERR0(?)
      KLA=KLASS-600
      GO TO (1,1,3,4,5,6,7,8,8,10,11,12,13,14,15,16,17) KLA
C
  1   CALL SINVDE
      GO TO 999
C
  3   CALL SIMULT
      GO TO 999
C
  4   CALL SIGMTP
      GO TO 999
C
  5   CONTINUE
C     CALL SITRCP
      GO TO 999
C
  6   CONTINUE
CEV   CALL SIDROP
      GO TO 999
C
  7   CALL SIDIAG
      GO TO 999
C
  8   CONTINUE
C     CALL SIEIGE
      GO TO 999
C
  10  CONTINUE
CEV   CALL SIPROJ
      GO TO 999
C
  11  CALL SIORDE
      GO TO 999
C
  12  CALL SISUMN
      GO TO 999
C
  13  CALL SIQUAD
      GO TO 999
C
  14  CALL SIEVAL
      GO TO 999
C
  15  CONTINUE
C   DUMMY       CALL WHIT
      GO TO 999
C
  16  CONTINUE
C
C
C     DUMMY CALL
C     CALL BESCJ
C
      GO TO 999
C
  17  CONTINUE
*     CALL SICONT
 999  END
