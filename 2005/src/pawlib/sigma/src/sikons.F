*
* $Id: sikons.F,v 1.1.1.1 1995/12/12 14:36:18 mclareni Exp $
*
* $Log: sikons.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:18  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/00 26/10/93  11.59.38  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIKONS
C
C
C
C    ..................................................
C         PURPOSE
C            TO HANDLE NUMERICAL CONSTANTS
C
C         USAGE
C            CALL SIKONS
C
C         COMM. BLOCKS USED
C            COM1
C
C         REMARKS
C            NOTE THAT TERE ARE TWO ADDITIONAL ENTRY POINTS.
C             - SINEGK  FOR SHORT NEGATIVE CONSTANTS
C             - SIPOSK  FOR SHORT POSITIVE CONSTANTS
C             NEXT CONTAINS POINTER TO DYNA,
C             WHERE THE VALUE IS STORED
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C            MESSI
C            GETT2
C
C...  PAW VERSION  ...  MAY 1988
C
C    ..................................................
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      DIMENSION DIM(10)
C
      CALL SITRAC (' SIKONS   ')
C
      IPOINT=IPOINT+1
      A=XPROGR(IPOINT)
    1 CONTINUE
      LENGTH=1
      NDIM=1
C*************************    CHECK IF NDIM NEEDED********
      LENGT=LENGTH
      MODE=1
      DIM(1)=1.0
      DIM(2)=1.0
      DIM(3)=1.0
      CALL SIGTT2(IDX,LENGT,NDIM,DIM)
      IF(IERRNO.NE.0)RETURN
      DYNA(IDX)=A
      RETURN
C
C
      ENTRY SIPOSK
C
C
      CALL SITRAC (' SIPOSK  ')
      CALL SINEXT(NA)
      A=NA
      GO TO 1
C
C
      ENTRY SINEGK
C
C
      CALL SITRAC (' SINEGK  ')
      CALL SINEXT(NA)
      A=-NA
      GO TO 1
      END
