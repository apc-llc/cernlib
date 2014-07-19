*
* $Id: sistri.F,v 1.1.1.1 1995/12/12 14:36:20 mclareni Exp $
*
* $Log: sistri.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.20  by  Carlo E. Vandoni
*-- Author :
*-- Author :
      SUBROUTINE SISTRI
C
C
C
C    ....................................................
C
C
C         PURPOSE
C            STRING HANDLING
C
C
C...  PAW VERSION  ...  July-August 1991
C
C    ......................................................
C
C
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
         DIMENSION DIM(10)
C
C
      CALL SITRAC (' SISTRI   ')
C
      CALL SINEXT(N)
C 9july
      LENGTH=N
      NDIM=1
      LENGT=LENGTH
      MODE=1
      DIM(1)=N
      DIM(2)=1.0
      DIM(3)=1.0
      CALL SIGTT2(IDX,LENGT,NDIM,DIM)
      IF(IERRNO.NE.0)RETURN
      DO 1 J=1,N
      CALL SINEXT (IST)
      IDYNA(IDX)=IST
c 9 july
      IDX=IDX+1
 1    CONTINUE
C
C
 999  END
