*
* $Id: sickon.F,v 1.1.1.1 1995/12/12 14:36:15 mclareni Exp $
*
* $Log: sickon.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:15  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/00 26/10/93  11.59.38  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SICKON
C
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      DIMENSION DIM(10)
C
C
      CALL SITRAC(' SICKON  ')
C
C
      IPOINT=IPOINT+1
      A=XPROGR(IPOINT)
      IPOINT=IPOINT+1
      B=XPROGR(IPOINT)
      LENGTH=1
      NDIM=1
      LENGT=LENGTH
      DIM(1)=1.0
      DIM(2)=1.0
      DIM(3)=1.0
      MODE=2
      CALL SIGTT2(IDX,LENGT,NDIM,DIM)
      IF(IERRNO.NE.0)RETURN
      DYNA(IDX)=A
      DYNA(IDX+1)=B
C
 999  END
