*
* $Id: gufld.F,v 1.1.1.1 1995/10/24 10:22:13 cernlib Exp $
*
* $Log: gufld.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:13  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUFLD(X,F)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       User routine to compute the magnetic field F             *
C.    *       at space point X                                         *
C.    *       Called by GRkuta,GHELIX                                  *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
      COMMON/DLSFLD/ISWFLD,FLDVAL
C
      DIMENSION X(3),F(3)
C.
C.    ------------------------------------------------------------------
C.
      F(1)=0.
      F(2)=FLDVAL
      F(3)=0.
C
      RETURN
      END
