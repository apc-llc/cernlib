*
* $Id: uinit.F,v 1.2 1996/02/01 14:54:53 ravndal Exp $
*
* $Log: uinit.F,v $
* Revision 1.2  1996/02/01 14:54:53  ravndal
* Clean up the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:12  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE UINIT
*
************************************************************************
*                                                                      *
*             To book the user's histograms                            *
*                                                                      *
************************************************************************
*
#include "geant321/gckine.inc"
#include "cdhscom.inc"
*
*     ------------------------------------------------------------------
*
      ELOW = PKINE(1) * .2
      EHIG = PKINE(1) * 20.
      CALL HBOOK1(101,'TOT ENERGY IN SINT$', 100, ELOW, EHIG, 0.)
      CALL HBOOK1(102,'NEPS PER PLANE$',96,0.5,16.5,0.)
      CALL HBOOK1(103,'NEPS FIRST PLANE EVENT$',96,0.5,16.5,0.)
      CALL HBOOK1(104,'PULSE HEIGTH PER PLANE$',100,0.,5.,0.)
*
      END
