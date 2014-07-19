*
* $Id: uinit.F,v 1.1.1.1 1995/10/24 10:22:17 cernlib Exp $
*
* $Log: uinit.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:17  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UHINIT
*
************************************************************************
*                                                                      *
*             To book the user's histograms                            *
*                                                                      *
************************************************************************
*
*
#include "geant321/gckine.inc"
*
*     ------------------------------------------------------------------
*
      ELOW = 0.
      EHIG = 1.2*PKINE(1)
      CALL HBOOK1(101,'TOT ENERGY IN D1$', 100, ELOW, EHIG, 0.)
      CALL HBOOK1(102,'TOT ENERGY IN D2$', 100, ELOW, EHIG, 0.)
      CALL HBOOK1(103,'TOT ENERGY $', 100, ELOW, EHIG, 0.)
      CALL HBOOK2(104,'ENERGY IN D1 VS IN D2$',
     + 40,ELOW,EHIG,40,ELOW,EHIG,10000.)
*
      END
