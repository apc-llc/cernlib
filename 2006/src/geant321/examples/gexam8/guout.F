*
* $Id: guout.F,v 1.2 1996/02/22 13:26:57 ravndal Exp $
*
* $Log: guout.F,v $
* Revision 1.2  1996/02/22 13:26:57  ravndal
* Cleaning up CARTOCVS interface
*
* Revision 1.1.1.1  1995/10/24 10:22:23  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUOUT
#include "pvolum.inc"
      SETOT=ETOT(ITR)
      SESEEN=ESEEN(ITR)
      CALL HF1(10+ITR,SETOT,1.)
      CALL HF1(20+ITR,SESEEN,1.)
      END
