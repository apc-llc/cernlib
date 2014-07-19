*
* $Id: igcyl.F,v 1.1.1.1 1996/02/14 13:10:42 mclareni Exp $
*
* $Log: igcyl.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:42  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 19/12/95  17.23.24  by  O.Couet
*-- Author :
      SUBROUTINE IGCYL(LIGHT,T,H,R,NSTEP)
************************************************************************
*                                                                      *
* Name: IGCYL                                      Date:    10.12.95   *
* Author: E.Chernyaev (IHEP/Protvino)              Revised:            *
*                                                                      *
* Function: Draw cylinder                                              *
*                                                                      *
************************************************************************
      REAL T(4,3)
      CALL IGCONE(LIGHT,T,H,R,R,NSTEP)
      END
