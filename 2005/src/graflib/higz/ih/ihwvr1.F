*
* $Id: ihwvr1.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihwvr1.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/00 20/04/93  14.20.34  by  O.Couet
*-- Author :
      SUBROUTINE IHWVR1(IOPT,PHI1,PHI2,VAL)
************************************************************************
*                                                                      *
*     IHWVR1                                     Date:    20.04.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Encode side visibilities and order along R for sector  *
*                                                                      *
*     References: IHWZN                                                *
*                                                                      *
*     Input: IOPT - options: 1 - from BACK to FRONT 'BF'               *
*                            2 - from FRONT to BACK 'FB'               *
*            PHI1 - 1st phi of sector                                  *
*            PHI2 - 2nd phi of sector                                  *
*                                                                      *
*     Output: VAL - encoded value                                      *
*                                                                      *
************************************************************************
      EQUIVALENCE       (Q,K)
*-
      K      = 0
      CALL IHWZN(0.,0.,1.,ZN)
      IF (ZN .GT. 0.)                           K = K + 64
      IF (ZN .LT. 0.)                           K = K + 32
      CALL IHWZN(-SIN(PHI2),COS(PHI2),0.,ZN)
      IF (ZN .GT. 0.)                           K = K + 16
      CALL IHWZN(SIN(PHI1),-COS(PHI1),0.,ZN)
      IF (ZN .GT. 0.)                           K = K + 4
      PHI    = (PHI1 + PHI2) / 2.
      CALL IHWZN(COS(PHI),SIN(PHI),0.,ZN)
      IF (ZN .GT. 0.)                           K = K + 8
      IF (ZN .LT. 0.)                           K = K + 2
      IF ((ZN.LE.0. .AND. IOPT.EQ.1) .OR.
     &    (ZN.GT.0. .AND. IOPT.EQ.2))           K = K + 1
      VAL    = Q
      END
