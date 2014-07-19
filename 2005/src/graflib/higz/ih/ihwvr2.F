*
* $Id: ihwvr2.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihwvr2.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/00 02/04/93  11.28.07  by  O.Couet
*-- Author :
      SUBROUTINE IHWVR2(VAL,IV1,IV2,IV3,IV4,IV5,IV6,IR)
************************************************************************
*                                                                      *
*     IHWVR2                                     Date:    31.03.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Decode side visibilities and order along R for sector  *
*                                                                      *
*     Input: VAL - encoded value                                       *
*                                                                      *
*     Output: IV1 ... IV6  - visibility of the sides                   *
*             IR           - increment along R                         *
*                                                                      *
************************************************************************
      EQUIVALENCE       (Q,K)
      INTEGER           IVIS(6)
*-
      Q      = VAL
      NUM    = 128
      DO 100 I=1,6
        IVIS(I) = 0
        NUM     = NUM / 2
        IF (K .LT. NUM)         GOTO 100
        K       = K - NUM
        IVIS(I) = 1
  100 CONTINUE
      IR     = 1
      IF (K .EQ. 1)             IR  = -1
      IV1    = IVIS(6)
      IV2    = IVIS(5)
      IV3    = IVIS(4)
      IV4    = IVIS(3)
      IV5    = IVIS(2)
      IV6    = IVIS(1)
      END
