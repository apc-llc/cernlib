*
* $Id: ihclev.F,v 1.1.1.1 1996/02/14 13:10:48 mclareni Exp $
*
* $Log: ihclev.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:48  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 15/01/92  15.34.56  by  O.Couet
*-- Author :
      SUBROUTINE IHCLEV(NL,FL,ICL,IREP)
************************************************************************
*                                                                      *
*     IHCLEV                                     Date:    09.01.92     *
*     Author: E.Chernyaev (IHEP/Protvino)        Revised:              *
*                                                                      *
*     Function: Set correspondance between function and color levels   *
*                                                                      *
*     Input: NL        - number of levels                              *
*            FL(NL)    - function levels                               *
*            ICL(NL+1) - colors for levels                             *
*                                                                      *
*     Output: IREP     - reply: 0 O.K.                                 *
*                              -1 error in parameters:                 *
*                          illegal number of levels                    *
*                          function levels must be in increasing order *
*                          negative color index                        *
*                                                                      *
************************************************************************
#include "higz/hclevl.inc"
      REAL              FL(*)
      INTEGER           ICL(*)
*-
      IREP   = 0
      IF (NL .EQ. 0) THEN
        NLEV   = 0
        GOTO 999
      END IF
*
**          C H E C K   P A R A M E T E R S
*
      IF (NL.LT.0 .OR. NL.GT.LLEV)      GOTO 996
      DO 110 I=2,NL
        IF (FL(I) .LE. FL(I-1))         GOTO 997
  110 CONTINUE
      DO 120 I=1,NL+1
        IF (ICL(I) .LT. 0)              GOTO 998
  120 CONTINUE
*
**          S E T   L E V E L S
*
      NLEV   = NL
      DO 210 I=1,NLEV
        FUNLEV(I) = FL(I)
  210 CONTINUE
      DO 220 I=1,NLEV+1
        ICLEV(I) = ICL(I)
  220 CONTINUE
      GOTO 999
*
  996 WRITE(*,*) 'IHCLEV: illegal number of levels'
      IREP   =-1
      GOTO 999
  997 WRITE(*,*) 'IHCLEV: function levels must be in increasing order'
      IREP   =-1
      GOTO 999
  998 WRITE(*,*) 'IHCLEV: negative color index'
      IREP   =-1
      GOTO 999
  999 RETURN
      END
