*
* $Id: ihcspe.F,v 1.1.1.1 1996/02/14 13:10:48 mclareni Exp $
*
* $Log: ihcspe.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:48  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 14/01/92  14.38.06  by  O.Couet
*-- Author :
      SUBROUTINE IHCSPE(NL,FMIN,FMAX,IC,IDC,IREP)
************************************************************************
*                                                                      *
*     IHCSPE                                     Date:    09.01.92     *
*     Author: E.Chernyaev (IHEP/Protvino)        Revised:              *
*                                                                      *
*     Function: Set spectr                                             *
*                                                                      *
*     Input: NL   - number of levels                                   *
*            FMIN - MIN fuction value                                  *
*            FMAX - MAX fuction value                                  *
*            IC   - initial color index (for 1st level)                *
*            IDC  - color index increment                              *
*                                                                      *
*     Output: IREP - reply: 0 O.K.                                     *
*                          -1 error in parameters                      *
*                             F_max less than F_min                    *
*                             illegal number of levels                 *
*                             initial color index is negative          *
*                             color index increment must be positive   *
*                                                                      *
************************************************************************
#include "higz/hclevl.inc"
*-
      IREP   = 0
      IF (NL .EQ. 0) THEN
        NLEV   = 0
        GOTO 999
      END IF
*
**          C H E C K   P A R A M E T E R S
*
      IF (FMAX .LE. FMIN)               GOTO 995
      IF (NL.LT.0 .OR. NL.GT.LLEV)      GOTO 996
      IF (IC .LT. 0)                    GOTO 997
      IF (IDC .LT. 0)                   GOTO 998
*
**          S E T  S P E C T R
*
      DELF   = (FMAX-FMIN) / NL
      NLEV   =-(NL+1)
      DO 100 I=1,NL+1
        FUNLEV(I)  = FMIN + (I-1)*DELF
        ICLEV(I+1) = IC + (I-1)*IDC
  100 CONTINUE
      ICLEV(1) = ICLEV(2)
      ICLEV(NL+2) = ICLEV(NL+1)
      GOTO 999
*
  995 WRITE (*,*) 'IHCSPE: F_max less than F_min'
      IREP   =-1
      GOTO 999
  996 WRITE (*,*) 'IHCSPE: illegal number of levels'
      IREP   =-1
      GOTO 999
  997 WRITE (*,*) 'IHCSPE: initial color index is negative'
      IREP   =-1
      GOTO 999
  998 WRITE (*,*) 'IHCSPE: color index increment must be positive'
      IREP   =-1
      GOTO 999
  999 RETURN
      END
