*
* $Id: ihwlin.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihwlin.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/00 19/02/92  10.16.08  by  O.Couet
*-- Author :
      SUBROUTINE IHWLIN(NP,F,T)
************************************************************************
*                                                                      *
*     IHWLIN                                     Date:    10.01.92     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Find level lines for face                              *
*                                                                      *
*     Input: NP      - number of nodes                                 *
*            F(3,NP) - face                                            *
*            T(NP)   - additional function                             *
*                                                                      *
*     Error: number of points for line .NE. 2                          *
*                                                                      *
************************************************************************
#include "higz/hclevl.inc"
#include "higz/hcline.inc"
      REAL              F(3,*),T(*)
*-
      NLINES = 0
      IF (NLEV .EQ. 0)                  GOTO 999
      NL     = NLEV
      IF (NL .LT. 0)                    NL =-NL
*           F I N D   Tmin   A N D   Tmax
      TMIN   = T(1)
      TMAX   = T(1)
      DO 100 I=2,NP
        IF (T(I) .LT. TMIN)             TMIN = T(I)
        IF (T(I) .GT. TMAX)             TMAX = T(I)
  100   CONTINUE
      IF (TMIN .GE. FUNLEV(NL))         GOTO 999
      IF (TMAX .LE. FUNLEV(1))          GOTO 999
*
**          F I N D   L E V E L S   L I N E S
*
      DO 340 IL=1,NL
        IF (TMIN .GE. FUNLEV(IL))       GOTO 340
        IF (TMAX .LE. FUNLEV(IL))       GOTO 999
        IF (NLINES .GE. LLINES)         GOTO 999
        NLINES   = NLINES + 1
        ILLEVL(NLINES) = IL
        K    = 0
        DO 330 I=1,NP
          I1     = I
          I2     = I + 1
          IF (I .EQ. NP)                I2 = 1
          D1     = T(I1) - FUNLEV(IL)
          D2     = T(I2) - FUNLEV(IL)
          IF (D1 .EQ. 0.)               GOTO 310
          IF (D1*D2 .LT. 0.)            GOTO 320
          GOTO 330
  310     K      = K + 1
          PLINES(1,K,NLINES) = F(1,I1)
          PLINES(2,K,NLINES) = F(2,I1)
          PLINES(3,K,NLINES) = F(3,I1)
          IF (K .EQ. 1)                 GOTO 330
          GOTO 340
  320     K      = K + 1
          D1     = D1 / (T(I2)-T(I1))
          D2     = D2 / (T(I2)-T(I1))
          PLINES(1,K,NLINES) = D2*F(1,I1) - D1*F(1,I2)
          PLINES(2,K,NLINES) = D2*F(2,I1) - D1*F(2,I2)
          PLINES(3,K,NLINES) = D2*F(3,I1) - D1*F(3,I2)
          IF (K .EQ. 1)                 GOTO 330
          GOTO 340
  330   CONTINUE
        IF (K .NE. 2) THEN
          WRITE(*,*) 'IHWLIN: number of points for line .NE. 2'
          NLINES = NLINES - 1
        END IF
  340 CONTINUE
*
  999 RETURN
      END
