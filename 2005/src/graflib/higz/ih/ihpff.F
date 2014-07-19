*
* $Id: ihpff.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihpff.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 14/01/92  14.01.24  by  O.Couet
*-- Author :
      SUBROUTINE IHPFF(N,P,F)
************************************************************************
*                                                                      *
*     IHPFF                                      Date:    09.01.92     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Fill polygon with fuction values at vertexes           *
*                                                                      *
*     References: IHERAN                                               *
*                                                                      *
*     Input: N      - number of vertexes                               *
*            P(3,*) - polygon                                          *
*            F(*)   - function values at nodes                         *
*                                                                      *
*     Errors: - illegal number of vertexes in polygon                  *
*             - illegal call of IHPFF: no levels                       *
*                                                                      *
************************************************************************
#include "higz/hclevl.inc"
      REAL              P(3,*),F(*),X(12),Y(12),P3(3,12)
*-
      IF (N .LT. 3)             GOTO 997
      IF (NLEV .EQ. 0)          GOTO 998
      NP     = N
      NL     = NLEV
      IF (NL .LT. 0)            NL =-NL
      FMIN   = F(1)
      FMAX   = F(1)
      DO 100 I=2,NP
        IF (FMIN .GT. F(I))     FMIN = F(I)
        IF (FMAX .LT. F(I))     FMAX = F(I)
  100 CONTINUE
      FUNMIN = FUNLEV(1) - 1.
      IF (FMIN .LT. FUNMIN)     FUNMIN = FMIN - 1.
      FUNMAX = FUNLEV(NL) + 1.
      IF (FMAX .GT. FUNMAX)     FUNMAX = FMAX + 1.
*
**          F I N D   A N D   D R A W   S U B P O L Y G O N S
*
      F2     = FUNMIN
      DO 300 ILEV=1,NL+1
*           S E T   L E V E L   L I M I T S
        F1     = F2
        IF (ILEV .EQ. NL+1) THEN
          F2     = FUNMAX
        ELSE
          F2     = FUNLEV(ILEV)
        END IF
        IF (FMAX .LT. F1)       GOTO 999
        IF (FMIN .GT. F2)       GOTO 300
*           F I N D   S U B P O L Y G O N
        K      = 0
        DO 210 I=1,NP
          I1 = I
          I2 = I + 1
          IF (I .EQ. NP)          I2 = 1
          CALL IHERAN(P(1,I1),P(1,I2),F(I1),F(I2),F1,F2,K,P3)
  210   CONTINUE
*           D R A W   S U B P O L Y G O N
        IF (K .LT. 3)           GOTO 300
        DO 220 I=1,K
          X(I) = P3(1,I)
          Y(I) = P3(2,I)
  220   CONTINUE
        ICOL   = ICLEV(ILEV)
        CALL ISFACI(ICOL)
        CALL ISFAIS(1)
        CALL IFA(K,X,Y)
  300 CONTINUE
      GOTO 999
*
  997 WRITE (*,*) 'IHPFF: illegal number of vertexes in polygon'
      GOTO 999
  998 WRITE (*,*) 'IHPFF: illegal call of IHPFF: no levels'
      GOTO 999
  999 RETURN
      END
