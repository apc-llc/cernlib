*
* $Id: ihmcx.F,v 1.1.1.1 1996/02/14 13:10:52 mclareni Exp $
*
* $Log: ihmcx.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:52  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/06 27/08/93  13.53.33  by  O.Couet
*-- Author :
      SUBROUTINE IHMCX(A00,A10,A11,A01,B00,B10,B11,B01,IREP)
************************************************************************
*                                                                      *
*     IHMCX                                      Date:    11.08.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Check for sursafe penetration ("bottle neck")          *
*                                                                      *
*     Input: A00,A10,A11,A01 - vertex values for 1st face              *
*            B00,B10,B11,B01 - vertex values for opposite face         *
*                                                                      *
*     Output: IREP - 1,2 - there is surface penetration                *
*                    0   - there is not surface penetration            *
*                                                                      *
************************************************************************
      IREP   = 0
      A      = (A11-A01)*(B00-B10) - (A00-A10)*(B11-B01)
      IF (A .EQ. 0.)                    GOTO 999
      B      = A01*(B00-B10)-(A11-A01)*B00-(A00-A10)*B01+A00*(B11-B01)
      C      = A00*B01 - A01*B00
      D      = B*B-4*A*C
      IF (D .LE. 0.)                    GOTO 999
      D      = SQRT(D)
      IF (ABS(-B+D) .GT. ABS(2*A))      GOTO 999
      S1     = (-B+D) / (2*A)
      IF (S1.LT.0. .OR. S1.GT.1.)       GOTO 999
      IF (ABS(-B-D) .GT. ABS(2*A))      GOTO 999
      S2     = (-B-D) / (2*A)
      IF (S2.LT.0. .OR. S2.GT.1.)       GOTO 999
*
**          C A S E   N O   4 ?
*
      IPOSA = 0
      IF (A00 .GE. 0)                   IPOSA = IPOSA + 1
      IF (A01 .GE. 0)                   IPOSA = IPOSA + 2
      IF (A10 .GE. 0)                   IPOSA = IPOSA + 4
      IF (A11 .GE. 0)                   IPOSA = IPOSA + 8
      IF (IPOSA.EQ.6 .OR. IPOSA.EQ.9)   GOTO 100
      IREP   = 1
      GOTO 999
*
**          N O T   C A S E   N O   4
*
  100 S0     = (A00-A01) / (A00+A11-A10-A01)
      IF (S1.GE.S0 .AND. S2.LT.S0)      GOTO 999
      IF (S1.LT.S0 .AND. S2.GE.S0)      GOTO 999
      IREP   = 1
      IF (S1 .GE. S0)                   IREP = 2
*
**          C A S E S   N O   10, 13 ?
*
      IPOSB = 0
      IF (B00 .GE. 0)                   IPOSB = IPOSB + 1
      IF (B01 .GE. 0)                   IPOSB = IPOSB + 2
      IF (B10 .GE. 0)                   IPOSB = IPOSB + 4
      IF (B11 .GE. 0)                   IPOSB = IPOSB + 8
      IF (IPOSB.NE.6 .AND. IPOSB.NE.9)  GOTO 999
      S0     = (B00-B01) / (B00+B11-B10-B01)
      IF (IPOSA .NE. IPOSB)             GOTO 200
**          C A S E   N O   10
      IF (IREP.EQ.1 .AND. S1.GT.S0)     GOTO 999
      IF (IREP.EQ.2 .AND. S1.LT.S0)     GOTO 999
      IREP   = 0
      GOTO 999
**          C A S E   N O   13
  200 IF (IREP.EQ.1 .AND. S1.LT.S0)     GOTO 999
      IF (IREP.EQ.2 .AND. S1.GT.S0)     GOTO 999
      IREP   = 0
      GOTO 999
*
  999 RETURN
      END
