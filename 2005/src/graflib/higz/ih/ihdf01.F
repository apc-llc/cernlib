*
* $Id: ihdf01.F,v 1.1.1.1 1996/02/14 13:10:49 mclareni Exp $
*
* $Log: ihdf01.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:49  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.38.31  by  O.Couet
*-- Author :
      SUBROUTINE IHDF01(ICODES,XYZ,NP,IFACE,T)
************************************************************************
*                                                                      *
*     IHDF01                                     Date:    17.09.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 10.01.92     *
*                                                         15.08.93     *
*     Function: Draw face - 1st variant                                *
*               (2 colors: 1st for external surface, 2nd for internal) *
*                                                                      *
*     References: IHWTON                                               *
*                                                                      *
*     Input: ICODES(*) - set of codes for the line (not used)          *
*              ICODES(1) - IX                                          *
*              ICODES(2) - IY                                          *
*            XYZ(3,*)  - coordinates of nodes                          *
*            NP        - number of nodes                               *
*            IFACE(NP) - face                                          *
*            T(NP)     - additional function defined on this face      *
*                        (not used in this routine)                    *
*                                                                      *
************************************************************************
      REAL      XYZ(3,*),P3(2,12),X(13),Y(13),T(*)
      INTEGER   ICODES(*),IFACE(*)
*-
**          T R A N S F E R   T O   N O R M A L I S E D   COORDINATES
*
      IFNEG  = 0
      DO 100 I=1,NP
        K      = IFACE(I)
        IF (K .LT. 0)   IFNEG = 1
        IF (K .LT. 0)   K =-K
        CALL IHWTON(XYZ(1,K),P3(1,I))
        X(I)   = P3(1,I)
        Y(I)   = P3(2,I)
  100   CONTINUE
*
**          F I N D   N O R M A L
*
      Z      = 0.
      DO 200 I=1,NP
        I1 = I
        I2 = I1 + 1
        IF (I2 .GT. NP)  I2 = 1
        Z      =  Z + P3(2,I1)*P3(1,I2) - P3(1,I1)*P3(2,I2)
  200   CONTINUE
*
**          D R A W   F A C E
*
      IF (Z .GT. 0) CALL ISFACI(2)
      IF (Z .LE. 0) CALL ISFACI(3)
      CALL ISFAIS(1)
      CALL IFA(NP,X,Y)
*
**          D R A W   B O R D E R
*
      IF (IFNEG .EQ. 0) THEN
        CALL ISFAIS(0)
        CALL ISFACI(1)
        CALL IFA(NP,X,Y)
      ELSE
        X(NP+1) = X(1)
        Y(NP+1) = Y(1)
        CALL ISPLCI(1)
        DO 300 I=1,NP
          IF (IFACE(I) .GT. 0) CALL IPL(2,X(I),Y(I))
  300   CONTINUE
      END IF
      END
