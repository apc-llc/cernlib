*
* $Id: ihdf02.F,v 1.1.1.1 1996/02/14 13:10:49 mclareni Exp $
*
* $Log: ihdf02.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:49  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 27/01/92  16.47.04  by  O.Couet
*-- Author :
      SUBROUTINE IHDF02(ICODES,XYZ,NP,IFACE,T)
************************************************************************
*                                                                      *
*     IHDF02                                     Date:    17.09.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 10.01.92     *
*                                                                      *
*     Function: Draw face - 2nd option                                 *
*               (fill in correspondance with function levels)          *
*                                                                      *
*     References: IHWTON, IHPFF                                        *
*                                                                      *
*     Input: ICODES(*) - set of codes for the line (not used)          *
*              ICODES(1) - IX                                          *
*              ICODES(2) - IY                                          *
*            XYZ(3,*)  - coordinates of nodes                          *
*            NP        - number of nodes                               *
*            IFACE(NP) - face                                          *
*            T(NP)     - additional function defined on this face      *
*                                                                      *
************************************************************************
#include "higz/hihid.inc"
      REAL      XYZ(3,*),P3(3,12),X(12),Y(12),T(*)
      INTEGER   ICODES(*),IFACE(*)
*-
**          T R A N S F E R   T O   N O R M A L I S E D   COORDINATES
*
      DO 100 I=1,NP
        K       = IFACE(I)
        CALL IHWTON(XYZ(1,K),P3(1,I))
        X(I)   = P3(1,I)
        Y(I)   = P3(2,I)
  100 CONTINUE
*
**          D R A W   F A C E   &   B O R D E R
*
      CALL IHPFF(NP,P3,T)
      IF(IDRGR.EQ.1)THEN
        CALL ISFACI(1)
        CALL ISFAIS(0)
        CALL IFA(NP,X,Y)
      ENDIF
      END
