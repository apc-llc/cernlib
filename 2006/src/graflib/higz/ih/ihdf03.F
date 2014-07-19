*
* $Id: ihdf03.F,v 1.1.1.1 1996/02/14 13:10:49 mclareni Exp $
*
* $Log: ihdf03.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:49  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 14/01/92  14.01.24  by  O.Couet
*-- Author :
      SUBROUTINE IHDF03(ICODES,XYZ,NP,IFACE,T)
************************************************************************
*                                                                      *
*     IHDF03                                     Date:    17.09.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 12.01.92     *
*                                                                      *
*     Function: Draw face - 3rd option                                 *
*               (draw face for stacked lego plot)                      *
*                                                                      *
*     References: IHWTON                                               *
*                                                                      *
*     Input: ICODES(*) - set of codes for the line                     *
*              ICODES(1) - IX coordinate of the line cell              *
*              ICODES(2) - IY coordinate of the line cell              *
*              ICODES(3) - lego number                                 *
*              ICODES(4) - side: 1-face,2-right,3-back,4-left,         *
*                                5-bottom, 6-top                       *
*              XYZ(3,*)  - coordinates of nodes                        *
*              NP        - number of nodes                             *
*              IFACE(NP) - face                                        *
*              T(*)      - additional function (not used here)         *
*                                                                      *
*                                                                      *
************************************************************************
#include "higz/hihid.inc"
      REAL      XYZ(3,*),X(4),Y(4),P3(3,4),T(*)
      INTEGER   ICODES(4),IFACE(*)
*-
      IF (ICODES(4).EQ.6) ICOL = ISTCOT
      IF (ICODES(4).EQ.5) ICOL = ISTCOB
      IF (ICODES(4).EQ.1) ICOL = ISTCOL(ICODES(3))
      IF (ICODES(4).EQ.2) ICOL = ISTCO2(ICODES(3))
      IF (ICODES(4).EQ.3) ICOL = ISTCOL(ICODES(3))
      IF (ICODES(4).EQ.4) ICOL = ISTCO2(ICODES(3))
      DO 100 I=1,NP
        K      = IFACE(I)
        CALL IHWTON(XYZ(1,K),P3(1,I))
        X(I)   = P3(1,I)
        Y(I)   = P3(2,I)
  100 CONTINUE
*
      CALL ISFAIS(1)
      CALL ISFACI(ICOL)
      CALL IFA(NP,X,Y)
      IF(IDRGR.NE.0)THEN
         CALL ISLN(1)
         CALL ISFAIS(0)
         CALL ISFACI(1)
         CALL IFA(NP,X,Y)
      ENDIF
      END
