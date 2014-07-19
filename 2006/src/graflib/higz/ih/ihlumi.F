*
* $Id: ihlumi.F,v 1.3 1996/09/25 15:02:14 couet Exp $
*
* $Log: ihlumi.F,v $
* Revision 1.3  1996/09/25 15:02:14  couet
* *** empty log message ***
*
* Revision 1.2  1996/09/25 14:58:22  couet
* - Protection added to prevent division by 0
*
* Revision 1.1.1.1  1996/02/14 13:10:50  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 17/01/92  11.37.23  by  O.Couet
*-- Author :
      SUBROUTINE IHLUMI(ANORM,FLUM)
************************************************************************
*                                                                      *
*     IHLUMI                                     Date:    11.10.91     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Find surface luminosity at given point                 *
*                                                                      *
*                                          --                          *
*     Lightness model formula: Y = YD*QA + > YLi*(QD*cosNi+QS*cosRi)   *
*                                          --                          *
*                                                                      *
*             B1     = VN(3)*VL(2) - VN(2)*VL(3)                       *
*             B2     = VN(1)*VL(3) - VN(3)*VL(1)                       *
*             B3     = VN(2)*VL(1) - VN(1)*VL(2)                       *
*             B4     = VN(1)*VL(1) + VN(2)*VL(2) + VN(3)*VL(3)         *
*             VR(1)  = VN(3)*B2 - VN(2)*B3 + VN(1)*B4                  *
*             VR(2)  =-VN(3)*B1 + VN(1)*B3 + VN(2)*B4                  *
*             VR(3)  = VN(2)*B1 - VN(1)*B2 + VN(3)*B4                  *
*             S      = SQRT(VR(1)*VR(1)+VR(2)*VR(2)+VR(3)*VR(3))       *
*             VR(1)  = VR(1)/S                                         *
*             VR(2)  = VR(2)/S                                         *
*             VR(3)  = VR(3)/S                                         *
*             COSR   = VR(1)*0. + VR(2)*0. + VR(3)*1.                  *
*                                                                      *
*     References: IHWTON                                               *
*                                                                      *
*     Input: ANORM(3) - surface normal at given point                  *
*                                                                      *
*     Output: FLUM - luminosity                                        *
*                                                                      *
************************************************************************
#include "higz/hcligh.inc"
      REAL              ANORM(3),VN(3),VL(3)
*-
      FLUM   = 0.
      IF (LOFF .NE. 0) RETURN
*
**          T R A N S F E R   N O R M A L  T O   SCREEN COORDINATES
*
      CALL IHWWNN(ANORM,VN)
      S      = SQRT(VN(1)*VN(1)+VN(2)*VN(2)+VN(3)*VN(3))
      IF (VN(3) .LT. 0.)                S =-S
      IF (S.NE.0.) THEN
         VN(1)  = VN(1)/S
         VN(2)  = VN(2)/S
         VN(3)  = VN(3)/S
      ELSE
         RETURN
      ENDIF
*
**          F I N D   L U M I N O S I T Y
*
      FLUM   = YDL*QA
      DO 100 I=1,LLIGHT
        IF (YLS(I) .LE. 0.)             GOTO 100
        VL(1)  = VLS(1,I)
        VL(2)  = VLS(2,I)
        VL(3)  = VLS(3,I)
        COSN   = VL(1)*VN(1) + VL(2)*VN(2) + VL(3)*VN(3)
        IF (COSN .LT. 0.)               GOTO 100
        COSR   = VN(2)*(VN(3)*VL(2)-VN(2)*VL(3))
     &          -VN(1)*(VN(1)*VL(3)-VN(3)*VL(1)) + VN(3)*COSN
        IF (COSR .LE. 0.)               COSR = 0.
        FLUM   = FLUM + YLS(I)*(QD*COSN + QS*COSR**NQS)
  100 CONTINUE
*
      END
