*
* $Id: ihprop.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihprop.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 14/01/92  14.01.24  by  O.Couet
*-- Author :
      SUBROUTINE IHPROP(QQA,QQD,QQS,NNQS,IREP)
************************************************************************
*                                                                      *
*     IHPROP                                     Date:    11.10.91     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Set surface property coefficients                      *
*                                                                      *
*     Input: QQA  - diffusion coefficient for diffused light  [0.,1.]  *
*            QQD  - diffusion coefficient for direct light    [0.,1.]  *
*            QQS  - diffusion coefficient for reflected light [0.,1.]  *
*            NNCS - power coefficient for reflected light     (.GE.1)  *
*                                                                      *
*                                          --                          *
*     Lightness model formula: Y = YD*QA + > YLi*(QD*cosNi+QS*cosRi)   *
*                                          --                          *
*                                                                      *
*     Output: IREP   - reply : 0 - O.K.                                *
*                             -1 - error in cooefficients              *
*                                                                      *
************************************************************************
#include "higz/hcligh.inc"
*-
      IREP   = 0
      IF (QQA.LT.0. .OR. QQA.GT.1.)             GOTO 998
      IF (QQA.LT.0. .OR. QQA.GT.1.)             GOTO 998
      IF (QQA.LT.0. .OR. QQA.GT.1.)             GOTO 998
      IF (NNQS .LT. 1)                          GOTO 998
      QA     = QQA
      QD     = QQD
      QS     = QQS
      NQS    = NNQS
      GOTO 999
*
  998 WRITE(*,*) 'IHPROP: error in coefficients'
      IREP   =-1
  999 RETURN
      END
