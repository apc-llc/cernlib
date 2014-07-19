*
* $Id: ihrini.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihrini.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/06 27/05/93  15.47.54  by  O.Couet
*-- Author :
      SUBROUTINE IHRINI(XMIN,YMIN,XMAX,YMAX,NX,NY,IBUF)
************************************************************************
*                                                                      *
*     IHRINI                                     Date:    14.05.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Initialise hidden lines removal algorithm              *
*               "RASTER SCREEN"                                        *
*                                                                      *
*     References: IHRCLE                                               *
*                                                                      *
*     Input: XMIN - Xmin in the normalized coordinate system           *
*            YMIN - Ymin in the normalized coordinate system           *
*            XMAX - Xmax in the normalized coordinate system           *
*            YMAX - Ymax in the normalized coordinate system           *
*            NX   - number of pixels along X                           *
*            NY   - number of pixels along Y                           *
*            IBUF(NX*NY/4) - buffer for raster                         *
*                                                                      *
************************************************************************
#include "higz/hcrast.inc"
      INTEGER           IBUF(*)
*-
      NXRAST = NX
      NYRAST = NY
      XRAST  = XMIN
      DXRAST = XMAX - XMIN
      YRAST  = YMIN
      DYRAST = YMAX - YMIN
      JRAST  = LOCF(IBUF(1)) - LOCF(IRAST(1))
*
**          S E T   M A S K S
*
      K      = 0
      DO 100 I=1,NBW
        JMASK(I) = K
        K        = K + NBW - I + 1
        MASK(I)  = 2**(I-1)
  100 CONTINUE
      J      = NBW
      DO 130 NB=2,NBW
        DO 120 IB=1,NBW-NB+1
          K      = 0
          DO 110 I=IB,IB+NB-1
            K      = IOR(K,MASK(I))
  110     CONTINUE
          J       = J + 1
          MASK(J) = K
  120   CONTINUE
  130 CONTINUE
*
**          C L E A R   R A S T E R   S C R E E N
*
      CALL IHRCLE()
      RETURN
      END
