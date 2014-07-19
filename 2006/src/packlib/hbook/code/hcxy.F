*
* $Id: hcxy.F,v 1.1.1.1 1996/01/16 17:07:34 mclareni Exp $
*
* $Log: hcxy.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:34  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.33.53  by  Julian Bunn
*-- Author :
      FUNCTION HCXY(ICX,ICY,IOPT)
*.==========>
*.           RETURNS EITHER THE CONTENTS OR ERROR ON 2D CELL ICX,IXY
*.
*. IOPT = 1   RETURN CONTENTS
*. IOPT = 2   RETURN ERROR (COMPUTED OR PACKED)
*.
*. Modified 20/2/95  JJB Add argument IOPT
*.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
#include "hbook/jbyt.inc"
C
C FIRST GET CONTENTS
C
      NW=32/NB
      J=(IQ(LCID+KNCY)-ICY+1)*(IQ(LCID+KNCX)+2)
      L2=ICX+J
      L1=L2/NW+LSCAT+KCON2
      IF(NW.NE.1)THEN
         L2=(NW-1-MOD(L2,NW))*NB +1
         HCXY=JBYT(IQ(L1),L2,NB)
      ELSE
         HCXY=Q(L1)
      ENDIF
C
C IF IOPT=2, THEN GET ERROR
C
      IF(IOPT.EQ.2) THEN
         LW = LQ(LCONT)
         IF(LW.NE.0) THEN
C
C ERROR IS PACKED IN HISTOGRAM
C
            NCX = IQ(LCID+KNCX)
            IOFF = (ICY-1)*NCX + ICX
            HCXY = SQRT(Q(LW+IOFF))
         ELSE
C
C ERROR MUST BE CALCULATED AS SQRT CONTENTS
C
            HCXY = SQRT(ABS(HCXY))
         ENDIF
      ENDIF
      END
