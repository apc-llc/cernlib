*
* $Id: ighcxy.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: ighcxy.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/09 09/03/95  15.49.40  by  O.Couet
*-- Author :
      REAL FUNCTION IGHCXY(LSCAT,ICX,ICY,IOPT)
*.==========>
*.
*.   Returns the value of cell defined by ICX and ICY if IOPT=1. Returns
*. the error associated with the cell defined by ICX and ICY if IOPT=2.
*.
*..=========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hipack.inc"
      PARAMETER(KNBIT=1,KCON2=3,KCON1=9)
*.___________________________________________
*
      NB = IQ(LSCAT+KNBIT)
      IF (NB.LE.0) NB = 32
      NW = 32/NB
*
      IF (IDIM1.NE.0) THEN
*
*              1D histos
*
         IF (NW.NE.1) THEN
            L1     = ICX*NB
            NBITH  = 32-MOD(32,NB)
            L2     = MOD(L1,NBITH)+1
            L1     = LSCAT+KCON1+L1/NBITH
            IGHCXY = JBYT(IQ(L1),L2,NB)
         ELSE
            IGHCXY = Q(LSCAT+KCON1+ICX)
         ENDIF
*
* If IOPT=2, then get error
*
         IF(IOPT.EQ.2) THEN
            LW = LQ(LSCAT)
            IF (LW.NE.0) THEN
*
* Error is packed in histogram
*
               IGHCXY = SQRT(Q(LW+ICX))
            ELSE
*
* Error must be calculated as SQRT contents
*
               IGHCXY = SQRT(ABS(IGHCXY))
            ENDIF
         ENDIF
      ELSE
*
*              2D histos
*
* First get contents
*
         J  = (NCY-ICY+1)*(NCX+2)
         L2 = ICX+J
         L1 = L2/NW+LSCAT+KCON2
         IF (NW.NE.1) THEN
            L2     = (NW-1-MOD(L2,NW))*NB +1
            IGHCXY = JBYT(IQ(L1),L2,NB)
         ELSE
            IGHCXY = Q(L1)
         ENDIF
*
* If IOPT=2, then get error
*
         IF(IOPT.EQ.2) THEN
            LW = LQ(LSCAT)
            IF (LW.NE.0) THEN
*
* Error is packed in histogram
*
               IOFF = (ICY-1)*NCX+ICX
               IGHCXY = SQRT(Q(LW+IOFF))
            ELSE
*
* Error must be calculated as SQRT contents
*
               IGHCXY = SQRT(ABS(IGHCXY))
            ENDIF
         ENDIF
      ENDIF
*
      END
