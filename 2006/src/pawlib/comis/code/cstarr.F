*
* $Id: cstarr.F,v 1.1.1.1 1996/02/26 17:16:23 mclareni Exp $
*
* $Log: cstarr.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.57  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSTARR(IDES,LT,IT)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
#include "comis/cslun.inc"
       DOUBLE PRECISION DQ(99)
       REAL Q(99)
       EQUIVALENCE (IQ,DQ,Q)
      REC='   PRINT ARRAY   '// REC(1:LT-1)
      CALL CSCLRB('*T  ')
      CALL CSADDB(REC(1:LT+18))
      CALL CSOUTB
      NDIM=IQ(IDES+3)
      LENEL=MOD(IQ(IDES),KON3)
      NEL=IQ(IDES+3+NDIM)
      GO TO (1,2,3,4,5,1,7),IT
   1   I=MHLOC(NEL)
      J=I+NEL-1
      CALL COPYAJ(IQ(IDES+1),LOCF(IQ(I)),NEL)
      WRITE(*,11)(IQ(K),K=I,J)
   11 FORMAT(2X,8I10)
      IF(ISTLOG.NE.0)THEN
        WRITE(LUNLOG,12) (IQ(K),K=I,J)
   12  FORMAT('*T  ',8I10)
      ENDIF
      CALL MHFREE(I)
      RETURN
   2   I=MHLOC(NEL)
      J=I+NEL-1
      CALL COPYAJ(IQ(IDES+1),LOCF(IQ(I)),NEL)
      WRITE(*,21)(Q(K),K=I,J)
   21 FORMAT(2X,5G14.7)
      IF(ISTLOG.NE.0)THEN
        WRITE(LUNLOG,22) (Q(K),K=I,J)
   22  FORMAT('*T  ',5G14.7)
      ENDIF
      CALL MHFREE(I)
      RETURN
   7   I=MHLOC(NEL*2)
      J=I+NEL*2-1
      CALL COPYAJ(IQ(IDES+1),LOCF(IQ(I)),NEL*2)
      WRITE(*,71)(Q(K),K=I,J)
   71 FORMAT(2X, '(' , G14.7, ',' ,G14.7, ')': )
      IF(ISTLOG.NE.0)THEN
        WRITE(LUNLOG,72) (Q(K),K=I,J)
   72  FORMAT(('*T  ',2('(',G14.7, ',' ,G14.7, ')': )))
      ENDIF
      CALL MHFREE(I)
      RETURN
   5   L=(NEL+1)*LENEL
      I=MHLOC(L)
      I1=I+LENEL
      J=(I1-1)/LENEL+1
      N=J+NEL-1
      I1=(J-1)*LENEL+1
      CALL COPYAJ(IQ(IDES+1),LOCF(IQ(I1)),NEL*LENEL)
      WRITE(*,51)(DQ(K),K=J,N)
   51 FORMAT(2X,3D25.14)
      IF(ISTLOG.NE.0)THEN
        WRITE(LUNLOG,52) (DQ(K),K=J,N)
   52  FORMAT('*T  ',3D25.14)
      ENDIF
      CALL MHFREE(I)
      RETURN
  3   CALL CSCLRB('*T  ')
      N=MIN0(LENEL,76)
      LENT=N+4
      REC(1:4)=' '
      JA=IQ(IDES+1)
      JR=JSR+4
      DO 31 I=1,NEL
       CALL CCOPYS(JA,JR,N)
       CALL CSADDB(REC(1:LENT))
  31  JA=JA+LENEL
      CALL CSOUTB
      RETURN
   4  LENT=11
      CALL CSCLRB('*T  ')
      REC(1:4)=' '
      JA=IQ(IDES+1)
      JR=JSR+4
      DO 41 I=1,NEL
       L=MIWORD(JA)
       IF(L.NE.0)THEN
         CALL CCOPYS(MJSCHA('.TRUE. '),JR,7)
       ELSE
         CALL CCOPYS(MJSCHA('.FALSE.'),JR,7)
       ENDIF
       CALL CSADDB(REC(1:LENT))
  41  JA=JA+1
      CALL CSOUTB
      END
