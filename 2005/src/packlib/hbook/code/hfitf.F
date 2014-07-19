*
* $Id: hfitf.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfitf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 14/11/89  11.48.56  by  Rene Brun
*-- Author :
      SUBROUTINE HFITF(CHEX,NP)
*.==========>
*        To decode the kind of expression to be fitted
*         Ex: G+P7   or P7*E+G
*..=========> ( R.Brun)
*
#include "hbook/hcfitf.inc"
      COMMON /QUEST/ IQUEST(100)
      CHARACTER*(*) CHEX
      CHARACTER*32 CHEXP
      DIMENSION ITOK(11)
*.___________________________________________
      CHEXP=CHEX
      CALL CLTOU(CHEXP)
      NCH=LENOCC(CHEXP)
      CALL VZERO(LOPER,3*MAXOP)
*
*         Scan for delimiters
*
      NP=0
      NOPER=1
      ITOK(1)=0
      LOPER(1)=1
      DO 10 I=1,NCH
         IF(NOPER.GE.MAXOP)GO TO 10
         IF(CHEXP(I:I).EQ.'+')THEN
            NOPER=NOPER+1
            ITOK(NOPER)=I
            LOPER(NOPER)=1
            GO TO 10
         ENDIF
         IF(CHEXP(I:I).EQ.'*')THEN
            NOPER=NOPER+1
            ITOK(NOPER)=I
            LOPER(NOPER)=2
         ENDIF
  10  CONTINUE
      ITOK(NOPER+1)=NCH+1
*
      DO 30 I=1,NOPER
         IK1=ITOK(I)+1
         IK2=ITOK(I+1)-1
         IF(IK2.LT.IK1)GO TO 90
         IGAUS=INDEX(CHEXP(IK1:IK1),'G')
         IF(IGAUS.NE.0)THEN
            IFTYPE(I)=1
            NPF(I)=3
         ENDIF
         IEXPO=INDEX(CHEXP(IK1:IK1),'E')
         IF(IEXPO.NE.0)THEN
            IFTYPE(I)=2
            NPF(I)=2
         ENDIF
         IPOLY=INDEX(CHEXP(IK1:IK1),'P')
         IF(IPOLY.NE.0)THEN
            NPOL=1
            CALL HICTOI(CHEXP(IK1+1:IK2),NPOL)
            IF(IQUEST(1).NE.0)GO TO 90
            IFTYPE(I)=3
            NPF(I)=NPOL+1
         ENDIF
         IF(IFTYPE(I).EQ.0)GO TO 90
         NP=NP+NPF(I)
  30  CONTINUE
      GO TO 99
*
*          Error
*
  90  CALL HBUG('Invalid expression','HFITHN',0)
      NOPER=0
*
  99  END
