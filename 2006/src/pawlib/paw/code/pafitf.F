*
* $Id: pafitf.F,v 1.2 1996/06/04 15:53:25 couet Exp $
*
* $Log: pafitf.F,v $
* Revision 1.2  1996/06/04 15:53:25  couet
* - Mods to handel properly the FILECASE KEEP
*
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/01 10/05/93  16.41.43  by  Rene Brun
*-- Author :
      SUBROUTINE PAFITF(CHEXP,NP)
*
*        To decode the kind of expression to be fitted
*         Ex: G+P7   or P7*E+G
*
      CHARACTER*(*) CHEXP
#include "paw/pcfitf.inc"
#include "paw/quest.inc"
#include "paw/pcchar.inc"
*
      DIMENSION ITOK(11)
*
      CHTEMP = ' '
      CHTEMP = CHEXP
      CALL CLTOU(CHTEMP)
*
      NCH=LENOCC(CHTEMP)
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
         IF(CHTEMP(I:I).EQ.'+')THEN
            NOPER=NOPER+1
            ITOK(NOPER)=I
            LOPER(NOPER)=1
            GO TO 10
         ENDIF
         IF(CHTEMP(I:I).EQ.'*')THEN
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
         IGAUS=INDEX(CHTEMP(IK1:IK1),'G')
         IF(IGAUS.NE.0)THEN
            IF(IK2.NE.IK1)GO TO 90
            IFTYPE(I)=1
            NPF(I)=3
         ENDIF
         IEXPO=INDEX(CHTEMP(IK1:IK1),'E')
         IF(IEXPO.NE.0)THEN
            IF(IK2.NE.IK1)GO TO 90
            IFTYPE(I)=2
            NPF(I)=2
         ENDIF
         IPOLY=INDEX(CHTEMP(IK1:IK1),'P')
         IF(IPOLY.NE.0)THEN
            NPOL=1
            CALL KICTOI(CHTEMP(IK1+1:IK2),NPOL)
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
  90  NOPER=0
*
  99  END
