*
* $Id: hgetff.F,v 1.1.1.1 1996/01/16 17:07:38 mclareni Exp $
*
* $Log: hgetff.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 28/04/93  11.39.30  by  Rene Brun
*-- Author :
      SUBROUTINE HGETFF(ICONTR,IFLRET,CHOPT)
*.==========>
*.           SETS  FLAGS  LINEAR,IFLSF,IFLBUF,IDER,IWEIGH,ITFUM,ISUPIM
*.                 INTO THE /HCFIT2/
*..=========> ( I.Ivanchenko )
#include "hbook/hcfit2.inc"
#include "hbook/hcfitr.inc"
      COMMON/QUEST/IQUEST(100)
      DIMENSION IFLARR(7)
      EQUIVALENCE (IFLARR(1),LINEAR)
      CHARACTER*(*) CHOPT
*.___________________________________________
*
*          Build CHOPT
*
      JCONTR=ICONTR
      DO 10 I=1,7
         IFLARR(I)=JCONTR/10**(7-I)
         JCONTR=JCONTR-IFLARR(I)*10**(7-I)
  10  CONTINUE
      CHOPT='TU'
      J=3
      IF(LINEAR.NE.0)THEN
         CHOPT(J:J)='L'
         J=J+1
      ENDIF
      IF(IFLSF.NE.0)THEN
         CHOPT(J:J)='R'
         IFTRNG=1
         IFXLOW=IQUEST(11)
         IFXUP =IQUEST(12)
         IFYLOW=IQUEST(13)
         IFYUP =IQUEST(14)
         J=J+1
      ENDIF
      IF(IFLBUF.NE.0)THEN
         CHOPT(J:J)='M'
         J=J+1
      ENDIF
      IF(IDER.NE.0)THEN
         CHOPT(J:J)='D'
         J=J+1
      ENDIF
      IF(IWEIGH.NE.0)THEN
         CHOPT(J:J)='W'
         J=J+1
      ENDIF
      IF(ITFUM.EQ.0)THEN
         CHOPT(J:J)='Q'
         J=J+1
      ELSE
         IF(MOD(ITFUM,2).EQ.0)THEN
            CHOPT(J:J)='E'
            J=J+1
         ENDIF
      ENDIF
      IF(ITFUM.GT.5)THEN
         CHOPT(J:J)='V'
         J=J+1
      ENDIF
      IF(ISUPIM.NE.2)THEN
         CHOPT(J:J)='N'
      ENDIF
      LINEAR=0
      IFLSF =0
      ISUPIM=0
      IFLBUF=0
      IDER  =0
      IWEIGH=0
      ITFUM =0
      IDIMPN=3
*
*          Get main histogram parameters
*
      CALL HHIPAR(IFLRET)
*
  99  RETURN
      END
