*
* $Id: hdelet.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hdelet.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/01 01/11/93  19.01.14  by  Fons Rademakers
*-- Author :
      SUBROUTINE HDELET(ID1)
*.==========>
*.           DELETE ID FROM MEMORY
*..=========> ( R.Brun )
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcpiaf.inc"
#if defined(CERNLIB_CZ)
#include "hbook/hcache.inc"
#endif
*.___________________________________________
      IF(LCDIR.LE.0)GO TO 999
      IF(ID1.EQ.0)GO TO 120
      ID=ID1
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.LE.0)THEN
#if defined(CERNLIB_CZ)
         IF (SERVPF) THEN
            IDNPOS = LOCATI(IDNOT,IDTNOT,ID)
            IF (IDNPOS .GT. 0) THEN
               DO 5 I = IDNPOS+1, IDTNOT
                  IDNOT(I-1) = IDNOT(I)
   5           CONTINUE
               IDTNOT = IDTNOT - 1
            ENDIF
         ENDIF
#endif
         CALL HBUG('Unknown histogram','HDELET',ID1)
         GO TO 999
      ENDIF
*
      LCID=LQ(LTAB-IDPOS)
*
*-- if new N-tuple drop also storage buffers
*
      IF (JBIT(IQ(LCID+KBITS),4).NE.0 .AND. IQ(LCID-2).EQ.ZLINK) THEN
         CALL HNBUFD(ID1)
      ENDIF
*
      CALL MZDROP(IHDIV,LCID,' ')
      LIDS=LQ(LCDIR-2)
      LQ(LTAB-IDPOS)=0
*
*             Remove ID from table of ordered IDs
*
      NRHIST=IQ(LCDIR+KNRH)
      DO 10 I=IDPOS,NRHIST-1
         IQ(LTAB+I)=IQ(LTAB+I+1)
         LQ(LTAB-I)=LQ(LTAB-I-1)
  10  CONTINUE
      IQ(LCDIR+KNRH)=NRHIST-1
      NRHIST=IQ(LCDIR+KNRH)
*
*             Update LLDIR in case deleted ID was the last one
*
      IF(LQ(LCDIR-9).EQ.LCID)THEN
         LREF=0
         LCID=LIDS
  20     IF(LCID.NE.0)THEN
            LREF=LCID
            LCID=LQ(LCID)
            GO TO 20
         ENDIF
         LQ(LCDIR-9)=LREF
      ENDIF
      GO TO 999
*
*             Delete all IDs
*
 120  IF(LIDS .GT. 0) THEN
         CALL HNBUFD(0)
         CALL MZDROP(IHDIV,LIDS ,'L')
      ENDIF
      NRHIST=IQ(LCDIR+KNRH)
      IF(NRHIST.GT.0.AND.LTAB.GT.0)THEN
         CALL VZERO(LQ(LTAB-NRHIST),NRHIST)
      ENDIF
      IQ(LCDIR+KNRH)=0
      LQ(LCDIR-2)=0
      LQ(LCDIR-9)=0
      LIDS=0
      LLID=0
      NRHIST=0
#if defined(CERNLIB_CZ)
      IDTNOT = 0
#endif
*
  999 IDLAST=0
      IDHOLD=0
      LID   =0
      END
