*
* $Id: hldir2.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hldir2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 12/09/93  18.56.12  by  Rene Brun
*-- Author :
      SUBROUTINE HLDIR2
*.==========>
*.           To list the contents of a RZ directory
*.           in format ID  Title
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcmail.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcdire.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*1 HTYPE
      INTEGER     KEYS(2)
*.___________________________________________
*
*          Check if directory in GLOBAL SECTION
*
      IF(ICHTOP(ICDIR).LT.0)THEN
         LOCQ=1-LOCF(IQUEST(1))-ICHTOP(ICDIR)
         CALL HCOPYM(0,IQUEST(LOCQ),IOFSET)
         GO TO 99
      ENDIF
*             Write name of directory pointed by LCDIR
*
      CHMAIL=' '
      CALL HPAFF(CHCDIR,NLPAT,CHMAIL)
      NCH=LENOCC(CHMAIL)
      WRITE(LOUT,1000)CHMAIL(1:NCH)
*
*
*          Find first ID in the RZ directory
*
      KEYS(1) = 1
      KEYS(2) = 0
      CALL HRZIN(IHWORK,0,0,KEYS,9999,'SC')
      IDN=IQUEST(21)
      IQ42=IQUEST(22)
 
*
*             Enough space left ?
*
  10  KEYS(1) = IDN
      KEYS(2) = IQ42
      CALL HRZIN(IHWORK,0,0,KEYS,9999,'NC')
      IF(IQUEST(1).NE.0)GO TO 90
      IQ40=IQUEST(40)
      IQ41=IQUEST(41)
      IQ42=IQUEST(42)
      NWORDS=IQUEST(12)
      IOPTA=JBIT(IQUEST(14),4)
      IF(IOPTA.NE.0)GO TO 40
      CALL HSPACE(NWORDS+1000,'HLDIR ',IDD)
      IF(IERR.NE.0)                    GO TO 90
*
*             Read histogram data structure
*
      KEYS(1) = IDN
      CALL HRZIN(IHWORK,LHWORK,1,KEYS,9999,'ND')
      IF(IQUEST(1).NE.0)THEN
         CALL HBUG('Bad sequence for RZ','HLDIR',IDN)
         GO TO 90
      ENDIF
*
      IF(IQ(LHWORK-2).EQ.0)THEN
         WRITE(LOUT,2100)IDN
      ELSEIF(JBIT(IQ(LHWORK+KBITS),1).NE.0)THEN
         HTYPE='1'
         NWTITL=IQ(LHWORK-1)-KTIT1+1
         WRITE(LOUT,2000)IDN,HTYPE,(IQ(LHWORK+KTIT1+I-1),I=1,NWTITL)
      ELSEIF(JBYT(IQ(LHWORK+KBITS),2,2).NE.0)THEN
         HTYPE='2'
         NWTITL=IQ(LHWORK-1)-KTIT2+1
         WRITE(LOUT,2000)IDN,HTYPE,(IQ(LHWORK+KTIT2+I-1),I=1,NWTITL)
      ELSEIF(JBIT(IQ(LHWORK+KBITS),4).NE.0)THEN
         HTYPE='N'
         ITIT1=IQ(LHWORK+9)
         NWTITL=IQ(LHWORK+8)
         WRITE(LOUT,2000)IDN,HTYPE,(IQ(LHWORK+ITIT1+I-1),I=1,NWTITL)
      ENDIF
*
      CALL MZDROP(IHWORK,LHWORK,' ')
  40  LHWORK=0
*
      IF(IQ40.EQ.0)THEN
         CALL MZWIPE(IHWORK)
         GO TO 99
      ENDIF
      IDN=IQ41
      GO TO 10
*
  90  CONTINUE
*
 1000 FORMAT(//,' ===> Directory : ',A)
 2000 FORMAT(1X,I10,1X,'(',A,')',3X,20A4)
 2100 FORMAT(1X,I10,1X,'(A)   Unnamed array')
  99  RETURN
      END
