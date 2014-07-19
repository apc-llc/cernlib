*
* $Id: zsldir.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zsldir.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZSLDIR
*.==========>
*.           To list the contents of a RZ directory
*.           in format ID  Title
*..=========> ( R.Brun )
#include "cspack/hcbook.inc"
#include "cspack/hcflag.inc"
#include "cspack/hcntpar.inc"
*
      CHARACTER*100 LOUT
      COMMON/QUEST/IQUEST(100)
      CHARACTER*1 HTYPE
      INTEGER KEYS(2)
*.___________________________________________
*
*
*          Find first ID in the RZ directory
*
      LCID=0
      KEYS(1) = 1
      KEYS(2) = 0
      CALL RZIN(IHDIV,0,0,KEYS,9999,'SC')
      IDN=IQUEST(21)
      IQ42 = IQUEST(22)
*
*             Enough space left
*
 10   KEYS(1) = IDN
      KEYS(2) = IQ42
      CALL RZIN(IHDIV,0,0,KEYS,9999,'NC')
      IF(IQUEST(1).NE.0)GO TO 90
      IQ40=IQUEST(40)
      IQ41=IQUEST(41)
      IQ42 = IQUEST(42)
      NWORDS=IQUEST(12)
      IOPTA=JBIT(IQUEST(14),4)
      IF(IOPTA.NE.0)GO TO 40
      CALL HSPACE(NWORDS+105,'ZSLDIR ',IDD)
**      IF(IERR.NE.0)                    GO TO 90
*
*             Read histogram data structure
*
      CALL RZIN(IHDIV,LCID,1,KEYS,9999,'ND')
      IF(IQUEST(1).NE.0)THEN
         CALL CZPUTA('2Bad sequence for RZ',ISTAT)
         GO TO 90
      ENDIF
*
      LOUT=' '
      IF(JBIT(IQ(LCID+KBITS),1).NE.0)THEN
         HTYPE='1'
         NWTITL=IQ(LCID-1)-KTIT1+1
         WRITE(LOUT,2000)IDN,HTYPE,(IQ(LCID+KTIT1+I-1),I=1,NWTITL)
      ELSEIF(JBYT(IQ(LCID+KBITS),2,2).NE.0) THEN
         HTYPE='2'
         NWTITL=IQ(LCID-1)-KTIT2+1
         WRITE(LOUT,2000)IDN,HTYPE,(IQ(LCID+KTIT2+I-1),I=1,NWTITL)
      ELSEIF(JBIT(IQ(LCID+KBITS),4).NE.0) THEN
         HTYPE='N'
         ITIT1=IQ(LCID+9)
         NWTITL=MIN(IQ(LCID+8),20)
         WRITE(LOUT,2000)IDN,HTYPE,(IQ(LCID+ITIT1+I-1),I=1,NWTITL)
      ELSE
         WRITE(LOUT,3000)IDN
      ENDIF
      CALL CZPUTA(LOUT,ISTAT)
      IF(ISTAT.NE.0)GO TO 90
*
      CALL MZDROP(IHDIV,LCID,' ')
  40  LCID=0
*
      IF(IQ40.EQ.0)GO TO 99
      IDN=IQ41
      GO TO 10
*
  90  CONTINUE
*
 2000 FORMAT('2',I10,1X,'(',A,')',3X,20A4)
 3000 FORMAT('2',I10)
  99  RETURN
      END
