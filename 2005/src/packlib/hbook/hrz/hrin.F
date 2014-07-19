*
* $Id: hrin.F,v 1.1.1.1 1996/01/16 17:08:08 mclareni Exp $
*
* $Log: hrin.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/14 06/10/94  15.43.19  by  Fons Rademakers
*-- Author :
      SUBROUTINE HRIN(IDD,ICYCLE,KOFSET)
*.==========>
*.           Read histogram IDD from current directory (RZ or GLOBAL)
*.           The histogram stored in memory will be IDD+IOFSET
*..=========> ( R.Brun )
#include "hbook/hcntpar.inc"
#include "hbook/hntcur.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcdire.inc"
*
      COMMON/QUEST/IQUEST(100)
      CHARACTER*128 CHWOLD
      INTEGER       KEYS(2)
      DATA KHIDE,KHID1,KHID2,KHCO1,KHCO2/4HHIDE,4HHID1,4HHID2,
     +                                   4hHCO1,4HHCO2/
*.___________________________________________
*
*          Check if directory in GLOBAL SECTION
*
      IOFSET=KOFSET
      IF(ICHTOP(ICDIR).LT.0)THEN
         IF(INDEX(HFNAME(ICDIR),'memory').NE.0)THEN
            LOCQ=1-LOCF(IQUEST(1))-ICHTOP(ICDIR)
            CALL HCOPYU(IDD,IQUEST(LOCQ),IOFSET)
         ELSE
            LOCQ=1-LOCF(IQUEST(1))-ICHTOP(ICDIR)
            CALL HCOPYM(IDD,IQUEST(LOCQ),IOFSET)
         ENDIF
         CALL SBIT1(IQ(LCID),5)
         GO TO 80
      ENDIF
      IF(ICYCLE.GT.1000.AND.IDD.EQ.0)THEN
         CALL HPAFF(CHCDIR,NLCDIR,CHWOLD)
         LQ(LHBOOK-NLPAT-10)=LCDIR
      ENDIF
*
      NRHIST=IQ(LCDIR+KNRH)
      IF(KOFSET.EQ.99999.AND.NRHIST.GT.0)THEN
         IF(IQ(LTAB+NRHIST).GE.KOFSET)IOFSET=IQ(LTAB+NRHIST)+1000000
      ENDIF
      KEYS(2) = 0
      IQ42=0
*
*          If IDD=0 find first ID in the RZ directory
*
      IDN=IDD
      IF(IDD.EQ.0)THEN
         KEYS(1) = 1
         CALL HRZIN(IHDIV,0,0,KEYS,9999,'SC')
         IDN=IQUEST(21)
         IQ42=IQUEST(22)
      ENDIF
 
*             Check if ID already in the table
*
   10 ID=IDN+IOFSET
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,ID)
      INMEM=0
      IF(IDPOS.GT.0)THEN
         LC=LQ(LTAB-IDPOS)
         IF(JBIT(IQ(LC),5).EQ.0)THEN
            INMEM=1
         ELSE
            CALL HBUG('+Already existing histogram replaced','HRIN',ID)
            CALL HDELET(ID)
            NRHIST=IQ(LCDIR+KNRH)
            IDPOS=-IDPOS+1
         ENDIF
      ENDIF
*
*             Enough space left ?
*
      KEYS(1) = IDN
      KEYS(2) = IQ42
      CALL HRZIN(IHDIV,0,0,KEYS,ICYCLE,'NC')
      IF(IQUEST(1).NE.0)GO TO 70
      IQ40=IQUEST(40)
      IQ41=IQUEST(41)
      IQ42=IQUEST(42)
      NWORDS=IQUEST(12)
      IOPTA=JBIT(IQUEST(14),4)
      IF(IOPTA.NE.0)GO TO 60
      IF(INMEM.NE.0)GO TO 60
      CALL HSPACE(NWORDS+1000,'HRIN  ',IDD)
      IF(IERR.NE.0)                    GO TO 70
*
*             Enter ID in the list of ordered IDs
*
      IDPOS=-IDPOS+1
      IF(NRHIST.GE.IQ(LTAB-1))THEN
         CALL MZPUSH(IHDIV,LTAB,500,500,' ')
      ENDIF
      DO 20 I=NRHIST,IDPOS,-1
         IQ(LTAB+I+1)=IQ(LTAB+I)
         LQ(LTAB-I-1)=LQ(LTAB-I)
   20 CONTINUE
*
*             Read histogram data structure
*
      IF(LIDS.EQ.0)THEN
         KEYS(1) = IDN
         CALL HRZIN(IHDIV,LCDIR,-2,KEYS,ICYCLE,'ND')
         IF(IQUEST(1).NE.0)THEN
            CALL HBUG('Bad sequence for RZ','HRIN',IDN)
            GO TO 70
         ENDIF
         LIDS=LQ(LCDIR-2)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-9)
         KEYS(1) = IDN
         CALL HRZIN(IHDIV,LLID,  0,KEYS,ICYCLE,'ND')
         IF(IQUEST(1).NE.0)THEN
            CALL HBUG('Bad sequence for RZ','HRIN',IDN)
            GO TO 70
         ENDIF
         LCID=LQ(LLID)
      ENDIF
      IQ(LCID-5)=ID
      LQ(LCDIR-9)=LCID
      IQ(LCDIR+KNRH)=IQ(LCDIR+KNRH)+1
      IQ(LTAB+IDPOS)=ID
      LQ(LTAB-IDPOS)=LCID
      CALL SBIT1(IQ(LCID),5)
      IF(JBIT(IQ(LCID+KBITS),1).NE.0)THEN
         IF(IQ(LCID-4).EQ.KHIDE)THEN
            IQ(LCID-4)=KHID1
            L=LQ(LCID-1)
            IF(L.NE.0)IQ(L-4)=KHCO1
         ENDIF
      ENDIF
      IF(JBYT(IQ(LCID+KBITS),2,2).NE.0)THEN
         IF(IQ(LCID-4).EQ.KHIDE)THEN
            IQ(LCID-4)=KHID2
            L=LQ(LCID-1)
            IF(L.NE.0)IQ(L-4)=KHCO2
         ENDIF
*-- check old style convention for KMIN2/KMAX2
***   This code can only be introduced when version 4.19 or later
*     will be largely disseminated
***         IOLD2=0
***         J20=JBIT(IQ(LCID+KBITS),20)
***         J21=JBIT(IQ(LCID+KBITS),21)
***         IF(J20.EQ.0.AND.Q(LCID+KMAX2).NE.0.)IOLD2=1
***         IF(J21.EQ.0.AND.Q(LCID+KMIN2).NE.0.)IOLD2=1
***         IF(IOLD2.NE.0)THEN
***            Q(LCID+KMAX2)=0.
***            Q(LCID+KMIN2)=0.
***            IF(J20.NE.0)Q(LCID+KMAX2)=Q(LCID+KSCAL2)
***            IF(J21.NE.0)Q(LCID+KMIN2)=Q(LCID+KSCAL2)
***         ENDIF
      ENDIF
      IF(JBIT(IQ(LCID+KBITS),4).NE.0)THEN
*
*-- old n-tuple
*
         IF (IQ(LCID-2) .EQ. 2) THEN
**            IF(IOFSET.NE.0)THEN
**               CALL HDELET(ID)
**               CALL HBUG('IOFSET must be 0 for ntuples','HRIN',ID)
**               GO TO 60
**            ENDIF
            NCHRZ=IQ(LCID+11)
            IF(NCHRZ.LE.0)GO TO 30
            ITAG1=IQ(LCID+10)
            NW=IQ(LCID-1)-ITAG1+1
            NPLUS=32-ITAG1
            IF(NPLUS.GT.0)THEN
               CALL MZPUSH(IHDIV,LCID,0,NPLUS,' ')
               CALL UCOPY2(IQ(LCID+ITAG1),IQ(LCID+32),NW)
               IQ(LCID+9)=IQ(LCID+9)+NPLUS
               IQ(LCID+10)=32
            ENDIF
            CALL HPAFF(CHCDIR,NLCDIR,CHWOLD)
            NCHRZ=LENOCC(CHWOLD)
            CALL UCTOH(CHWOLD,IQ(LCID+12),4,NCHRZ)
            IQ(LCID+11)=NCHRZ
   30       IQ(LCID)=9999
            LC=LQ(LCID-1)
            CALL SBIT0(IQ(LC),1)
*           If memory-resident Ntuple, compute  pointers to banks
            IF(NCHRZ.LE.0)THEN
               NMORE=IQ(LCID+5)+3-IQ(LCID-3)
               IF(NMORE.GT.0)THEN
                  CALL MZPUSH(IHDIV,LCID,NMORE,0,' ')
               ENDIF
               IF(IQ(LCID+5).GE.1)THEN
                  DO 40 IB=1,IQ(LCID+5)
                     LQ(LCID-3-IB)=LC
                     LC=LQ(LC)
                     IF(LC.EQ.0)THEN
                        LC=LQ(LCID-1)
                        GO TO 60
                     ENDIF
   40             CONTINUE
                  LC=LQ(LCID-1)
               ENDIF
            ELSE
*           If disk-resident Ntuple, compute  pointers to KEYS
               IF(ICHTOP(ICDIR).LT.1000)CALL HRZKEY(IDN)
*           save ID in data structure in case of offset
               IQ(LCID+5)=IDN
            ENDIF
            GO TO 60
*
*-- new n-tuple
*
         ELSE
            NCHRZ=IQ(LCID+ZNCHRZ)
            IF(NCHRZ.LE.0)GO TO 50
            ITIT1=IQ(LCID+ZITIT1)
            NW=IQ(LCID-1)-ITIT1+1
            NPLUS=34-ITIT1
            IF(NPLUS.GT.0)THEN
               CALL MZPUSH(IHDIV,LCID,0,NPLUS,' ')
               CALL UCOPY2(IQ(LCID+ITIT1),IQ(LCID+34),NW)
               IQ(LCID+ZITIT1)=34
            ENDIF
            CALL HPAFF(CHCDIR,NLCDIR,CHWOLD)
            NCHRZ=LENOCC(CHWOLD)
            CALL UCTOH(CHWOLD,IQ(LCID+ZNCHRZ+1),4,NCHRZ)
            IQ(LCID+ZNCHRZ)=NCHRZ
   50       IQ(LCID)=9999
            LC = LQ(LCID-1)
            CALL SBIT0(IQ(LC),1)
            CALL SBIT0(IQ(LC),2)
            CALL SBIT0(IQ(LC),3)
            CALL HNMSET(ID,ZIBANK,0)
            CALL HNMSET(ID,ZITMP,0)
            IQ(LCID+ZIFTMP) = 2
            IQ(LCID+ZID)    = IDN
            NTCUR = 0
            GO TO 60
         ENDIF
      ENDIF
*
*             Add option ?
*
      IF(KOFSET.EQ.99999)THEN
         ID2=ID
         CALL HOPERA(IDN,'+',ID2,IDN,1.,1.)
         CALL HDELET(ID2)
      ENDIF
*
   60 IF(IQ40.EQ.0)GO TO 80
      IDN=IQ41
      IF(IDD.EQ.0)GO TO 10
*
   70 CONTINUE
*
   80 RETURN
      END
