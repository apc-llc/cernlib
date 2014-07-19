*
* $Id: hgetid.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: hgetid.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/10 23/08/95  09.40.55  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE HGETID(NAME)
*
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcdire.inc"
#include "paw/pawcom.inc"
#include "paw/pcrang.inc"
#include "paw/quest.inc"
      CHARACTER*(*) NAME
      CHARACTER*80 CHPATH,CHOLD
      CHARACTER*32 CHIDE
      CHARACTER*8 CHNUM,CHCYC
      LOGICAL HEXIST
      SAVE CHOLD
*
*           Find Pathname if present
*
      ICRANG=0
      KHRIN=0
      CHPATH=' '
      KPATH=0
      NCH=LENOCC(NAME)
      IF(NCH.LE.0)THEN
         ID=-98765
         LCID=0
         GO TO 999
      ENDIF
      NCHP=LEN(CHPATH)
      DO 10 KID=NCH,1,-1
         IF(NAME(KID:KID).EQ.'/')GO TO 20
   10 CONTINUE
      KID=0
   20 IF(KID.NE.0)THEN
         KPATH=MIN(KID,NCHP)-1
         CHPATH=NAME(1:KPATH)
         KID=KID+1
      ELSE
         KID=1
      ENDIF
*
*           Search cycle
*
      KLPAR=INDEX(NAME(1:NCH),'(')
      KRPAR=INDEX(NAME(1:NCH),')')
      IF(KLPAR.NE.0.AND.KRPAR.EQ.0)GO TO 100
      IF(KLPAR.EQ.0.AND.KRPAR.NE.0)GO TO 100
      KSCAN=KLPAR-1
      IF(KSCAN.LE.0)KSCAN=NCH
      CHCYC='9999'
      DO 30 KCYC=1,NCH
         IF(NAME(KCYC:KCYC).EQ.';')GO TO 40
   30 CONTINUE
      KCYC=0
   40 IF(KCYC.NE.0)THEN
         KSCAN=KCYC-1
         IF(KCYC.LT.NCH)THEN
            CHCYC=NAME(KCYC+1:NCH)
         ENDIF
      ENDIF
*
*           Search for HIST,PROX,SLIX,etc.
*
      CHNUM='1'
      DO 50 KCASE=KID,KSCAN
         IF(NAME(KCASE:KCASE).EQ.'.')GO TO 60
   50 CONTINUE
      KCASE=0
   60 IF(KCASE.EQ.0)THEN
         CHIDE=NAME(KID:KSCAN)
         CHCASE='HIST'
      ELSE
         CHIDE=NAME(KID:KCASE-1)
         KCASE=KCASE+1
*
*           Search for NUM
*
         DO 70 KNUM=KCASE,KSCAN
            IF(NAME(KNUM:KNUM).EQ.'.')GO TO 80
   70    CONTINUE
         KNUM=0
   80    IF(KNUM.NE.0)THEN
            CHNUM=NAME(KNUM+1:KSCAN)
            CHCASE=NAME(KCASE:KNUM-1)
         ELSE
            CHCASE=NAME(KCASE:KSCAN)
         ENDIF
      ENDIF
*
*          Convert parameters
*
      IP1=INDEX(CHIDE,'(')
      IP2=INDEX(CHIDE,')')
      IF(IP1*IP2.EQ.0)THEN
         CALL KICTOI(CHIDE,ID)
      ELSE
         CALL KICTOI(CHIDE(1:IP1-1),ID)
      ENDIF
      IF(IQUEST(1).NE.0)GO TO 100
      CALL KICTOI(CHNUM,NUM)
      IF(IQUEST(1).NE.0)GO TO 100
      CALL KICTOI(CHCYC,ICYCLE)
      IF(IQUEST(1).NE.0)GO TO 100
*
*          Save the current directory in CHOLD, and set CHPATH as the
*          new current directory. CHOLD is restore at the end of HGETID.
*
      CALL HCDIR(CHOLD,'R')
      CALL HCDIR(CHPATH,' ')
*
      ID1=ID
      IF(ICHTOP(ICDIR).NE.0)THEN
         IF(JOFSET.EQ.-99999)GO TO 999
         IF(JOFSET.LT.0)THEN
            CALL HSCR(ID,ICYCLE,' ')
            GO TO 999
         ENDIF
         IF(HEXIST(ID+JOFSET))THEN
            ID1=ID+JOFSET
            CALL HFIND(ID1,'HGETID')
            IF(JBIT(IQ(LCID),5).NE.0.OR.KPATH.NE.0)THEN
               ID2 = ID1
   90          ID2 = ID2+1
               IF (HEXIST(ID2).OR.ID2.EQ.0) GOTO 90
               CALL HCOPY(ID1,ID2,' ')
               CALL HDELET(ID1)
               CALL HRIN(ID,ICYCLE,JOFSET)
               IF(IQUEST(1).NE.0)THEN
                  CALL HCOPY(ID2,ID1,' ')
                  CALL HDELET(ID2)
                  CALL HBUG('Unknown histogram','HGETID',ID)
                  CALL HCDIR(CHOLD,' ')
                  GO TO 999
               ELSE
                  CALL HDELET(ID2)
               ENDIF
               KHRIN=1
            ENDIF
         ELSE
            ID1=ID+JOFSET
            CALL HRIN(ID,ICYCLE,JOFSET)
            IF(IQUEST(1).NE.0)THEN
               IF(JOFSET.NE.0.AND.CHPATH.EQ.' '.AND.HEXIST(ID))THEN
                  IQUEST(1)=0
                  ID1=ID
               ELSE
                  CALL HBUG('Unknown histogram','HGETID',ID)
                  CALL HCDIR(CHOLD,' ')
                  LCID=0
                  GO TO 999
               ENDIF
            ENDIF
            KHRIN=1
         ENDIF
      ENDIF
*
      LFIX=0
      ID=ID1
      IF(ID.EQ.0)GO TO 999
      CALL HFIND(ID1,'HGETID')
      IF(LCID.EQ.0)GO TO 999
      CALL HDCOFL
      IF(I123.EQ.0)THEN
         IF(I4.NE.0.AND.IDOLD.NE.0)THEN
            IDOLD=0
            GO TO 999
         ENDIF
         LCID=0
         CALL KUALFA
         PRINT 10000,ID1
10000    FORMAT(' ***** Error in HGETID, ID= ',I8,' not an Histogram')
         GO TO 999
      ENDIF
*-* Get range
      IBX1 = 1
      IBX2 = IQ(LCID+KNCX)
      IF(I1.EQ.0)THEN
         IBY1 = 1
         IBY2 = IQ(LCID+KNCY)
      ENDIF
      IF(KLPAR.NE.0)THEN
         ICOM=INDEX(NAME(KLPAR:KRPAR),',')
         IF(ICOM.EQ.0)THEN
            CALL HGETIR(ID,NAME(KLPAR+1:KRPAR-1),IBX1,IBX2,I1,1,ICRANG)
         ELSE
            ICOM=ICOM+KLPAR-1
            IF(ICOM.GT.KLPAR+1)THEN
               CALL HGETIR(ID,NAME(KLPAR+1:ICOM-1),IBX1,IBX2,I1,1
     +,                    ICRANG)
            ENDIF
            IF(ICOM.LT.KRPAR-1)THEN
               CALL HGETIR(ID,NAME(ICOM+1:KRPAR-1),IBY1,IBY2,I1,2
     +,                    ICRANG)
            ENDIF
         ENDIF
      ENDIF
*
      ICX1=MAX(1,IBX1)
      ICY1=MAX(1,IBY1)
      ICX2=MAX(1,IBX2)
      ICY2=MAX(1,IBY2)
      ICX2=MIN(IBX2,IQ(LCID+KNCX))
      ICY2=MIN(IBY2,IQ(LCID+KNCY))
      ICX1=MIN(ICX1,ICX2)
      ICY1=MIN(ICY1,ICY2)
      IF(CHCASE.NE.'HIST')CALL HFIXID(ID1,CHCASE,NUM)
      GO TO 999
*
*             Invalid Identifier
*
  100 CALL HBUG('Invalid identifier','HGETID',0)
      IQUEST(1)=0
      GO TO 999
*
*====>        Restore Current Directory
*
      ENTRY HSETCD
*
      LFIX=0
      CALL HCDIR(CHOLD,' ')
*
  999 END
