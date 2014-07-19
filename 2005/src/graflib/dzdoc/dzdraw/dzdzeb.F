*
* $Id: dzdzeb.F,v 1.1.1.1 1996/03/04 16:13:00 mclareni Exp $
*
* $Log: dzdzeb.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:00  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE  DZDZEB(BRNAME,BRCLAS,BRPATH,OBNAME,OBCLAS,STEXT,LTEXT)
      CHARACTER*(*)      BRNAME,BRCLAS,BRPATH,OBNAME,OBCLAS,STEXT,LTEXT
#include "zebra/mzbits.inc"
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzcn.inc"
#include "zebra/zunit.inc"
#include "dzdzbrinc.inc"
*
      INTEGER IQUEST(100)
      COMMON/QUEST/IQUEST
 
      CHARACTER*16 CHTITL
      CHARACTER*8 CTEMP
      CHARACTER*2 CDIV
      INTEGER ICSTOR,ICDIV,IIST,IST, LGOP, IXDIV, IDH, NUMB, INIFLG
      CHARACTER*8 CHTAG(100)
      CHARACTER*100 CHFORM
      CHARACTER*80 CHPATH
      CHARACTER*1 CF
      CHARACTER*40 CHKEY, SPACES
      CHARACTER*256 LBRPAT
      INTEGER ICONTT,ICONTF,ICONTD,ICONTK,IRET, NCH, NWKEY, I, IP,ISAVD
      SAVE ICONTT,ICONTF, ICONTD, ICONTK,ISAVD, NWKEY
      SAVE ICSTOR,ICDIV, LGOP, NUMB, NID, INIFLG
      SAVE CHTAG, CHFORM, CHPATH
      DATA ICSTOR,ICDIV, INIFLG,ICONTT,ICONTF, ICONTD, ICONTK ,ISAVD
     &     /0,0,0,0,0,0,0,0/
 
*
*   Browser interface to return stores, div, banks, RZ and FZ files
*
      IF(INIFLG.EQ.0)THEN
         CALL DZDIBR
         INIFLG=1
      ENDIF
 
*     the top page: Stores, RZfiles, FZfiles
 
      IF(BRPATH.EQ.' ')THEN
         IF(OBNAME.EQ.' ') THEN
            ICSTOR=0
            ICONTT=0
            ICONTF=0
            ICONTD=0
            ICONTK=0
         ENDIF
         OBNAME=' '
         IF(ICSTOR.GE.0)THEN
            ICSTOR=ICSTOR+1
            IF(ICSTOR.LE.NQSTOR+1)THEN
*              return stores
               IIST = 0
               CALL SBYT(ICSTOR-1,IIST,27,6)
               CALL MZSDIV(IIST,-7)
               CALL UHTOC(IQTABV(KQT+11),4,CTEMP,8)
                  DO 26 I=1,8
                     IF(CTEMP(I:I).EQ.'/')CTEMP(I:I)='_'
26                CONTINUE
               WRITE(CDIV,'(I2.2)')ICSTOR-1
               OBNAME= 'M_'//'Store'//CDIV//'.'//CTEMP
               STEXT=' '
               LTEXT='Store'
               OBCLAS='STORE'
               GOTO 999
            ELSE
               ICSTOR=-1
            ENDIF
         ENDIF
*        Rzfiles
         IF(ICONTT.GE.0)THEN
*           next top directory  (RZFILE)
            CALL DZDNTD(CHTITL,STEXT,ICONTT,IRET)
            IF(ICONTT.EQ.0)THEN
               ICONTT=-1
            ELSEIF(IRET.EQ.0)THEN
               OBCLAS='rzfil'
               OBNAME='R_'//CHTITL
               LTEXT='Rz_file'
               GOTO 999
            ENDIF
         ENDIF
*        Fzfiles
         IF(ICONTF.GE.0)THEN
*           next Fzfile
            CALL DZDNFZ(CHTITL,STEXT,ICONTF,IRET)
            IF(ICONTF.EQ.0)THEN
               ICONTF=-1
            ELSEIF(IRET.EQ.0)THEN
               OBCLAS='FZ'
               OBNAME='F_'//CHTITL
               LTEXT='Fzfile'
               GOTO 999
            ENDIF
         ENDIF
 
         GOTO 999
*        end of top page
      ENDIF
 
*     skip leading /
 
      IF(BRPATH(1:1).EQ.'/')THEN
         IFC=2
      ELSE
         IFC=1
      ENDIF
 
*     is it MZ or RZ of FZ
 
      IF(BRPATH(IFC:IFC+1).EQ.'R_')GOTO 100
      IF(BRPATH(IFC:IFC+1).EQ.'F_')GOTO 200
 
*     do MZ stuff
 
      IF      (INDEX(BRPATH,'Div').NE.0)THEN
         IFC=INDEX(BRPATH,'Div')+3
         READ(BRPATH(IFC:IFC+1),'(I2)') IXDIV
         IFC=INDEX(BRPATH,'Store')
         IF(IFC.GT.0)THEN
            IFC=IFC+5
*           switch to store + division
            READ(BRPATH(IFC:IFC+1),'(I2)') IST
         ELSE
            WRITE(*,*)'Incomplete path specified: ',BRPATH
            GOTO 999
         ENDIF
         CALL SBYT(IST,IXDIV,27,6)
         CALL MZSDIV(IXDIV,1)
*        banks
         IF(OBNAME.EQ.' ') THEN
            LGOP=0
            NUMB=0
            IF(LTBROW(IACTST).NE.0)THEN
               IIST=0
               CALL SBYT(IACTST,IIST,27,6)
               CALL MZSDIV(IIST,-7)
               CALL MZDROP(IIST,LTBROW(IACTST),' ')
               LTBROW(IACTST)=0
            ENDIF
            CALL MZSDIV(IXDIV,1)
            IACTST=IST
*           get number of banks
            NUMB=0
5           CALL  DZDNBK(IXDIV,LGOP,LBNK,LFW,LLW,IDH,IDRFLG)
            LGOP=LBNK
            IF(LBNK.NE.0)THEN
*              dropped?
               IF(IDRFLG.EQ.0)NUMB = NUMB+1
               GOTO 5
            ENDIF
            IIST=0
            CALL SBYT(IST,IIST,27,6)
            IXD20=IIST+20
            IF(LTBFLG(IST).EQ.0)THEN
               CALL MZLINK(IIST,'DZDLNT',
     &         LTBROW(IST),LTBROW(IST),LTBROW(IST))
               LTBFLG(IST)=1
               DO 6 K=21,24
6               CALL MZXREF(IXD20,K+IIST,'A')
            ENDIF
            NUMB=NUMB+1
            CALL MZBOOK(IXD20,LTBROW(IST),LTBROW(IST),1,
     &      'ZBRO',NUMB,0,1,2,0)
            IQ(LTBROW(IST)+KQS+1)=IXDIV
            NUMB=0
         ENDIF
         OBNAME=' '
         STEXT=' '
         LTEXT='Bank'
         OBCLAS='BANK'
10       CALL  DZDNBK(IXDIV,LGOP,LBNK,LFW,LLW,IDH,IDRFLG)
         LGOP=LBNK
         IF(LBNK.NE.0)THEN
*           dropped?
            IF(IDRFLG.NE.0)GOTO 10
            NID=IQ(LBNK+KQS-5)
            IF(NIDMSK.NE.0)THEN
               IF(NID.NE.NIDMSK)GOTO 10
            ENDIF
            CALL UHTOC(IDH,4,OBNAME,4)
*           check mask
            IF(CIDMSK.NE.'    ')THEN
               DO 15 I=1,4
                  IF(CIDMSK(I:I).NE.' ')THEN
                     IF(CIDMSK(I:I).NE.OBNAME(I:I))THEN
                        OBNAME(1:4)=' '
                        GOTO 10
                     ENDIF
                  ENDIF
15             CONTINUE
            ENDIF
 
            NUMB = NUMB+1
            WRITE(STEXT,'(I8)')NUMB
 
            LQ(LTBROW(IST)+KQS-NUMB)=LBNK
            WRITE(CTEMP,'(I8)')NID
            IFC=INDEXN(CTEMP,' ')
            IFC=MAX(IFC,1)
            ILC=LENOCC(OBNAME)+1
            OBNAME(ILC:ILC)='.'
            ILC=ILC+1
            OBNAME(ILC:)=CTEMP(IFC:)
            ILC=LENOCC(OBNAME)
            WRITE(CTEMP,'(I8)')NUMB
            IFC=INDEXN(CTEMP,' ')
            IFC=MAX(IFC,1)
            OBNAME(ILC+1:ILC+1)=' '
            OBNAME(ILC+2:)=CTEMP(IFC:)
            GOTO 999
         ENDIF
      ELSE IF (INDEX(BRPATH,'Store').NE.0)THEN
         IFC=INDEX(BRPATH,'Store')+5
*        switch to store
         READ(BRPATH(IFC:IFC+1),'(I2)') IST
         IIST = 0
         CALL SBYT(IST,IIST,27,6)
         CALL MZSDIV(IIST,-7)
         IACTST=IST
*        divisions
         IF(OBNAME.EQ.' ') THEN
            ICDIV=0
         ENDIF
         OBNAME=' '
20       ICDIV=ICDIV+1
         IF(ICDIV.LE.20)THEN
            IFAD = LQSTA(KQT+ICDIV)
            ILAD = LQEND(KQT+ICDIV)
            IF(IFAD .NE. 0 .AND. IFAD .NE. ILAD)THEN
               CALL UHTOC(IQDN1(KQT+ICDIV),4,CTEMP(1:4),4)
               CALL UHTOC(IQDN2(KQT+ICDIV),4,CTEMP(5:8),4)
               DO 25 I=1,8
                  IF(CTEMP(I:I).EQ.'/')CTEMP(I:I)='_'
25             CONTINUE
               WRITE(CDIV,'(I2.2)')ICDIV
               OBNAME= 'Div'//CDIV//'.'//CTEMP
               STEXT='Div'
               LTEXT='Division'
               OBCLAS='DIV'
               GOTO 999
            ELSE
               GOTO 20
            ENDIF
         ENDIF
      ENDIF
*     end of MZ stuff
      GOTO 999
 
100   CONTINUE
 
*     RZ stuff
*     save current directory
      IF(ISAVD.EQ.0)THEN
         CALL RZCDIR(CHPATH,'R')
         ISAVD=1
      ENDIF
      IF(BRPATH.NE.' ')THEN
         IF(ISAVD.EQ.1)THEN
            LBRPAT = '//'//BRPATH(IFC+2:LENOCC(BRPATH))
            CALL RZCDIR(LBRPAT,' ')
            ISAVD=2
         ENDIF
         IF(OBNAME.EQ.' ') THEN
            ICONTD=0
            ICONTK=0
         ENDIF
      ENDIF
      OBNAME=' '
      CHTITL=' '
      IF(ICONTD.GE.0)THEN
*        next ord directory
         CALL DZDNDI(CHTITL,ICONTD,IRET)
         IF(ICONTD.EQ.0)THEN
            ICONTD=-1
         ELSEIF(IRET.EQ.0)THEN
            OBCLAS='rzdir'
            OBNAME=CHTITL
            STEXT='d'
            LTEXT='directory'
            GOTO 888
         ENDIF
      ENDIF
*     next key
105   ICONTK=ICONTK+1
      ICYCLE=ICYDSP
      CALL RZIN(0,LL,2,ICONTK,ICYCLE,'CS')
      IF(IQUEST(1).EQ.0)THEN
*        dont show if append mode ?
         IF(IAPPFL.EQ.0 .AND. IAND(IQUEST(14),8).NE.0)GOTO 105
*        look for mask
 
         IF(NKYMSK.GT.0)THEN
            NWKEY=MIN(IQUEST(8),9)
            DO 106 I = 1,NWKEY
               IF(KACMSK(I).NE.0)THEN
                  IF(KEYMSK(I).NE.IQUEST(20+I))GOTO 105
               ENDIF
106         CONTINUE
         ENDIF
         IF(ISAVD.EQ.2)THEN
            CALL RZKEYD(NWKEY,CHFORM,CHTAG)
            ISAVD=3
         ENDIF
*         NWKEY=MIN(NWKEY,2)
         CHKEY=' '
         IP=1
         IF(IKDSP1.LE.0 .OR. IKDSP1.GT. NWKEY)THEN
            IK1=1
         ELSE
            IK1=IKDSP1
         ENDIF
         IF(IKDSP2.LE.0 .OR. IKDSP2.GT. NWKEY)THEN
            IK2=2
         ELSE
            IK2=IKDSP2
         ENDIF
         DO 110 I=1,NWKEY
            IF(I.NE.IK1 .AND. I.NE.IK2)GOTO 110
            CF=CHFORM(I:I)
            IF(CF.EQ.'A' .OR. CF.EQ.'H')THEN
               CALL UHTOC(IQUEST(20+I),4,CHKEY(IP:IP+3),4)
               IF(I.LT.NWKEY .AND. CF.EQ.'H')CHKEY(IP+4:IP+4)='_'
            ELSE
               WRITE(CHKEY(IP:IP+10),'(I11)')IQUEST(20+I)
               IF(I.LT.NWKEY)CHKEY(IP+11:IP+11)='_'
            ENDIF
110      IP=IP+12
         CHKEY=SPACES(CHKEY,0)
         NCH=LENOCC(CHKEY)
         WRITE(CHKEY(NCH+2:),'(I10)')ICONTK
         CHKEY = SPACES(CHKEY,1)
         OBNAME = CHKEY
         OBCLAS = 'KEY'
         CALL RZDATE(IQUEST(14),IDATE,ITIME,1)
         WRITE(LTEXT,'(I6,A,I4)')IDATE,'/',ITIME
         WRITE(STEXT,'(I10)')ICONTK
      ELSE
         CALL RZCDIR(CHPATH,' ')
         ISAVD=0
         OBNAME = ' '
      ENDIF
 
*   888   CALL RZCDIR(CHPATH,' ')
888   CONTINUE
 
*      end of RZ stuff
 
200   CONTINUE
 
*     FZ stuff
 
999   CONTINUE
      END
*********************************************************************
