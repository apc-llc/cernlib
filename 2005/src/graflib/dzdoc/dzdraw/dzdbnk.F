*
* $Id: dzdbnk.F,v 1.1.1.1 1996/03/04 16:13:09 mclareni Exp $
*
* $Log: dzdbnk.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:09  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDBNK(LB,IB,L,X0, Y0, DX, DY, HID, HIDNX, HIDU,
     &          NID, ND)
*.
*...DZDBNK  draw a bank (center, data box, next bank)
*.
*. INPUT     :   X0, Y0 lower left corner of bank center
*.               DX     length of the box for data
*.               DY     thickness of bank
*.               HID    hollerith Id of bank
*.               HIDNX   hollerith Id of next bank
*.               HIDU    hollerith Id of up - bank
*.               NID     numerical Id of bank
*.               ND      number of data words
*. OUTPUT    :   none
*.
*. COMMON    :
*. SEQUENCE  : DZDCHV DZDPRM
*. CALLS     : DZDBOX DZDGDS DZDLI2 DZDTXT
*. CALLED    : DZDRA2
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
      CHARACTER*4 HID, HIDNX,HIDU
      INTEGER IB(9),LB(9)
      CHARACTER*8 CHKEY
      CHARACTER*12 CHFORM,CTEMP3
      CHARACTER*40 CTITL
      CHARACTER*20 CHNID,HIDPR,CHND
#include "zebra/mzbits.inc"
#include "dzdprm.inc"
#include "dzdchv.inc"
#include "dzdzbrinc.inc"
      INTEGER ITITLE(4),IFC,ILC,IFORM,IST,IEND
      REAL XPOL(6),YPOL(6)
*     shaded box of center
      X0D = X0 + 2.5*GRIDX
*10/2      X0D = X0 + GRIDX
      IF(LTDISP(ISTUSE).NE.0)THEN
         IF(NLKUSE.GE.IB(LTDISP(ISTUSE)-3))THEN
*            WRITE(*,*)'DZDBNK: Too many banks'
         ELSE
            NLKUSE=NLKUSE+1
            CALL IGPID(1,'BANK',NLKUSE,' ')
            LB(LTDISP(ISTUSE)-NLKUSE)= L
         ENDIF
      ENDIF
      IF(LFCOL.NE.0)THEN
        ICOL=IBCCOL
        IFILL=1
      ELSE
        ICOL=-1
        IFILL=0
      ENDIF
      IF(LFCOL.NE.0 .OR. LTDISP(ISTUSE).NE.0 .AND. IFLDIR.EQ.0)
     &   CALL DZDIFA(X0,X0D,Y0,Y0+DY,IFILL,ICOL)
 
      CALL IGQ('LWID',WIDL)
      CALL ISLWSC(5.)
*      CALL DZDBOX(X0,X0D,Y0,Y0+DY,1000*IFLDIR+3)
      CALL DZDBOX(X0,X0D,Y0,Y0+DY,1000*IFLDIR)
      CALL ISLWSC(WIDL)
 
*10/2     CALL DZDBOX(X0,X0D,Y0,Y0+DY,1000*IFLDIR+3)
 
*     with option 'B' => IFLDIR = 1, (directory tree)
*     take text in data from bank itself
*     and dont write Id, ND ..
 
      IF(IFLDIR .NE. 0)THEN
*         RZ-directory
          IF(IB(L+1) .EQ. 1)THEN
*         box for data area
*          CALL IGBOX(X0D, X0D+DX, Y0, Y0+DY)
          CALL DZDBOX(X0D, X0D+DX, Y0, Y0+DY,1000)
*         get characters fitting in CLINE from end of bank or all
              LC = LEN(CLINE)
              LC4 = LC/4
              NWNAME = IB(L-1)-IB(L+2)
              IF(NWNAME .GT. LC4)THEN
                 IFW = IB(L+2)+NWNAME-LC4+1
                 NC = LC
              ELSE
                 IFW=IB(L+2)+1
                 NC = NWNAME*4
              ENDIF
              CLINE = ' '
              CALL UHTOC(IB(L+IFW),4,CLINE,NC)
*             get subdir name starting from /
              ILC = INDXBC(CLINE(1:NC),' ')
              IF(ILC.LE.0)ILC=NC
              ISL  = INDEXB(CLINE(1:NC),'/')
              IF(ISL.LE.1)THEN
                 ISL=1
              ELSE
                 IF(CLINE(ISL-1:ISL-1).EQ.'/')ISL=ISL-1
              ENDIF
              NC = ILC - ISL + 1
              XT= X0D + DX + 0.5 * GRIDX
              YT= Y0 + 0.6*DY
*               CALL DZDTXT('Dir:',4,XT,YT,CSIZE,0.,1)
              YT = YT - 1.1*CSIZE
              CALL DZDTXT(CLINE(ISL:ILC),NC,XT,YT,CSIZE,0.,1)
              NKEY = IB(L+3)
*             tagwords
              NWKEY=IB(L+4)
              XT= X0D + 0.5*DX
              YT= Y0 + 0.5*DY
              NCH=0
              IF(LUDORZ.NE.0)THEN
                 CTITL=' '
                 CALL CLTOU(CLINE)
                 CALL DZDIRD(LUDORZ,CLINE,CTITL,1)
*                  WRITE(*,*)' DZDBNK: ',CTITL
                 NCH=LENOCC(CTITL)
                 IF(NCH.GT.0)
     &           CALL DZDTXT(CTITL(1:NCH),NCH,XT,YT,CSIZE,0.,2)
              ENDIF
              IF(NCH.EQ.0)THEN
                 WRITE(CLINE,'(A,I5,A,I6)')
     &           ' Keys:',NKEY,' Keywords',NWKEY
                 CALL DZDTXT(CLINE(1:32),32,XT,YT,CSIZE,0.,2)
              ENDIF
              IF(NWKEY.GT.0 .AND. NKEY .GT.0 .AND. IFLPKY.GT.0)THEN
                  IOFFKT = 7 + (NWKEY+3)/4
                  NWKEY1=MIN(NWKEY,5)
                  IPL=2
                  CLINE = ' '
                  DO 10 K=1,NWKEY1
                      CALL UHTOC(IB(L+IOFFKT),4,CHKEY,8)
*                     right align
                      ILC = INDXBC(CHKEY,' ')
                      IS = 8-ILC
                      CLINE(IPL+IS:)=CHKEY(1:ILC)
                      IPL = IPL+9
                      IOFFKT=IOFFKT+2
10                CONTINUE
                  XT=X0+1.5*GRIDX+CSIZE
                  YT=Y0-GRIDY+.5*CSIZE
                  IPL = IPL-1
*  print keynames
                  CALL DZDTXT(CLINE(1:IPL),IPL,XT,YT,CSIZE,0.,1)
              ENDIF
          ENDIF
*         its a key
          IF(IB(L+1).EQ.2)THEN
          CALL DZDBOX(X0D, X0D+DX, Y0, Y0+DY,0)
              NWKEY = IB(L+6)
              NSKEY = 6+(NWKEY+3)/4
              NWKEY1 = MIN(NWKEY,12)
              CALL UHTOC(IB(L+7),4,CHFORM,NWKEY1)
              IPL = 1
              CLINE = ' '
              DO 20 K=1,NWKEY1
                  IF(CHFORM(K:K).EQ.'I')THEN
                      IVALU=IB(L+NSKEY+K)
*                     look for its tag
                      LUDIR=LB(L+1)
                      IOFFKT = 7 + (NWKEY+3)/4 + (K-1)*2
                      CALL UHTOC(IB(LUDIR+IOFFKT),4,CHKEY,8)
                      IF(CHKEY(1:6).EQ.'TSTAMP')THEN
                         CALL RZDATE(IVALU,IDATE1,ITIME1,1)
                         WRITE(CTEMP3,'(I7,A,I4)')IDATE1,'/',ITIME1
                      ELSE
                         WRITE(CTEMP3,'(I12)')IVALU
                      ENDIF
*                      WRITE(CLINE(IPL:),'(I9)')IB(L+NSKEY+K)
                  ELSEIF(CHFORM(K:K).EQ.'H'.OR.CHFORM(K:K).EQ.'A')THEN
                      WRITE(CTEMP3,'(''        '',A4)')IB(L+NSKEY+K)
*                      WRITE(CLINE(IPL:),'(''     '',A4)')IB(L+NSKEY+K)
                  ELSEIF(CHFORM(K:K).EQ.'B')THEN
                      WRITE(CTEMP3,'(Z12)')IB(L+NSKEY+K)
*                      WRITE(CLINE(IPL:),'(Z9)')IB(L+NSKEY+K)
                  ENDIF
                  IFC=INDEXC(CTEMP3,' ')
                  IF(IFC.LE.0)IFC=12
                  NC1=12-IFC
*                 max 36 characters fit in box
                  IF(IPL+NC1.GT.36)GOTO 21
                  CLINE(IPL:IPL+NC1)=CTEMP3(IFC:12)
                  IPL = IPL+NC1+2
20            CONTINUE
21            CONTINUE
              IPL = IPL-1
              XT = X0+GRIDX+0.5*CSIZE
              YT = Y0+.25*CSIZE
              CALL DZDTXT(CLINE(1:IPL),IPL,XT,YT,CSIZE,0.,1)
 
              WRITE(CLINE,'(A,I7)')'ND=',IB(L+5)
              XT= X0D + DX + 0.5 * GRIDX
              YT= Y0 + DY - CSIZE
              CALL DZDTXT(CLINE(1:10),10,XT,YT,CSIZE,0.,1)
              CALL RZDATE(IB(L+2),IDATE1,ITIME1,1)
              WRITE(CLINE,'(A,I6,A,I4)')'TS: ',IDATE1,'/',ITIME1
              YT = YT-1.5*CSIZE
              CALL DZDTXT(CLINE(1:15),15,XT,YT,CSIZE,0.,1)
              IFORM=JBYT(IB(L+2),1,3)
              IF(IFORM.NE.0)THEN
                 XT=X0
                 YT=Y0+0.8*DY
                 CALL DZDTXT('V',1,XT,YT,CSIZE,0.,1)
              ENDIF
              XT=X0-2.5*GRIDX
              YT=Y0
              WRITE(CLINE(1:5),'(I5)')IB(L+3)
              CLINE(1:1)='S'
              CALL DZDTXT(CLINE(1:5),7,XT,YT,CSIZE,0.,1)
              YT=Y0+1.5*CSIZE
              WRITE(CLINE(1:5),'(I5)')IB(L+4)
              CLINE(1:1)='C'
              CALL DZDTXT(CLINE(1:5),5,XT,YT,CSIZE,0.,1)
          ENDIF
          GOTO 999
      ENDIF
 
*     ZEBRA bank
 
      IF(JBIT(IB(L),IQDROP).EQ.0)THEN
          ISHFLG = 0
      ELSE
          ISHFLG=3
      ENDIF
*     CALL  IGBOX(X0D, X0D+DX, Y0, Y0+DY)
      IF(LFCOL.NE.0)THEN
         ICOL=IBDCOL
         IFILL=1
      ELSE
         ICOL=-1
         IFILL=0
      ENDIF
      IF(LFCOL.NE.0 .OR. LTDISP(ISTUSE).NE.0)
     &    CALL  DZDIFA(X0D, X0D+DX, Y0, Y0+DY,IFILL,ICOL)
      CALL  DZDBOX(X0D, X0D+DX, Y0, Y0+DY,ISHFLG)
      CALL IGPID(1,'DUMMY',99,' ')
*       next bank
      IF(HIDNX .NE. '    ')THEN
         XPOL(1) = X0D + DX
         XPOL(2) = XPOL(1)+ 1.5*GRIDX
         XPOL(3) = XPOL(2)+ GRIDX
         XPOL(4) =XPOL(2)
         XPOL(5) =XPOL(1)
         XPOL(6) =XPOL(1)
         YPOL(1) = Y0
         YPOL(2) = YPOL(1)
         YPOL(3) = YPOL(2)+DY/2.
         YPOL(4) = YPOL(1)+DY
         YPOL(5) = YPOL(4)
         YPOL(6) = YPOL(1)
 
         IF(LTDISP(ISTUSE).NE.0)THEN
            IF(NLKUSE.GE.IB(LTDISP(ISTUSE)-3))THEN
*               WRITE(*,*)'DZDBNK: Too many banks'
            ELSE
               NLKUSE=NLKUSE+1
               CALL IGPID(1,'BANK',NLKUSE,' ')
               LB(LTDISP(ISTUSE)-NLKUSE)=  LB(L)
            ENDIF
         ENDIF
         IF(LTDISP(ISTUSE).NE.0 .OR. LFCOL.NE.0)THEN
*           cyan
            CALL ISFACI(IBCCOL)
            CALL IFA(6,XPOL,YPOL)
         ENDIF
         CALL DZDGPL(6,XPOL,YPOL)
         HIDPR=HIDNX
         IF(IFLRZF .GT. 0 .AND. IFRCAR.EQ.0)THEN
*           has it a next bank
            LNEXT=LB(L)
            IF(LNEXT.LE.2)GOTO 35
            LUPUP=LB(LNEXT+1)
            IF(LUPUP.LE.2)THEN
               CHKEY(5:8)='NONE'
            ELSE
               CALL UHTOC(IB(LUPUP-4),4,CHKEY(5:8),4)
            ENDIF
            CALL UHTOC(IB(LNEXT-4),4,CHKEY(1:4),4)
            CALL DZDGDS(CHKEY,' ',CLINE,NLT)
            IF(NLT .LE. 0)GOTO 35
*            look if descriptor should be taken from bank
            IFC=INDEX(CLINE,'HID@D')
            IF(IFC.LE.0)GOTO 35
            IFC=IFC+5
            CALL DZDGFL(CLINE(IFC:),IST,IEND)
            IF(IST.EQ.0 .OR. IEND .LT.IST)GOTO 35
            IEND=MIN(IEND,ND)
            NC=MIN(IEND-IST+1,5)*4
            LNEXT=LB(L)
            CALL UHTOC(IB(LNEXT+IST),4,HIDPR,NC)
         ENDIF
35       CONTINUE
         XT=0.5*(XPOL(1)+XPOL(2)+0.5*GRIDX)
         YT=YPOL(3)
         CALL DZDTXT(HIDPR,4,XT,YT,CSIZE,0.,2)
         CALL IGPID(1,'DUMMY',99,' ')
      ENDIF
      CHND=' '
      CHNID=' '
      HIDPR=HID
      ISTA=5
 
*       descriptor in data box
 
      IF(IFLRZF .GT. 0)THEN
          CHKEY(1:4)=HID
          CHKEY(5:8)=HIDU
          CALL DZDGDS(CHKEY,' ',CLINE,NLT)
*
          IF(NLT .GT. 0)THEN
 
*            look if descriptor should be taken from bank
 
             IF(IFRCAR.GT.0)GOTO 60
             IFC=INDEX(CLINE,'HID@D')
             IF(IFC.LE.0)GOTO 40
             IF(IFC.EQ.1)ISTA=1
             IFC=IFC+5
             CALL DZDGFL(CLINE(IFC:),IST,IEND)
             IF(IST.GT.0 .AND. IEND .GE.IST)THEN
                IEND=MIN(IEND,ND)
                NCOPY=MIN(IEND-IST+1,5)
                CALL UHTOC(IB(L+IST),4,HIDPR,NCOPY*4)
             ENDIF
40           CONTINUE
 
             IFC=INDEX(CLINE,'ND@D')
             IF(IFC.LE.0)GOTO 45
             IF(IFC.EQ.1)ISTA=1
             IFC=IFC+4
             CALL DZDGFL(CLINE(IFC:),IST,IEND)
             IF(IST.GT.0 .AND. IEND .GE.IST)THEN
                IEND=MIN(IEND,ND)
                NCOPY=MIN(IEND-IST+1,5)
                CALL UHTOC(IB(L+IST),4,CHND,NCOPY*4)
             ENDIF
45           CONTINUE
 
             IFC=INDEX(CLINE,'NID@D')
             IF(IFC.LE.0)GOTO 50
             IF(IFC.EQ.1)ISTA=1
             IFC=IFC+5
             CALL DZDGFL(CLINE(IFC:),IST,IEND)
             IF(IST.GT.0 .AND. IEND .GE.IST)THEN
                IEND=MIN(IEND,ND)
                NCOPY=MIN(IEND-IST+1,5)
                CALL UHTOC(IB(L+IST),4,CHNID,NCOPY*4)
             ENDIF
50           CONTINUE
 
             IFC=INDEX(CLINE,'TITLE@D')
             IF(IFC.LE.0)GOTO 60
             IF(IFC.EQ.1)ISTA=1
             IFC=IFC+7
             CALL DZDGFL(CLINE(IFC:),IST,IEND)
             IFORM=0
             IFC=INDEX(CLINE(ILC+1:),'FORM')
             IF(IFC.NE.0)THEN
                IFC=IFC+ILC+4
                IF(CLINE(IFC:IFC).EQ.'Z')THEN
                   IFORM=1
                ENDIF
             ENDIF
             CLINE(ISTA:)=' '
             IEND=MIN(IEND,ND)
             NCOPY=MIN(IEND-IST+1,10)
             IF(NCOPY.LE.0)GOTO 60
             IF(IFORM.EQ.1)THEN
                CALL ZITOH(IB(L+IST),ITITLE,NCOPY)
                CALL UHTOC(ITITLE,4,CLINE(ISTA:),NCOPY*4)
             ELSE
                CALL UHTOC(IB(L+IST),4,CLINE(ISTA:),NCOPY*4)
             ENDIF
60           CONTINUE
             XT= X0D + 0.5*DX
             YT= Y0 + 0.5*DY
*            centered
             IALG = 2
             NCM =1.5*DX/CSIZE
             CALL DZDTXT(CLINE,NCM,XT,YT
     &       ,CSIZE,0.,IALG)
          ENDIF
      ENDIF
 
 
*     # of data in bank
      XT = X0D + DX
      YT = Y0 + DY + 0.1*CSIZE
*       ! right adjust
      IALG = 3
      CLINE=' '
      IF(CHND.EQ.' ')THEN
         IF(ND .GE. 0)THEN
           WRITE(CLINE,'(I10)')ND
         ELSE
           WRITE(CLINE,'(10A)')'      **'
         ENDIF
         IFC = INDEXC(CLINE,' ')
         IFC = IFC - 3
         IF(IFC .LT. 1) IFC = 1
         CLINE(IFC:IFC+2) = 'ND='
         ILC=10
      ELSE
         IFC=1
         ILC=LNBLNK(CHND)
         CLINE(1:ILC)=CHND(1:ILC)
      ENDIF
      CALL DZDTXT(CLINE(IFC:ILC)
     &  ,ILC-IFC+1,XT,YT,CSIZE,0.,IALG)
 
*     numerical ID
 
      IF(IFRCAR .EQ. 0)THEN
         IALG=1
         YT=Y0+1.1*DY
         XT=X0+GRIDX
         CLINE=' '
         IF(CHNID.EQ.' ')THEN
            WRITE(CLINE,'(I14)')NID
            IFC = INDEXC(CLINE,' ')
            IFC=MAX(IFC-4,1)
            CLINE(IFC:IFC+2)='NID'
            ILC=14
         ELSE
            IFC=1
            ILC=LNBLNK(CHNID)
            CLINE(1:ILC)=CHNID(1:ILC)
         ENDIF
         CALL DZDTXT(CLINE(IFC:ILC)
     &   ,ILC-IFC+1,XT,YT,CSIZE,0.,IALG)
      ENDIF
 
*     bank name (Hollerith ID)
 
      XT= X0 + 1.25*GRIDX
      YT= Y0 + 0.5*DY
      IALG=2
      CALL DZDTXT(HIDPR,20,XT,YT,CSIZE,0.,IALG)
 
999   END
************************************************************************
