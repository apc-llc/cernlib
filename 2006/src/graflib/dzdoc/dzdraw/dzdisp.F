*
* $Id: dzdisp.F,v 1.1.1.1 1996/03/04 16:13:06 mclareni Exp $
*
* $Log: dzdisp.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:06  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDISP(IXSTOR,LTOPIN,
     + RZPATH,CHOPT, IWDISP, IWMETA, ILOCNR, IWKTYP)
*.
*...DZDISP
*.
*. INPUT     : IXSTOR   Store index
*.             LTOPIN   Link to top bank to be displayed
*.             RZPATH   Pathname in RZ-file for bank documentation
*.             CHOPT    Character option:
*.                      'N' dont activate workstation (done by caller)
*.                      'M' write on metafile also the menu boxes
*.                      'V' caller is DZDDIV
*.                      'W' call GSWKVP and GSWKWN
*.                      'L' return link of selected bank to caller
*.                          in LTOPIN
*.                      'Q' handle "Modify data" as "Quit"
*.                      'D' draw the picture only, no locator requested
*.                      'G' continue with down banks
*.             IWDISP   workstation Id for display (screen)
*.             IWMETA   Id for possible metafile (0 if none)
*.             ILOCNR   locator number for cursor input
*.             IWKTYP   workstation type
*.
*. OUTPUT    : none
*.
*. COMMON    :
*. SEQUENCE  : DZDCHV DZDPRM MZCA   MZCB   QSTORE ZEBQ   ZUNIT
*. CALLS     : DZDBOX DZDDAW DZDGET DZDGST DZDOCO DZDRAW DZDSYW
*. CALLS     : DZDTXT GACWK  GCLRWK GDAWK  GQCNTN GQNT
*. CALLS     : GSELNT RZCDIR UHTOC
*. CALLED    : USER
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*. LAST MOD  : 24-Dec-91     O.Schaile
*.
*. Modification Log.
*. 4.Jun-88    Save LTOP, LRET by MZLINT
*.
*.**********************************************************************
*.
      CHARACTER*(*) RZPATH, CHOPT
*
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
      INTEGER IQUEST(100)
      REAL RQUEST(100)
      EQUIVALENCE (IQUEST,RQUEST)
      COMMON/QUEST/RQUEST
#include "zebra/zunit.inc"
#include "dzdprm.inc"
#include "dzdchv.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      CHARACTER CQSTAK*13,CQINFO*40
      PARAMETER (NLICHQ=130,NSTCHQ=8,NDVCHQ=8,NBKCHQ=4 )
      CHARACTER CQLINE*(NLICHQ),CQMAP(10)*(NLICHQ)
      CHARACTER CQSTOR*(NSTCHQ),CQDIV*(NDVCHQ),CQID*(NBKCHQ)
      COMMON /DZC1CH/ CQSTOR,CQDIV,CQID,CQMAP,CQSTAK,CQINFO
      EQUIVALENCE (CQLINE,CQMAP)
*      COMMON/DZDLK1/LAREF(2),LTOP,LSEL
      COMMON/DZDLK1/LNKLOC(4,0:15)
#include "dzdzbrinc.inc"
      LOGICAL DZDINB
      EXTERNAL DZDINB
*
      CHARACTER*12 CTEMP ,CHOPT1
      CHARACTER*4 CHOPDZ
      CHARACTER*100 CWDCUR
      CHARACTER*8 CHKEY,CHOPTM
      REAL RVALQ(11)
      PARAMETER(NMITEM=9, NPAR=6)
      CHARACTER*16 CHITEM(NMITEM),CHUSER(2),
     +CPNAME(NPAR), CPVAL(NPAR), CPDEF(NPAR)
      CHARACTER*8 CMENU
      PARAMETER (NMODMN=3)
      CHARACTER*8 CMODUS(2)
      CHARACTER*20 CMODTI
      CHARACTER*12 CMODMN(NMODMN)
      CHARACTER*12 CMODVA(NMODMN)
      CHARACTER*12 CMODDF(NMODMN)
      CHARACTER*1 CMMENU
*
      INTEGER IFIRST,ILAST,KEYV(2), ITFONT,ITPREC, ILKACT,
     &        LSAVE, INIFLG
      LOGICAL LACTWK,REQINP,NEWFIL
*      SAVE NEWFIL
*      SAVE INIFLG, METAFL,IPCSEQ, IOPFIL, ITFONT,ITPREC, ILKACT, LSAVE
 
      SAVE
 
      DATA NEWFIL/.TRUE./
      DATA IFIRST/0/, ILAST/0/, ITFONT/1/,ITPREC/0/
      DATA INIFLG/0/, IPCSEQ/0/, IOPFIL /0/
      DATA CHOPDZ/'    '/,CHOPTM/'ADVS'/
*----
      IF(LTOPIN.EQ.0)THEN
         WRITE(*,*)'DZDISP: Ltopin = 0'
         RETURN
      ENDIF
      CALL IGQWK(IWDISP,'OPEN',RVALQ)
      IF(RVALQ(1).LT.1. .AND. INDEX(CHOPT,'D').EQ.0)THEN
         WRITE(*,*)'DZDISP: Workstation', IWDISP, ' not open'
         RETURN
      ENDIF
#include "zebra/qstore.inc"
      IST = JQSTOR
      IF(LTOPIN.NE.LSAVE)ILKACT=0
      LSAVE=LTOPIN
      IF(INDEX(CHOPT,'G').EQ.0) ILKACT = 0
 
      IF(INIFLG.EQ.0)THEN
         IDVERS=0
         INIFLG=1
         CALL DZDIBR
*     set values for menus
         GRIDX=GRIDSC*1./32.
         GRIDY=GRIDX
         CMMENU=' '
         X0MEN=1.-5.1*GRIDX
         Y0MEN= FLOAT(NPAR+2)*0.8*GRIDY
         X1MEN=X0MEN+5.*GRIDX
         Y1MEN=Y0MEN+FLOAT(NMITEM)*0.8*GRIDY
         X0PAR=1.- 10.1*GRIDX
         Y0PAR=0.1*GRIDY
         X1PAR=X0PAR+10.*GRIDX
         Y1PAR=Y0PAR+FLOAT(NPAR+1)*0.8*GRIDY
         X0MODM=X0PAR-14.1*GRIDX
         Y0MODM=0.1*GRIDY
         X1MODM=X0MODM+14.*GRIDX
         Y1MODM=Y0MODM+FLOAT(NMODMN+2)*0.8*GRIDY
         CMODUS(1) = 'Quit'
         CMODUS(2) = 'Execute'
      ENDIF
      CALL MZLINT(IXSTOR,'/DSDLK1/',
     +LNKLOC(1,IST),LNKLOC(3,IST),LNKLOC(4,IST))
      LNKLOC(3,IST) = LTOPIN
 
      IF(INDEX(CHOPT,'D').NE. 0)THEN
*        links for the browser
         IF(LTDFLG(IST).EQ.0)THEN
            CALL MZLINK(IXSTOR,'/DZDLKP/',
     &               LTDISP(IST),LTDISP(IST),LTDISP(IST))
            LTDFLG(IST)=1
            IIST=0
            CALL SBYT(IST,IIST,27,6)
            DO 6 K=21,24
6            CALL MZXREF(IXSTOR,K+IIST,'A')
         ENDIF
         NLKUSE=0
         IF(LTDISP(IST).NE.0)THEN
            IF(LTOPIN.EQ.LTDISP(IST))THEN
               WRITE(*,*)'Cant show this bank, owned by ZBROWSE'
               GOTO 175
            ENDIF
            CALL MZDROP(IXSTOR,LTDISP(IST),' ')
            LTDISP(IST)=0
         ENDIF
         IIST=20
         CALL SBYT(IST,IIST,27,6)
         CALL MZBOOK(IIST,LTDISP(IST),LTDISP(IST),1,'ZBR1',50,0,0,0,0)
         ISTUSE=IST
      ENDIF
      IF(INDEX(CHOPT,'C').NE. 0)THEN
         LFCOL=1
      ELSE
         LFCOL=0
      ENDIF
      CHOPT1 = ' '
      ILC = INDXBC(CHOPT,' ')
      IF(ILC .GT. 0)THEN
         CHOPT1(1:)=CHOPT(1:ILC)
      ENDIF
      ILC = ILC+1
      IF(RZPATH  .NE. ' ')THEN
         CHOPT1(ILC:ILC) = 'R'
         CALL RZCDIR(CWDCUR,'R')
         CALL RZCDIR(RZPATH,' ')
      ENDIF
      IF(INDEX(CHOPT,'N').NE. 0)THEN
         LACTWK = .FALSE.
      ELSE
         LACTWK=.TRUE.
      ENDIF
 
      IFRDDV = INDEX(CHOPT,'V')
      CALL DZDGDV(CHOPT,IDV1)
      IF(IDV1.NE.0)IDVERS=IDV1
      XRANGE = 20.
      YRANGE = 20.
      CALL IGRNG( XRANGE, YRANGE)
 
      YBOTTM=0.
      IOPFIL=IQPRNT
      IF(LACTWK)CALL IACWK(IWDISP)
      METAFL = 0
   10 CONTINUE
*     clear workstations
      CALL ICLRWK(0,0)
*      CALL HPLNEW
      IF(IWMETA .GT. 0 .AND. METAFL .EQ. 1)THEN
*         IF(IWDISP.NE.0)CALL IDAWK(IWDISP)
         CALL IACWK(IWMETA)
         CMMENU='M'
         ILKACT=ILKSAV
         IF(INDEX(CHOPT,'M').EQ.0)THEN
            IF(INDEX(CHOPT,'W').NE.0)THEN
               XAPA = .1465
               CALL ISWKWN(IWMETA,0.,1.,YBOTTM,1.)
               CALL ISWKVP(IWMETA,0.,XAPA,0.,XAPA*(1.-YBOTTM))
            ENDIF
         ENDIF
         CMENU='DT'
         CALL ICLRWK(0,0)
*         CALL ICLRWK(IWMETA,1)
         CALL ISFASI(-103)
         IPCSEQ=IPCSEQ+1
         IF(IPCSEQ. GT. 999)IPCSEQ=1
         CALL UHTOC(IQ(LNKLOC(3,IST)+KQS-4),4,CTEMP(1:4),4)
         WRITE(CTEMP(5:7),'(I3.3)')IPCSEQ
      ELSEIF(IFOMED.NE.3)THEN
         CALL ICLRWK(IWDISP,1)
         CALL ISFASI(-5)
      ENDIF
      ILKSAV = ILKACT
      CALL DZDRA1(IXSTOR, LNKLOC(3,IST), ILKACT
     + , CHOPT1,XCUR1,YCUR1,.FALSE.,IFLC,YTOP,YBOTTM)
 
*     with Motif the rest is done outside
 
      IF(INDEX(CHOPT,'D').NE.0)GOTO 170
*
      CALL MZSDIV(IXSTOR,-7)
      IF(IWMETA .GT. 0 .AND. METAFL .EQ. 1 .AND. INDEX(CHOPT,'M').EQ.
     +0)GOTO 20
      LNKLOC(4,IST) = 0
      NT = 0
      IF(IWMETA .GT. 0 .AND. METAFL .EQ. 1
     +   .AND. INDEX(CHOPT,'M').EQ. 0)THEN
         CALL IDAWK(IWMETA)
         CMMENU=' '
*        CALL IACWK(IWDISP)
         METAFL = 0
      ENDIF
 
*     get locator input and take action
 
   20 CONTINUE
 
      ILKUSE = ILKSAV
      CHITEM(1) = 'Quit'
      IF(IFRDDV.NE.0)THEN
         CHITEM(2)='=>DZDDIV'
      ELSE
         CHITEM(2)=' '
      ENDIF
      IF(ILKACT.GT.0)THEN
         CHITEM(3)='Continue'
      ELSE
         CHITEM(3)=' '
      ENDIF
      IF(IWMETA.GT.0)CHITEM(4)='=>Plotf'
      CALL UHTOC(IQ(LNKLOC(3,IST)+KQS-4),4,CTEMP(1:4),4)
      CHITEM(5)='Modi '//CTEMP(1:4)
      CHITEM(6)='Drop '//CTEMP(1:4)
      CHITEM(7)='Help'
      CHITEM(8)='LaTeX'
      CHITEM(9)='TextStyl'
      CHUSER(1) = ' '
      CHUSER(2) = ' '
      CPNAME(1)='DZSHOW-O'
      CPDEF(1) = CHOPDZ
      CPNAME(2)='First W'
      CALL DZDPLA(IFIRST,CPDEF(2))
      CPNAME(3)='Last W'
      CALL DZDPLA(ILAST,CPDEF(3))
      CPNAME(4)='Unit-Nr'
      CALL DZDPLA(IQPRNT,CPDEF(4))
      CPNAME(5)='DZDTMP-O'
      CPDEF(5)=CHOPTM
      CPNAME(6)='DocVers'
      CALL DZDPLA(IDVERS,CPDEF(6))
 
*     display the menu
 
   30 CONTINUE
      CALL IGMENU(0,'Choose',X0MEN,X1MEN,Y0MEN,Y1MEN,
     +            0,CHUSER,NMITEM,CHITEM,
     +            CTEMP,CTEMP,ICHOIC,'DR'//CMMENU)
 
*     build the parameter menu
*     display the menu
      CALL IGMENU(0,'Parameters',X0PAR,X1PAR,Y0PAR,Y1PAR,
     +            0,CHUSER,NPAR,CPNAME,
     +            CPDEF,CPVAL,ICHOIC,'PDR'//CMMENU)
 
      IF(IWMETA .GT. 0 .AND. METAFL .EQ. 1)THEN
         METAFL=2
         CALL IDAWK(IWMETA)
         CMMENU=' '
*         CALL IACWK(IWDISP)
         CMENU='DHCT'
         GOTO 30
      ENDIF
 
      IF(IFOMED.EQ.3)THEN
         WRITE(LUNGRA,'(A)')' } '//BS//'ep'
         IFOMED=1
      ENDIF
 
      LNKLOC(4,IST) = LNKLOC(3,IST)
*    request choice
      CALL IGMENU(0,'Choose',X0MEN,X1MEN,Y0MEN,Y1MEN,
     +            0,CHUSER,NMITEM,CHITEM,
     +            CTEMP,CTEMP,ICHOIC,'RCH')
      IF(ICHOIC.EQ.-1000)ICHOIC=1
      IF(ICHOIC.NE.0)GOTO 70
 
      CALL IGMENU(0,'Parameter',X0PAR,X1PAR,Y0PAR,Y1PAR,
     +            0,CHUSER,NPAR,CPNAME,
     +            CPDEF,CPVAL,ICHOIC,'RPCN')
 
      CHOPDZ=CPVAL(1)(1:4)
      CALL CLTOU(CHOPDZ)
      CHOPTM=CPVAL(5)(1:8)
      CALL DZDCTI(CPVAL(2),IFIRST)
      CALL DZDCTI(CPVAL(3),ILAST)
      CALL DZDCTI(CPVAL(4),IQPRNT)
      CALL DZDCTI(CPVAL(6),IDVERS)
      IF(IQPRNT .NE. IOPFIL)THEN
         IF(IOPFIL.NE.6 .AND. IOPFIL.NE.0)THEN
            CLOSE(UNIT=IOPFIL)
         ENDIF
         NEWFIL=.TRUE.
         IF(IQPRNT.NE.6 .AND. IQPRNT.NE.0)THEN
            CTEMP(1:3)='FOR'
            WRITE(CTEMP(4:6),'(I3.3)')IQPRNT
            CALL KUOPEN(IQPRNT, CTEMP(1:6)//'.dat','UNKNOWN',ISTAT)
            IF(ISTAT.EQ.0)THEN
               WRITE(*,*)' Output will go on file: ',CTEMP(1:6)//'.dat'
            ELSE
               WRITE(*,*)' Error opening print file'
               IQPRNT=6
            ENDIF
         ENDIF
         IOPFIL = IQPRNT
      ENDIF
   50 CONTINUE
      DO 60 I=1,NPAR
   60 CPDEF(I)=CPVAL(I)
 
      IF(ICHOIC.LE.0)THEN
         PX=RQUEST(11)*XRANGE
         PY=RQUEST(12)*YRANGE
*         WRITE(*,*)RQUEST(11),RQUEST(12)
         CMENU='HCD'
         REQINP = .TRUE.
         LNKLOC(4,IST) = LNKLOC(3,IST)
         CALL DZDRA1(IXSTOR,LNKLOC(3,IST),ILKUSE
     +  ,CHOPT1, PX, PY, REQINP,IFLCUR,YTOP,YBOTTM)
         CALL MZSDIV(IXSTOR,-7)
         LSAVE = LNKLOC(3,IST)
         LNKLOC(3,IST) = LNKLOC(4,IST)
         LNKLOC(4,IST) = LSAVE
      ENDIF
 
   70 CONTINUE
 
*     quit
 
      IF(ICHOIC.EQ.1)THEN
         IF(IFRDDV.NE.0)THEN
            IFQUIT=1
         ELSE
            IFQUIT=0
         ENDIF
         GOTO 170
      ENDIF
 
*     help
 
      IF(ICHOIC.EQ.7)THEN
         CALL DZDHLP
         CALL DZDPTO(IWKTYP)
         GOTO 20
      ENDIF
*    => DZDDIV
      IF(IFRDDV .NE. 0 .AND. ICHOIC.EQ.2)THEN
         IFQUIT=0
         GOTO 170
      ENDIF
 
*     continue with down banks
 
      IF(ILKACT .NE. 0 .AND. ICHOIC.EQ.3)THEN
         LNKLOC(3,IST) = LNKLOC(4,IST)
         METAFL = 0
         GOTO 10
      ENDIF
 
*     LaTeX output
 
      IF(ICHOIC.EQ.8 .AND. METAFL.EQ.0)THEN
          LUNGRA=IQPRNT
          PAGECM=14./20.
          IFOMED=3
          IF(NEWFIL)THEN
             NEWFIL=.FALSE.
             CALL DZDLPE(LUNGRA)
          ENDIF
          WRITE(LUNGRA,'(A)')
     &    ' '//BS//'bp(14,14)(0, 0) '//BS//'thicklines {'
     &       //BS//'small '//BS//'sf '
          GOTO 10
      ENDIF
 
      IF(IFOMED.EQ.3)THEN
         WRITE(LUNGRA,'(A)')' } '//BS//'ep'
         IFOMED=1
      ENDIF
 
*     write on metafile
 
      IF(ICHOIC.EQ.4 .AND. IWMETA .GT. 0 .AND. METAFL .EQ. 0)THEN
         METAFL = 1
         IFOMED=1
         GOTO 10
      ENDIF

*     Drop bank + dependents
      IF(ICHOIC.EQ.6)THEN
*         build the menu
         WRITE(CMODTI,'(A,A4)') 'Drop Bank/Tree ',IQ(LNKLOC(3,IST)+KQS-
     +   4)
         CALL IGMENU(0,CMODTI,X0MODM,X1MODM,Y0MODM,Y1MODM, 2,CMODUS,0,
     +   CMODMN, CMODDF,CMODVA,ICHOIC,'CDR'//CMMENU)
 
*         quit?
         IF(ICHOIC.EQ.-1 .OR.ICHOIC.EQ.-1000)GOTO 80
         IF(ICHOIC.GE.0)GOTO 80
         CALL MZDROP(IXSTOR,LNKLOC(3,IST),' ')
         LNKLOC(3,IST)=0
   80    CALL IGMENU(0,CMODTI,X0MODM,X1MODM,Y0MODM,Y1MODM, 2,CMODUS,0,
     +   CMODMN, CMODDF,CMODVA,ICHOIC,'DR'//CMMENU)
 
         GOTO 10
      ENDIF
 
*     Modify data word
      IF(ICHOIC.EQ.5)THEN
         IF(INDEX(CHOPT,'Q').NE.0)THEN
            IF(IFRDDV.NE.0)THEN
               IFQUIT=1
            ELSE
               IFQUIT=0
            ENDIF
            GOTO 170
         ENDIF
*         build the menu
         CMODMN(1) = 'First word '
         CMODDF(1) = '1'
         CMODMN(2) = 'Last word '
         CMODDF(2) = '1'
         CMODMN(3) = 'New value '
         CMODDF(3) = ' '
         CMODVA(1)=' '
         CMODVA(2)=' '
         CMODVA(3)=' '
         WRITE(CMODTI,'(A,A4)') 'Modify data in ',IQ(LNKLOC(3,IST)+KQS-
     +   4)
   90    CALL IGMENU(0,CMODTI,X0MODM,X1MODM,Y0MODM,Y1MODM, 2,CMODUS,
     +   NMODMN,CMODMN, CMODDF,CMODVA,ICHOIC,'CPDR'//CMMENU)
 
*         quit?
         IF(ICHOIC.EQ.-1)GOTO 150
         IF(ICHOIC.EQ.-1000)GOTO 150
         IF(ICHOIC.GE.0)GOTO 90
         CALL DZDCTI(CMODVA(1),N1)
         IF(N1.GT.IQ(LNKLOC(3,IST)+KQS-1))THEN
            WRITE(6,*)' N1 bigger ND'
            GOTO 150
         ENDIF
         CALL DZDCTI(CMODVA(2),N2)
         IF(N2.GT.IQ(LNKLOC(3,IST)+KQS-1) .OR.N2 .LT. N1)THEN
 
            WRITE(6,*)' N2 > ND or N2 < N1'
            GOTO 150
         ENDIF
*         get old value
         IF(N1.GT.0)THEN
            CALL DZSHOW(' ',IXSTOR, LNKLOC(3,IST),'Q',1,0,N1,N1)
         ELSE
            WRITE(CQLINE(12:22),'(I11)') IQ(LNKLOC(3,IST)+KQS+N1)
 
         ENDIF
         IF(INDEX(CQLINE(12:22),'"').NE.0)THEN
            IOTYP=5
         ELSEIF(INDEX(CQLINE(12:22),'.').NE.0)THEN
            IOTYP=3
         ELSE
            IOTYP=2
         ENDIF
         IF(N1.EQ.N2)THEN
            WRITE(6,'(A,I6,A,A,A,Z9)') ' Word: ',N1,' Old value was: ',
     +      CQLINE(12:22) ,' Hex:',IQ(LNKLOC(3,IST)+KQS+N1)
 
         ELSE
            CALL DZSHOW('Old values were: ', IXSTOR, LNKLOC(3,IST),' ',
     +      1,0,N1,N2)
         ENDIF
         IF(IOTYP.EQ.5)THEN
            IFC=INDEXC(CMODVA(3),' ')
            IF(IFC.GT.0)THEN
               CTEMP(1:4)=CMODVA(3)(IFC:IFC+3)
            ELSE
               CTEMP(1:4)='    '
            ENDIF
            DO 100 K=N1,N2
  100       CALL UCTOH(CTEMP(1:4),IQ(LNKLOC(3,IST)+KQS+K),4,4)
         ELSEIF(IOTYP.EQ.3)THEN
            READ(CMODVA(3),'(F12.5)',ERR=140)REALVL
            DO 110 K=N1,N2
  110       Q(LNKLOC(3,IST)+KQS+K)=REALVL
         ELSE
            CALL DZDCTI(CMODVA(3),INTEVL)
            DO 120 K=N1,N2
  120       IQ(LNKLOC(3,IST)+KQS+K)=INTEVL
         ENDIF
         CMODDF(1)=CMODVA(1)
         CMODDF(2)=CMODVA(2)
         CMODDF(3)=CMODVA(3)
         GOTO 150
  140    WRITE(6,'(A,A)')' Illegal real number ',CMODVA(3)
 
  150    CALL IGMENU(0,CMODTI,X0MODM,X1MODM,Y0MODM,Y1MODM, 2,CMODUS,
     +   NMODMN,CMODMN, CMODDF,CMODVA,ICHOIC,'E')
 
         GOTO 30
      ENDIF
 
*     text style
      IF(ICHOIC.EQ.9)THEN
*         build the menu
         CMODMN(1) = 'TextFont '
         WRITE(CMODDF(1),'(I5)')ITFONT
         CMODMN(2) = 'Prec '
         WRITE(CMODDF(2),'(I5)')ITPREC
         CMODMN(3) = 'SizeFact'
         WRITE(CMODDF(3),'(F5.2)')CSFACT
         CMODVA(1)=' '
         CMODVA(2)=' '
         CMODVA(3)=' '
         WRITE(CMODTI,'(A)') 'Set Text font+size '
   95    CALL IGMENU(0,CMODTI,X0MODM,X1MODM,Y0MODM,Y1MODM, 2,CMODUS,
     +   NMODMN,CMODMN, CMODDF,CMODVA,ICHOIC,'CPDR'//CMMENU)
 
*         quit?
         IF(ICHOIC.EQ.-1)GOTO 155
         IF(ICHOIC.EQ.-1000)GOTO 155
         IF(ICHOIC.GE.0)GOTO 95
         CALL DZDCTI(CMODVA(1),ITFONT)
         CALL DZDCTI(CMODVA(2),ITPREC)
         READ(CMODVA(3),'(F12.5)',ERR=145)CSFACT
         CALL ISTXFP(ITFONT,ITPREC)
         GOTO 155
  145    WRITE(*,'(A,A)')' Illegal real number ',CMODVA(3)
 
  155    CALL IGMENU(0,CMODTI,X0MODM,X1MODM,Y0MODM,Y1MODM, 2,CMODUS,
     +   NMODMN,CMODMN, CMODDF,CMODVA,ICHOIC,'E')
 
         GOTO 30
      ENDIF
 
*     very top bank selected
 
      IF(IFLCUR .EQ. 1 .AND. LNKLOC(4,IST) .LE. 2)THEN
          IFQUIT=0
          GOTO 170
      ENDIF
*     display a bank or next bank selected
      IF(IFLCUR .EQ. 1 .OR. IFLCUR .EQ. 4)THEN
         IF(LNKLOC(3,IST) .NE. LNKLOC(4,IST))METAFL = 0
         LNKLOC(3,IST) = LNKLOC(4,IST)
         ILKACT = 0
         GOTO 10
      ENDIF
*     force DZDDWD with 'C' option
*     and DZDAW with W option
      IF(IFLCUR.EQ.3 .AND. INDEX(CHOPDZ,'C').NE.0)IFLCUR=6
      IF(IFLCUR.EQ.6 .AND. INDEX(CHOPDZ,'W').NE.0)IFLCUR=3
      IF(IFLCUR .EQ. 2)THEN
         CALL DZDSYW(IXSTOR,LNKLOC(4,IST),IQPRNT)
*           display links and system words
      ELSEIF(IFLCUR .EQ. 3)THEN
*           display data words
         LL=LNKLOC(4,IST)
         IF(IFIRST.LE.0)IFIRST=1
         IF(ILAST.GT.IQ(LL+KQS-1))ILAST=IQ(LL+KQS-1)
         IF(IFIRST.GT.ILAST)IFIRST=ILAST
         CALL DZDDAW(IXSTOR,LNKLOC(4,IST),CHOPDZ,IFIRST,ILAST)
      ELSEIF(IFLCUR .EQ. 5)THEN
*        display documentation + or make template for it
         WRITE(IQPRNT,'(A)')'   '
         IDTEMP=1
         IF(RZPATH  .NE. ' ')THEN
            KEYV(1)=IQ(KQS+LNKLOC(4,IST)-4)
            CALL UHTOC(KEYV(1),4,CHKEY,4)
            LUP = LQ(KQS+LNKLOC(4,IST)+1)
            IF(LUP .GT. 2)THEN
               KEYV(2)=IQ(KQS+LUP-4)
               CALL UHTOC(KEYV(2),4,CHKEY(5:8),4)
            ELSE
               CHKEY(5:8) = 'NONE'
               CALL UCTOH(CHKEY(5:8) ,KEYV(2),4,4)
            ENDIF
            ICYCLE=0
            CALL RZIN(0,LL,2,KEYV,ICYCLE,'C')
            IF(IQUEST(1).EQ.0)THEN
               IF(IDVERS.NE.0)THEN
                  CALL DZDOCO(IQPRNT,CHKEY,'OR')
               ELSE
                  CALL DZDOCO(IQPRNT,CHKEY,'R')
               ENDIF
               IDTEMP=0
            ENDIF
         ENDIF
         IF(IDTEMP.EQ.1)THEN
            CALL DZDTMP(IXSTOR,LNKLOC(4,IST),IQPRNT,CHOPTM)
         ENDIF
      ELSEIF(IFLCUR .EQ. 6)THEN
*        display data words with documentation
         IF(RZPATH  .NE. ' ')THEN
            CALL DZDDW1(IXSTOR,LNKLOC(4,IST),
     &      CHOPDZ,IFIRST,ILAST,IQPRNT)
         ELSE
            WRITE(*,*)' Sorry, no doc available'
         ENDIF
      ENDIF
  160 CONTINUE
      CALL DZDPTO(IWKTYP)
      GOTO 30
  170 CONTINUE
      IF(RZPATH .NE. ' ')THEN
         CALL RZCDIR(CWDCUR,' ')
      ENDIF
      IFWORD = IFIRST
      ILWORD = ILAST
      IF(INDEX(CHOPT,'L').NE.0)THEN
         IF(LNKLOC(4,IST).NE.0)THEN
            LTOPIN=LNKLOC(4,IST)
         ENDIF
      ENDIF
 175  LNKLOC(1,IST) = 0
      IF(LACTWK) CALL IDAWK(IWDISP)
      RETURN
      END
 
