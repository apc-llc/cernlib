*
* $Id: dzdirz.F,v 1.1.1.1 1996/03/04 16:13:16 mclareni Exp $
*
* $Log: dzdirz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:16  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDIRZ(IDIVIS,LDATA,JB,
     + RZPATH,CHOPT, IWDISP, IWMETA, ILOCNR)
*.
*...DZDDIR
*.
*. INPUT     : IXSTOR   Store index
*.             LDATA    Link where to put data structure (RZIN)
*.             JB       Jbias
*.             RZPATH   RZ - directory to treat
*.             CHOPT    Character option:
*.                      'N' dont activate workstation (done by caller)
*.                      'M' write on metafile also the menu boxes
*.                      'S' dont drop d/s at return
*.                      'A' display all keys (100000)
*.                      'T' template (2 lines/dir => LUN)
*.                      'H' headline for key names in display
*.                      'DU10' doc is on seq file unit 10
*.                      'NK5'  show max 5 keys
*.                      'LU11' output to LUN 11
*.                      'W'    print warning if more keys then asked for
*.
*.             IWDISP   workstation Id for display (screen)
*.             IWMETA   Id for possible metafile (0 if none)
*.             ILOCNR   locator number for cursor input
*.
*. OUTPUT    : none
*.
*. COMMON    :
*. SEQUENCE  : DZDCHV DZDPRM MZCA   MZCB   QSTORE ZEBQ   ZUNIT
*. CALLS     : DZDBOX  DZDGET DZDGST DZDOCO DZDSYW
*. CALLS     : DZDTXT GACWK  GCLRWK GDAWK  GQCNTN GQNT
*. CALLS     : GSELNT RZCDIR UHTOC
*. CALLED    : USER
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*. LAST MOD  :  4-Jun-88     O.Schaile
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
      COMMON/QUEST/RQUEST(100)
      INTEGER IQUEST(100)
      EQUIVALENCE(RQUEST,IQUEST)
#include "zebra/zunit.inc"
#include "dzdprm.inc"
#include "dzdchv.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
*      COMMON/DZDLK2/LAREF(2),LSEL,LSEL1
      COMMON/DZDLK2/LNKLOC(5,0:15)
      LOGICAL DZDINB
      EXTERNAL DZDINB
*
      CHARACTER*12 CTEMP ,CHOPT1
      CHARACTER*4 CHOPRZ
      CHARACTER*8 CHKEY
      CHARACTER*120 RZPSAV, RZPINT
      PARAMETER(NMITEM=7)
      CHARACTER*16 CHITEM(NMITEM),CHUSER(2)
      CHARACTER*8 CMENU
      PARAMETER (NMODMN=2)
      CHARACTER*8 CMODUS(2)
      CHARACTER*20 CMODTI
      CHARACTER*12 CMODMN(NMODMN)
      CHARACTER*12 CMODVA(NMODMN)
      CHARACTER*12 CMODDF(NMODMN)
      CHARACTER*1 CMMENU
      REAL RVALQ(11)
*
      INTEGER IFIRST,ILAST,MKEYS,MOLDK
      LOGICAL LACTWK
      SAVE INIFLG, METAFL,IPCSEQ, IOPFIL,ICYCLE,MKEYS
      SAVE RZPSAV
      DATA IFIRST/0/, ILAST/0/, MKEYS/10/
      DATA INIFLG/0/, IPCSEQ/0/, IOPFIL /0/
*----
      CALL IGQWK(IWDISP,'OPEN',RVALQ)
      IF(RVALQ(1).LT.1.)THEN
         WRITE(*,*)'DZDIRZ: Workstation', IWDISP, ' not open'
         RETURN
      ENDIF
      IF(INDEX(CHOPT,'A').NE.0)THEN
         MKEYS = 100000
      ELSE
         MKEYS=5
      ENDIF
      LFCOL=0
      IXSTOR=IDIVIS
#include "zebra/qstore.inc"
      IST = JQSTOR
      IF(LNKLOC(1,IST).EQ.0 .OR. INIFLG.EQ.0)
     &   CALL MZLINT(IXSTOR,'/DSDLK2/',
     +   LNKLOC(1,IST),LNKLOC(5,IST),LNKLOC(5,IST))
      INIFLG=1
      CHOPT1 = ' '
*     look if a unit for documentation is there
      LUDORZ=0
      LENOPT=LENOCC(CHOPT)
      IFCD=INDEX(CHOPT,'DU')
      IF(IFCD.NE.0)THEN
        CALL DZDGIV(CHOPT(IFCD+2:),LUDORZ,NN)
      ENDIF
*     unit for log output
      LLUNIT=6
      IFCL=INDEX(CHOPT,'LU')
      IF(IFCL.NE.0)THEN
        CALL DZDGIV(CHOPT(IFCL+2:),LLUNIT,NN)
        IF(LLUNIT.LE.0)LLUNIT=6
      ENDIF
*     number of keys to show
      IFCN=INDEX(CHOPT,'NK')
      IF(IFCN.NE.0)THEN
        CALL DZDGIV(CHOPT(IFCN+2:),NNK,NN)
        IF(NNK.NE.0)MKEYS=NNK
      ENDIF
      IF(IFCD.NE.0)LENOPT=IFCD-1
      IF(IFCL.NE.0.AND.IFCL.LT.LENOPT-1)LENOPT=IFCL-1
      IF(IFCN.NE.0.AND.IFCN.LT.LENOPT-1)LENOPT=IFCN-1
*      WRITE(*,*)'IFCD,IFCL,IFCN,LENOPT',IFCD,IFCL,IFCN,LENOPT
      IF(LENOPT .GT. 0)THEN
            CHOPT1(1:)=CHOPT(1:LENOPT)
      ELSE
            CHOPT1=' '
      ENDIF
*     LENOPT = LENOPT+1
      CALL CLTOU(CHOPT1(1:LENOPT))
 
*     CHOPT1(LENOPT:LENOPT) = 'B'
      IF(INDEX(CHOPT1,'N').NE. 0)THEN
           LACTWK = .FALSE.
      ELSE
           LACTWK=.TRUE.
      ENDIF
      IFLPKY=INDEX(CHOPT1,'H')
      NT = 0
      ICYCLE=1000000
      CHOPRZ=' '
      XRANGE = 20.
      YRANGE = 20.
      CALL IGRNG( XRANGE, YRANGE)
      IF(LACTWK)CALL IACWK(IWDISP)
      CMENU='DHCT'
      CMMENU=' '
      GRIDX = GRIDSC*XRANGE/32.
      GRIDY = GRIDX
      YBOTTM=0.
      MOLDK=MKEYS
*     look if dir changed
      IF(INDEX(CHOPT1,'S').NE.0 .AND. LNKLOC(4,IST).NE.0)THEN
         LNKLOC(3,IST)= LNKLOC(4,IST)
         LCSAV=LENOCC(RZPSAV)
         LCPATH=LENOCC(RZPATH)
         RZPINT(1:LCPATH)=RZPATH(1:LCPATH)
         CALL CLTOU(RZPINT(1:LCPATH))
         CALL CLTOU(RZPSAV(1:LCSAV))
         IF(INDEX(RZPINT(1:LCPATH),RZPSAV(1:LCSAV)).LE.0)THEN
*           not part, force new
            GOTO 9
         ELSE
*           exactly same?
            IF(LCSAV.EQ.LCPATH)GOTO 8
            RZPINT(LCPATH+1:)=' '
            LL=LNKLOC(3,IST)
            IF(LCPATH.GT.LCSAV+1)THEN
               LCACT=LCSAV+2
*              set link to selected dir
5              CONTINUE
               LASTC=INDEX(RZPINT(LCACT:),'/')
               IF(LASTC.EQ.0)THEN
                  LASTC=LCPATH
               ELSE
                  LASTC=LASTC+LCACT-1
               ENDIF
*              is selected dir there?
               NL=IQ(LL+KQS-2)
               IF(NL.LE.0)GOTO 9
               DO 6 IL=1,NL
                  LLD=LQ(LL+KQS-IL)
                  IF(LLD.EQ.0)GOTO 6
*                 is it dir
                  IF(IQ(LLD+KQS+1).NE.1)GOTO 6
                  IOFFNA=IQ(LLD+KQS+2)
                  NC=(IQ(LLD+KQS-1)-IOFFNA)*4
                  CALL UHTOC(IQ(LLD+KQS+IOFFNA+1),4,CLINE,NC)
                  CALL CLTOU(CLINE(1:NC))
                  NC=LENOCC(CLINE(1:NC))
                  IF(CLINE(1:NC).EQ.RZPINT(1:NC))THEN
                     IF(LASTC.EQ.LCPATH)THEN
                        LNKLOC(3,IST)=LLD
                        GOTO 8
                     ENDIF
*                    go down in tree
                     LL=LLD
                     LCACT=LASTC+2
                     GOTO 5
                  ENDIF
6              CONTINUE
*              fallen trough loop, not found
               GOTO 9
            ENDIF
         ENDIF
      ELSE
         GOTO 9
      ENDIF
*
8     ILKACT = 0
      METAFL = 0
      GOTO 10
 
9     CONTINUE
      MOLDK=-1
      RZPSAV=RZPATH
 
10    CONTINUE
      IF(MKEYS.NE.MOLDK)THEN
         CALL RZCDIR(RZPSAV,' ')
         IF(LNKLOC(4,IST).NE.0)CALL MZDROP(IXSTOR,LNKLOC(4,IST),' ')
         LNKLOC(4,IST)=0
         CALL DZDGRZ(IXSTOR,LNKLOC(3,IST),MKEYS,LLUNIT,CHOPT1)
         LNKLOC(4,IST) = LNKLOC(3,IST)
         MOLDK=MKEYS
         ILKACT = 0
         METAFL = 0
      ENDIF
 
*     clear workstations
      IF(IWMETA .GT. 0 .AND. METAFL .EQ. 1)THEN
*            CALL IDAWK(IWDISP)
            CALL IACWK(IWMETA)
            CMMENU='M'
            ILKACT = ILKSAV
            IF(INDEX(CHOPT,'M').EQ.0)THEN
                CALL ISWKWN(IWMETA,0.,1.,YBOTTM,1.)
                XAPA = .1465
                CALL ISWKVP(IWMETA,0.,XAPA,0.,XAPA*(1.-YBOTTM))
            ENDIF
            CMENU='DT'
            CALL ICLRWK(0,0)
*            CALL ICLRWK(IWMETA,1)
            CALL ISFASI(-103)
            IPCSEQ=IPCSEQ+1
            IF(IPCSEQ. GT. 999)IPCSEQ=1
            CTEMP(1:4)='RDIR'
            WRITE(CTEMP(5:7),'(I3.3)')IPCSEQ
      ELSEIF(IFOMED.NE.3)THEN
            CALL ICLRWK(IWDISP,1)
            CALL ISFASI(-5)
      ENDIF
 
      ILKSAV = ILKACT
      CALL DZDRA1(IXSTOR, LNKLOC(3,IST), ILKACT
     + , 'B'//CHOPT1,XCUR1,YCUR1,.FALSE.,IFLCUR,YTOP,YBOTTM)
      CALL MZSDIV(IXSTOR,-7)
      CLINE = 'CWD: '
      CALL RZCDIR(CLINE(6:),'R')
      ILC = INDXBC(CLINE,' ')
      CALL DZDTXT(CLINE(1:ILC),
     +  ILC,.25* GRIDX,YRANGE - 2.*CSIZE,1.5*CSIZE,0.,1)
*
      IF(IWMETA .GT. 0 .AND. METAFL .EQ. 1
     +   .AND. INDEX(CHOPT,'M').EQ. 0)THEN
            CALL IDAWK(IWMETA)
            CMMENU=' '
*            CALL IACWK(IWDISP)
            CMENU='DHCT'
            METAFL = 0
      ENDIF
 
*     get locator input and take action
 
   20 CONTINUE
      ILKUSE = ILKSAV
*     build the  menus
 
      GRIDX=GRIDSC*1./32.
      GRIDY=GRIDX
 
      X0MEN=.1*GRIDX
      Y0MEN= .1*GRIDY
      X1MEN=X0MEN+6.5*GRIDX
      Y1MEN=Y0MEN+FLOAT(NMITEM+1)*0.8*GRIDY
      CHITEM(1) = 'Quit'
      IF(INDEX(CHOPT1,'S').NE.0)THEN
         CHITEM(2)='Drop+Quit'
      ELSE
         CHITEM(2)=' '
      ENDIF
      IF(ILKACT.GT.0)THEN
         CHITEM(3)='Continue'
      ELSE
         CHITEM(3)=' '
      ENDIF
      CHITEM(4)='>Plotfile'
      CHITEM(5)='More keys'
      CHITEM(6)='Help'
      CHITEM(7)='LaTeX'
      CHUSER(1)=' '
      CHUSER(2)=' '
*     for more keys to input
      X0MODM=X1MEN+0.1*GRIDX
      Y0MODM=0.1*GRIDY
      X1MODM=X0MODM+12.*GRIDX
      Y1MODM=Y0MODM+FLOAT(NMODMN+2)*0.8*GRIDY
      CMODTI='Show more keys (CWD)'
      CMODMN(1) = 'First key '
      CMODDF(1) = '0'
      CMODMN(2) = 'Last key '
      CMODDF(2) = '0'
      CMODUS(1)='Quit'
      CMODUS(2)='Execute'
 
*     display the menu
   30 CONTINUE
 
      CALL IGMENU(0,'Choice',X0MEN,X1MEN,Y0MEN,Y1MEN,
     +            0,CHUSER,NMITEM,CHITEM,
     +            CTEMP,CTEMP,ICHOIC,'D'//CMMENU)
 
      IF(IWMETA .GT. 0 .AND. METAFL .EQ. 1)THEN
            METAFL=2
             CALL ICLRWK(IWMETA,0)
            CALL IDAWK(IWMETA)
            CMMENU=' '
*            CALL IACWK(IWDISP)
            CMENU='DHCT'
            GOTO 30
      ENDIF
 
      IF(IFOMED.EQ.3)THEN
         WRITE(LUNGRA,'(A)')' } '//BS//'ep'
         IFOMED=1
      ENDIF
 
      CALL IGMENU(0,'Choice',X0MEN,X1MEN,Y0MEN,Y1MEN,
     +            0,CHUSER,NMITEM,CHITEM,
     +            CTEMP,CTEMP,ICHOIC,'CH')
      LNKLOC(5,IST) = LNKLOC(3,IST)
      IF(ICHOIC.EQ.-1000)GOTO 50
      IF(ICHOIC.NE.0)GOTO 35
      IF(ICHOIC.LE.0)THEN
         PX=RQUEST(11)*XRANGE
         PY=RQUEST(12)*YRANGE
         LNKLOC(5,IST) = LNKLOC(3,IST)
         CALL DZDRA1(IXSTOR, LNKLOC(3,IST), ILKUSE
     + ,  CHOPT1,PX,PY,.TRUE.,IFLCUR,YTOP,YBOTTM)
*
         CALL MZSDIV(IXSTOR,-7)
         LSAVE = LNKLOC(3,IST)
         LNKLOC(3,IST) = LNKLOC(5,IST)
         LNKLOC(5,IST) = LSAVE
         GOTO 40
      ELSE
         LNKLOC(5,IST)=LNKLOC(3,IST)
      ENDIF
*     quit
 
35    CONTINUE
      IF(ICHOIC.EQ.1)GOTO 50
      IF(ICHOIC.EQ.2)THEN
        IF(INDEX(CHOPT1,'S').NE.0)THEN
           GOTO 50
        ELSE
           GOTO 30
        ENDIF
      ENDIF
*     help
 
      IF(ICHOIC.EQ.6)THEN
          CALL DZDHLR
          GOTO 30
      ENDIF
 
*     continue with down banks
 
      IF(ILKACT.GT.0 .AND. ICHOIC.EQ.3)THEN
          LNKLOC(3,IST) = LNKLOC(5,IST)
          METAFL = 0
          GOTO 10
      ENDIF
 
*     LaTeX output
 
      IF(ICHOIC.EQ.7 .AND. METAFL.EQ.0)THEN
          LUNGRA=IQPRNT
          PAGECM=14./20.
          IFOMED=3
          WRITE(LUNGRA,'(A)')
     &    ' '//BS//'bp(14,14)(0, 0) '//BS//
     &    'thicklines  {'//BS//'small '//BS//'sf '
          GOTO 10
      ENDIF
 
*     write on metafile
 
      IF(ICHOIC.EQ.4 .AND. IWMETA.GT.0 .AND. METAFL.EQ.0)THEN
          METAFL = 1
          GOTO 10
      ENDIF
 
      IF(ICHOIC.EQ.5)THEN
         CMODVA(1)=' '
         CMODVA(2)=' '
         CALL IGMENU(0,CMODTI,X0MODM,X1MODM,Y0MODM,Y1MODM, 2,CMODUS,
     +   NMODMN,CMODMN, CMODDF,CMODVA,ICHOIC,'CPD'//CMMENU)
 
*         quit?
         IF(ICHOIC.EQ.-1)GOTO 30
         IF(ICHOIC.EQ.-1000)GOTO 30
         CALL DZDCTI(CMODVA(1),N1)
         CALL DZDCTI(CMODVA(2),N2)
         IF(N1.GT.N2)THEN
            WRITE(6,*)' N2> N2'
            GOTO 30
         ENDIF
         CALL DZDSTK(IXSTOR,N1,N2,LNKLOC(5,IST))
         GOTO 30
      ENDIF
 
   40 CONTINUE
 
*     very top bank selected
*     WRITE(*,*)IFLCUR,LNKLOC(5,IST)
      IF(IFLCUR .EQ. 1 .AND. LNKLOC(5,IST) .LE. 2)GOTO 50
 
*     set current working directory
      IF(IQ(KQS+LNKLOC(5,IST)+1) .EQ. 1)THEN
          NEXTRA = IQ(KQS+LNKLOC(5,IST)+2)
          NC = (IQ(KQS+LNKLOC(5,IST)-1)-NEXTRA)*4
          IF(NC.GT.LEN(CLINE))THEN
              WRITE(*,*)' Directory name too long ',NC
              GOTO 20
          ENDIF
          CLINE=' '
          CALL UHTOC(IQ(KQS+LNKLOC(5,IST)+NEXTRA+1),4,CLINE,NC)
          CALL RZCDIR(CLINE,' ')
*         go up or down
          IF(IFLCUR .EQ. 1 .OR. IFLCUR .EQ. 4)THEN
              IF(LNKLOC(3,IST) .NE. LNKLOC(5,IST))METAFL = 0
              LNKLOC(3,IST) = LNKLOC(5,IST)
              ILKACT = 0
              GOTO 10
          ENDIF
*     directory listing
          IF(IFLCUR .EQ. 3)THEN
              CALL RZLDIR(' ','A')
*              CMENU='HCT'
              GOTO 30
           ENDIF
*     print documentation
          IF(IFLCUR .EQ. 5 .OR. IFLCUR .EQ. 6 )THEN
              CALL CLTOU(CLINE)
              CALL DZDIRD(LUDORZ,CLINE,CLINE,0)
              GOTO 30
           ENDIF
      ENDIF
*     its in a key - box
      IF(IQ(KQS+LNKLOC(5,IST)+1) .EQ. 2)THEN
*        find directory of this key and set it
         LUP = LQ(KQS+LNKLOC(5,IST)+1)
         IF(LUP.EQ.0)GOTO 30
         NEXTRA = IQ(KQS+LUP+2)
         NC = (IQ(KQS+LUP-1)-NEXTRA)*4
         IF(NC.GT.LEN(CLINE))THEN
            WRITE(*,*)' Directory name too long ',NC
            GOTO 20
         ENDIF
         CLINE=' '
         CALL UHTOC(IQ(KQS+LUP+NEXTRA+1),4,CLINE,NC)
         CALL RZCDIR(CLINE,' ')
         IF(IFLCUR.EQ.3)THEN
            IFORM=JBYT(IQ(KQS+LNKLOC(5,IST)+2),1,3)
            IF(IFORM.NE.0)THEN
               WRITE(*,*)
     &         ' Key contains a vector (see RZVOUT), format:',IFORM
            ENDIF
            KEYSEQ=IQ(KQS+LNKLOC(5,IST)+3)
            ICYCLE=IQ(KQS+LNKLOC(5,IST)+4)
            CALL RZIN(IDIVIS,LDATA,JB,KEYSEQ,ICYCLE,'S')
            IF(IQUEST(1).NE.0)THEN
               WRITE(*,*)'Error from RZIN,IQUEST(1)',IQUEST(1)
               LDATA=0
            ELSE
               WRITE(*,'(I6,A,A4)')
     &         IQUEST(12),' words read, Top bank: ',IQ(KQS+LDATA-4)
               IF(INDEX(CHOPT1,'K').NE.0)THEN
                  LUP = LQ(KQS+LNKLOC(5,IST)+1)
                  NWK =IQ(KQS+LUP+4)
                  NWKEYD=(NWK+3)/4
                  IPKVAL =7+NWKEYD
                  CALL MZBOOK(IDIVIS,LDATA,LDATA,0,'KEYW',0,0,NWK,0,0)
                  CALL UCOPY(IQ(KQS+LNKLOC(5,IST)+IPKVAL),
     &            IQ(KQS+LDATA+1),NWK)
                  LDATA=LQ(KQS+LDATA+2)
*                 update screen with cwd  return to caller
*                  CLINE = 'CWD: '
*                  CALL RZCDIR(CLINE(6:),'R')
                  ILC = INDXBC(CLINE,' ')
                  CALL DZDTXT('CWD: '//CLINE(1:ILC),
     +            ILC,.25* GRIDX,YRANGE - 2.*CSIZE,1.5*CSIZE,0.,1)
                  GOTO 50
               ENDIF
            ENDIF
            ILKACT = 0
            GOTO 10
         ELSE
            NWK =IQ(KQS+LUP+4)
            NWKEYD=(NWK+3)/4
            NWK = MIN(NWK,400)
            IPKTAG =7+NWKEYD
            IPKVAL =7+NWKEYD
            WRITE(*,*)' Current directory: ',CLINE(1:NC)
            WRITE(*,*)' Tag     Type      Value'
            CALL UHTOC(IQ(KQS+LUP+7),4,CLINE,NWK)
            IPKTAG =7+NWKEYD
            IPKVAL =7+NWKEYD
            DO 45 IWK=1,NWK
            CALL UHTOC(IQ(KQS+LUP+IPKTAG) ,4,CHKEY,8)
            IVAL=IQ(KQS+LNKLOC(5,IST)+IPKVAL)
            IF(CLINE(IWK:IWK).EQ.'I')THEN
               IF(CHKEY(1:6).EQ.'TSTAMP')THEN
                  CALL RZDATE(IVAL,IDATE1,ITIME1,1)
                  WRITE(CTEMP,'(I6,A,I4)')IDATE1,'/',ITIME1
               ELSE
                  CTEMP=' '
               ENDIF
               WRITE(*,'(1X,A,A,I11,2X,A)')
     &         CHKEY,'   Int',IVAL,CTEMP(1:11)
            ELSE
               WRITE(*,'(1X,A,A,A4)')
     &         CHKEY,'   Hol       ',IVAL
            ENDIF
            IPKTAG=IPKTAG+2
            IPKVAL=IPKVAL+1
45          CONTINUE
*           CALL DZDPTO(IWKTYP)
         ENDIF
      ENDIF
      GOTO 30
 
   50 CONTINUE
      IF(INDEX(CHOPT1,'S').EQ.0 .OR. ICHOIC.EQ.2)THEN
         IF(LNKLOC(4,IST).NE.0)CALL MZDROP(IXSTOR,LNKLOC(4,IST),' ')
         LNKLOC(1,IST) = 0
      ENDIF
      IF(LACTWK) CALL IDAWK(IWDISP)
      RETURN
      END
**********************************************************************
