*
* $Id: dzddiv.F,v 1.1.1.1 1996/03/04 16:13:15 mclareni Exp $
*
* $Log: dzddiv.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:15  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDDIV(IXDIVP,LDUMMY,RZPATH,CHOPT,
     +                  IWDISP, IWMETA, ILOCNR, IWKTYP)
*.
*...DZDDIV   see long write-up
*.
*. INPUT     : IXXDIVP  Division index
*.             LDUMMY   Not yet used
*.             RZPATH   Pathname in RZ-file for bank documentation
*.             CHOPT    Character option
*.                      'I' interactive,
*.                      'P' print addresses of of banks
*.                      'N' dont activate workstation (done by caller)
*.                      'M' write also the menu boxes to metafile
*.                      'C' use color
*.             IWDISP   workstation Id for display (screen)
*.             IWMETA   Id for possible metafile (0 if none)
*.             ILOCNR   locator number for cursor input
*.             IWKTYP   workstation type
*.
*. OUTPUT    :
*.
*. COMMON    : DZDVAR
*. SEQUENCE  : MZBITS MZCA   MZCB   MZCC   MZCN   QUEST  ZEBQ
*. CALLS     : DZDBOX DZDISP DZDST1 DZDTXT GACWK  GCLRWK GDAWK  GPL
*. CALLS     : GQCNTN GRQLC  GSELNT GUWK
*. CALLS     : MZSDIV SBYT   UHTOC  UOPTC  gacwk
*. CALLED    : USER
*.
*. REPORT CONDITIONS
*.
*. AUTHOR    : O. Schaile
*. VERSION   : 1.00
*. CREATED   :  3-Feb-88
*. LAST MOD  : 20-Feb-88
*.
*. Modification Log.
*.
*. Implement division list
*.
*.**********************************************************************
*.
      CHARACTER*(*) CHOPT,RZPATH
*
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
      REAL RQUEST(100)
      EQUIVALENCE (IQUEST,RQUEST)
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzcn.inc"
#include "zebra/zunit.inc"
#include "dzdprm.inc"
*.
#include "dzdzbrinc.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      LOGICAL DZDINB
      EXTERNAL DZDINB
      CHARACTER*8 CFWBLO
      CHARACTER*8 CTEMP,CTEMP1,CTEMP2,CHOPT1
      CHARACTER*2 CDIV
      PARAMETER(NITEM=5)
      CHARACTER*16 CHITEM(NITEM)
      CHARACTER*32 CDIVLI(20),CSTOLI(16),CTITLE
      INTEGER IDIVLI(20),ISTOLI(16)
      CHARACTER*1 CMMENU
      REAL RVALQ(11)
      PARAMETER (NROWS=40)
      COMMON/DZDVAR/X0,DX,Y0,DY,GRYDDV, CSDDV,
     +              LENBLO, XENBLO, NROWAC, IPRTFL
      DIMENSION    IXDIVP(9), X(3), Y(3),XDIV(2,20),YDIV(2,20)
      LOGICAL REQINP,LACTWK
      INTEGER  INIFLG
      SAVE IFIRST,ILAST,IPCSEQ,  INIFLG
      DATA IFIRST,ILAST/0,-1/ , IPCSEQ/0/,  INIFLG/0/
 
      CALL IGQWK(IWDISP,'OPEN',RVALQ)
      IF(RVALQ(1).LT.1.)THEN
         WRITE(*,*)'DZDDIV: Workstation', IWDISP, ' not open'
         RETURN
      ENDIF
      IF(INIFLG.EQ.0)THEN
         IDVERS=0
         INIFLG=1
         CALL DZDIBR
      ENDIF
      IPRTFL = INDEX(CHOPT,'P')
      IF(INDEX(CHOPT,'N').NE. 0)THEN
         LACTWK = .FALSE.
      ELSE
         LACTWK=.TRUE.
      ENDIF
      KWTYP=IWKTYP
*     compose option for DZDISP
      IPOPT = 1
      CHOPT1 = 'V'
      IF(INDEX(CHOPT,'N').NE.0)THEN
         IPOPT=IPOPT+1
         CHOPT1(IPOPT:IPOPT)='N'
      ENDIF
      IF(INDEX(CHOPT,'M').NE.0)THEN
         IPOPT=IPOPT+1
         CHOPT1(IPOPT:IPOPT)='M'
      ENDIF
      IF(INDEX(CHOPT,'C').NE.0)THEN
         IPOPT=IPOPT+1
         CHOPT1(IPOPT:IPOPT)='C'
      ENDIF
      IXIN = IXDIVP(1)
      XRANGE = 20.
      YRANGE = 20.
      CALL IGRNG( XRANGE, YRANGE)
      RYRX =  XRANGE
      IF(LACTWK)THEN
         CALL IACWK(IWDISP)
      ENDIF
      CMMENU=' '
 
   10 CONTINUE
      CALL MZSDIV (IXIN,0)
      CALL UZERO(XDIV,1,40)
 
      IST = JQSTOR
*     default to div 2
      IF(JQDIVI .EQ. 0)THEN
         IXIN=IXIN+2
         CALL MZSDIV(IXIN,1)
      ENDIF
      IF(INIFLG.EQ.0)THEN
         IDVERS=0
         INIFLG=1
      ENDIF
*     links for the browser
      IF(INDEX(CHOPT,'D').NE. 0)THEN
         IF(LTDFLG(IST).EQ.0)THEN
            CALL MZLINK(IXIN,'DZDDIS',
     &      LTDISP(IST),LTDISP(IST),LTDISP(IST))
            LTDFLG(IST)=1
            IIST=0
            CALL SBYT(IST,IIST,27,6)
            DO 6 K=21,24
6           CALL MZXREF(IXIN,K+IIST,'A')
         ENDIF
         IF(LTDISP(IST).NE.0)THEN
            CALL MZDROP(IXIN,LTDISP(IST),' ')
            LTDISP(IST)=0
         ENDIF
         NLKUSE=0
*        get number of banks in division
         NUMB=0
         LGOP=0
15       CALL  DZDNBK(IXIN,LGOP,LBNK,LFW,LLW,IDH,IDRFLG)
         LGOP=LBNK
         IF(LBNK.NE.0)THEN
*        dropped?
*         IF(IDRFLG.EQ.0)NUMB = NUMB+1
            NUMB = NUMB+1
           GOTO 15
         ENDIF
         NUMB=NUMB+5
         IIST=20
         CALL SBYT(IST,IIST,27,6)
         CALL MZBOOK(IIST,LTDISP(IST),LTDISP(IST),1,'ZBR1',NUMB,0,0,0,0)
         ISTUSE=IST
      ENDIF
 
      CALL MZSDIV (IXIN,0)
      NWSTOR = LQSTA(KQT+21)
      XWSTOR = NWSTOR
 
      METAFL = 0
 
*   20 CONTINUE
 
      LSTA = LQSTA(KQT+JQDIVI)
      LEND = LQEND(KQT+JQDIVI)
      IFW = LSTA
      ILW = LEND
      IFW1 = IFW + 1
      ILW1 = ILW
   20 CONTINUE
      CALL MZSDIV (IXIN,1)
      LENGLQ = ILW - IFW
*     activate and clear workstations
      XRANGE = 20.
      YRANGE = 20.
      CALL IGRNG( XRANGE, YRANGE)
      RYRX=XRANGE
*      CALL HPLNEW
      CALL ICLRWK(0,0)
      IF(IWMETA .GT. 0 .AND. METAFL .EQ. 1)THEN
*         CALL IDAWK(IWDISP)
         CALL IACWK(IWMETA)
         CALL IGRNG( XRANGE, YRANGE)
         CALL ISLN(1)
         CMMENU='M'
         CALL ICLRWK(0,0)
         CALL ISFASI(-103)
      ELSEIF(IFOMED.NE.3)THEN
         CALL ICLRWK(IWDISP,1)
         CALL ISFASI(-5)
      ENDIF
 
      GRYDDV = RYRX/FLOAT(NROWS)
      GRXDDV = GRYDDV
      NROWHD = 6
      NROWBO = 3
 
      CSDDV = 0.6 * GRYDDV
      DY = 0.8 * GRYDDV
      X0 = 0.08 * RYRX
      X2 = 0.98 * RYRX
      DX = X2-X0
 
*     draw top box for store
 
      Y0 = GRYDDV * FLOAT(NROWS-1)
      Y0STOR = Y0 - GRYDDV-DY/2.
      CALL UHTOC(IQTABV(KQT+11),4,CTEMP,8)
      WRITE(CTEMP1,'(I8)')NWSTOR
      CALL DZDTXT(
     + 'Store '//CTEMP//'  Length(Words) '//CTEMP1, 0
     + ,X0+.5*DX, Y0+0.5*DY, 1.5*CSDDV, 0., 2)
 
      CALL MZSDIV (IXIN,1)
      CALL UHTOC(IQDN1(KQT+JQDIVI),4,CTEMP(1:4),4)
      CALL UHTOC(IQDN2(KQT+JQDIVI),4,CTEMP(5:8),4)
      WRITE(CTEMP1,'(I8)')IFW1
      WRITE(CTEMP2,'(I8)')ILW1
      WRITE(CDIV,'(I2)')JQDIVI
      YT = GRYDDV * (FLOAT(NROWS-NROWHD+1)+0.5)
      CALL DZDTXT(
     + 'Div '//CDIV//' '//CTEMP//' range '//CTEMP1//' - '//CTEMP2, 0
     + ,X0+.5*DX, YT,1.2*CSDDV,0.,2)
      CALL DZDBOX(X0,X2,Y0STOR,Y0STOR+DY,0)
      CALL DZDBOX(X0,X2,Y0STOR,Y0STOR+DY,0)
 
*     Y0 for the first box for banks
 
      Y0 = GRYDDV * FLOAT(NROWS-NROWHD)
 
*     loop on divisions
 
      DO 30 I=1,20
         CALL MZSDIV (IXIN,1)
         IFAD = LQSTA(KQT+I)
         ILAD = LQEND(KQT+I)
         IF(IFAD .NE. 0 .AND. IFAD .NE. ILAD)THEN
            Y(1) = Y0STOR
            Y(2) = Y0STOR+DY
            XL = X0+DX*FLOAT(IFAD)/XWSTOR
            X(1) = XL
            X(2) = XL
            XDIV(1,I)=XL
            CALL DZDGPL(2,X,Y)
            CALL MZSDIV (IXIN,1)
            IF(I .EQ. JQDIVI)THEN
               Y(2) = Y0+2.*DY
               X(2) = X0
               Y(3) = Y(2) - DY
               X(3) = X(2)
               CALL DZDGPL(3,X,Y)
               CALL MZSDIV (IXIN,1)
               Y(2) = Y0STOR + DY
               X(2) = XL
            ENDIF
            XR = X0+DX*FLOAT(ILAD)/XWSTOR
            X(1) = XR
            X(2) = XR
            XDIV(2,I)=XR
            CALL DZDGPL(2,X,Y)
            IF(INDEX(CHOPT,'C').NE.0)
     &        CALL DZDIFA(XL,XR,Y(1),Y(2),1,3)
            CALL MZSDIV (IXIN,1)
            IF(I .EQ. JQDIVI)THEN
               Y(2) = Y0+2.*DY
               X(2) = X2
               Y(3) = Y(2) - DY
               X(3) = X(2)
               CALL DZDGPL(3,X,Y)
               CALL MZSDIV (IXIN,1)
               Y(2) = Y0STOR+DY
               X(2) = XR
            ENDIF
            CALL UHTOC(IQDN1(KQT+I),4,CTEMP(1:4),4)
            CALL UHTOC(IQDN2(KQT+I),4,CTEMP(5:8),4)
            Y(1) = Y0STOR + DY/2.
            Y(2) = Y(1)
            X(1) = XL
            X(2) = XR
            XM = 0.5*(XL+XR)
            IF(XR-XL .GT. 6.*CSDDV)THEN
*               CALL DZDTXT(CTEMP,0,XM,Y(1),CSDDV,0.,2)
               CALL MZSDIV (IXIN,1)
               X(2) = XM-3.*CSDDV
               CALL DZDGPL(2,X,Y)
               X(1) = XM+3.*CSDDV
               X(2) = XR
               CALL DZDGPL(2,X,Y)
            ELSE
*               CALL DZDTXT(CTEMP,0,XM,Y(1)-GRYDDV,CSDDV,0.,2)
               CALL DZDGPL(2,X,Y)
            ENDIF
         ENDIF
   30 CONTINUE
 
      CALL MZSDIV (IXIN,1)
      IF(LENGLQ .LE. 1) GOTO 70
 
      LENBLO = LENGLQ/(NROWS-NROWHD-NROWBO-1)
      NLDIV = 1
   40 CONTINUE
      NLDIV = NLDIV * 10
      IF(LENBLO .GT. 10*NLDIV)GOTO 40
      LENBLO = (LENBLO+NLDIV) / NLDIV * NLDIV
 
      NROWAC = NROWS
      XENBLO = LENBLO
   50 CONTINUE
      LENLAS = LENGLQ - LENBLO*(NROWAC-1)
      IF(LENLAS .LT. 0)THEN
         NROWAC = NROWAC - 1
         GOTO 50
      ENDIF
 
*     draw boxes containing the banks
 
      X1B = X0
      X2B = X2
      Y1B = Y0 - (FLOAT(NROWAC-1)+.1)*GRYDDV
      Y2B = Y0 + DY - 0.1*GRYDDV
 
      DO 60 I=1,NROWAC
         Y1 = Y0 - (FLOAT(I-1)+0.1)*GRYDDV
         Y2 = Y1 + DY
         IF(I .NE. NROWAC)THEN
            X22 = X0+DX
         ELSE
            X22 = X0 + DX*FLOAT(LENLAS)/XENBLO
         ENDIF
         CALL DZDBOX(X0,X22,Y1,Y2,0)
         IFWBLO = (I-1)*LENBLO+1+IFW
         WRITE(CFWBLO,'(I8)')IFWBLO
         XT = .04
         CALL DZDTXT(CFWBLO,0,X0-0.5*CSDDV,Y1+0.1*CSDDV,CSDDV,0.,3)
         CALL MZSDIV (IXIN,1)
   60 CONTINUE
 
      REQINP = .FALSE.
      CALL DZDST1(IXIN,IFW,ILW,REQINP,PX,PY,
     +            LRET,IFARET,ILARET,CHOPT)
 
   70 CONTINUE
 
*     is interactive version required ?
      IF(INDEX(CHOPT,'I') .EQ. 0)GOTO 220
      GRYMEN = 1./FLOAT(NROWS)
      GRXMEN = GRYMEN
 
*     build the menu
 
      X0MEN=0.1*GRXMEN
      Y0MEN=0.1*GRYMEN
      X1MEN=X0MEN+8.*GRXMEN
      Y1MEN=Y0MEN+FLOAT(NITEM)*0.9*GRYMEN
      CHITEM(1)='Quit'
      CHITEM(2)='Zoom'
      IF(IWMETA.GT.0)THEN
         CHITEM(3)='>Plotfi'
      ELSE
         CHITEM(3)=' '
      ENDIF
      CHITEM(4)='Help'
      CHITEM(5)='LaTeX'
*     display only
      CALL IGMENU(0,' ',X0MEN,X1MEN,Y0MEN,Y1MEN,
     +            0,CTEMP,NITEM,CHITEM,
     +            CTEMP,CTEMP,ICHOIC,'DT'//CMMENU)
      ICHOIC=0
 
*     fix x-locations where menus go
 
      X0DIVL=X1MEN  + 0.1*GRXMEN
      Y0DIVL=0.1*GRYMEN
      X1DIVL=X0DIVL+16.*GRXMEN
 
      X0STOL=X1DIVL + 0.1*GRXMEN
      Y0STOL=0.1*GRYMEN
      X1STOL=X0STOL+16.*GRXMEN
 
      IPDIV=0
      IPSTO=0
 
*     loop here  and execute commands
 
   90 CONTINUE
 
*     division list
 
      CALL UHTOC(IQTABV(KQT+11),4,CTEMP,8)
      WRITE(CTEMP1,'(I8)')NWSTOR
      CTITLE='Store '//CTEMP//' Len'//CTEMP1
      DO 100 I=1,20
         YDIV(1,I) = 0.
         YDIV(2,I) = 0.
         IFAD = LQSTA(KQT+I)
         ILAD = LQEND(KQT+I)
         IF(IFAD .NE. 0 .AND. IFAD .NE. ILAD)THEN
            YDIV(1,I) = Y0STOR - FLOAT(I+1)*GRYMEN
            YDIV(2,I) = YDIV(1,I)+DY
            CALL UHTOC(IQDN1(KQT+I),4,CTEMP(1:4),4)
            CALL UHTOC(IQDN2(KQT+I),4,CTEMP(5:8),4)
            WRITE(CDIV,'(I2)')I
            LW=ILAD-IFAD+1
            WRITE(CTEMP1,'(I8)')LW
            IPDIV=IPDIV+1
            IDIVLI(IPDIV)=I
            CDIVLI(IPDIV)= 'Div '//CDIV//' '//CTEMP//' length'//CTEMP1
 
         ENDIF
  100 CONTINUE
*           display the division list
 
      Y1DIVL=Y0DIVL+FLOAT(IPDIV+1)*0.9*GRYMEN
      CALL IGMENU(0,CTITLE,X0DIVL,X1DIVL,Y0DIVL,Y1DIVL, 0,CTEMP,IPDIV,
     +CDIVLI, CTEMP,CTEMP,ICHOIC,'D'//CMMENU)
 
*     store list
 
      DO 130 I=1,NQSTOR+1
         IIST = 0
         CALL SBYT(I-1,IIST,27,6)
         CALL MZSDIV(IIST,-7)
         NWST1 = LQSTA(KQT+21)
         CALL UHTOC(IQTABV(KQT+11),4,CTEMP,8)
         WRITE(CTEMP1,'(I8)')NWST1
         WRITE(CDIV,'(I2)')I-1
         IPSTO=IPSTO+1
         ISTOLI(IPSTO)=I-1
         CSTOLI(IPSTO)= 'Store '//CDIV//' '//CTEMP//' length'//CTEMP1
 
  130 CONTINUE
      CALL MZSDIV(IXIN,-7)
 
*           display the store list
 
      Y1STOL=Y0STOL+FLOAT(IPSTO+1)*0.9*GRYMEN
      CALL IGMENU(0,'Stores',X0STOL,X1STOL,Y0STOL,Y1STOL, 0,CTEMP,
     +IPSTO,CSTOLI, CTEMP,CTEMP,ICHOIC,'D'//CMMENU)
 
 
*       ENDIF
      IF(IWMETA .GT. 0 .AND. METAFL .EQ. 1)THEN
         CALL IDAWK(IWMETA)
         CMMENU=' '
*         CALL IACWK(IWDISP)
         METAFL = 2
         CALL IGMENU(0,' ',X0MEN,X1MEN,Y0MEN,Y1MEN, 0,CTEMP,NITEM,
     +   CHITEM, CTEMP,CTEMP,ICHOIC,'DT'//CMMENU)
 
         ICHOIC=0
      ENDIF
      IF(IFOMED.EQ.3)THEN
         WRITE(LUNGRA,'(A)')' } '//BS//'ep'
         IFOMED=1
      ENDIF
 
  160 CONTINUE
*     quit
 
      IF(ICHOIC.EQ.1)GOTO 220
 
*     Help
      IF(ICHOIC.EQ.4)THEN
         CALL DZDHLV
         GOTO 80
      ENDIF
 
*     LaTeX output
 
      IF(ICHOIC.EQ.5 .AND. METAFL.EQ.0)THEN
          LUNGRA=IQPRNT
          PAGECM=14./20.
          IFOMED=3
          WRITE(LUNGRA,'(A)')
     &    ' '//BS//'bp(14,14)(0, 0) {'//BS//'small '//BS//'sf '
          GOTO 10
      ENDIF
 
*     write on metafile
 
      IF(IWMETA .GT. 0 .AND. METAFL .EQ. 0
     +.AND. ICHOIC.EQ.3)THEN
         METAFL = 1
         GOTO 20
      ENDIF
 
*     zoom
 
      IF(ICHOIC.EQ.2)THEN
         IFAD = 0
         ILAD = 0
         IPNT = 0
         REQINP = .TRUE.
  170    CONTINUE
         CALL IRQLC(IWDISP,ILOCNR,ISTAT,NTR,PX,PY)
         IF(ISTAT.EQ.0)GOTO 220
*        quit
         IF(.NOT. DZDINB(PX,PY,X1B,X2B,Y1B,Y2B))GOTO 180
         CALL DZDST1(IXIN,IFW,ILW,REQINP,PX,PY,
     &   LRET,IFARET,ILARET,CHOPT)
 
         IF(LRET .EQ. 0)GOTO 180
         IF(IFAD .EQ. 0)THEN
            IFAD = IFARET
         ELSE
            IF(IFARET .LT. IFAD)IFAD = IFARET
         ENDIF
         IF(ILARET .GT. ILAD)ILAD = ILARET
         IPNT = IPNT + 1
         IF(IPNT .LT. 2)THEN
            GOTO 170
         ELSE
            GOTO 190
         ENDIF
  180    CONTINUE
         WRITE(*,*)' Locator not inside bank'
         GOTO 170
  190    CONTINUE
         METAFL = 0
         IFW = IFAD
         ILW = ILAD
         GOTO 20
      ENDIF
 
*     request inputs
 
*     the main menu
 
   80 CONTINUE
 
      CALL IGMENU(0,'Choose',X0MEN,X1MEN,Y0MEN,Y1MEN,
     +            0,CTEMP,NITEM,CHITEM,
     +            CTEMP,CTEMP,ICHOIC,'HCT')
      IF(ICHOIC.EQ.-1000)GOTO 220
      IF(ICHOIC.NE.0)GOTO 160
 
*     the division list
      IF(IPDIV.GT.0)THEN
         CALL IGMENU(0,CTITLE,X0DIVL,X1DIVL,Y0DIVL,Y1DIVL, 0,CTEMP,
     +   IPDIV,CDIVLI, CTEMP,CTEMP,ICHOIC,'CN')
 
         IF(ICHOIC.GT.0 .AND. ICHOIC.LE.20)THEN
            IXIN=IDIVLI(ICHOIC)
            CALL SBYT(IST, IXIN,27,6)
            GOTO 10
         ENDIF
      ENDIF
 
      IF(IPSTO.NE.0)THEN
         CALL IGMENU(0,'Stores',X0STOL,X1STOL,Y0STOL,Y1STOL,
     +   0,CTEMP,IPSTO,CSTOLI,
     +   CTEMP,CTEMP,ICHOIC,'CN')
         IF(ICHOIC.GT.0 .AND.ICHOIC.LE.16)THEN
            I=ISTOLI(ICHOIC)
            IXIN=2
            CALL SBYT(I,IXIN,27,6)
            METAFL = 0
            GOTO 10
         ENDIF
      ENDIF
      PX=RQUEST(11)*XRANGE
      PY=RQUEST(12)*YRANGE
  200 CONTINUE
*     in store box
 
      IF(DZDINB(PX,PY,X0,X2,Y0STOR,Y0STOR+DY))THEN
         DO 210 I=1,20
            IF(PX .GT. XDIV(1,I) .AND. PX .LE. XDIV(2,I))THEN
               IXIN = I
               CALL SBYT(IST, IXIN,27,6)
               METAFL = 0
               GOTO 10
            ENDIF
  210    CONTINUE
         WRITE(*,*)' No active division'
         GOTO 80
      ENDIF
 
*     in division area
 
      IF(DZDINB(PX,PY,X1B,X2B,Y1B,Y2B))THEN
         REQINP = .TRUE.
         CALL DZDST1(IXIN,IFW,ILW,REQINP,PX,PY,
     &   LRET,IFARET,ILARET,CHOPT)
 
         IF(LRET .NE. 0)THEN
            IF(LACTWK)CALL IDAWK(IWDISP)
            CALL DZDISP(IXIN,LRET,RZPATH,CHOPT1, IWDISP, IWMETA,
     +      ILOCNR, IWKTYP)
            IF(LACTWK)CALL IACWK(IWDISP)
            IF(IFQUIT .NE. 0)GOTO 220
         ENDIF
         GOTO 20
      ENDIF
      GOTO 80
  220 CONTINUE
      IF(LACTWK)THEN
         CALL IDAWK(IWDISP)
      ENDIF
      END
