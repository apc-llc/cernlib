*
* $Id: hplopt.F,v 1.1.1.1 1996/01/19 10:50:10 mclareni Exp $
*
* $Log: hplopt.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:10  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/05 27/10/95  11.11.59  by  O.Couet
*-- Author :
      SUBROUTINE HPLOPT(CHOPTN,N)
*.==========>
*.       this routine sets options in /HPL1/ for HPLOT
*..=========>
#include "hbook/hcunit.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl3.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/quest.inc"
*
      PARAMETER (IDEF=20, IDEF1=6)
      PARAMETER (KIDEF=40, KIDEF1=12)
      PARAMETER (IOTHER=19)
*
      DIMENSION LOPTN(IDEF)
      EQUIVALENCE (LOPTN(1),LOVERT)
*
      LOGICAL LSETOP,LOPTN,LRESET
*
      CHARACTER*(*) CHOPTN(1)
      CHARACTER*4 CHOPTB
      CHARACTER*32 KEXP(KIDEF)
      CHARACTER*32 KEXP1(KIDEF1)
      CHARACTER*4 KDEF(IDEF),KNDEF(IDEF)
      CHARACTER*4 KDEF1(IDEF1),KNDEF1(IDEF1)
      CHARACTER*4 KTRUE(IDEF),KFALSE(IDEF),KOTHER(IOTHER)
      CHARACTER*4 CHSIZ
      CHARACTER*4 CHOPT
      LOGICAL LOHSTA
      SAVE LOHSTA
*     set up constants for allowed range of A4 paper sizes
*     range of values from 0 to 6 (A0 down to A6)
*
      DIMENSION ASIZE(14)
      DATA ASIZE /336.,475.2,168.,237.6,84.,118.8
     +            ,42.,59.4,21.,29.7,10.5,14.8,5.2,7.4/
*
      DATA KEXP/' VERTical or HORIzontal',' orientation of paper'
     +,' Error bars And Histogram are',' plotted (if both are present)'
     +,' scatter plots drawn with dots',' (NCHA) or 1 char./bin (CHA)'
     +,' functions drawn with (AST)',' or without (NAST) asterisks'
     +,' SOFTware or HARDware characters',' are used'
     +,' size is set to the largest',' square (SQR)'
     +,' HBOOK TITle (HTIT)',' or User TITle (UTIT) is printed'
     +,' table printed as TABles (TAB)',' or scatter plots (NTAB)'
     +,' a box is (BOX) or is not (NBOX)',' drawn around picture'
     +,' cross-wires are drawned (TIC)',' or not (NTIC) on each plot'
     +,' STAtistics are printed (STA)',' or not (NSTA) on each plot'
     +,' FIT parameters are printed',' or not (NFIT) on each plot'
     +,' picture is (ZFL) or is not',' (NZFL) put in Z data base'
     +,' PTO (Please Turn Over)',' (NPTO)'
     +,' BAR charts for histogram', ' (NBAR)'
     +,' Integer (DVXI) or Real (DVXR)',' divisions for X axis'
     +,' Integer (DVYI) or Real (DVYR)',' divisions for Y axis'
     +,' GRID or not grid (NGRI)',' on X and Y axis'
     +,' DATE is printed (DATE)',' or not (NDAT) on each plot'
     +,' FILE name is printed (FILE)',' or not (NFIL) on each plot'/

      DATA KEXP1/' page format for the plotter'
     +,' (A0,A1,A2,A3,A4,A5,A6)'
     +,' page number is (P   )',' or is not (NOPG) printed'
     +,' LINear or LOGarithmic scale',' in Y'
     +,' LINear or LOGarithmic scale',' in X'
     +,' LINear or LOGarithmic scale',' in Z (Lego or Surface)'
     +,' Filling statistics (HSTA)',' (HNST)'/
*
      DATA KDEF/ 'VERT','NEAH','NCHA','NAST'
     +,          'SOFT','NSQR','HTIT','TAB ','BOX '
     +,          'NTIC','NSTA','NFIT','NZFL','NPTO'
     +,          'NBAR','DVXR','DVYR','NGRI','NDAT'
     +,          'NFIL'/

      DATA KDEF1/ 'A4  ','NOPG','LINY','LINX','LINZ','HNST'/
*
      DATA KNDEF/'HORI','EAH ','CHA ','AST '
     +,          'HARD','SQR ','UTIT','NTAB','NBOX'
     +,          'TIC ','STA ','FIT ','ZFL ','PTO '
     +,          'BAR ','DVXI','DVYI','GRID','DATE'
     +,          'FILE'/

      DATA KNDEF1/ 'A0/6','P   ','LOGY','LOGX','LOGZ','HSTA'/
*
      DATA KTRUE/'VERT','EAH ','CHA ','AST '
     +,          'SOFT','SQR ','UTIT','TAB ','BOX '
     +,          'TIC ','STA ','FIT ','ZFL ','PTO '
     +,          'BAR ','DVXI','DVYI','GRID','DATE'
     +,          'FILE'/
*
      DATA KFALSE/'HORI','NEAH','NCHA','NAST'
     +,           'HARD','NSQR','HTIT','NTAB','NBOX'
     +,           'NTIC','NSTA','NFIT','NZFL','NPTO'
     +,           'NBAR','DVXR','DVYR','NGRI','NDAT'
     +,           'NFIL'/
*
      DATA KOTHER/'A0  ','A1  ','A2  ','A3  ','A4  ','A5  '
     +,           'A6  ','NOPG','LINX','LINY','LOGX','LOGY'
     +,           'LINZ','LOGZ','ZFL1','SHOW','*   ','HSTA','HNST'/
*
      DATA LOHSTA/.FALSE./
*
*._____________________________
*
      NARG=2
      CALL NOARG(NARG)
*
*           Return in IQUEST(11) the current options selected.
*
      IF(N.LT.0)THEN
         IQUEST(11)=0
         IF(INDEX(CHOPTN(1)(1:3),'LOG').NE.0)THEN
            IF(CHOPTN(1)(4:4).EQ.'X'.AND.LOGX.NE.0)IQUEST(11)=1
            IF(CHOPTN(1)(4:4).EQ.'Y'.AND.LOGY.NE.0)IQUEST(11)=1
            IF(CHOPTN(1)(4:4).EQ.'Z'.AND.LOGZ.NE.0)IQUEST(11)=1
            RETURN
         ENDIF
         DO 10 K=1,IDEF
            IF(CHOPTN(1)(1:4).EQ.KTRUE(K))THEN
               IF(LOPTN(K))IQUEST(11)=1
               RETURN
            ENDIF
   10    CONTINUE
         RETURN
      ENDIF
*
      NUM=1
      IF(NARG.GE.2) NUM=N
      NUMOPT=0
      LRESET=.FALSE.
*
   20 NUMOPT=NUMOPT+1
      IF(NUMOPT.GT.NUM) GOTO 120
      I=0
      LSETOP=.TRUE.
      DO 30 K=1,IDEF
         CHOPTB=CHOPTN(NUMOPT)
         IF(CHOPTB.EQ.'STAT')CHOPTB='STA'
         IF(CHOPTB.EQ.KTRUE(K))THEN
            I=K
            IF(I.EQ.1)LRESET=.TRUE.
            IF(I.EQ.13)LOZFL1=.FALSE.
            GOTO 80
         ENDIF
   30 CONTINUE
      LSETOP=.FALSE.
      DO 40 K=1,IDEF
         IF(CHOPTN(NUMOPT).EQ.KFALSE(K))THEN
            I=K
            IF(I.EQ.1)LRESET=.TRUE.
            IF(I.EQ.13)THEN
               CALL IGZSET('X')
               LOZFL1 = .FALSE.
            ENDIF
            GOTO 80
         ENDIF
   40 CONTINUE
      DO 50 K=1,IOTHER
         IF(CHOPTN(NUMOPT).EQ.KOTHER(K))THEN
            I=K
            IF(I.LE.7)LRESET=.TRUE.
            GOTO 60
         ENDIF
   50 CONTINUE
      IF(I.EQ.0) GOTO 70
*
   60 CONTINUE
      IF(I.LE.7)THEN
*
*           Decide here which metric paper sizes to allow
*
         IVSIZE=I-1
         K=2*IVSIZE+1
         IF (LOVERT) THEN
            XSIZ=ASIZE(K)
            YSIZ=ASIZE(K+1)
         ELSE
            XSIZ=ASIZE(K+1)
            YSIZ=ASIZE(K)
         ENDIF
         GOTO 20
      ENDIF
*
*          No page requested
*
      IF(I.EQ.8)THEN
         IPGSIZ=0
         GOTO 20
      ENDIF
*
*          System of coordinates : linear or logarithmic
*
      IF(I.EQ.9)THEN
         LINX=1
         LOGX=0
         GOTO 20
      ELSEIF(I.EQ.10)THEN
         LINY=1
         LOGY=0
         GOTO 20
      ELSEIF(I.EQ.11)THEN
         LINX=0
         LOGX=1
         GOTO 20
      ELSEIF(I.EQ.12)THEN
         LINY=0
         LOGY=1
         GOTO 20
      ELSEIF(I.EQ.13)THEN
         LINZ=1
         LOGZ=0
         GOTO 20
      ELSEIF(I.EQ.14)THEN
         LINZ=0
         LOGZ=1
         GOTO 20
      ELSEIF(I.EQ.15)THEN
         LOZFL=.TRUE.
         LOZFL1=.TRUE.
         GOTO 20
      ELSEIF(I.EQ.16)THEN
         GOTO 130
      ENDIF
*
*     Set default value
*
      IF(I.EQ.17)THEN
         LRESET=.TRUE.
         LOVERT=.TRUE.
         LOEAH=.FALSE.
         LOCHA=.FALSE.
         LOASTK=.FALSE.
         LOSOFT=.TRUE.
         LOSQR=.FALSE.
         LOUTIT=.FALSE.
         LOTAB=.TRUE.
         LOBOX=.TRUE.
         LOHBOX=.TRUE.
         LOTIC=.FALSE.
         LOSTAT=.FALSE.
         LOFIT=.FALSE.
         LOZFL=.FALSE.
         LOZFL1=.FALSE.
         CALL IGZSET('X')
         LOPTO=.FALSE.
         LOBAR=.FALSE.
         LODVXI=.FALSE.
         LODVYI=.FALSE.
         LOGRID=.FALSE.
         LODATE=.FALSE.
         LOFILE=.FALSE.
         IVSIZE=4
         XSIZ=20.
         YSIZ=20.
         IPGSIZ=0
         LINX=1
         LINY=1
         LOGX=0
         LOGY=0
         LINZ=1
         LOGZ=0
         CALL HSTAF('NO')
         LOHSTA=.FALSE.
         GOTO 20
      ENDIF
*
*     HSTA and HNST
*
      IF(I.EQ.18)THEN
         CALL HSTAF('YES')
         LOHSTA=.TRUE.
         GOTO 20
      ENDIF
      IF(I.EQ.19)THEN
         CALL HSTAF('NO')
         LOHSTA=.FALSE.
         GOTO 20
      ENDIF
*----
*     Check for valid page number
*----
   70 CONTINUE
      IF(   (CHOPTN(NUMOPT).EQ.'*P  ')
     +.OR.  (CHOPTN(NUMOPT).EQ.'P   '))THEN
         IPGSIZ=1
         GOTO 20
      ELSE IF(CHOPTN(NUMOPT).EQ.'**P ')THEN
         IPGSIZ=2
         GOTO 20
      ELSE IF(CHOPTN(NUMOPT).EQ.'***P')THEN
         IPGSIZ=3
         GOTO 20
      ELSE
         GOTO 110
      ENDIF
*
   80 CONTINUE
      LOPTN(I)=LSETOP
*
*          Option VERT/HORI called
*
      IF (I.EQ.1) THEN
         IF (LSETOP) THEN
*
*          On impose XSIZ<=YSIZ
*
            IF (XSIZ.GT.YSIZ) THEN
               XX=XSIZ
               XSIZ=YSIZ
               YSIZ=XX
            ENDIF
         ELSE
*
*           On impose YSIZ<=XSIZ
*
            IF (XSIZ.LT.YSIZ) THEN
               XX=XSIZ
               XSIZ=YSIZ
               YSIZ=XX
            ENDIF
         ENDIF
         GOTO 20
      ENDIF
*
*     option SOFT/HARD called
*
      IF (I.EQ.5) THEN
         IF (LSETOP) THEN
            DO 90  I=1,5
               IHFONT(I)=2
   90       CONTINUE
         ELSE
            DO 100 I=1,5
               IHFONT(I)=10
  100       CONTINUE
         ENDIF
         GOTO 20
      ENDIF
*
      GOTO 20
*
  110 WRITE(LERR,10000) CHOPTN(NUMOPT)
      GOTO 20
*
  120 IF(IPLNUM.LT.0)RETURN
*
*         Reset windowing if A3/A4 or HORI/VERT options called
*          or if OPT * (set all default value)
*
      IF(.NOT.LRESET)RETURN
      CALL HPLZON(1,1,1,' ')
      IF(XSIZ.LE.YSIZ)THEN
         LOVERT=.TRUE.
      ELSE
         LOVERT=.FALSE.
      ENDIF
      CALL HPLSCM
      RETURN
*
*          Show all the options which are set
*
  130 CONTINUE
      WRITE (LERR,10100)
      WRITE (LERR,10200)
      WRITE (LERR,10300)
      WRITE (LERR,10400)
      WRITE (LERR,10300)
      DO 140 I=1,IDEF
         IF(LOPTN(I))THEN
            WRITE (LERR,10500) KTRUE(I),KDEF(I),KNDEF(I) ,KEXP(2*I-1),
     +      KEXP(2*I)
         ELSE
            WRITE (LERR,10500) KFALSE(I),KDEF(I),KNDEF(I) ,KEXP(2*I-1),
     +      KEXP(2*I)
         ENDIF
  140 CONTINUE
      WRITE (CHSIZ,10600) IVSIZE
      CHOPT='A'//CHSIZ
      WRITE (LERR,10500) CHOPT,KDEF1(1),KNDEF1(1),KEXP1(1),KEXP1(2)
      IF(IPGSIZ.EQ.0)THEN
         CHOPT='NOPG'
      ELSE
         CHOPT='P   '
      ENDIF
      WRITE (LERR,10500) CHOPT,KDEF1(2),KNDEF1(2),KEXP1(3),KEXP1(4)
      IF(LOGY.EQ.1)THEN
         CHOPT='LOGY'
      ELSE
         CHOPT='LINY'
      ENDIF
      WRITE (LERR,10500) CHOPT,KDEF1(3),KNDEF1(3),KEXP1(5),KEXP1(6)
      IF(LOGX.EQ.1)THEN
         CHOPT='LOGX'
      ELSE
         CHOPT='LINX'
      ENDIF
      WRITE (LERR,10500) CHOPT,KDEF1(4),KNDEF1(4),KEXP1(7),KEXP1(8)
      IF(LOGZ.EQ.1)THEN
         CHOPT='LOGZ'
      ELSE
         CHOPT='LINZ'
      ENDIF
      WRITE (LERR,10500) CHOPT,KDEF1(5),KNDEF1(5),KEXP1(9),KEXP1(10)
      CHOPT='HNST'
      IF(LOHSTA)CHOPT='HSTA'
      WRITE (LERR,10500) CHOPT,KDEF1(6),KNDEF1(6),KEXP1(11),KEXP1(12)
      WRITE (LERR,10300)
      GOTO 20
*
10000 FORMAT(' ***** ERROR in HPLOPT : Unvailable option --> ',A4)
10100 FORMAT(1X,'+',77(1H-),'+')
10200 FORMAT(1X,'|',28X,'HPLOPT : Option values',27X,'|')
10300 FORMAT(1X,'+',13(1H-),'+',13(1H-),'+',15(1H-),'+',33(1H-),'+')
10400 FORMAT(1X,'|   Current   |   Default   |  Alternative  '
     +,      '|           Explanation           |')
10500 FORMAT(' |',5X,A4,4X,'|',5X,A4,4X,'|',6X,A4,5X,'|',A32,' |',/
     +,' |',13X,'|',13X,'|',15X,'|',A32,' |')
10600 FORMAT (I1)
*
      END
