*
* $Id: slinit.F,v 1.1.1.1 1996/03/01 11:39:29 mclareni Exp $
*
* $Log: slinit.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:29  mclareni
* Paw
*
*
*CMZ :  1.10/00 20/09/90  10.54.07  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE SLINIT
      COMMON /SLIDE0/ NBLINE
      COMMON /SLIDE1/ XB1,XB2,YB1,YB2,HC
      PARAMETER (ILINE=99)
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 LINECP(99)
      CHARACTER*80 LINE(99)
      EQUIVALENCE (LINE(1),GUID(1)),(LINECP(1),GUID(100))
      PARAMETER (MAXCMD=1000)
      COMMON /SLIDE7/  LSTCMD(MAXCMD,2),PARCMD(MAXCMD,2),JCD
      COMMON /SLIDE18/ ERR
      COMMON /SLIDE19/ TDISP
      COMMON /SLIDE24/ KOLD
      COMMON /SLIDE25/ LSTOPN
      COMMON /SLIDE26/ ADISP
      COMMON /SLIDE27/ FONT,IFONT,OLDFNT,OLDPRE,PREC,OLDCOL,ICOL
      COMMON /SLIDE28/ FACTL
      COMMON /SLIDE29/ PASS,CSHI
      COMMON /SLIDE30/ TSPACE(4,1)
      COMMON /SLIDE32/ HLIST(20),OPLS
      COMMON /SLIDE31/STRPNT,SPNT
      CHARACTER*3 STRPNT,SPNT
      COMMON /SLIDE34/ FACT(13)
*.______________________________________
*
* FACT contain the proportional factor about FONT 0/2
* and PostScript fonts -1/0 to -7/0 and -12/0 and -13/0.
* Factors in position 8 to 11 are the larger constants of
* non proportional fonts -8 to -11.
*
      FACT(1) =0.94
      FACT(2)=0.92
      FACT(3)=0.92
      FACT(4) =0.91
      FACT(5) =0.91
      FACT(6) =0.98
      FACT(7) =0.98
****
      FACT(8) =1.09
      FACT(9) =1.1
      FACT(10)=1.06
      FACT(11)=1.05

*      FACT(8) =1.14
*      FACT(9) =1.16
*      FACT(10)=1.138
*      FACT(11)=1.135
****
      FACT(12)=1.12
      FACT(13)=0.93
*
*     default values for BOX
*
      XB1=0.
      XB2=20.
      YB1=0.
      YB2=25.
*
*     default character FONT and PRECISION
*
      FONT =0.
      IFONT=0
      OLDFNT=0.
      PREC=2.
      OLDPRE=2.
      FACTL=1.
*
*     default values for BOLD
*
      PASS=20.
      CSHI=0.005
*
*     default value for start symbol of list(STAR)
*
      STRPNT='"4#'
      SPNT  ='H  '
*
*     default value for left margin
*
      TSPACE(3,1)=0
*
*     default value for right margin
*
      TSPACE(4,1)=0
*
*     default color (BLACK=1)
*
      OLDCOL=1.
      ICOL=1
*
      OPLS=0
      ADISP=0.
      TDISP=0.
      LSTOPN=0
      NBLINE = 0
      JCD=0
      ERR=0
      KOLD=-1
      CALL IGSET('TXFP',PREC)
      CALL ISTXCI(INT(OLDCOL))
      CALL IGSET('PASS',1.)
      DO 10 I=1,20
         HLIST(I)=0
   10 CONTINUE
      DO 20 I=1,ILINE
         LINE(I) = ' '
   20 CONTINUE
      DO 30 I=1,MAXCMD
         LSTCMD(I,1)=0
         LSTCMD(I,2)=0
         PARCMD(I,1)=0.
         PARCMD(I,2)=0.
   30 CONTINUE
*
      END
