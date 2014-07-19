*
* $Id: igmenu.F,v 1.1.1.1 1996/02/14 13:11:15 mclareni Exp $
*
* $Log: igmenu.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:15  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/02 03/11/93  11.47.50  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGMENU(MN,TITLE,X1,X2,Y1,Y2,NBU,CHUSER
     +,                 N,CHITEM,CHDEF,CHVAL,ICHOIC,CHOPT)
*.===========>
*.
*.   This routine displays a menu and returns the users choice in ICHOIC
*.
*. _Input parameters:
*.
*. INTEGER MN            : Menu number. To use bitmap capabilities.
*. CHARACTER TITLE       : Menu title.
*. REAL X1,Y1            : Down left corner menu coordinates.
*. REAL X2,Y2            : Up right coner menu coordinates.
*. INTEGER NBU           : User squares number.
*. CHARACTER CHUSER(NBU) : Text in the users squares.
*. INTEGER N             : Number of items.
*. CHARACTER CHITEM(N)   : Text of the item.
*. CHARACTER CHDEF(N)    : Text of parameters.
*. INTEGER ICHOIC        : Input parameter if CHOPT='U' or CHOPT='H'.
*. CHARACTER CHOPT       : Options.
*.
*.      CHOPT='H' : The picked choice is Highlighted the last current
*.                  choice is given in ICHOIC.
*.      CHOPT='D' : Display the menu.
*.      CHOPT='C' : Permit a choice in the displayed menu.
*.      CHOPT='E' : Erase the menu.
*.      CHOPT='P' : The menu is a menu with parameters.
*.      CHOPT='R' : Return the current position of the menu.
*.      CHOPT='S' : Software character are used.
*.      CHOPT='U' : Update the user text user squares. The user square
*.                  number is given in ICHOIC. N.B. that options 'U'
*.                  and 'H' are incompatible because they used both
*.                  ICHOIC as input parameter.
*.      CHOPT='N' : The last input position is used to compute the value
*.                  of ICHOIC.
*.      CHOPT='M' : GKS Metafiles are activated.
*.      CHOPT='Z' : HIGZ metafiles are activated.
*.      CHOPT='B' : A rubberbanding boxe is used for the locator.
*.      CHOPT='T' : The title bar is not drawn, then the menu can not
*.                  be moved interactively.
*.      CHOPT='W' : The menu is drawn with Width.
*.      CHOPT='A' : The menu is drawn with shAdow.
*.      CHOPT='V' : Draw only the vertical part of Width.
*.      CHOPT='O' : 'V' Without "Oreilles".
*.      CHOPT='I' : Input menu. A parameter menu is displayed and IGMENU
*.                  Enter directly in request string. This is usefull to
*.                  do a request string without having a very complicated
*.                  initialization part.
*.      CHOPT='K' : Key menu. The user keys are drawn as a keyboard
*.
*. _Output parameters:
*.
*. CHARACTER CHVAL(N) : Array of parameters choiced.
*. INTEGER ICHOIC : Choice number.
*.
*. _Specials characters in first position of CHITEM(i):
*.
*. '|' The left of CHITEM(i) is colored in black
*. '-' The box of CHITEM(i) is colored in gray
*.
*..==========> (O.Couet)
#include "higz/hiques.inc"
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/himenu.inc"
#include "higz/himeta.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      DIMENSION IOPT(19),IPLACE(50)
      SAVE IOPT
      DIMENSION X(2),Y(2)
      CHARACTER*(*) CHOPT,CHITEM(*),TITLE,CHDEF(*),CHVAL(*),CHUSER(*)
      CHARACTER*256 CHDEFT
      EQUIVALENCE (IOPT(1),IOPTH) ,(IOPT(2),IOPTD) ,(IOPT(3),IOPTC)
      EQUIVALENCE (IOPT(4),IOPTE) ,(IOPT(5),IOPTP) ,(IOPT(6),IOPTR)
      EQUIVALENCE (IOPT(7),IOPTS) ,(IOPT(8),IOPTU) ,(IOPT(9),IOPTN)
      EQUIVALENCE (IOPT(10),IOPTM),(IOPT(11),IOPTZ),(IOPT(12),IOPTB)
      EQUIVALENCE (IOPT(13),IOPTT),(IOPT(14),IOPTW),(IOPT(15),IOPTV)
      EQUIVALENCE (IOPT(16),IOPTO),(IOPT(17),IOPTI),(IOPT(18),IOPTA)
      EQUIVALENCE (IOPT(19),IOPTK)
      LOGICAL ZFS,GFS,MFS
*.______________________________________
*
      CALL UOPTC (CHOPT,'HDCEPRSUNMZBTWVOIAK',IOPT)
#if defined(CERNLIB_MAIL)
*
*              MAIL option
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(4I3,I10)') 557,MN,NBU,N,ICHOIC
         CALL IMWRIT(1)
         WRITE (CHMAIL,'(4E16.7,A16)') X1,X2,Y1,Y2,CHOPT
         CALL IMWRIT(2)
         IF(IOPTT.EQ.0)THEN
            CHMAIL=' '
            CHMAIL=TITLE
            CALL IMWRIT(2)
         ENDIF
         DO 10 I=1,N
            CHMAIL=' '
            WRITE (CHMAIL,'(2A32)') CHITEM(I),CHDEF(I)
            CALL IMWRIT(2)
   10    CONTINUE
         DO 20 I=1,NBU
            CHMAIL=' '
            CHMAIL=CHUSER(I)
            CALL IMWRIT(2)
   20    CONTINUE
*
         IF(IOPTC.NE.0)THEN
            READ (5,'(I5)') ICHOIC
            IF(IOPTP.NE.0.AND.N.GT.0)THEN
               CHMAIL='DUMMY'
               DO 30 I=1,N
                  CALL IMWRIT(2)
                  READ (5,'(A)') CHVAL(I)
   30          CONTINUE
            ENDIF
         ENDIF
         IF(IOPTI.NE.0)THEN
            READ (5,'(A)') CHVAL(1)
         ENDIF
         CALL IMWRIT(5)
         RETURN
      ENDIF
#endif
*
*              Check the valididy of the menu
*
      IF((NBU+N.EQ.0).AND.(IOPTT.EQ.1))THEN
         ICHOIC=0
         CALL IGERR('Empty menu','IGMENU')
         RETURN
      ENDIF
*
*              Save the currents attributes and the current NT
*
      CALL IZSAV
*
      ZFS=ZFLAG
      GFS=GFLAG
      IF(IOPTZ.NE.0)THEN
         ZFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      ELSE
         ZFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      ENDIF
      GFLAG=.TRUE.
*
      MFS=METACT
      IF(IOPTM.NE.0)THEN
         IF(.NOT.METACT)CALL IACWK(IDMETA)
         METACT=.TRUE.
      ELSE
         IF(METACT)CALL IDAWK(IDMETA)
         METACT=.FALSE.
      ENDIF
*
      CALL ISTXCI(1)
      CALL ISPLCI(1)
      CALL ISFAIS(1)
      CALL ISLN(1)
      CALL ISLWSC(1.)
      IF(ISOFT.NE.0)THEN
         CALL ISTXFP(0,2)
      ELSE
#if defined(CERNLIB_X11)
         CALL ISTXFP(-8,0)
#endif
#if !defined(CERNLIB_X11)
         CALL ISTXFP(1,0)
#endif
      ENDIF
      CALL IGSET('TANG',0.)
*
      REDLOC=REDIT
      CALL IGSRAP(0.)
      CALL ISELNT(0)
*
*              Set the locator type
*
      IF(IOPTB.NE.0)THEN
         ICURS=51
      ELSE
         ICURS=1
      ENDIF
*
*              Initialize the menu position
*
      MENNUM=MN
      NBCHOI=N
      NBUSER=NBU
      IF((ABS(X1-X2).LT.0.01).OR.(ABS(Y1-Y2).LT.0.01))THEN
         ICHOIC=0
         IF(IOPTC.NE.0)THEN
            REDIT=REDLOC
            CALL IGLOC(ICURS,NT,IBN,XP,YP,XWC,YWC)
            REDIT=0.
         ENDIF
         IF(IBN.EQ.0)ICHOIC=-1000
         RETURN
      ENDIF
      XPOS(1)=X1
      XPOS(2)=X2
      YPOS(1)=Y1
      YPOS(2)=Y2
      RATIO=RMDSY/RMDSX
      IF(RMDSX.GT.RMDSY)THEN
         YPOS(1)=Y1*RATIO
         YPOS(2)=Y2*RATIO
      ELSE
         XPOS(1)=X1/RATIO
         XPOS(2)=X2/RATIO
      ENDIF
      IF(XPOS(2).LT.XPOS(1))THEN
         R=XPOS(2)
         XPOS(2)=XPOS(1)
         XPOS(1)=R
      ENDIF
      IF(YPOS(2).LT.YPOS(1))THEN
         R=YPOS(2)
         YPOS(2)=YPOS(1)
         YPOS(1)=R
      ENDIF
*
*              Set the menu parameters in case of choice
*
      IF((IOPTN.NE.0).OR.(IOPTC.NE.0))THEN
         CALL IGSMP(IOPT)
      ENDIF
*
*              Input menu
*
      IF(IOPTI.NE.0)THEN
         IOPT(13)=1
         IOPT(5)=1
         IOPT(19)=0
         NBUSER=0
         CHVAL(1)=' '
         CALL IGDIME(TITLE,CHUSER,CHITEM,CHDEF,CHVAL,IOPT)
         CALL ISTXAL(0,0)
         RQUEST(81)=((XM+RINT)*RMDSX)/RDWXMA
         RQUEST(82)=((XPOS(2)-RINT)*RMDSX)/RDWXMA
         RQUEST(91)=((Y3-YSTEP)*RMDSY)/RDWYMA
         RQUEST(92)=((Y3-YSTEP+OTH)*RMDSY)/RDWYMA
         CHDEFT=CHVAL(1)
         IWTMP=-IDID
         CALL IRQST(IWTMP,1,ISTA,ILEN,CHDEFT)
         IF(ISTA.NE.0)THEN
            IILEN=LENOCC(CHDEFT)
            CHVAL(1)=CHDEFT(1:IILEN)
         ENDIF
      ENDIF
*
*              Update the user keys
*
      IF((IOPTU.NE.0).AND.(ICHOIC.GT.0))THEN
         CALL IGSMP(IOPT)
         CALL ISFAIS(1)
         CALL ISFACI(0)
         CALL ISPLCI(1)
         CALL ISTXCI(1)
         CALL ISCHH(UTH)
         CALL ISTXAL(2,0)
         CALL IGSET('BORD',1.)
         Y(1)=YPOS(1)
         Y(2)=Y4
         X(1)=XPOS(1)+(ICHOIC-1)*USIZ+IOPT(19)*(ICHOIC-1)*10.*DEC
         X(2)=X(1)+USIZ
         CALL IGBOX(X(1),X(2),Y(1),Y(2))
         YU=YPOS(1)+RINT/2-UTH/2
         XU=X(1)+(USIZ/2.)
         CALL ITX(XU,YU,CHUSER(ICHOIC))
      ENDIF
*
*              Set the current highlihted choice
*
      IF(IOPTH.NE.0)THEN
         IF(ICHOIC.LT.0.OR.ICHOIC.GT.N)THEN
            ICUCH=0
         ELSE
            ICUCH=ICHOIC
         ENDIF
      ENDIF
*
*              Display the menu
*
      IF(IOPTD.NE.0)THEN
         IF(NBUSER.GE.100)NBUSER=99
         ISOFT=0
         IF(IOPTS.NE.0)ISOFT=1
         IF(IOPTP.NE.0)THEN
            DO 40 I=1,NBCHOI
               ILEN=INDEX(CHDEF(I),',')-1
               IF(ILEN.LT.0)ILEN=LENOCC(CHDEF(I))
               CHVAL(I)=CHDEF(I)(1:ILEN)
   40       CONTINUE
         ENDIF
         ICUCH=0
         CALL IGDIME(TITLE,CHUSER,CHITEM,CHDEF,CHVAL,IOPT)
      ENDIF
*
*              Request the choice
*
      IF(IOPTC.NE.0)THEN
         DO 50 I=1,50
            IPLACE(I)=1
   50    CONTINUE
         IF(IOPTP.NE.0)THEN
            DO 60 I=1,NBCHOI
               ILEN=INDEX(CHDEF(I),',')-1
               IF(ILEN.LT.0)ILEN=LENOCC(CHDEF(I))
               CHVAL(I)=CHDEF(I)(1:ILEN)
   60       CONTINUE
            IF(IOPTN.NE.0)GOTO 80
   70       REDIT=REDLOC
            CALL IGLOC(ICURS,NT,IBN,XP,YP,XWC,YWC)
            REDIT=0.
            IQUEST(10)=NT
            RQUEST(11)=XP
            RQUEST(12)=YP
            RQUEST(13)=XWC
            RQUEST(14)=YWC
            IF(IBN.EQ.0)THEN
               ICHOIC=-1000
               GOTO 120
            ENDIF
   80       CALL IGGCH(XP,YP,ICHOIC,IOPT)
            IF(ICHOIC.EQ.-101)THEN
               CALL IGCMP(TITLE,CHUSER,CHITEM,CHDEF,CHVAL,IOPT,0)
               GOTO 70
            ELSEIF(ICHOIC.EQ.-102)THEN
               CALL IGCMP(TITLE,CHUSER,CHITEM,CHDEF,CHVAL,IOPT,1)
               GOTO 70
            ELSEIF(ICHOIC.GT.0)THEN
               ICUCH=ICHOIC
               IF(XP.LE.XM)THEN
                  CALL IGGDEF(CHDEF(ICUCH),1,CHVAL(ICUCH))
                  CALL ISFAIS(1)
                  CALL ISFACI(0)
                  CALL ISPLCI(1)
                  CALL ISTXCI(1)
                  CALL ISCHH(OTH)
                  CALL IGSET('BORD',1.)
                  X(1)=XM
                  X(2)=XM+XSIZ-RINT
                  Y(1)=Y3-ICUCH*YSIZ
                  Y(2)=Y3-(ICUCH-1)*YSIZ
                  CALL IGBOX(X(1),X(2),Y(1),Y(2))
                  CALL ISTXAL(0,0)
                  CALL ITX(XM+RINT,Y3-YSIZ*(ICUCH-1)-YSTEP
     +,                    CHVAL(ICUCH))
                  CALL ISTXAL(2,0)
                  LINENO=ICUCH
                  IF(LINENO.GT.50)LINENO=50
                  IPLACE(LINENO)=1
                  GOTO 70
               ELSEIF((XP.GE.(XPOS(2)-RINT)).AND.
     +                (INDEX(CHDEF(ICUCH),',').NE.0))THEN
                  CHDEFT=CHDEF(ICUCH)
                  IMOD=1
                  ILEN=LENOCC(CHDEFT)
                  IF(CHDEFT(ILEN:ILEN).EQ.',')CHDEFT(ILEN:ILEN)=' '
   90             ILEN=INDEX(CHDEFT,',')
                  IF(ILEN.NE.0)THEN
                     CHDEFT=CHDEFT(ILEN+1:LENOCC(CHDEFT))
                     IMOD=IMOD+1
                     GOTO 90
                  ENDIF
                  LINENO=ICUCH
                  IF(LINENO.GT.50)LINENO=50
                  IPLACE(LINENO)=IPLACE(LINENO)+1
                  IF(IPLACE(LINENO).GT.IMOD)IPLACE(LINENO)=1
                  CALL IGGDEF(CHDEF(ICUCH),IPLACE(LINENO)
     +,                       CHVAL(ICUCH))
                  CALL ISFAIS(1)
                  CALL ISFACI(0)
                  CALL ISPLCI(1)
                  CALL ISTXCI(1)
                  CALL ISCHH(OTH)
                  CALL IGSET('BORD',1.)
                  X(1)=XM
                  X(2)=XM+XSIZ-RINT
                  Y(1)=Y3-ICUCH*YSIZ
                  Y(2)=Y3-(ICUCH-1)*YSIZ
                  CALL IGBOX(X(1),X(2),Y(1),Y(2))
                  CALL ISTXAL(0,0)
                  CALL ITX(XM+RINT,Y3-YSIZ*(ICUCH-1)-YSTEP
     +,                    CHVAL(ICUCH))
                  CALL ISTXAL(2,0)
                  GOTO 70
               ELSEIF((XP.GT.XM).AND.(XP.LT.(XPOS(2)-RINT)))THEN
                  CALL ISFAIS(1)
                  CALL ISFACI(0)
                  CALL ISPLCI(1)
                  CALL ISTXCI(1)
                  CALL IGSET('BORD',1.)
                  CALL ISCHH(OTH)
                  X(1)=XM
                  X(2)=XM+XSIZ-RINT
                  Y(1)=Y3-ICUCH*YSIZ
                  Y(2)=Y3-(ICUCH-1)*YSIZ
                  CALL IGBOX(X(1),X(2),Y(1),Y(2))
                  CALL ISTXAL(0,0)
                  RQUEST(81)=((XM+RINT)*RMDSX)/RDWXMA
                  RQUEST(82)=((XPOS(2)-RINT)*RMDSX)/RDWXMA
                  RQUEST(91)=((Y3-YSIZ*(ICUCH-1)-YSTEP)*RMDSY)/RDWYMA
                  RQUEST(92)=((Y3-YSIZ*(ICUCH-1)-YSTEP+OTH)*RMDSY)/
     +                        RDWYMA
                  CHDEFT=CHVAL(ICUCH)
                  IWTMP=-IDID
                  CALL IRQST(IWTMP,1,ISTA,ILEN,CHDEFT)
                  IF(ISTA.NE.0)THEN
                     CHVAL(ICUCH)=' '
                     IILEN=LENOCC(CHDEFT)
                     CHVAL(ICUCH)=CHDEFT(1:IILEN)
                  ENDIF
                  CALL IGBOX(X(1),X(2),Y(1),Y(2))
                  CALL ITX(XM+RINT,Y3-YSIZ*(ICUCH-1)-YSTEP
     +,                    CHVAL(ICUCH))
                  CALL ISTXAL(2,0)
                  GOTO 70
               ENDIF
               GOTO 70
            ENDIF
         ELSE
            IF(IOPTN.NE.0)GOTO 110
  100       REDIT=REDLOC
            CALL IGLOC(ICURS,NT,IBN,XP,YP,XWC,YWC)
            REDIT=0.
            IQUEST(10)=NT
            RQUEST(11)=XP
            RQUEST(12)=YP
            RQUEST(13)=XWC
            RQUEST(14)=YWC
            IF(IBN.EQ.0)THEN
               ICHOIC=-1000
               GOTO 120
            ENDIF
  110       CALL IGGCH(XP,YP,ICHOIC,IOPT)
            IF(ICHOIC.EQ.-101)THEN
               CALL IGCMP(TITLE,CHUSER,CHITEM,CHDEF,CHVAL,IOPT,0)
               GOTO 100
            ELSEIF(ICHOIC.EQ.-102)THEN
               CALL IGCMP(TITLE,CHUSER,CHITEM,CHDEF,CHVAL,IOPT,1)
               GOTO 100
            ELSEIF(ICHOIC.LT.0.AND.ICHOIC.GT.-100)THEN
               IF(IOPTH.NE.0.AND.IOPTP.EQ.0)CALL IGHIGH(ICHOIC,CHITEM
     +,                                                 CHUSER,IOPT)
            ELSEIF(ICHOIC.GT.0)THEN
               IF(IOPTH.NE.0.AND.IOPTP.EQ.0)CALL IGHIGH(ICHOIC,CHITEM
     +,                                                 CHUSER,IOPT)
               ICUCH=ICHOIC
            ENDIF
         ENDIF
      ENDIF
*
*              Erase the menu
*
      IF(IOPTE.NE.0)THEN
         CALL IGDELM(IOPT)
      ENDIF
*
*              Return the menu position
*
  120 IF(IOPTR.NE.0)THEN
         X1=XPOS(1)
         X2=XPOS(2)
         Y1=YPOS(1)
         Y2=YPOS(2)
         IF(RMDSX.GT.RMDSY)THEN
            Y1=Y1/RATIO
            Y2=Y2/RATIO
         ELSE
            X1=X1*RATIO
            X2=X2*RATIO
         ENDIF
      ENDIF
*
*              Reset the initial environment
*
      CALL IZSET
*
      CALL IGTERM
      CALL IGSRAP(REDLOC)
      ZFLAG=ZFS
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=GFS
      IF(MFS)THEN
         IF(METACT)THEN
            RETURN
         ELSE
            CALL IACWK(IDMETA)
         ENDIF
      ELSE
         IF(METACT)THEN
            CALL IDAWK(IDMETA)
         ELSE
            RETURN
         ENDIF
      ENDIF
      METACT=MFS
*
      END
