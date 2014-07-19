*
* $Id: igpave.F,v 1.1.1.1 1996/02/14 13:10:37 mclareni Exp $
*
* $Log: igpave.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/11 07/04/95  10.07.30  by  O.Couet
*-- Author :    O.Couet   28/03/89
      SUBROUTINE IGPAVE(X1I,X2I,Y1I,Y2I,DZ,ISBOX,ISFRAM,CHOPT)
*.==============>
*.
*.   Draw a paving-block (box with 3D effect). Useful for slides.
*. ISBOX (ISFRAM) may be 1000+ICOLOR where ICOLOR is the color index
*. of the box (frame), or 2000+IPAT where IPAT is the pattern index of
*. the box (frame), otherwise the style index.
*. If ISBOX(ISFRAM)=0, only the box contour is drawn with the current
*. polyline attributes.
*. By default the Top and the Right frame are drawn. CHOPT='TR'.
*.
*. _Input parameters:
*.
*.  REAL X1I : X bottom left corner of box
*.  REAL X2I : X top right corner of box
*.  REAL Y1I : Y bottom left corner of box
*.  REAL Y2I : Y top right corner of box
*.  REAL DZ : Box width
*.  INTEGER ISBOX : Box style
*.  INTEGER ISFRAM : Frame style' I D=5
*.  CHARACTER CHOPT : Character option
*.            CHOPT='T' Top frame
*.            CHOPT='B' Bottom frame
*.            CHOPT='R' Right frame
*.            CHOPT='L' Left frame
*.            CHOPT='-' Negative sense
*.            CHOPT='S' Shadow
*.            CHOPT='P' Cut the shadow
*.            CHOPT='K' Key
*.            CHOPT='D' Delete (usefull for IGMENU)
*.
*.============> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      DIMENSION IOPT(9)
      EQUIVALENCE (IOPTT,IOPT(1)),(IOPTB,IOPT(2))
      EQUIVALENCE (IOPTL,IOPT(3)),(IOPTR,IOPT(4))
      EQUIVALENCE (IOPTM,IOPT(5)),(IOPTS,IOPT(6))
      EQUIVALENCE (IOPTP,IOPT(7)),(IOPTK,IOPT(8))
      EQUIVALENCE (IOPTD,IOPT(9))
      CHARACTER*(*) CHOPT
      DIMENSION X(4),Y(4)
      LOGICAL ZFSAV
*.______________________________________
*
      CALL UOPTC(CHOPT,'TBLR-SPKD',IOPT)
      IF(CHOPT.EQ.' ')THEN
         IOPTT=1
         IOPTR=1
      ELSE
         IF((IOPTT+IOPTR+IOPTL+IOPTB).GE.3
     +.OR.  (IOPTL+IOPTR).EQ.2
     +.OR.  (IOPTT+IOPTB).EQ.2      )THEN
            CALL IGERR('Incompatible options, default is taken'
     +,                'IGPAVE')
            IOPTT=1
            IOPTR=1
            IOPTB=0
            IOPTL=0
            IOPTM=0
         ENDIF
      ENDIF
*
*              Sort the PAVE coordinates.
*
      IF (X1I.GT.X2I) THEN
         X1 = X2I
         X2 =X 1I
      ELSE
         X1 = X1I
         X2 = X2I
      ENDIF
      IF (Y1I.GT.Y2I) THEN
         Y1 = Y2I
         Y2 = Y1I
      ELSE
         Y1 = Y1I
         Y2 = Y2I
      ENDIF
*
*              Store the PAVE inside the current picture.
*              The sorted coordinates are stored otherwise picking
*              is impossible.
*
#if defined(CERNLIB_ZEBRA)
      IF(GLFLAG)CALL IZPAVE(X1,X2,Y1,Y2,DZ,ISBOX,ISFRAM,IOPT)
#endif
      IF((.NOT.GFLAG).AND.(.NOT.PFLAG))RETURN
      ZFSAV  = ZFLAG
      ZFLAG  = .FALSE.
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
*              Save the context
*
      IBORDS = IBORD
      IFACIS = IFACI
      IFAISS = IFAIS
      IFASIS = IFASI
*
*              Delete a Paving-Block
*
      IF(IOPTD.NE.0)THEN
         CALL ISFACI(0)
         CALL ISPLCI(0)
         CALL ISFAIS(1)
         CALL ISLN(1)
         CALL IGSET('BORD',1.)
         GOTO 20
      ENDIF
*
*              Draw the inside of the box
*
      IF(ISBOX.EQ.0)THEN
         CALL ISFAIS(0)
         GOTO 10
      ENDIF
      IF(ISBOX.LT.1000)THEN
         CALL ISFAIS(3)
         CALL ISFASI(ISBOX)
         GOTO 10
      ENDIF
      IF(ISBOX.GE.1000.AND.ISBOX.LT.2000)THEN
         CALL ISFAIS(1)
         CALL ISFACI(ISBOX-1000)
         GOTO 10
      ENDIF
      IF(ISBOX.GE.2000)THEN
         CALL ISFAIS(2)
         CALL ISFASI(ISBOX-2000)
      ENDIF
   10 CALL IGSET('BORD',0.)
   20 CALL IGBOX(X1,X2,Y1,Y2)
      IF(IOPTD.NE.0)GOTO 30
*
*              Draw the frame
*
      IF(DZ.LE.0.)GOTO 40
      IF(ISFRAM.EQ.ISBOX)GOTO 30
      IF(ISFRAM.EQ.0)THEN
         CALL ISFAIS(0)
         GOTO 30
      ENDIF
      IF(ISFRAM.LT.1000)THEN
         CALL ISFAIS(3)
         CALL ISFASI(ISFRAM)
         CALL ISFACI(1)
         GOTO 30
      ENDIF
      IF(ISFRAM.GE.1000.AND.ISFRAM.LT.2000)THEN
         CALL ISFAIS(1)
         CALL ISFACI(ISFRAM-1000)
         GOTO 30
      ENDIF
      IF(ISFRAM.GE.2000)THEN
         CALL ISFAIS(2)
         CALL ISFASI(ISFRAM-2000)
      ENDIF
*
   30 DZ2=0.5*DZ
      IF(IOPTM.NE.0)THEN
         M = -1
      ELSE
         M = 1
      ENDIF
      IF(IOPTS.NE.0)THEN
         IS = 1
      ELSE
         IS = 0
      ENDIF
      IF(IOPTK.NE.0)THEN
         IS    = 0
         M     = 1
         IOPTP = 0
         IOPTS = 0
         K     = -2
      ELSE
         K = 0
      ENDIF
*
* Top side
*
      IF(IOPTT.NE.0.OR.IOPTK.NE.0)THEN
         IF(IOPTL.NE.0.AND.IOPTK.EQ.0)M=-1
         X(1)=X1+IS*M*DZ2
         X(2)=X1+M*DZ2+K*DZ2
         X(3)=X2+M*DZ2
         X(4)=X2+IS*M*DZ2
         Y(1)=Y2
         Y(2)=Y2+DZ2
         Y(3)=Y2+DZ2
         Y(4)=Y2
         IF(IOPTP.NE.0)THEN
            IF(M.GT.0)THEN
               X(3)=X2
            ELSE
               X(2)=X1
            ENDIF
         ENDIF
         IF(IOPTT.NE.0.OR.IOPTD.NE.0)CALL IFA(4,X,Y)
         IF(IOPTS.EQ.0)CALL IPL(4,X,Y)
         M=1
      ENDIF
*
* Bottom side
*
      IF(IOPTB.NE.0.OR.IOPTK.NE.0)THEN
         IF(IOPTL.NE.0.AND.IOPTK.EQ.0)M=-1
         X(1)=X1+IS*M*DZ2
         X(2)=X1+M*DZ2+K*DZ2
         X(3)=X2+M*DZ2
         X(4)=X2+IS*M*DZ2
         Y(1)=Y1
         Y(2)=Y1-DZ2
         Y(3)=Y1-DZ2
         Y(4)=Y1
         IF(IOPTP.NE.0)THEN
            IF(M.GT.0)THEN
               X(3)=X2
            ELSE
               X(2)=X1
            ENDIF
         ENDIF
         IF(IOPTB.NE.0.OR.IOPTD.NE.0)CALL IFA(4,X,Y)
         IF(IOPTS.EQ.0)CALL IPL(4,X,Y)
         M=-1
      ENDIF
*
* Left side
*
      IF(IOPTL.NE.0.OR.IOPTK.NE.0)THEN
         X(1)=X1
         X(2)=X1-DZ2
         X(3)=X1-DZ2
         X(4)=X1
         Y(1)=Y1+IS*M*DZ2
         Y(2)=Y1+M*DZ2
         Y(3)=Y2+M*DZ2-K*DZ2
         Y(4)=Y2+IS*M*DZ2
         IF(IOPTP.NE.0)THEN
            IF(M.GT.0)THEN
               Y(3)=Y2
            ELSE
               Y(2)=Y1
            ENDIF
         ENDIF
         IF(IOPTL.NE.0.OR.IOPTD.NE.0)CALL IFA(4,X,Y)
         IF(IOPTS.EQ.0)CALL IPL(4,X,Y)
      ENDIF
*
* Right side
*
      IF(IOPTR.NE.0.OR.IOPTK.NE.0)THEN
         X(1)=X2
         X(2)=X2+DZ2
         X(3)=X2+DZ2
         X(4)=X2
         Y(1)=Y1+IS*M*DZ2
         Y(2)=Y1+M*DZ2
         Y(3)=Y2+M*DZ2-K*DZ2
         Y(4)=Y2+IS*M*DZ2
         IF(IOPTP.NE.0)THEN
            IF(M.GT.0)THEN
               Y(3)=Y2
            ELSE
               Y(2)=Y1
            ENDIF
         ENDIF
         IF(IOPTR.NE.0.OR.IOPTD.NE.0)CALL IFA(4,X,Y)
         IF(IOPTS.EQ.0)CALL IPL(4,X,Y)
      ENDIF
      IF(IOPTD.NE.0)GOTO 50
*
*              Draw boundary of the box
*
   40 CALL ISFAIS(0)
      CALL IGSET('BORD',1.)
      CALL IGBOX(X1,X2,Y1,Y2)
*
*              Restore the context
*
   50 CALL IGSET('BORD',FLOAT(IBORDS))
      CALL ISFACI(IFACIS)
      CALL ISFAIS(IFAISS)
      CALL ISFASI(IFASIS)
      ZFLAG  = ZFSAV
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
