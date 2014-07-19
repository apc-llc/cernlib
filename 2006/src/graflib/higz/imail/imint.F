*
* $Id: imint.F,v 1.1.1.1 1996/02/14 13:11:02 mclareni Exp $
*
* $Log: imint.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:02  mclareni
* Higz
*
*
#if defined(CERNLIB_MAIL)
#include "higz/pilot.h"
*CMZ :  1.17/00 16/02/93  11.58.28  by  O.Couet
*-- Author :    O.Couet   30/01/89
      SUBROUTINE IMINT(CHBUFI,IO)
*.===========>
*.
*.   Interface between: TELNET "G" and HIGZ
*.                      3270   "G" and HIGZ
*.
*. _Input parameters:
*.
*.  CHARACTER CHBUFI : Buffer to be interpreted
*.  INTEGER IO : Lenght of the significant characters
*.               in CHBUFI.
*.
*. _Output parameters:
*.
*.  INTEGER IO : =0 Ouput
*.               =1 Input
*.
*..==========> (O.Couet)
      PARAMETER (IMEN=50)
      PARAMETER (NWORDS=20000)
      COMMON /PAWC/ H(NWORDS)
#include "higz/hiques.inc"
#include "higz/hiatnb.inc"
#include "higz/hilabs.inc"
#include "higz/hiflag.inc"
      CHARACTER*80 CHBUFI,CHBUF
      CHARACTER*80 CHARS
      CHARACTER*32 CHOPT
      CHARACTER*32 CHITEM(IMEN),TITLE,CHDEF(IMEN)
      CHARACTER*32 CHVAL(IMEN),CHUSER(IMEN)
      PARAMETER (IBUFSI=10000)
      DIMENSION X(IBUFSI),Y(IBUFSI)
      SAVE CHOPT,CHITEM,TITLE,CHDEF,CHVAL,CHUSER,CHARS
      SAVE X,Y
      SAVE NEWCOD,NT,N,NN,IXREAD,IERR,NX,NY,NYY,NPAR
      SAVE MN,NBU,ICHOIC,IPTR,ICODE,IO2
#include "higz/hiatnm.inc"
      DATA NEWCOD /1/
      DATA IXREAD /1/
      DATA IERR   /0/
      DATA IO2    /0/
*.______________________________________
*
      CHBUF=' '
      CHBUF=CHBUFI(1:IO)
*
*              Errors management
*
      IF(IERR.EQ.-1)THEN
         IF(CHBUF(1:1).EQ.'%')THEN
            IERR=0
            NEWCOD=1
         ELSE
            GOTO 999
         ENDIF
      ENDIF
*
*              Read the code of the HIGZ function to be performed
*
      IO=0
      IF(NEWCOD.NE.0)THEN
         IF(CHBUF(1:1).EQ.'%')THEN
            READ (CHBUF,'(1X,I3)',ERR=2) ICODE
         ELSE
            GOTO 999
         ENDIF
      ENDIF
*
*              Start with HIGZ
*
      IF(ICODE.EQ.100)THEN
         READ (CHBUF,'(4X,I5,I10)',ERR=99)IFIL,IWT
         CALL MZEBRA(-3)
         CALL MZPAW(NWORDS,' ')
         CALL IGINIT(100)
         CALL IGSSE(IFIL,IWT)
         CALL IGZSET('G')
         WRITE (CHBUFI,'(2E16.7)') RQUEST(11),RQUEST(12)
         CHBUFI(79:79)=CHAR(13)
         CHBUFI(80:80)=CHAR(10)
         IO=1
         GOTO 999
      ENDIF
*
*              Clear workstation
*
      IF(ICODE.EQ.101)THEN
         IF(ZFLAG)THEN
            CALL IZPICT(' ','S')
            CALL IZPICT(' ','M')
         ENDIF
         CALL ICLRWK(0,0)
         GOTO 999
      ENDIF
*
*              Normalization transformations
*
      IF(ICODE.EQ.200)THEN
         IF(NEWCOD.NE.0)THEN
            READ (CHBUF,'(4X,I5)',ERR=99) NT
            NEWCOD=0
            IXREAD=1
         ELSE
            IF(IXREAD.EQ.1)THEN
               READ (CHBUF,'(4E16.7)') X(1),X(2),X(3),X(4)
               IXREAD=0
            ELSE
               READ (CHBUF,'(4E16.7)') X(5),X(6),X(7),X(8)
               CALL ISWN(NT,X(1),X(2),X(3),X(4))
               CALL ISVP(NT,X(5),X(6),X(7),X(8))
               CALL ISELNT(NT)
               NEWCOD=1
            ENDIF
         ENDIF
         GOTO 999
      ENDIF
*
*              IGRNG
*
      IF(ICODE.EQ.201)THEN
         READ (CHBUF,'(4X,2E16.7)',ERR=99) X(1),Y(1)
         CALL IGRNG(X(1),Y(1))
         WRITE (CHBUFI,'(5E15.6)') RQUEST(11),RQUEST(12),RQUEST(13)
     +,                            RQUEST(14),RQUEST(15)
         CHBUFI(79:79)=CHAR(13)
         CHBUFI(80:80)=CHAR(10)
         IO=1
         GOTO 999
      ENDIF
*
*              Request locator
*
      IF(ICODE.EQ.555)THEN
         READ (CHBUF,'(4X,2I3)',ERR=99) N,NN
         CALL IRQLC(N,NN,ISTAT,NT,XLOC,YLOC)
         WRITE (CHBUFI,'(2I5,2E16.7)') ISTAT,NT,XLOC,YLOC
         CHBUFI(79:79)=CHAR(13)
         CHBUFI(80:80)=CHAR(10)
         IO=1
         GOTO 999
      ENDIF
*
*              Request string
*
      IF(ICODE.EQ.556)THEN
         READ (CHBUF,'(4X,2I3)',ERR=99) N,NN
         CALL IRQST(N,NN,ISTAT,L,CHOPT)
         IF(L.GT.78)L=32
         WRITE (CHBUFI,'(2I5,A)') ISTAT,L,CHOPT
         CHBUFI(79:79)=CHAR(13)
         CHBUFI(80:80)=CHAR(10)
         IO=1
         GOTO 999
      ENDIF
*
*              IGMENU
*
      IF(ICODE.EQ.557)THEN
         IF(IO2.NE.0)GOTO 5571
         IF(NEWCOD.NE.0)THEN
            READ (CHBUF,'(4X,3I3,I10)',ERR=99) MN,NBU,N,ICHOIC
            NEWCOD=0
            IXREAD=1
            GOTO 999
         ENDIF
         IF(IXREAD.NE.0)THEN
            READ (CHBUF,'(4E16.7,A16)',ERR=99) X(1),X(2)
     +,                                        Y(1),Y(2),CHOPT
            IXREAD=0
            IF(INDEX(CHOPT,'T').EQ.0)THEN
               IPTR=0
            ELSE
               IPTR=1
               TITLE=' '
            ENDIF
            IF(NBU.EQ.0.AND.N.EQ.0)THEN
               IPTR=0
               GOTO 5570
            ENDIF
            GOTO 999
         ENDIF
         IF(IPTR.EQ.0)THEN
            READ (CHBUF,'(A)') TITLE
            IPTR=1
            GOTO 999
         ENDIF
         IF(IPTR.NE.0)THEN
            IF(IPTR.LE.N)THEN
               READ (CHBUF,'(2A32)',ERR=99) CHITEM(IPTR),CHDEF(IPTR)
               IPTR=IPTR+1
               IF(NBU.EQ.0.AND.IPTR.EQ.N+1)THEN
                  IPTR=0
                  GOTO 5570
               ENDIF
               GOTO 999
            ENDIF
            IF(IPTR.LE.N+NBU)THEN
               READ (CHBUF,'(A)') CHUSER(IPTR-N)
               IPTR=IPTR+1
               IF(IPTR.EQ.N+NBU+1)THEN
                  IPTR=0
                  GOTO 5570
               ENDIF
               GOTO 999
            ENDIF
         ENDIF
*
 5570    CALL IGMENU(MN,TITLE,X(1),X(2),Y(1),Y(2),NBU,CHUSER
     +,              N,CHITEM,CHDEF,CHVAL,ICHOIC,CHOPT)
*
 5571    IF(INDEX(CHOPT,'C').NE.0)THEN
            IF(IPTR.EQ.0)THEN
               WRITE (CHBUFI,'(I5)') ICHOIC
               CHBUFI(79:79)=CHAR(13)
               CHBUFI(80:80)=CHAR(10)
               IO=1
               IPTR=1
               IF(INDEX(CHOPT,'P').NE.0.AND.N.GT.0)THEN
                  IO2=1
                  NEWCOD=0
               ELSE
                  NEWCOD=1
               ENDIF
               GOTO 999
            ENDIF
            IF(INDEX(CHOPT,'P').NE.0)THEN
               IF(IPTR.NE.0)THEN
                  CHBUFI=CHVAL(IPTR)
                  CHBUFI(79:79)=CHAR(13)
                  CHBUFI(80:80)=CHAR(10)
                  IO=1
                  IPTR=IPTR+1
                  IF(IPTR.GT.N)THEN
                     NEWCOD=1
                     IO2=0
                  ENDIF
                  GOTO 999
               ENDIF
            ENDIF
         ENDIF
         IF(INDEX(CHOPT,'I').NE.0)THEN
            CHBUFI=CHVAL(1)
            CHBUFI(79:79)=CHAR(13)
            CHBUFI(80:80)=CHAR(10)
            IO=1
         ENDIF
         NEWCOD=1
         GOTO 999
      ENDIF
*
*              Set the REDIT factor
*
      IF(ICODE.EQ.558)THEN
         READ (CHBUF,'(4X,E16.7)',ERR=99) R1
         R2=R1
         CALL IGSRAP(R1)
         IF(R2.LT.0)THEN
            WRITE (CHBUFI,'(E16.7)') R1
            CHBUFI(79:79)=CHAR(13)
            CHBUFI(80:80)=CHAR(10)
            IO=1
         ENDIF
         GOTO 999
      ENDIF
*
*              IGTERM
*
      IF(ICODE.EQ.559)THEN
         CALL IGTERM
         GOTO 999
      ENDIF
*
*              END with HIGZ
*
      IF(ICODE.EQ.999)THEN
         CALL IGEND
         RETURN
      ENDIF
*
      GOTO (2,2,2,2,2
     +,     60,70,80,90,100,110,120,130,140,150,160,170,180,190
     +,     200,210,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
     +,     480,490
     +,     5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
     +,     8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
     +      ),ICODE
*
    2 CONTINUE
      GOTO 999
*
*              Histograms
*
   60 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,A10,I5)',ERR=99) CHOPT,N
         NEWCOD=0
         IXREAD=1
         NN=2
         IF(INDEX(CHOPT,'N').NE.0)NN=N+1
         GOTO 999
      ENDIF
      IF(IXREAD.NE.0)THEN
         IERR=IMFIN(CHBUF,NN,X)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=0
         ENDIF
         GOTO 999
      ELSE
         IERR=IMFIN(CHBUF,N,Y)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=1
            GOTO 61
         ENDIF
         GOTO 999
      ENDIF
   61 IF(INDEX(CHOPT,'R').NE.0)THEN
         CALL IGHIST(N,Y,X,CHOPT)
      ELSE
         CALL IGHIST(N,X,Y,CHOPT)
      ENDIF
      NEWCOD=1
      GOTO 999
*
*              Polymarker with one point
*
   70 READ (CHBUF,'(4X,2E16.7)',ERR=99) X(1),Y(1)
      CALL IPM(1,X,Y)
      GOTO 999
*
*              Polyline with two points
*
   80 READ (CHBUF,'(4X,4E16.7)',ERR=99) X(1),X(2),Y(1),Y(2)
      CALL IPL(2,X,Y)
      GOTO 999
*
*              Polyline
*
   90 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,I5)',ERR=99) N
         NEWCOD=0
         IXREAD=1
         GOTO 999
      ENDIF
      IF(IXREAD.NE.0)THEN
         IERR=IMFIN(CHBUF,N,X)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=0
         ENDIF
         GOTO 999
      ELSE
         IERR=IMFIN(CHBUF,N,Y)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=1
            GOTO 91
         ENDIF
         GOTO 999
      ENDIF
   91 CALL IPL(N,X,Y)
      NEWCOD=1
      GOTO 999
*
*              Polymarker
*
  100 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,I5)',ERR=99) N
         NEWCOD=0
         IXREAD=1
         GOTO 999
      ENDIF
      IF(IXREAD.NE.0)THEN
         IERR=IMFIN(CHBUF,N,X)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=0
         ENDIF
         GOTO 999
      ELSE
         IERR=IMFIN(CHBUF,N,Y)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=1
            GOTO 101
         ENDIF
         GOTO 999
      ENDIF
  101 CALL IPM(N,X,Y)
      NEWCOD=1
      GOTO 999
*
*              Fill area
*
  110 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,I5)',ERR=99) N
         NEWCOD=0
         IXREAD=1
         GOTO 999
      ENDIF
      IF(IXREAD.NE.0)THEN
         IERR=IMFIN(CHBUF,N,X)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=0
         ENDIF
         GOTO 999
      ELSE
         IERR=IMFIN(CHBUF,N,Y)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=1
            GOTO 111
         ENDIF
         GOTO 999
      ENDIF
  111 CALL IFA(N,X,Y)
      NEWCOD=1
      GOTO 999
*
*              Text
*
  120 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,2E16.7)',ERR=99) X(1),Y(1)
         NEWCOD=0
         GOTO 999
      ELSE
         READ (CHBUF,'(A)',ERR=99) CHARS
         NEWCOD=1
      ENDIF
      CALL ITX(X(1),Y(1),CHARS)
      GOTO 999
*
*              Box
*
  130 READ (CHBUF,'(4X,4E16.7)',ERR=99) X(1),X(2),X(3),X(4)
      CALL IGBOX(X(1),X(2),X(3),X(4))
      GOTO 999
*
*              Frame box
*
  140 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,4E16.7)',ERR=99) X(1),X(2),X(3),X(4)
         NEWCOD=0
         GOTO 999
      ELSE
         READ (CHBUF,'(4E16.7)',ERR=99) X(5),X(6),X(7),X(8)
         NEWCOD=1
      ENDIF
      CALL IGFBOX(X(1),X(2),X(3),X(4),X(5),X(6),X(7),X(8))
      GOTO 999
*
*              Arc
*
  150 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,4E16.7)',ERR=99) X(1),X(2),X(3),X(4)
         NEWCOD=0
         GOTO 999
      ELSE
         READ (CHBUF,'(2E16.7)',ERR=99) X(5),X(6)
         NEWCOD=1
      ENDIF
      CALL IGARC(X(1),X(2),X(3),X(4),X(5),X(6))
      GOTO 999
*
*              Axis
*
  160 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,4E16.7)',ERR=99) X(1),X(2),X(3),X(4)
         NEWCOD=0
         GOTO 999
      ELSE
         READ (CHBUF,'(2E16.7,I10,A10)',ERR=99) X(5),X(6),NDIV,CHOPT
         NEWCOD=1
      ENDIF
      CALL IGAXIS(X(1),X(2),X(3),X(4),X(5),X(6),NDIV,CHOPT)
      GOTO 999
*
*              Software characters
*
  170 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,4E16.7,A4)',ERR=99) X(1),X(2),X(3),X(4),CHOPT
         NEWCOD=0
         GOTO 999
      ELSE
         READ (CHBUF,'(A)',ERR=99) CHARS
         NEWCOD=1
      ENDIF
      CALL IGTEXT(X(1),X(2),CHARS,X(3),X(4),CHOPT)
      GOTO 999
*
*              Multiline
*
  180 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,I5)',ERR=99) N
         NEWCOD=0
         IXREAD=1
         GOTO 999
      ENDIF
      IF(IXREAD.NE.0)THEN
         IERR=IMFIN(CHBUF,N,X)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=0
         ENDIF
         GOTO 999
      ELSE
         IERR=IMFIN(CHBUF,N,Y)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=1
            GOTO 181
         ENDIF
         GOTO 999
      ENDIF
  181 CALL IML(N,X,Y)
      NEWCOD=1
      GOTO 999
*
*              Alpha numerique axis labels
*
  190 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,I5)',ERR=99) NHILAB
         NEWCOD=0
         N=1
         GOTO 999
      ELSE
         READ (CHBUF,'(A)',ERR=99) HILABS(N)
         N=N+1
         IF(N.GT.NHILAB)GOTO 999
      ENDIF
      NEWCOD=1
      GOTO 999
*
*              Tables
*
  200 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,A20,3I5)',ERR=99) CHOPT,NX,NY,NPAR
         NYY=MAX(NY,1)
         IF(NX*NY.GT.IBUFSI)THEN
            CALL IGERR('To many data','TELNETG')
            GOTO 99
         ENDIF
         NEWCOD=0
         IXREAD=0
         GOTO 999
      ENDIF
      IF(IXREAD.EQ.0)THEN
         IERR=IMFIN(CHBUF,NPAR,X)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=1
         ENDIF
         GOTO 999
      ELSE
         READ (CHBUF,'(6G13.7)',ERR=99) (Y((IXREAD-1)*6+I),I=1,6)
         IXREAD=IXREAD+1
         IF(IXREAD*6.GT.NX*NYY)GOTO 201
         GOTO 999
      ENDIF
  201 CALL IGTABL(NX,NY,Y,NPAR,X,CHOPT)
      NEWCOD=1
      GOTO 999
*
*              Graphs
*
  210 IF(NEWCOD.NE.0)THEN
         READ (CHBUF,'(4X,A10,I5)',ERR=99) CHOPT,N
         NEWCOD=0
         IXREAD=1
         GOTO 999
      ENDIF
      IF(IXREAD.NE.0)THEN
         IERR=IMFIN(CHBUF,NN,X)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=0
         ENDIF
         GOTO 999
      ELSE
         IERR=IMFIN(CHBUF,N,Y)
         IF(IERR.EQ.-1)GOTO 999
         IF(IERR.EQ.1)THEN
            IXREAD=1
            GOTO 211
         ENDIF
         GOTO 999
      ENDIF
  211 CALL IGRAPH(N,X,Y,CHOPT)
      NEWCOD=1
      GOTO 999
*
*
*              Set color representation
*
  480 READ (CHBUF,'(4X,I3,I5,3E16.7)',ERR=99)IWKID,IC,X(1),X(2),X(3)
      CALL ISCR(IDID,IC,X(1),X(2),X(3))
      GOTO 999
*
*              Set clipping indicator
*
  490 READ (CHBUF,'(4X,I2)',ERR=99)ICLSW
      CALL ISCLIP(ICLSW)
      GOTO 999
*
*              Real attributes
*
    5 READ (CHBUF,'(4X,E16.7)',ERR=99) X(1)
      CALL IGSET(CHRATT(ICODE-49),X(1))
      GOTO 999
*
*              Integer attributes
*
    8 READ (CHBUF,'(4X,E16.7)',ERR=99) X(1)
      CALL IGSET(CHIATT(ICODE-79),X(1))
      GOTO 999
*
*              Read Errors
*
   99 CONTINUE
      IERR=-1
  999 CONTINUE
      CALL IGSA(0)
*
      END
#endif
