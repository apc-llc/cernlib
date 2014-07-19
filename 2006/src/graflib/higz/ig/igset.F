*
* $Id: igset.F,v 1.1.1.1 1996/02/14 13:10:38 mclareni Exp $
*
* $Log: igset.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:38  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 20/11/95  17.05.29  by  O.Couet
*-- Author :
      SUBROUTINE IGSET(PNAME,RVAL)
*.===========>
*.
*.   This routine sets the parameter PNAME to the value VAL .
*.
*. _Input parameters:
*.
*.  CHARACTER PNAME : Parameter name .
*.  REAL RVAL : Parameter value .
*.
*.   PNAME='FAIS' VAL=Fill Area Interior Style (0,1,2,3)
*.   PNAME='FASI' VAL=Fill Area Style Index
*.   PNAME='LTYP' VAL=Line TYPe
*.   PNAME='BASL' VAL=BAsic Segment Length
*.   PNAME='LWID' VAL=Line WIDth
*.   PNAME='MTYP' VAL=Marker TYPe
*.   PNAME='MSCF' VAL=Marker SCale Factor
*.   PNAME='PLCI' VAL=PolyLine Color Index
*.   PNAME='PMCI' VAL=PolyMarker Color Index
*.   PNAME='FACI' VAL=Fill Area Color Index
*.   PNAME='TXCI' VAL=TeXt Color Index
*.   PNAME='TXAL' VAL=10*(alignment horizontal) + (alignment vertical)
*.   PNAME='CHHE' VAL=CHaracter HEight)
*.   PNAME='TANG' VAL=Text ANGle
*.   PNAME='TXFP' VAL=10*(TeXt Font) + (TeXt Precision)
*.   PNAME='TMSI' VAL=Tick Marks SIze (in WC)
*.   PNAME='LASI' VAL=LAbels SIze (in WC)
*.   PNAME='LAOF' VAL=LAbels OFfset
*.   PNAME='PASS' VAL=IGTEXT Width
*.   PNAME='CSHI' VAL=IGTEXT Shift
*.   PNAME='BORD' VAL=Border for IGBOX, IGFBOX and IGARC (0=No , 1=Yes)
*.   PNAME='BARO' VAL=IGHIST or IGRAPH BAR charts Offset (%)
*.   PNAME='BARW' VAL=IGHIST or IGRAPH BAR charts Width (%)
*.   PNAME='AWLN' VAL=Axis Wire LeNght
*.   PNAME='NLIN' VAL=Number of lines for 3D shapes.
*.
*.   PNAME='*'    All the default values are reset
*.   PNAME='PICT' VAL=Starting number for automatic naming of RZ pictures
*.   PNAME='AURZ' VAL=Automatic saving of pictures
*.   PNAME='SHOW' Shows the current values of the IGSET parameters
*.   PNAME='DIME' VAL=2D or 3D
*.   PNAME='NCOL' VAL=Number of entry in the COLor map.
*.   PNAME='DRMD' VAL=Drawing mode: 1.=copy 2.=xor
*.   PNAME='SYNC' VAL=Synchronise the graphics in X11 1.=yes 0.=no
*.   PNAME='CLIP' VAL=Clipping mode: 1.=on 0.=off
*.   PNAME='2BUF' VAL=10*(WKID)+(double buffer mode: 1.=on 0.=off)
*.   PNAME='ZBUF' VAL=Z-buffer mode: 1.=on 0.=off
*.
*.                How to add an HIGZ attribute:
*.
*.    .In HIATT: add a variable to store the attribute and
*.     supress one word in REAATT, if the attribute is REAL,
*.     or in INTATT if the attribute is INTEGER.
*.
*.    .In HIATNB: increment by one NBRATT, if the attribute is REAL,
*.     or NBIATT if the attribute is INTEGER.
*.
*.    .In HIATNM: add the IGSET name of the attribute in CHRATT,
*.     if the attribute is REAL, or in CHIATT, if the attribute
*.     is INTEGER.
*.
*.    .In HIFLAT: add a variable to store the attribute flag and
*.     supress one word in KRFLAG, if the attribute is REAL,
*.     or in KIFLAG if the attribute is INTEGER
*.
*.    .In HICODE: add a parameter to store the attribute code IATTCO.
*.     ( 49 < IATTCO < 80, if the attribute is REAL)
*.     ( 79 < IATTCO < 110 if the attribute is INTEGER)
*.
*.    .Modify IGSET to take care about this attribute.
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/hicode.inc"
#include "higz/hilun.inc"
#include "higz/hilut.inc"
      CHARACTER*4 PNAME
      CHARACTER*6 PZNAME
      CHARACTER*30 COMM
      REAL IGSET1
*.______________________________________
*
      VAL=RVAL
      IF(PNAME(1:4).EQ.'FAIS')THEN
         CALL ISFAIS(INT(VAL))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'FASI')THEN
         CALL ISFASI(INT(VAL))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'LTYP')THEN
         CALL ISLN(INT(VAL))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'BASL')THEN
         VAL=IGSET1(VAL)
         RBSL=VAL
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(IBSLCO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'LWID')THEN
         VAL=IGSET1(VAL)
         CALL ISLWSC(VAL)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'MTYP')THEN
         CALL ISMK(INT(VAL))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'MSCF')THEN
         VAL=IGSET1(VAL)
         CALL ISMKSC(VAL)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'PLCI')THEN
         VAL=IGSET1(VAL)
         CALL ISPLCI(INT(VAL))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'PMCI')THEN
         VAL=IGSET1(VAL)
         CALL ISPMCI(INT(VAL))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'FACI')THEN
         VAL=IGSET1(VAL)
         CALL ISFACI(INT(VAL))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'TXCI')THEN
         VAL=IGSET1(VAL)
         CALL ISTXCI(INT(VAL))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'TXAL')THEN
         VAL=IGSET1(VAL)
         ITXA1=NINT(VAL/10.)
         ITXA2=NINT(VAL-10*ITXA1)
         CALL ISTXAL(ITXA1,ITXA2)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'CHHE')THEN
         VAL=IGSET1(VAL)
         CALL ISCHH(VAL)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'TANG')THEN
         CALL ISCHUP(COS(((VAL+90.)*3.14159)/180.)
     +,              SIN(((VAL+90.)*3.14159)/180.))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'TXFP')THEN
         CALL ISTXFP(INT(VAL/10.)
     +,              ABS(INT(VAL-10*INT(VAL/10.))))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'TMSI')THEN
         IF(VAL.EQ.RTMS)RETURN
         RTMS=VAL
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(ITMSCO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'LASI')THEN
         VAL=IGSET1(VAL)
         IF(VAL.EQ.RALH)RETURN
         RALH=VAL
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(IALHCO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'LAOF')THEN
         IF(VAL.EQ.RALD)RETURN
         RALD=VAL
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(IALDCO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'PASS')THEN
         VAL=IGSET1(VAL)
         IF(INT(VAL).EQ.INPASS)RETURN
         INPASS=INT(VAL)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(IGTWCO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'CSHI')THEN
         VAL=IGSET1(VAL)
         IF(VAL.EQ.RCSHIF)RETURN
         RCSHIF=VAL
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(IGTSCO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'BORD')THEN
         IF(INT(VAL).EQ.IBORD)RETURN
         IF(VAL.LT.0.)VAL=0.
         IF(VAL.GT.1.)VAL=1.
         IBORD=INT(VAL)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(IBORCO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'BARO')THEN
         IF(ABS(VAL).GT.1.)THEN
            CALL IGERR('BARO must verify -1. < "BARO" < 1.','IGSET')
            RETURN
         ENDIF
         RBOF=VAL
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(IBOFCO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'BARW')THEN
         VAL=IGSET1(VAL)
         IF((VAL.GT.1.).OR.(VAL.LT.0.))THEN
            CALL IGERR('BARW must verify 0. < "BARW" < 1.','IGSET')
            RETURN
         ENDIF
         RBWD=VAL
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(IBWDCO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'NLIN')THEN
         IF(INT(VAL).EQ.INLINE)RETURN
         INLINE=INT(IGSET1(VAL))
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(INLICO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'PICT')THEN
         VAL=IGSET1(VAL)
         IPICT=INT(VAL)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'DIME')THEN
         IF((VAL.NE.2.).AND.(VAL.NE.3.))THEN
            CALL IGERR('DIME = 2 or DIME = 3','IGSET')
            RETURN
         ENDIF
         IDIM=INT(VAL)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'AURZ')THEN
         VAL=IGSET1(VAL)
         IF(VAL.NE.0.)THEN
            ASFLAG=.TRUE.
         ELSE
            ASFLAG=.FALSE.
         ENDIF
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'AWLN')THEN
         RAWL=VAL
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
         IF(GLFLAG)CALL IZATT(IAWLCO)
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'NCOL')THEN
         VAL=IGSET1(VAL)
         IF(VAL.LE.0.)VAL=8.
         INBCOL=INT(VAL)
         IF(INBCOL.LE.NBCLUT.AND.INBCOL.GT.8)THEN
            DCOL=1./(INBCOL-8)
            DO 10 I=8,INBCOL
               GREY=1.-DCOL*(I-8)
               CALL ISCR(1,I,GREY,GREY,GREY)
   10       CONTINUE
         ENDIF
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'DRMD')THEN
#if defined(CERNLIB_X11)
         CALL IXDRMDE(INT(VAL))
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'SYNC')THEN
#if defined(CERNLIB_X11)
         CALL IXSYNC(INT(VAL))
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'CLIP')THEN
         CALL ISCLIP(INT(VAL))
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'2BUF')THEN
#if defined(CERNLIB_X11)
         IWID2B = INT(VAL/10.)
         IF(IWID2B.EQ.0)THEN
            I2BUF = MOD(INT(VAL),10)
            CALL IXS2BUF(999,I2BUF)
            RETURN
         ENDIF
         ITY    = IGIWTY(IWID2B)
         IF(ITY.GT.0)THEN
            IF(ITY.EQ.7878.OR.ITY.EQ.7879)RETURN
            I2BUF = MOD(INT(VAL),10)
            IWIN  = IWINID(IGIWIN(IWID2B))
            CALL IXS2BUF(IWIN,I2BUF)
         ENDIF
#endif
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'ZBUF')THEN
         IZBUF = INT(VAL)
         RETURN
      ENDIF
      IF(PNAME(1:1).EQ.'*')THEN
         CALL ISFAIS(0)
         CALL ISFASI(1)
         CALL ISLN(1)
         CALL ISLWSC(1.)
         CALL ISMK(1)
         CALL ISMKSC(1.)
         CALL ISPLCI(1)
         CALL ISPMCI(1)
         CALL ISFACI(1)
         CALL ISTXAL(0,0)
         CALL ISCHH(0.28)
         CALL ISCHUP(0.,1.)
         CALL ISTXCI(1)
         CALL ISTXFP(0,2)
         CALL ISCLIP(1)
         IBORD=0
         INPASS=1
         RCSHIF=0.02
         RALH=0.018
         RALD=0.013
         RTMS=0.019
         RBSL=0.01
         RBOF=0.25
         RBWD=0.5
         RAWL=0.0
         IDIM=2
         INLINE=40
         INBCOL=8
         IZBUF=0
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'SHOW')THEN
         CALL IGSA(0)
         WRITE(LUNOUT,10100)
         WRITE(LUNOUT,10000)
         WRITE(LUNOUT,10200)
         WRITE(LUNOUT,10300)
         WRITE(LUNOUT,10200)
         PZNAME='FAIS '
         COMM='Fill area interior style'
         WRITE(LUNOUT,10400) PZNAME,IFAIS,0,COMM
         PZNAME='FASI '
         COMM='Fill area style index'
         WRITE(LUNOUT,10400) PZNAME,IFASI,1,COMM
         PZNAME='LTYP '
         COMM='Line type'
         WRITE(LUNOUT,10400) PZNAME,ILN,1,COMM
         PZNAME='BASL '
         COMM='Basic segment length (NDC)'
         WRITE(LUNOUT,10500) PZNAME,RBSL,0.01,COMM
         PZNAME='LWID '
         COMM='Line width'
         WRITE(LUNOUT,10500) PZNAME,RLWSC,1.,COMM
         PZNAME='MTYP '
         COMM='Marker type'
         WRITE(LUNOUT,10400) PZNAME,IMK,1,COMM
         PZNAME='MSCF '
         COMM='Marker scale factor'
         WRITE(LUNOUT,10500) PZNAME,RMKSC,1.,COMM
         PZNAME='PLCI '
         COMM='Polyline color index'
         WRITE(LUNOUT,10400) PZNAME,IPLCI,1,COMM
         PZNAME='PMCI '
         COMM='Polymarker color index'
         WRITE(LUNOUT,10400) PZNAME,IPMCI,1,COMM
         PZNAME='FACI '
         COMM='Fill area color index'
         WRITE(LUNOUT,10400) PZNAME,IFACI,1,COMM
         PZNAME='TXCI '
         COMM='Text color index'
         WRITE(LUNOUT,10400) PZNAME,ITXCI,1,COMM
         PZNAME='TXAL '
         COMM='Text alignment'
         WRITE(LUNOUT,10600) PZNAME,ITXALH,ITXALV,0,0,COMM
         PZNAME='CHHE '
         COMM='Character height'
         WRITE(LUNOUT,10500) PZNAME,RCHH,0.28,COMM
         PZNAME='TANG '
         COMM='Text angle'
         ANGLE=((ACOS(RCHUX/SQRT(RCHUX**2+RCHUY**2))*180.)
     +           /3.14159)-90.
         IF(RCHUY.LT.0)ANGLE=360.-ANGLE
         WRITE(LUNOUT,10500) PZNAME,ANGLE,0.,COMM
         PZNAME='TXFP '
         COMM='Text font and precision'
         WRITE(LUNOUT,10600) PZNAME,IFONT,IPREC,0,2,COMM
         PZNAME='PICT '
         COMM='Current automatic number'
         WRITE(LUNOUT,10400) PZNAME,IPICT,1,COMM
         PZNAME='BORD '
         COMM='Border flag'
         WRITE(LUNOUT,10400) PZNAME,IBORD,0,COMM
         PZNAME='PASS '
         COMM='Number of pass in IGTEXT'
         WRITE(LUNOUT,10400) PZNAME,INPASS,1,COMM
         PZNAME='CSHI '
         COMM='IGTEXT shift'
         WRITE(LUNOUT,10500) PZNAME,RCSHIF,0.02,COMM
         PZNAME='LASI '
         COMM='Label axis size'
         WRITE(LUNOUT,10500) PZNAME,RALH,0.018,COMM
         PZNAME='LAOF '
         COMM='Label axis offset'
         WRITE(LUNOUT,10500) PZNAME,RALD,0.013,COMM
         PZNAME='TMSI '
         COMM='Tick marks size'
         WRITE(LUNOUT,10500) PZNAME,RTMS,0.019,COMM
         PZNAME='AWLN '
         COMM='Axis wire lenght'
         WRITE(LUNOUT,10500) PZNAME,RAWL,0.,COMM
         PZNAME='BARO '
         COMM='Offset of IGHIST (IGRAPH) bars'
         WRITE(LUNOUT,10500) PZNAME,RBOF,0.25,COMM
         PZNAME='BARW '
         COMM='Width of IGHIST (IGRAPH) bars'
         WRITE(LUNOUT,10500) PZNAME,RBWD,0.5,COMM
         PZNAME='NCOL '
         COMM='Number of COLors'
         WRITE(LUNOUT,10400) PZNAME,INBCOL,8,COMM
         PZNAME='CLIP '
         COMM='Clipping mode'
         WRITE(LUNOUT,10400) PZNAME,ICLIP,1,COMM
         PZNAME='NLIN '
         COMM='Number of line for 3D shapes'
         WRITE(LUNOUT,10400) PZNAME,INLINE,40,COMM
         PZNAME='AURZ '
         COMM='Automatic saving flag'
         I=0
         IF(ASFLAG)I=1
         WRITE(LUNOUT,10400) PZNAME,I,0,COMM
         PZNAME='DIME '
         COMM='Dimension used (2D or 3D)'
         WRITE(LUNOUT,10400) PZNAME,IDIM,2,COMM
         PZNAME='ZBUF '
         COMM='Z-Buffer (1=on or 0=off)'
         WRITE(LUNOUT,10400) PZNAME,IZBUF,0,COMM
         WRITE(LUNOUT,10200)
         RETURN
      ENDIF
      CALL IGERR('Unavailable option','IGSET')
*
10000 FORMAT(1X,'|',24X,'IGSET : Current values in use',24X,'|')
10100 FORMAT(1X,'+',77(1H-),'+')
10200 FORMAT(1X,'+',13(1H-),'+',15(1H-),'+',15(1H-),'+',31(1H-),'+')
10300 FORMAT(1X,'|  Parameter  | Current value | Default value '
     +,      '|          Explanation          |')
10400 FORMAT(' |',4X,A,'   |',I8,'       | ',I8
     +,'      | ',A,'|')
10500 FORMAT(' |',4X,A,'   |   ',F9.3,'   |    ',F9.3
     +,'  | ',A,'|')
10600 FORMAT(' |',4X,A,'   |',I8,I3,'    | ',I8,I3
     +,'   | ',A,'|')
*
      END
