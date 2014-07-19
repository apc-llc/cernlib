*
* $Id: igq.F,v 1.1.1.1 1996/02/14 13:10:37 mclareni Exp $
*
* $Log: igq.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/00 20/04/95  09.57.34  by  O.Couet
*-- Author :
      SUBROUTINE IGQ(PNAME,RVAL)
*.===========>
*.
*.   This routine inquire the primitive attribute PNAME to the value VAL .
*.
*. _Input parameters:
*.
*.  CHARACTER PNAME : Parameter name .
*.
*.   PNAME='FAIS' RVAL=Fill Area Interior Style (0,1,2,3)
*.   PNAME='FASI' RVAL=Fill Area Style Index
*.   PNAME='LTYP' RVAL=Line TYPe
*.   PNAME='BASL' RVAL=BAsic Segment Length
*.   PNAME='LWID' RVAL=Line WIDth
*.   PNAME='MTYP' RVAL=Marker TYPe
*.   PNAME='MSCF' RVAL=Marker SCale Factor
*.   PNAME='PLCI' RVAL=PolyLine Color Index
*.   PNAME='PMCI' RVAL=PolyMarker Color Index
*.   PNAME='FACI' RVAL=Fill Area Color Index
*.   PNAME='TXCI' RVAL=TeXt Color Index
*.   PNAME='TXAL' RVAL(1)=Alignment horizontal
*.                RVAL(2)=Alignment vertical
*.   PNAME='CHHE' RVAL=CHaracter HEight)
*.   PNAME='TANG' RVAL=Text ANGle
*.   PNAME='TXFP' RVAL(1)=TeXt Font
*.                RVAL(2)=TeXt Precision
*.   PNAME='TMSI' RVAL=Tick Marks SIze (in WC)
*.   PNAME='LASI' RVAL=LAbels SIze (in WC)
*.   PNAME='LAOF' RVAL=LAbels OFfset
*.   PNAME='PASS' RVAL=IGTEXT Width
*.   PNAME='CSHI' RVAL=IGTEXT Shift
*.   PNAME='BORD' RVAL=Border for IGBOX, IGFBOX and IGARC (0=No , 1=Yes)
*.   PNAME='BARO' RVAL=IGHIST or IGRAPH BAR charts Offset (%)
*.   PNAME='BARW' RVAL=IGHIST or IGRAPH BAR charts Width (%)
*.   PNAME='AWLN' RVAL=Axis Wire LeNght
*.   PNAME='DIME' RVAL=2D or 3D
*.   PNAME='NCOL' RVAL=Number of entry in the COLor map.
*.   PNAME='RGB ' RVAL=R G B values
*.   PNAME='CLIP' RVAL=Clipping mode: 1.=on 0.=off
*.
*. _Output parameters:
*.
*.  REAL RVAL : Parameter value .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hilut.inc"
#if defined(CERNLIB_GL)
#include "higz/hikern.inc"
#endif
      CHARACTER*(*) PNAME
      DIMENSION RVAL(*)
#if defined(CERNLIB_GL)
      INTEGER*2 IR,IG,IB
#endif
*.______________________________________
*
      IF(PNAME(1:4).EQ.'FAIS')THEN
         RVAL(1)=FLOAT(IFAIS)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'FASI')THEN
         RVAL(1)=FLOAT(IFASI)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'LTYP')THEN
         RVAL(1)=FLOAT(ILN)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'BASL')THEN
         RVAL(1)=RBSL
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'LWID')THEN
         RVAL(1)=RLWSC
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'MTYP')THEN
         RVAL(1)=IMK
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'MSCF')THEN
         RVAL(1)=RMKSC
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'PLCI')THEN
         RVAL(1)=FLOAT(IPLCI)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'PMCI')THEN
         RVAL(1)=FLOAT(IPMCI)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'FACI')THEN
         RVAL(1)=FLOAT(IFACI)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'TXCI')THEN
         RVAL(1)=FLOAT(ITXCI)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'TXAL')THEN
         RVAL(1)=FLOAT(ITXALH)
         RVAL(2)=FLOAT(ITXALV)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'CHHE')THEN
         RVAL(1)=RCHH
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'TANG')THEN
         RVAL(1)=RANGLE
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'TXFP')THEN
         RVAL(1)=FLOAT(IFONT)
         RVAL(2)=FLOAT(IPREC)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'TMSI')THEN
         RVAL(1)=RTMS
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'LASI')THEN
         RVAL(1)=RALH
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'LAOF')THEN
         RVAL(1)=RALD
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'PASS')THEN
         RVAL(1)=FLOAT(INPASS)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'CSHI')THEN
         RVAL(1)=RCSHIF
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'BORD')THEN
         RVAL(1)=FLOAT(IBORD)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'BARO')THEN
         RVAL(1)=RBOF
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'BARW')THEN
         RVAL(1)=RBWD
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'AWLN')THEN
         RVAL(1)=RAWL
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'DIME')THEN
         RVAL(1)=FLOAT(IDIM)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'NCOL')THEN
         RVAL(1)=FLOAT(INBCOL)
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'RGB ')THEN
         I=RVAL(1)+1
         IF(I.LE.NBCLUT)THEN
            RVAL(2)=REDLUT(I)
            RVAL(3)=GRNLUT(I)
            RVAL(4)=BLULUT(I)
         ELSE
            RVAL(2)=1.
            RVAL(3)=1.
            RVAL(4)=1.
         ENDIF
         RETURN
      ENDIF
      IF(PNAME(1:4).EQ.'CLIP')THEN
         RVAL(1)=FLOAT(ICLIP)
         RETURN
      ENDIF
      CALL IGERR('Unavailable option','IGQ')
*
      END
