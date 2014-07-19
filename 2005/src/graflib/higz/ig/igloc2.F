*
* $Id: igloc2.F,v 1.1.1.1 1996/02/14 13:10:36 mclareni Exp $
*
* $Log: igloc2.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:36  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.11/00 22/10/90  10.27.20  by  O.Couet
*-- Author :
      SUBROUTINE IGLOC2(IWKID,NT,XLOC1,YLOC1,XLOC2,YLOC2,ISTAT,CHOPT)
*.===========>
*.
*.   This routine returns the graphic cursor position in WC space of
*.   2 points (XLOC1,YLOC1) AND (XLOC2,YLOC2) and the corresponding
*.   normalize transformation number NT.
*.   Rubber_banding option is used to visualize the area (box) delimited
*.   by the 2 points.
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Workstation identifier .
*.  CHARACTER CHOPT : Option flags
*.            CHOPT = ' ' NT is an output parameter
*.            CHOPT = 'P' NT is an input parameter
*.            CHOPT = 'L' Rubberbanding Line
*.
*. _Output parameters:
*.
*.  INTEGER NT : Normalization transformation number .
*.  REAL XLOC1 YLOC1 : Cursor position in WC space of first point .
*.  REAL XLOC2 YLOC2 : Cursor position in WC space of second point .
*.  INTEGER ISTAT : Return status: 0=BREAK, 1=OK .
*.
*..==========> (R.Brun)
      CHARACTER*(*) CHOPT
*.______________________________________
*
      IF(INDEX(CHOPT,'P').NE.0)CALL ISELNT(NT)
      CALL IRQLC(IWKID,1,ISTAT,NT,XLOC1,YLOC1)
      IF(ISTAT.EQ.0)RETURN
*
      IPET=51
      IF(INDEX(CHOPT,'L').NE.0)THEN
         IPET=41
      ENDIF
      CALL IRQLC(IWKID,IPET,ISTAT,NT2,XLOC2,YLOC2)
      IF(NT2.NE.NT)NT=0
*
      END

