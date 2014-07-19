*
* $Id: dzdget.F,v 1.1.1.1 1996/03/04 16:13:08 mclareni Exp $
*
* $Log: dzdget.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:08  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGET(IXSTOR,LTOP, ILKACT
     + ,CHOPT,IWDISP, ILOCNR, LSEL, IFLCUR, PX, PY)
*.
*...DZDGET get locator input from workstation IWDISP
*.
*. INPUT     : IXSTOR    Store index
*.             LTOP      Link to top bank to be displayed
*.             ILKACT    the link where drawing of down banks starts
*.             CHOPT     Character option (not yet used)
*.             IWDISP    workstation Id for display (screen)
*.             ILOCNR    locator number for cursor input
*. OUTPUT    :  LSEL     link to a selected bank
*.              IFLCUR   flag indicating where locator points to:
*.                  -1 : break was hit (right mouse button)
*.                   0 : not inside a bank
*.                   1 : in the (shaded bank center
*.                   2 : in the link field
*.                   3 : in the data box
*.                   4 : in the next link arrow
*.                   5 : in a square above the bank center
*.                   6 : above the data box
*.              PX,PY    coordinates of the locator
*. COMMON    :
*. SEQUENCE  : DZDPRM
*. CALLS     : DZDRA1 GINLC  GQCNTN GQNT   GQWKT  GRQLC  GSELNT
*. CALLED    : DZDISP, USER
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
      CHARACTER*(*) CHOPT
#include "dzdprm.inc"
      LOGICAL DZDINB
      LOGICAL REQINP
      EXTERNAL DZDINB
      SAVE PXR,PYR
      INTEGER IPET
      DATA IPET/1/
      DATA PXR,PYR/0.,0./
*----
      LSEL = 0
      NT = 1
      CALL ISELNT(NT)
      IF(PX .LT. 0.001)THEN
          PX = XRANGE/2.
          PY = YRANGE/2.
      ENDIF
      CALL IRQLC(IWDISP,ILOCNR,ISTAT,NTU,PX,PY)
      IF(ISTAT.EQ.0)THEN
          IFLCUR=-1
          GOTO 10
      ENDIF
      PXR = PX
      PYR = PY
      REQINP = .TRUE.
      LSAVE = LTOP
      CALL DZDRA1(IXSTOR,LTOP,ILKACT
     + ,CHOPT, PX, PY, REQINP,IFLCUR,YTOP,YBOTTM)
      LSEL = LTOP
      LTOP = LSAVE
   10 CONTINUE
      RETURN
      END
***********************************************************************
