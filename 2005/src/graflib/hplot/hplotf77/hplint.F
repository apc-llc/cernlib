*
* $Id: hplint.F,v 1.2 1998/04/08 14:08:07 couet Exp $
*
* $Log: hplint.F,v $
* Revision 1.2  1998/04/08 14:08:07  couet
* - VIDQQ was not used, and was initialised as a wrong "what" string.
*
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/06 31/10/95  14.51.23  by  O.Couet
*-- Author :
      SUBROUTINE HPLINT(IWTYP)
*.==========>
*.         Initialisation routine for HPLOT
*.         If IWTYP > 0 : Output to screen
*..=========>
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl13.inc"
#include "hplot/hpl14.inc"
*._____________________________
*
*----
*     HIGZ Initialization (if not already done)
*----
      IF(IXHIGZ.EQ.0)CALL IGINIT(1000)
      CALL IGSSE(LERR,IWTYP)
*----
*     COMMON/HPL1/
*----
      IPLNUM = -1
      IPLTNO = 0
      LOSLIC = .FALSE.
      LOZOOM = .FALSE.
      LOGTIT = .FALSE.
      LO3DPL = .FALSE.
*----
*     COMMON/HPL4/
*----
      XYTWN = 1.
*----
*      COMMON/HPL9/
*----
      XMINI = 0.
      XMAXI = 1.
      YMINI = 0.
      YMAXI = 1.
*----
*      COMMON/HPL13/
*----
      DO 10 I=1,NOPT
         IOPT(I) = 0
   10 CONTINUE
*----
*      COMMON/HPL14/
*----
      DO 30 I=1,NBLIST
         LSTTYP(I) = 0
         NBEL(I)   = 6
         DO 20 J=2,7
            ILIST(I,J-1) = J
   20    CONTINUE
   30 CONTINUE
      IHLIST = 1
*----
*     set up defaults via HPLOPT
*----
      CALL HPLOPT('*   ',1)
*----
*     set up margin constants page and characters sizes
*----
      CALL HPLSET('*',0)
      CALL HPLSCM
*----
*     create a top bank on which all the HPLOT data structure is linked
*
*     ******************************************************
*     *                                                    *
*     *           Structural links in LHPLOT               *
*     *                                                    *
*     *   LQ(LHPLOT-1)  : Zone data structure              *
*     *   LQ(LHPLOT-2)  : Label data structure             *
*     *   LQ(LHPLOT-3)  : Dynamic structure for X arrays   *
*     *   LQ(LHPLOT-4)  : Dynamic structure for Y arrays   *
*     *   LQ(LHPLOT-5)  : Used in HPLAER                   *
*     *   LQ(LHPLOT-6)  : Used in HPLAER                   *
*     *   LQ(LHPLOT-7)  : Used in HPLAER                   *
*     *   LQ(LHPLOT-8)  : Used in HPLAER                   *
*     *   LQ(LHPLOT-9)  : Used in HPLAER                   *
*     *   LQ(LHPLOT-10) : Used in HPLAER                   *
*     *                                                    *
*     ******************************************************
*
*----
      IF (LHPLOT.NE.0) CALL MZDROP(IHDIV,LHPLOT,' ')
      CALL MZBOOK(IHDIV,LHPLOT,LHPLOT,1,'PLOT',10,10,0,0,0)
*
      END
