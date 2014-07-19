*
* $Id: hfill.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfill.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/02 05/05/95  08.51.58  by  Francois Dardare
*-- Author :
      SUBROUTINE HFILL(ID1,X,Y,W)
*.==========>
*.           GENERAL FILLING ROUTINE
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
*.___________________________________________
 
#if defined(CERNLIB_SUN)||defined(CERNLIB_SOLARIS)||defined(CERNLIB_SGI)||defined(CERNLIB_DECS)||defined(CERNLIB_ALPHA)||defined(CERNLIB_HPUX)||defined(CERNLIB_IBMRT)
*test the floating point X
         J=IFPS(X)
         IF(J .EQ. 0) THEN
           CALL HFPBUG(X,'HFILL first float variable: X',ID1)
         ENDIF
*test the floating point Y
         J=IFPS(Y)
         IF(J .EQ. 0) THEN
           CALL HFPBUG(Y,'HFILL second float variable: Y',ID1)
         ENDIF
*test the floating point W
         J=IFPS(W)
         IF(J .EQ. 0) THEN
           CALL HFPBUG(W,'HFILL third float variable: W',ID1)
         ENDIF
*
#endif
 
         IF(ID1.NE.IDLAST)THEN
           ID=ID1
           IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.LE.0)RETURN
         LCID=LQ(LTAB-IDPOS)
         I1=JBIT(IQ(LCID+KBITS),1)
         I5=JBIT(IQ(LCID+KBITS),5)
         I6=JBIT(IQ(LCID+KBITS),6)
         I7=JBIT(IQ(LCID+KBITS),7)
         I8=JBIT(IQ(LCID+KBITS),8)
         LCONT=LQ(LCID-1)
         LPRX=LCID+KNCX
         IDLAST=ID1
      ENDIF
*
      IF(I1.NE.0)THEN
*
*             1-DIM HISTOGRAM OR PROFILE
*
         IF(I8.EQ.0)THEN
            CALL HF1(ID1,X,W)
         ELSE
            CALL HFILPF(ID1,X,Y,W)
         ENDIF
      ELSE
*
*             2-DIM HISTOGRAM
*
         CALL HF2 (ID1,X,Y,W)
         CALL HFIDP(ID1,X,Y,W)
         LCONT=LQ(LCID-1)
      ENDIF
      END
 
 
 
 
 
 
 
 
 
 
 
