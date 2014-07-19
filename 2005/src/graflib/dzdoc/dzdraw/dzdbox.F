*
* $Id: dzdbox.F,v 1.1.1.1 1996/03/04 16:13:14 mclareni Exp $
*
* $Log: dzdbox.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:14  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDBOX(X0,X1,Y0,Y1,INTS)
*.
*...DZDBOX    draw a box with possible shading
*.
*. INPUT     : X0,X1,Y0,Y1  the coordinates for the box
*.             INTS         fill area index (0=hollow)
*.
*. CALLS     : GFA    GPL    GSFACI GSFAIS
*. CALLED    : DZDRA2, DZDBNK, DZDHEA, DZDLBX, DZDSBX
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*. Modification Log.
*. 20-Feb-90 : O.Schaile
*.             add LATEX
*. 12-Dec-90 : O.Schaile
*.             add PostScript
*.
*.**********************************************************************
*.
#include "dzdprm.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      REAL X(7),Y(7)
      CHARACTER*24 CCINT
*----
      NP=5
      ISTYLE = MOD(INTS,1000)
      IFLDIA = MOD(INTS/1000,10)
      IF(IFLDIA .EQ. 0)THEN
          IF(IFOMED.LE.2)THEN
              NP = 5
              X(1) = X0
              X(2) = X1
              X(3) = X(2)
              X(4) = X(1)
              X(5) = X(1)
              Y(1) = Y0
              Y(2) = Y(1)
              Y(3) = Y1
              Y(4) = Y(3)
              Y(5) = Y(1)
          ELSEIF(IFOMED.EQ.3)THEN
              X(1)=     X0*PAGECM
              X(2)=(X1-X0)*PAGECM
              Y(1)=     Y0*PAGECM
              Y(2)=(Y1-Y0)*PAGECM
              WRITE(LUNGRA,'(A,4(F5.2,A))')
     +        BS//'p(',X(1),',',Y(1),'){'//BS//'f(',X(2),',',
     +        Y(2),'){ }}'
              IF(INTS.GT.0)THEN
*                shade
                 NL=Y(2)/0.05
                 WRITE(LUNGRA,'(A,2(F5.2,A),I3,A,F5.2,A)')
     +           BS//'mup(',X(1),',',Y(1)+0.05,')(0.,0.05){'
     +           ,NL,'}{'//BS//'l(1,0){',X(2),'}}'
              ENDIF
              GOTO 999
          ENDIF
      ELSE
          DX = X1-X0
          DY = Y1-Y0
          IF(DX .LE. DY)THEN
*             square <>
              IF(IFOMED.EQ.3)THEN
                 X(1)=(X0+0.5*DX)*PAGECM
                 Y(1)=(Y0+0.5*DY)*PAGECM
                 IF(DX.LT.DY-0.1*DY)THEN
                    WRITE(LUNGRA,'(A,2(F5.2,A))')
     &              BS//'p(',X(1),',',Y(1),'){'//BS//'m(0,0)[c] {'
     &              //BS//'large $ '//BS//'diamondsuit $}}'
                 ELSE
                    WRITE(LUNGRA,'(A,2(F5.2,A))')
     &              BS//'p(',X(1),',',Y(1),'){'//BS//'m(0,0)[c] {'
     &              //BS//'LARGE $ '//BS//'diamondsuit $}}'
                    WRITE(LUNGRA,'(A,2(F5.2,A))')
     &              BS//'p(',X(1),',',Y(1),'){'//BS//'m(0,0)[c] {'
     &              //BS//'large $ '//BS//'diamondsuit $}}'
                    WRITE(LUNGRA,'(A,2(F5.2,A))')
     &              BS//'p(',X(1),',',Y(1),'){'//BS//'m(0,0)[c] {$ '
     &              //BS//'diamondsuit $}}'
                 ENDIF
                 GOTO 999
              ENDIF
 
              NP = 5
              X(1) = X0
              Y(1) = Y0+0.5*DY
              X(2) = X0+0.5*DX
              Y(2) = Y0
              X(3) = X1
              Y(3) = Y(1)
              X(4) = X(2)
              Y(4) = Y1
              X(5) = X(1)
              Y(5) = Y(1)
          ELSE
*              ____
*             <____>
 
              IF(IFOMED.EQ.3)THEN
                 X(1)=(X0+0.61*DY)*PAGECM
                 X(2)=(X1-X0-1.22*DY)*PAGECM
                 Y(1)=Y0*PAGECM
                 Y(2)=(Y0+DY)*PAGECM
                 WRITE(LUNGRA,'(A,2(F5.2,A),F5.2,A)')
     &           BS//'p(',X(1),',',Y(1),'){'//BS//'l(1,0){',X(2),'}}'
                 WRITE(LUNGRA,'(A,2(F5.2,A),F5.2,A)')
     &           BS//'p(',X(1),',',Y(2),'){'//BS//'l(1,0){',X(2),'}}'
                 X(1)=(X0+0.16*DY)*PAGECM
                 Y(1)=0.5*(Y(1)+Y(2))
                 WRITE(LUNGRA,'(A,2(F5.2,A),F5.2,A)')
     &           BS//'p(',X(1),',',Y(1),'){'//BS//'m(0,0)[c]{'
     &           //BS//'huge $<$}}'
                 X(1)=(X0+DX-0.16*DY)*PAGECM
                 WRITE(LUNGRA,'(A,2(F5.2,A),F5.2,A)')
     &           BS//'p(',X(1),',',Y(1),'){'//BS//'m(0,0)[c]{'
     &           //BS//'huge $>$}}'
                 GOTO 999
              ENDIF
              NP = 7
              DY2 = 0.5*DY
              X(1) = X0
              Y(1) = Y0+DY2
              X(2) = X0+DY2
              Y(2) = Y0
              X(3) = X1-DY2
              Y(3) = Y0
              X(4) = X1
              Y(4) = Y(1)
              X(5) = X(3)
              Y(5) = Y1
              X(6) = X(2)
              Y(6) = Y1
              X(7) = X(1)
              Y(7) = Y(1)
          ENDIF
      ENDIF
      IF(INTS .GT. 0)THEN
          IF(IFOMED.LT.2 .AND. LFCOL.EQ.0)THEN
              CALL ISFAIS(ISTYLE)
               CALL ISFACI(1)
               CALL IFA(NP,X,Y)
          ELSEIF(IFOMED.EQ.2)THEN
              IF(PAMM10.LE.0.001)PAMM10=70.
              DO 20 I=1,NP
              IX=X(I)*PAMM10
              IY=Y(I)*PAMM10
              IF(I.EQ.1)THEN
                 WRITE(CCINT,'(2I9,A)')IX,IY,' m'
              ELSE
                 WRITE(CCINT,'(2I9,A)')IX,IY,' d'
              ENDIF
              CALL DZDPLN(LUNGRA,CCINT,-1)
20            CONTINUE
              CALL DZDPLN(LUNGRA,'cl 0.95 sg fill 0. sg ',0)
          ENDIF
      ENDIF
      CALL DZDGPL(NP,X,Y)
  999 END
***********************************************************************
