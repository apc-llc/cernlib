*
* $Id: dzdst1.F,v 1.1.1.1 1996/03/04 16:13:15 mclareni Exp $
*
* $Log: dzdst1.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:15  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
 
      SUBROUTINE DZDST1(IXIN,IFW,ILW,REQINP,XC,YC,
     +                  LRET,IFARET,ILARET,CHOPT)
      CHARACTER*(*) CHOPT
*.
*...DZDST1
*.
*. INPUT     : IXIN   division index
*.             IFW     first word in division to display
*.             ILW     last word
*.             REQINP  return link to bank where locator
*.                     points to, dont do display
*.             XC,YC   position of locator
*.
*. OUTPUT    : LRET    link to bank
*.             IFARET  first address of selected bank
*.             ILARET  last address of selected bank
 
*.
*. COMMON    : DZDVAR
*. SEQUENCE  :
*. CALLS     : DZDBOX DZDNBK UHTOC
*. CALLED    : DZDDIV
*.
*. AUTHOR    : O. Schaile
*. VERSION   : 1.00
*. CREATED   :  3-Feb-88
*.**********************************************************************
*.
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzcn.inc"
*.
#include "dzdzbrinc.inc"
      LOGICAL REQINP
      CHARACTER*4 CHIDBK
      COMMON/DZDVAR/X0,DX,Y0,DY,GRYDDV, CSDDV,
     +              LENBLO, XENBLO, NCOLAC, IPRTFL
      REAL X(2),Y(2), XB(5),YB(5)
      INTEGER IHIDBK
 
      DY2 = DY/2.
      LGO = 0
      LRET = 0
      IF(INDEX(CHOPT,'D').NE.0)THEN
         NLL=IQ(LTDISP(ISTUSE)+KQS-3)
         LQ(LTDISP(ISTUSE)+KQS-NLL)= 0
      ENDIF
   10 CONTINUE
 
      CALL MZSDIV (IXIN,1)
      IF(INDEX(CHOPT,'D').NE.0)THEN
          LGO= LQ(LTDISP(ISTUSE)+KQS-NLL)
      ENDIF
      CALL DZDNBK(IXIN, LGO ,LL,IFADR,ILA,IHIDBK,IDRFLG)
      IF(LL .EQ. 0)GOTO 999
      ISHADE = 3
      IF(INDEX(CHOPT,'D').NE.0)THEN
         LQ(LTDISP(ISTUSE)+KQS-NLL) = LL
      ELSE
         LGO=LL
      ENDIF
      IF(IFADR .GE. ILW) GOTO 10
      IF(ILA .LE. IFW) GOTO 10
      IFADD = IFADR - IFW
      ILADD1 = ILA - IFW
      IF(IFADD .LT. 0)IFADD = 0
 
*     left boundary
      IBL1 = IFADD/LENBLO + 1
      IW1 = IFADD-(IBL1-1)*LENBLO
      XW1 = FLOAT(IW1)/XENBLO*DX+X0
      X(1) = XW1
      X(2) = X(1)
      Y(1) = Y0 - (FLOAT(IBL1-1)+0.1)*GRYDDV
      Y(2) = Y(1) + DY
 
      IF(REQINP)THEN
 
*     cursor above bank or left and above?
 
            IF(YC .GT. Y(2))GOTO 30
            IF(YC .GT. Y(1) .AND. XC .LT. X(1))GOTO 30
      ELSE
            CALL DZDGPL(2,X,Y)
      ENDIF
*     right boundary
      IBL2 = ILADD1/LENBLO + 1
      IW2 = ILADD1-(IBL2-1)*LENBLO
      XW2 = FLOAT(IW2)/XENBLO*DX+X0
      X(1) = XW2
      X(2) = X(1)
      Y(1) = Y0 - (FLOAT(IBL2-1)+0.1)*GRYDDV
      Y(2) = Y(1) + DY
      IF(REQINP)THEN
 
*     cursor below bank or right and below?
 
            IF(YC .LT. Y(1))GOTO 30
            IF(YC .LT. Y(2) .AND. XC .GT. X(1))GOTO 30
            LRET = LL
            IFARET = IFADR
            ILARET = ILA
            GOTO 999
      ENDIF
      IF(IPRTFL .GT. 0)
     +  WRITE(*,'(1X,A4,1X,2I10)')IHIDBK,IFADR,ILA
 
      CALL DZDGPL(2,X,Y)
      Y(1) = Y(1) + DY2
      Y(2) = Y(1)
      X(1) = XW1
*     left and right boundaries are not in same line
      IF(IBL1 .NE. IBL2)THEN
           DX1 = X0 + DX - XW1
           DX2 = XW2 - X0
*          fill smaller gap with a line
           IF(DX1 .GT. DX2)THEN
                X(1) = X0
                IF(INDEX(CHOPT,'C').NE.0)THEN
                   IF(IDRFLG .NE. 0)THEN
                      ICOL=2
                   ELSE
                      ICOL=3
                   ENDIF
                   CALL DZDIFA(X(1),X(2),
     +                 Y(1)-DY2,Y(1)+DY2,1,ICOL)
                ELSE
                   IF(IDRFLG .NE. 0)THEN
                        CALL DZDBOX(X(1),X(2),
     +                 Y(1)-DY2,Y(1)+DY2,ISHADE)
                   ELSE
                        CALL DZDGPL(2,X,Y)
                   ENDIF
                ENDIF
                DXBDDV = DX1
                X(1) = XW1
                X(2) = X0+DX
                Y(1) = Y0 - (FLOAT(IBL1-1)+0.1)*GRYDDV+DY2
                Y(2) = Y(1)
                XW2 = X0 + DX
           ELSE
                X(2) = X0 + DX
                Y(1) = Y0 - (FLOAT(IBL1-1)+0.1)*GRYDDV+DY2
                Y(2) = Y(1)
                IF(INDEX(CHOPT,'C').NE.0)THEN
                   IF(IDRFLG .NE. 0)THEN
                      ICOL=2
                   ELSE
                      ICOL=3
                   ENDIF
                   CALL DZDIFA(X(1),X(2),
     +                 Y(1)-DY2,Y(1)+DY2,1,ICOL)
                ELSE
                   IF(IDRFLG .NE. 0)THEN
                      CALL DZDBOX(X(1),X(2),
     +                Y(1)-DY2,Y(1)+DY2,ISHADE)
                   ELSE
                      CALL DZDGPL(2,X,Y)
                   ENDIF
                ENDIF
                Y(1) = Y0 - (FLOAT(IBL2-1)+0.1)*GRYDDV+DY2
                Y(2) = Y(1)
                DXBDDV = DX2
                X(1) = X0
                X(2) = XW2
                XW1 = X0
          ENDIF
      ELSE
          DXBDDV = XW2 - XW1
      ENDIF
 
*     look if text fits in gap
 
      NCHFIT = DXBDDV/CSDDV
      IF(NCHFIT .LE. 0)THEN
         IF(INDEX(CHOPT,'C').NE.0)THEN
            IF(IDRFLG .NE. 0)THEN
               ICOL=2
            ELSE
               ICOL=3
            ENDIF
            CALL DZDIFA(X(1),X(2),
     +                 Y(1)-DY2,Y(1)+DY2,1,ICOL)
         ELSE
            IF(IDRFLG .NE. 0)THEN
               CALL DZDBOX(X(1),X(2),
     +         Y(1)-DY2,Y(1)+DY2,ISHADE)
            ELSE
               CALL DZDGPL(2,X,Y)
            ENDIF
         ENDIF
         GOTO 30
      ENDIF
 
*     mark bank for browser
 
      CALL MZSDIV (IXIN,1)
      IF(INDEX(CHOPT,'D').NE.0)THEN
         IF(NLKUSE.GE.IQ(LTDISP(ISTUSE)+KQS-3))THEN
            WRITE(*,*)'DZDDIV: Too many banks',NLKUSE+1
         ELSE
            NLKUSE=NLKUSE+1
            LQ(LTDISP(ISTUSE)+KQS-NLKUSE)= LQ(LTDISP(ISTUSE)+KQS-NLL)
            CALL IGPID(1,'BANK',NLKUSE,' ')
         ENDIF
      ENDIF
      XB(1)=X(1)
      YB(1)=Y(1)-DY2
      XB(2)=X(2)
      YB(2)=YB(1)
      XB(3)=X(2)
      YB(3)=Y(1)+DY2
      XB(4)=XB(1)
      YB(4)=YB(3)
      XB(5)=XB(1)
      YB(5)=YB(1)
      CALL ISFAIS(0)
      CALL IFA(5,XB,YB)
 
      XT = .5*(X(1)+X(2))
      YT = Y(1)
      IF(INDEX(CHOPT,'C').NE.0)THEN
         IF(IDRFLG .NE. 0)THEN
            ICOL=2
         ELSE
            ICOL=3
         ENDIF
         CALL DZDIFA(XW1,XW2,Y(1)-DY2,Y(1)+DY2,1,ICOL)
      ELSE
         IF(IDRFLG .EQ. 0)THEN
            IF(NCHFIT .GT. 4)THEN
               X(1) = XW1
               X(2) = XT-2.*CSDDV
               CALL DZDGPL(2,X,Y)
               X(1) = XT+2.*CSDDV
               X(2) = XW2
               CALL DZDGPL(2,X,Y)
            ENDIF
         ELSE
            CALL DZDBOX(XW1,XW2,Y(1)-DY2,Y(1)+DY2,ISHADE)
         ENDIF
      ENDIF
      NCH = MIN(NCHFIT,4)
      CALL UHTOC(IHIDBK,4,CHIDBK,4)
      CALL DZDTXT(CHIDBK(1:NCH),0,XT,YT,CSDDV,0.,2)
 
      NEXTRA = IBL2 - IBL1 - 1
      IF(NEXTRA .GT. 0)THEN
         X(1) = X0
         X(2) = X(1) + DX
         DO 20 IBL = IBL1+1, IBL2-1
            Y(1) = Y0 - (FLOAT(IBL-1)+0.1)*GRYDDV+DY2
            Y(2) = Y(1)
            IF(INDEX(CHOPT,'C').NE.0)THEN
               IF(IDRFLG .NE. 0)THEN
                  ICOL=2
               ELSE
                  ICOL=3
               ENDIF
               CALL DZDIFA(X(1),X(2),
     +            Y(1)-DY2,Y(1)+DY2,1,ICOL)
            ELSE
               IF(IDRFLG .NE. 0)THEN
                     CALL DZDBOX(X(1),X(2),
     +               Y(1)-DY2,Y(1)+DY2,ISHADE)
               ELSE
                     CALL DZDGPL(2,X,Y)
               ENDIF
            ENDIF
   20          CONTINUE
      ENDIF
   30 CONTINUE
 
      GOTO 10
 999  CONTINUE
*      WRITE(*,*)'DZDST1, NLKUSE',NLKUSE
 
*
      END
