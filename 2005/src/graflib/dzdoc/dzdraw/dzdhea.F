*
* $Id: dzdhea.F,v 1.1.1.1 1996/03/04 16:13:12 mclareni Exp $
*
* $Log: dzdhea.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:12  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDHEA(LB,IB,L)
*.
*...DZDHEA   draw the top bank + its up bank and link in between
*.
*. INPUT     :  LB, IB   the array containing the ZEBRA store
*.                       as used in the call to MZSTOR
*.              L        link to bank
*. OUTPUT    :  none
*.
*. SEQUENCE  : DZDPRM
*. CALLS     : DZDBNK DZDBOX DZDLI2 DZDPAR DZDTXT
*. CALLED    : DZDRA2
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
      INTEGER LB(999),IB(999)
*
#include "dzdprm.inc"
#include "dzdchv.inc"
#include "dzdzbrinc.inc"
*
      REAL XPOL(10), YPOL(10)
*
      CHARACTER*4 HIDBK, HIDUP, HIDNX
      CHARACTER*8 CHKEY
      CHARACTER*10 CTEMP
*
*       get parameters
      CALL DZDPAR(LB,IB,L,
     &  HIDBK, NUMID, HIDUP, HIDNX, JBIAS, NL, NS, ND)
      CALL CLTOU(HIDUP)
      IF(IFRCAR.NE.0 .AND. HIDUP.EQ.'NONE')THEN
*         look true up-bank in 4. word
          CONTINUE
      ENDIF
*       box for UP bank
      IF(LTDISP(ISTUSE).NE.0)THEN
         IF(NLKUSE.GE.IB(LTDISP(ISTUSE)-3))THEN
*            WRITE(*,*)'DZDHEA: Too many banks'
         ELSE
            NLKUSE=NLKUSE+1
            CALL IGPID(1,'BANK',NLKUSE,' ')
            LB(LTDISP(ISTUSE)-NLKUSE)=  LB(L+1)
         ENDIF
      ENDIF
      IF(LFCOL.NE.0)THEN
         ICOL=IBCCOL
         IFILL=1
      ELSE
         ICOL=-1
         IFILL=0
      ENDIF
      IF(LTDISP(ISTUSE).NE.0 .OR. LFCOL.NE.0)
     &  CALL DZDIFA(X0UB, X0UB+DXUB, Y0UB, Y0UB+DYB,IFILL,ICOL)
 
*      CALL IGBOX(X0UB, X0UB+DXUB, Y0UB, Y0UB+DYB)
      CALL DZDBOX(X0UB, X0UB+DXUB, Y0UB, Y0UB+DYB,1000*IFLDIR+0)
*     CALL IGPID(1,'DUMMY',99,' ')
 
      IF(IFLDIR.EQ.0 .OR. HIDUP .EQ. 'NONE')THEN
*          left
*          XT= X0UB + 0.25*CSIZE
*          YT= Y0UB + 0.15*CSIZE
*          IALG = 1
*          left
          XT= X0UB + 0.5*DXUB
          YT= Y0UB + 0.5*DYB
*        centered
          IALG = 2
*          IF(LFCOL.NE.0)THEN
*             CALL ISTXCI(1)
*          ENDIF
          CTEMP=HIDUP
          NC=4
          IF(IFLRZF .GT. 0 .AND. IFRCAR.EQ.0)THEN
*            has it a upbank
             LUP=LB(L+1)
             IF(LUP.LE.2)GOTO 40
             LUPUP=LB(LUP+1)
             IF(LUPUP.LE.2)THEN
                CHKEY(5:8)='NONE'
             ELSE
                CALL UHTOC(IB(LUPUP-4),4,CHKEY(5:8),4)
             ENDIF
             CALL UHTOC(IB(LUP-4),4,CHKEY(1:4),4)
             CALL DZDGDS(CHKEY,' ',CLINE,NLT)
             IF(NLT .LE. 0)GOTO 40
*            look if descriptor should be taken from bank
             IFC=INDEX(CLINE,'HID@D')
             IF(IFC.LE.0)GOTO 40
             IFC=IFC+5
             CALL DZDGFL(CLINE(IFC:),IST,IEND)
             IF(IST.EQ.0 .OR. IEND .LT.IST)GOTO 40
             IEND=MIN(IEND,ND)
             NC=MIN(IEND-IST+1,5)*4
             CALL UHTOC(IB(LUP+IST),4,CTEMP,NC)
          ENDIF
 40       CONTINUE
 
          CALL DZDTXT(CTEMP,NC,XT,YT,0.9*CSIZE,0.,IALG)
*          IF(LFCOL.NE.0)THEN
*             CALL ISTXCI(1)
*          ENDIF
*         link bias in up bank
          IF(JBIAS .NE. 0)THEN
              IF(JBIAS .GT. 0)THEN
                 WRITE(CTEMP,'(I10)')JBIAS
              ELSE
                 IF(JBIAS .EQ. -1)THEN
                     CTEMP='linear str'
                 ELSE
                     CTEMP=' '
                 ENDIF
              ENDIF
              IALG = 3
              XT = X0UB - 0.5*CSIZE
              YT = Y0UB + 0.5*DYB + 0.5*CSIZE
              CALL DZDTXT(CTEMP,10,XT,YT,
     &        CSIZE,0.,IALG)
          ENDIF
      ELSE
*     get name of mother directory
*      get characters fitting in CLINE from end of bank or all
          LC = LEN(CLINE)
          LC4 = LC/4
          NWNAME = IB(L-1)-IB(L+2)
          IF(NWNAME .GT. LC4)THEN
             IFW = IB(L+2)+NWNAME-LC4+1
             NC = LC
          ELSE
             IFW=IB(L+2)+1
             NC = NWNAME*4
          ENDIF
          CALL UHTOC(IB(L+IFW),4,CLINE,NC)
*         get subdir name starting from /
          ISL2  = INDEXB(CLINE(1:NC),'/')
          IF(ISL2.GT.0)THEN
              ISL2 = ISL2-1
              ISL1 = INDEXB(CLINE(1:ISL2),'/')
              IF(ISL1.LE.1)THEN
                  ISL1 = 1
              ELSE
                  IF(CLINE(ISL1-1:ISL1-1).EQ.'/')ISL1=ISL1-1
              ENDIF
              NC=ISL2-ISL1+1
              XT=X0UB+DXUB+0.5*CSIZE
              YT=Y0UB
              CALL DZDTXT(CLINE(ISL1:ISL2),NC,XT,YT,CSIZE,0.,1)
          ENDIF
      ENDIF
*       link from Up bank to top bank
      XPOL(1) = X0UB
      YPOL(1) = Y0UB + 0.5*DYB
      XPOL(2) = X0TB + 0.5*DXB
      YPOL(2) = YPOL(1)
      XPOL(3) = XPOL(2)
      YPOL(3) = Y0TB + DYB
      XPOL(4) = ARRLEN
      YPOL(4) = 270.
      CALL DZDLI2(4,XPOL,YPOL,0)
*       draw the bank
*12/2
*     try to fit a next bank
 
      IF(X0TB+2.*DXTB .LE. XRANGE .AND. HIDNX.NE.' ')THEN
         DXX=DXTB-2.5*GRIDX
         HIDNX=' '
         CALL DZDBNK(LB,IB,L,
     +   X0TB, Y0TB, DXX, DYB, HIDBK, HIDNX,HIDUP,
     +   NUMID, ND)
         LD=LB(L)
         CALL DZDPAR(LB,IB,LD,
     +   HIDBK, NUMID, HIDUP, HIDNX, JBIAS, NL, NS, ND)
         CALL DZDBNK(LB,IB,LD,
     +   X0TB+DXX+3.*GRIDX, Y0TB, DXX, DYB, HIDBK, HIDNX,HIDUP,
     +   NUMID, ND)
         XPOL(1) = X0TB+DXX+2.5*GRIDX
         YPOL(1) = Y0TB + 0.5*DYB
         XPOL(2) = X0TB+DXX+3.*GRIDX
         YPOL(2) = YPOL(1)
         XPOL(3) = ARRLEN
         YPOL(3) = 0.
         CALL DZDLI2(3,XPOL,YPOL,0)
 
      ELSE
         CALL DZDBNK(LB,IB,L,
     &    X0TB, Y0TB, DXTB, DYB, HIDBK, HIDNX,HIDUP,
     &    NUMID, ND)
      ENDIF
      END
**********************************************************************
