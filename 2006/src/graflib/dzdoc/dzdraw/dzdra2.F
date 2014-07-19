*
* $Id: dzdra2.F,v 1.1.1.1 1996/03/04 16:13:08 mclareni Exp $
*
* $Log: dzdra2.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:08  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDRA2(IXSTOR,LB,IB,L,ILKACT
     + ,CHOPT, XCUR, YCUR, REQINP, IFLCUR,YTOP,YBOTTM)
*.
*...DZDRA2   draw a data structure on open workstation(s) or
*.           request input of locator assuming the picture
*.           has previously drawn (with (REQINP=.FALSE.)
*.           in the second case the picture is redrawn only
*.           virtually
*.
*. INPUT     : IXSTOR    Store index
*.             LB, IB    the array containing the ZEBRA store
*.                       as used in the call to MZSTOR
*.             L         Link to top bank to be displayed
*.             ILKACT    the link where drawing of down banks starts
*.             CHOPT     Character option:
*.                       'N' opening and closing WS is done by caller
*.                       'T' D/S generated from documentation
*.                       'R' Rz-file with documentation available
*.                       'B' draw boxes as <>
*.                       '1' draw banks od 1 level only if not the
*.                           complete 2nd level fits
*.                       'M' mark subtree complete
*.             REQINP    indicates if picture should be drawn
*.                       physically or locator input is requested
*. OUTPUT    : XCUR,YCUR coordinates of locator
*.             IFLCUR    flag indicating where locator is(see DZDGET)
*. COMMON    :
*. SEQUENCE  : DZDPRM
*. CALLS     : DZDBNK DZDCKC DZDHEA DZDLBX DZDLNK DZDPAR DZDSBX GQCNTN
*. CALLS     : GQNT   GSASF  GSELNT GSPLCI
*. CALLED    : DZDRA1
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*. LAST MOD  : 3-Dec-93
*.
*. Modification Log.
*. 18-Mar-88   implement input from doc file
*. 2-OCT-90    skip more then 4 zero down links in picture
*. 3-Dec-93    check for valid link (DZDGLK)
*.**********************************************************************
*.
*
      INTEGER LB(999),IB(999)
      CHARACTER*(*) CHOPT
      LOGICAL REQINP
#include "dzdprm.inc"
      INTEGER LREF(-1:4,0:15)
      COMMON/DZDLKA/LREF
      INTEGER INIFLG, IST, DZDGLK
      SAVE INIFLG
      REAL XPOL(3),YPOL(3)
*
      CHARACTER*4 HIDDW, HIDUPX, HIDNXX, HIDNXN
      LOGICAL DZDINB
      EXTERNAL DZDINB
      DATA INIFLG/0/
*
*----
      IF(L.EQ.0)THEN
         WRITE(*,*)'DZDRA2: Ltopin = 0'
         RETURN
      ENDIF
      IST = JBYT(IXSTOR,27,6)
 
      CALL MZLINT(IXSTOR,'/DZDLKA/'
     &,LREF(-1,IST),LREF(1,IST),LREF(4,IST))
      IF(INIFLG.EQ.0)THEN
         INIFLG=1
         CSFACT=1.
      ENDIF
      IFMASC = INDEX(CHOPT,'M')
      IFRCAR = INDEX(CHOPT,'T')
      IF(IFRCAR.NE.0)IFMASC=1
      IFLRZF = INDEX(CHOPT,'R')
      IFLDIR = INDEX(CHOPT,'B')
      XRANGE = 20.
      YRANGE = 20.
*     IFOMED=3 : LaTex
      IF(IFOMED.NE.3)THEN
         CALL IGRNG( XRANGE, YRANGE)
         CALL ISLN(1)
         CALL ISLWSC(3.)
         CALL ISTXCI(1)
         CALL ISPLCI(1)
         IF(IDZTFO.NE.0)CALL ISTXFP(IDZTFO,IDZTPR)
      ENDIF
      GRIDX = GRIDSC*XRANGE/32.
      GRIDY = GRIDX
*      L = LTOP
      IFLCUR = 0
   10   CONTINUE
*
      NS = IB(L-2)
      NL = IB(L-3)
      IF(ILKACT .LE. 0 .OR. ILKACT .GT. NL)THEN
            ILD1 = 1
      ELSE
            ILD1 = ILKACT
      ENDIF
      NBLEV1=0
      MNZERO=0
      DO 20 I=ILD1,NL
         IF(LB(L-I) .NE. 0)THEN
            NBLEV1 = NBLEV1+1
            MNZERO=I
         ENDIF
   20 CONTINUE
      NULEV1=MNZERO
      MNZERO=MNZERO+1-ILD1
      NBTOT=NBLEV1
 
      Y0 = YRANGE
      DXTB    = 11. * GRIDX
      DXUB    = 2.  * GRIDX
      DXLK    = GRIDX/2
      DXB     = GRIDX
      DYB     = GRIDY
      X0HEAD  = GRIDX
      Y0HEAD  = Y0 - GRIDY
 
*11/2     X0TB    = 11. * GRIDX
 
      XNBK= MIN(MNZERO,13)
*     need space for a big box?
      IF(ILKACT.GT.0)XNBK=XNBK+4
      X0TB    = (1. + 0.5*XNBK) * GRIDX
      IF(IFMASC.NE.0)X0TB=X0TB+2.* GRIDX
      IF(NBLEV1.GT.13)X0TB=X0TB+2.*GRIDX
 
      Y0TB    = Y0 - 4. * GRIDY
      X0UB    = X0TB + DXTB - 2. * GRIDX
      Y0UB    = Y0TB + 2. * GRIDY
*      CSIZE   = 0.2* GRIDY
*      CSIZE   = 0.4 * GRIDY
      CSIZE=0.5 * GRIDY * CSFACT
      ARRLEN = GRIDX/4.
*       # of first level down banks to be drawn
*       # of grid points available
      NGRIDY = YRANGE/GRIDY
*       need 5 gridunits for top bank, 2 units for down banks
      NBKODM = (NGRIDY-6)/2
      X0LK = X0TB
      Y0DB = Y0TB
      IF(REQINP)THEN
*        is cursor in up-bank
         IF(DZDINB(XCUR,YCUR,
     +   X0UB, X0UB+DXUB,Y0UB,Y0UB+DYB))THEN
            L = LB(L+1)
            IFLCUR = 1
            GOTO 90
         ENDIF
         CALL DZDCKC(XCUR,YCUR,X0TB,Y0TB,LB,IB,L,IFLCUR)
         IF(IFLCUR .GT. 0)THEN
*           LTOP = L
            GOTO 90
         ENDIF
      ELSE
         CALL DZDHEA(LB,IB,L)
         IF(ILKACT .GT. 1)THEN
            IF(IFLDIR.EQ.0)THEN
               CALL DZDLBX(X0TB,Y0TB,1,ILKACT-1,NS)
            ELSE
               CALL DZDBOX(X0TB-4.*DXLK,X0TB,Y0TB,Y0TB+DYB,1000)
            ENDIF
            X0LK = X0TB-4.*DXLK
         ENDIF
      ENDIF
      IF(ILKACT .LE. 0)THEN
         ILD1 = 1
      ELSE
         ILD1 = ILKACT
      ENDIF
 
*     total number of live banks 2 levels deep
      CALL DZDGNZ(LB,IB,L,ILD1,NLL1,NBLEV1, NULEV1, NBLEVD)
      NLIVET=NBLEV1+NBLEVD
      IF(INDEX(CHOPT,'1').EQ.0)NLIVET=0
*     loop on down first level down banks (until label 70)
 
      ILLEV1=ILD1
25    CONTINUE
 
*     all done?
      IF(ILLEV1.GT.NL)GOTO 75
 
      LREF(1,IST) = DZDGLK(LB,IB,L,ILLEV1)
      X0LK = X0LK - DXLK
*     link zero draw just the little box
      IF(LREF(1,IST) .EQ. 0)THEN
         IF(.NOT. REQINP)THEN
*           look if more then 4 zero links are there, skip them
            NL0=0
            DO 26 I=ILLEV1,NL
            IF(LB(L-I).NE.0)GOTO 27
26          NL0=NL0+1
 
*           all rest = 0
            GOTO 70
 
27          IF(NL0.GT.4)THEN
               IF(ILLEV1+NL0.GT.NL)GOTO 75
               X0LK=X0LK+DXLK
               IF(IFLDIR.EQ.0)THEN
                  CALL DZDLBX(X0LK,Y0TB,ILLEV1,ILLEV1+NL0-1,NS)
               ELSE
                  CALL DZDBOX(X0LK-4.*DXLK,X0LK,Y0TB,Y0TB+DYB,1000)
               ENDIF
               ILLEV1=ILLEV1+NL0
               X0LK=X0LK-4.*DXLK
               GOTO 25
            ELSE
               CALL DZDSBX(X0LK,Y0TB,ILLEV1,NS)
            ENDIF
         ENDIF
         GOTO 70
      ENDIF
*     mark for subtree complete
      IF(IFMASC.NE.0)IB(LREF(1,IST))=IOR(IB(LREF(1,IST)),ALDBIT)
 
*     get the parameter for this down bank
      CALL DZDPAR(LB,IB,LREF(1,IST),
     +  HIDDW, NUMIDD, HIDUPX, HIDNXX, JBIASX, NLD, NSD, NDD)
      X0LEV1 = X0TB + 2.*DXB
      Y0DB = Y0DB - 2.*GRIDY
 
*     arrived at bottom of paper?
 
      IF(Y0DB .LT. 1.*GRIDY)THEN
         X0LB = X0LK + DXLK
         IF(.NOT. REQINP)THEN
            IF(ILLEV1.EQ.NL)THEN
               CALL DZDSBX(X0LB,Y0TB,ILLEV1,NS)
            ELSE
               CALL DZDLBX(X0LB,Y0TB,ILLEV1,NL,NS)
            ENDIF
         ENDIF
         ILKACT = ILLEV1
         GOTO 90
      ENDIF
*
      IF(REQINP)THEN
         CALL DZDCKC(XCUR,YCUR,X0LEV1,Y0DB,LB,IB,LREF(1,IST),IFLCUR)
         IF(IFLCUR .GT. 0)THEN
            L = LREF(1,IST)
            GOTO 90
         ENDIF
      ELSE
         NRL = NLD - NSD
*12/2
*        try to fit a next bank
 
         IF(X0LEV1+2.*DXTB .LE. XRANGE .AND. HIDNXX.NE.' ')THEN
*           make it a bit smaller
            DXX=DXTB-3.*GRIDX
*            HIDNXX=' '
*           draw the bank
            CALL DZDBNK(LB,IB,LREF(1,IST),
     +      X0LEV1, Y0DB, DXX, DYB, HIDDW, '    ',HIDUPX,
*     +      X0LEV1, Y0DB, DXX, DYB, HIDDW, HIDNXX,HIDUPX,
     +      NUMIDD, NDD)
*           now the next bank
            LREF(1,IST)=LB(LREF(1,IST))
            CALL DZDPAR(LB,IB,LREF(1,IST),
     +      HIDDW, NUMIDD, HIDUPX, HIDNXN, JBIASX, NNN, NSD, NDD)
*           if its documentation only 1 next bank was booked assume more
            IF(IFRCAR.NE.0 .AND. HIDNXN.EQ.' ')HIDNXN=HIDNXX
            CALL DZDBNK(LB,IB,LREF(1,IST),
     +      X0LEV1+DXX+3.*GRIDX, Y0DB, DXX, DYB, HIDDW, HIDNXN,HIDUPX,
     +      NUMIDD, NDD)
            XPOL(1) = X0LEV1+DXX+2.5*GRIDX
            YPOL(1) = Y0DB + 0.5*DYB
            XPOL(2) = X0LEV1+DXX+3.*GRIDX
            YPOL(2) = YPOL(1)
            XPOL(3) = ARRLEN
            YPOL(3) = 0.
            CALL DZDLI2(3,XPOL,YPOL,0)
         ELSE
            CALL DZDBNK(LB,IB,LREF(1,IST),
     +      X0LEV1, Y0DB, DXTB, DYB, HIDDW, HIDNXX,HIDUPX,
     +      NUMIDD, NDD)
         ENDIF
         LREF(1,IST) = LB(L-ILLEV1)
*        link to bank
         CALL DZDLNK(X0LK, Y0TB, X0LEV1, Y0DB, ILLEV1, NS)
      ENDIF
 
*     count number of non zero down links at level 2
 
      CALL DZDGNZ(LB,IB,LREF(1,IST),1,NLDON1,NBLEV2, NULEV2, NN)
 
      Y0LEV1= Y0DB
 
*     make sure:
*               highest non zero link <= MALEV2 (avoid crossings)
*               not more than NBKODM (13) in total
      MALEV2=4+ILLEV1
      IF(ILLEV1.EQ.NULEV1)MALEV2=MAX(MALEV2,8)
      IF(NBLEV2.GT.0               .AND.
     &   NLIVET .LE. NBKODM        .AND.
     &   NBTOT+NBLEV2 .LE. NBKODM  .AND.
     &   (NULEV2.LE.MALEV2         .OR.
     &   (NLDON1.EQ.1 .AND. NBLEV2.EQ.1)))THEN
 
*        total number of banks we have to draw
         NBTOT=NBTOT+NBLEV2
 
*        draw the links at down level 2
 
         DO 50 ILLEV2=1,NULEV2
            CALL DZDTRE(LB,IB,LREF(1,IST),
     &      ILLEV2,X0LEV1,Y0LEV1,Y0DB,
     &      REQINP,L,XCUR,YCUR,IFLCUR,0)
            IF(REQINP .AND.IFLCUR .GT. 0)GOTO 90
 
            LREF(2,IST)=DZDGLK(LB,IB,LREF(1,IST),ILLEV2)
            IF(LREF(2,IST).EQ.0) GOTO 50
 
*           can we fit another level?
 
            X0LEV2=X0LEV1+4.*DXLK
            XRIGEN=X0LEV2+8.*DXLK+DXTB
            CALL DZDGNZ(LB,IB,LREF(2,IST),1,NLDON1,NBLEV3, NULEV3,NN)
            IF(NULEV3.EQ.0) GOTO 50
            MALEV3=4+ILLEV2
            IF(ILLEV2.EQ.NULEV2)MALEV3=MAX(MALEV3,8)
 
            Y0LEV2= Y0DB
            IF(XRIGEN.LT.XRANGE           .AND.
     &         NBLEV3.GT.0                .AND.
     &         NBTOT+NBLEV3 .LE. NBKODM   .AND.
     &         (NULEV3.LE.MALEV3          .OR.
     &         (NLDON1.EQ.1 .AND. NBLEV3.EQ.1)))THEN
               NBTOT=NBTOT+NBLEV3
 
*              draw the links at down level 3
 
               DO 45 ILLEV3=1,NULEV3
                  CALL DZDTRE(LB,IB,LREF(2,IST),
     &            ILLEV3,X0LEV2,Y0LEV2,Y0DB,
     &            REQINP,L,XCUR,YCUR,IFLCUR,0)
                  IF(REQINP .AND. IFLCUR .GT. 0)GOTO 90
 
                  LREF(3,IST)=DZDGLK(LB,IB,LREF(2,IST),ILLEV3)
                  IF(LREF(3,IST).EQ.0) GOTO 45
 
*                 yet another one?
 
                  X0LEV3=X0LEV2+4.*DXLK
                  XRIGEN=X0LEV3+8.*DXLK+DXTB
                  CALL DZDGNZ(LB,IB,LREF(3,IST),1,
     &                        NLDON1,NBLEV4,NULEV4,NN)
                  IF(NULEV4.EQ.0) GOTO 45
                  Y0LEV3= Y0DB
                  MALEV4=4+ILLEV3
                  IF(ILLEV3.EQ.NULEV3)MALEV4=MAX(MALEV4,8)
                  IF(XRIGEN.LT.XRANGE          .AND.
     &               NBLEV4.GT.0               .AND.
     &               NBTOT+NBLEV4 .LE. NBKODM  .AND.
     &               (NULEV4.LE.MALEV4         .OR.
     &               (NLDON1.EQ.1 .AND. NBLEV4.EQ.1)))THEN
                     NBTOT=NBTOT+NBLEV4
 
*                    draw the links at down level 4
 
                     DO 40 ILLEV4=1,NULEV4
                        CALL DZDTRE(LB,IB,LREF(3,IST),
     &                  ILLEV4,X0LEV3,Y0LEV3,Y0DB,
     &                  REQINP,L,XCUR,YCUR,IFLCUR,1)
                        IF(REQINP .AND. IFLCUR .GT. 0)GOTO 90
*                    end level 4
40                   CONTINUE
 
                  ELSE
                     IF(.NOT.REQINP)CALL DZDLBX(X0LEV3,Y0LEV3,1,NULEV4,
     &                              IB(LREF(3,IST)-3))
*                   mark for subtree not complete
                    IF(IFMASC.NE.0)
     &              IB(LREF(1,IST))=IAND(IB(LREF(1,IST)),NOT(ALDBIT))
                  ENDIF
*              end level 3
45             CONTINUE
 
            ELSE
                IF(.NOT.REQINP)CALL DZDLBX(X0LEV2,Y0LEV2,1,NULEV3,
     &                         IB(LREF(2,IST)-3))
*               mark for subtree not complete
                IF(IFMASC.NE.0)
     &          IB(LREF(1,IST))=IAND(IB(LREF(1,IST)),NOT(ALDBIT))
            ENDIF
 
*        end level 2
50       CONTINUE
 
*        adjust X0 at down level 1
         X01 = X0LEV1 - FLOAT(NULEV2+1)*DXLK
 
         IF(.NOT. REQINP .AND. IFMASC .NE. 0 .AND. IB(L+2) .LT. 0)THEN
            CALL DZDSBX(X01,Y0LEV1,ILLEV1,NSD)
*           mark for subtree not complete
            IF(IFMASC.NE.0)
     &      IB(LREF(1,IST))=IAND(IB(LREF(1,IST)),NOT(ALDBIT))
         ENDIF
      ELSE
 
*     only indicate links of down bank
         IF(NSD .NE. 0 .AND. .NOT.REQINP )THEN
            IF(IFRCAR .NE. 0 .AND. IB(LREF(1,IST)+2) .LT. 0)THEN
               NS1 = -1
            ELSE
               NS1 = NSD
            ENDIF
*           only if its doc or if links are not 0
            IF(IFRCAR .NE. 0 .OR. NBLEV2.GT.0)
     &      CALL DZDLBX(X0LEV1, Y0DB, 1,NS1,NSD)
         ENDIF
*        mark for subtree not complete
         IF(IFMASC.NE.0)
     &   IB(LREF(1,IST))=IAND(IB(LREF(1,IST)),NOT(ALDBIT))
      ENDIF
 
   70 CONTINUE
      ILLEV1=ILLEV1+1
      GOTO 25
 
*     end loop down banks
 
75    CONTINUE
 
      X0LK = X0LK - DXLK
      IF(IFRCAR .NE. 0 .AND. IB(L+2) .LT. 0)THEN
         IF(.NOT. REQINP)THEN
            CALL DZDSBX(X0LK,Y0TB,-1,NSD)
         ENDIF
      ENDIF
   80 CONTINUE
      ILKACT = 0
   90 CONTINUE
*     deactivate link area
      LREF(-1,IST) = 0
      YBOTTM = Y0DB/YRANGE
      IF (YBOTTM .LT. 0.)YBOTTM = 0.
      END
******************************************************************
