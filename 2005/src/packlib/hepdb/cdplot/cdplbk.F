*
* $Id: cdplbk.F,v 1.1.1.1 1996/02/28 16:24:22 mclareni Exp $
*
* $Log: cdplbk.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:22  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPLBK (LAD, IRC)
*     ============================
*
************************************************************************
*                                                                      *
*        SUBR. CDPLBK (LAD, IRC*)                                      *
*                                                                      *
*   Draw the nodes on the screen as given in the bank at address LAD.  *
*   This bank is usually prepared by a call to the routine CDFPAT.     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LAD      Address of the bank containing the information          *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by Interactive interface                                    *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       CHVAR*16
      DIMENSION       X(5), Y(5), LAD(9)
      PARAMETER       (XRNG=24.0, YRNG=24.0, HIGH=0.5, SMARG=1.0)
      PARAMETER       (CSIZ=0.6, HSIZ=1.5, GAP=3.5, FRAM=1.2)
*
*     ------------------------------------------------------------------
*
* *** Derive the number of tree structure levels
*
      IRC  = 0
      LREFCD(1) = LAD(1)
      NLEV = IQ(KOFUCD+LREFCD(1)+MNLVCD)
      IF (NLEV.LE.0)            GO TO 999
*
* *** Set the default values first
*
      SIZH = HSIZ
      SIZC = CSIZ
      GAPH = GAP
      GAPV = HIGH
      SGAP = SMARG
*
* *** Find the X- and Y- ranges in each level
*
      IPNL  = IQ(KOFUCD+LREFCD(1)+MPNLCD)
      YSIZM = FRAM*25+GAPV*24+SGAP*2+SIZH
      XSIZE = 0.
      NTEMP = 0
      KMAX  = 1
      DO 15 ILEV  =1, NLEV
        NODES = IQ(KOFUCD+LREFCD(1)+IPNL+MNODCD)
        IPNN  = IQ(KOFUCD+LREFCD(1)+IPNL+MPNNCD)
        MXNCH = 0
        DO 10 INOD =1, NODES
          NCHRN = IQ(KOFUCD+LREFCD(1)+IPNN+MNCHCD)
          IF (NCHRN.GT.MXNCH) MXNCH = NCHRN
          IPNN  = IQ(KOFUCD+LREFCD(1)+IPNN+MPNLCD)
   10   CONTINUE
        XSIZE = XSIZE + SIZC*MXNCH
        Q(KOFUCD+LREFCD(1)+IPNL+MYFLCD) = NODES*FRAM + (NODES-1)*GAPV
     +                                  + SGAP*2
        IF (NTEMP.LT.NODES) THEN
          NTEMP = NODES
          KMAX  = ILEV
        ENDIF
        IPNL  = IQ(KOFUCD+LREFCD(1)+IPNL+MPNLCD)
   15 CONTINUE
      XSIZE = XSIZE+(NLEV-1)*GAPH+SGAP*2
      NTEMP = NTEMP + 6
      YSIZE = FRAM*NTEMP + GAPV*(NTEMP-1) + SGAP*2 + SIZH
      IF (YSIZE.LT.YSIZM) YSIZE = YSIZM
*
* *** Now choose the scaling factor
*
      SCAL  = XRNG/XSIZE
      SCL0  = YRNG/YSIZE
      IF (SCAL.GT.SCL0) THEN
        SCAL = SCL0
        XOFF = 0.5*(XRNG-SCAL*XSIZE)
      ELSE
        XOFF = 0.
      ENDIF
      SIZH  = SCAL*SIZH
      SIZC  = SCAL*SIZC
      SGAP  = SCAL*SGAP
      GAPH  = SCAL*GAPH
      GAPV  = SCAL*GAPV
      HFRAM = SCAL*FRAM
      XVAL  = XOFF+SGAP
      Q(KOFUCD+LREFCD(1)+MHFMCD) = HFRAM
      IPNL  = IQ(KOFUCD+LREFCD(1)+MPNLCD)
      DO 25 ILEV =1, NLEV
        NODES = IQ(KOFUCD+LREFCD(1)+IPNL+MNODCD)
        IPNN  = IQ(KOFUCD+LREFCD(1)+IPNL+MPNNCD)
        MXNCH = 0
        DO 20 INOD =1, NODES
          NCHRN = IQ(KOFUCD+LREFCD(1)+IPNN+MNCHCD)
          IF (NCHRN.GT.MXNCH) MXNCH = NCHRN
          IPNN  = IQ(KOFUCD+LREFCD(1)+IPNN+MPNLCD)
   20   CONTINUE
        YSIZV = Q(KOFUCD+LREFCD(1)+IPNL+MYFLCD)
        Q(KOFUCD+LREFCD(1)+IPNL+MXOFCD) = XVAL
        Q(KOFUCD+LREFCD(1)+IPNL+MYFLCD) = YRNG - 2*SGAP
     +                                  - 0.5*(YRNG-SCAL*YSIZV)
        Q(KOFUCD+LREFCD(1)+IPNL+MXWDCD) = SIZC*MXNCH
        XVAL  = XVAL + Q(KOFUCD+LREFCD(1)+IPNL+MXWDCD) + GAPH
        IPNL  = IQ(KOFUCD+LREFCD(1)+IPNL+MPNLCD)
   25 CONTINUE
*
* *** Now set the range; choose colour, line style, line width
*
      CALL IGRNG (XRNG, YRNG)
      CALL ISPLCI (7)
      CALL ISLN (1)
      CALL ISLWSC (2.0)
*
* *** Now draw the box and title
*
      CALL IGBOX (0., XRNG, 0.,YRNG)
      CALL IGTEXT (0.5*XRNG, SGAP, 'DBL3 Tree Structure', SIZH, 0., 'C')
*
* *** Now arrange the node places into the tree
*
      YOFF1 = 0.
*
*  ** Start with the level with maximum number of nodes
*
      LPNL  = IQ(KOFUCD+LREFCD(1)+MPNLCD)
      IF (KMAX.GT.1) THEN
        DO 30 I = 2, KMAX
   30   LPNL  = IQ(KOFUCD+LREFCD(1)+LPNL+MPNLCD)
      ENDIF
      YOFFN =  Q(KOFUCD+LREFCD(1)+LPNL+MYFLCD)
      IPNN  = IQ(KOFUCD+LREFCD(1)+LPNL+MPNNCD)
      NODES = IQ(KOFUCD+LREFCD(1)+LPNL+MNODCD)
      DO 35 INOD = 1, NODES
        NDOWN = IQ(KOFUCD+LREFCD(1)+IPNN+MNDWCD)
        IF (NDOWN.GT.0) THEN
          IF (YOFF1.GT.0) THEN
            IF (YOFFN.GT.YOFF1) YOFFN = YOFF1
          ENDIF
          Q(KOFUCD+LREFCD(1)+IPNN+MYFNCD) = YOFFN
          HH    = 0.5 * (NDOWN*HFRAM + (NDOWN-1)*GAPV)
          YOFF1 = YOFFN - HH - GAPV - HFRAM*0.5
          YOFFN = YOFFN - HFRAM - GAPV
        ELSE
          Q(KOFUCD+LREFCD(1)+IPNN+MYFNCD) = YOFFN
          YOFFN = YOFFN - HFRAM - GAPV
        ENDIF
        IPNN  = IQ(KOFUCD+LREFCD(1)+IPNN+MPNLCD)
   35 CONTINUE
*
*  ** Go up with level number from one with maximum nodes
*
      YOFFP = 0.
      KPNL  = LPNL
      DO 55 ILEV = KMAX+1,  NLEV
        IPNL  = IQ(KOFUCD+LREFCD(1)+KPNL+MPNLCD)
        IPNNU = IQ(KOFUCD+LREFCD(1)+KPNL+MPNNCD)
        KDOWN = 1
        NODES = IQ(KOFUCD+LREFCD(1)+KPNL+MNODCD)
        NODEH = IQ(KOFUCD+LREFCD(1)+IPNL+MNODCD)
        DO 45 IUP = 1, NODES
          NDOWN = IQ(KOFUCD+LREFCD(1)+IPNNU+MNDWCD)
          IF (NDOWN.GT.0) THEN
            HH    = 0.5 * (NDOWN*HFRAM + (NDOWN-1)*GAPV)
            YOFFN = HH + Q(KOFUCD+LREFCD(1)+IPNNU+MYFNCD) - HFRAM*0.5
            IF (KDOWN.GT.1) THEN
              IF (YOFFN.GT.YOFFP) YOFFN = YOFFP
            ENDIF
            IPNN  = IQ(KOFUCD+LREFCD(1)+IPNL+MPNNCD)
            DO 40 INOD = 1, NODEH
              IF (IQ(KOFUCD+LREFCD(1)+IPNN+MNFNCD).EQ.IUP) THEN
                Q(KOFUCD+LREFCD(1)+IPNN+MYFNCD) = YOFFN
                YOFFN = YOFFN - HFRAM - GAPV
                KDOWN = KDOWN + 1
              ENDIF
              IPNN  = IQ(KOFUCD+LREFCD(1)+IPNN+MPNLCD)
   40       CONTINUE
            IF (KDOWN.GT.NODEH) GO TO 50
            YOFFP = YOFFN
          ENDIF
          IPNNU = IQ(KOFUCD+LREFCD(1)+IPNNU+MPNLCD)
   45   CONTINUE
   50   KPNL  = IPNL
   55 CONTINUE
*
*  ** Now go down with level number from one with maximum nodes
*
      KPNL  = LPNL
      DO 75 ILEV = 1, KMAX-1
        IPNL  = IQ(KOFUCD+LREFCD(1)+KPNL+MPPLCD)
        IPNNU = IQ(KOFUCD+LREFCD(1)+IPNL+MPNNCD)
        YOFFP = 0.
        NODES = IQ(KOFUCD+LREFCD(1)+IPNL+MNODCD)
        NODEH = IQ(KOFUCD+LREFCD(1)+KPNL+MNODCD)
        KCOUN = 0
        LCOUN = 0
        DO 65 IUP = 1, NODES
          NDOWN = IQ(KOFUCD+LREFCD(1)+IPNNU+MNDWCD)
          IF (NDOWN.GT.0) THEN
            ICOUN = 0
            IPNN  = IQ(KOFUCD+LREFCD(1)+KPNL+MPNNCD)
            YSTRT = 0.
            YSTOP = 0.
            DO 60 INOD = 1, NODEH
              IF (IQ(KOFUCD+LREFCD(1)+IPNN+MNFNCD).EQ.IUP) THEN
                ICOUN = ICOUN + 1
                IF (ICOUN.EQ.1) YSTRT = Q(KOFUCD+LREFCD(1)+IPNN+MYFNCD)
                YSTOP = Q(KOFUCD+LREFCD(1)+IPNN+MYFNCD)
              ENDIF
              IPNN  = IQ(KOFUCD+LREFCD(1)+IPNN+MPNLCD)
   60       CONTINUE
            YOFFN = (YSTRT+YSTOP)*0.5
            Q(KOFUCD+LREFCD(1)+IPNNU+MYFNCD) = YOFFN
            KCOUN = KCOUN + 1
            IF (KCOUN.EQ.1) YOFFP = YOFFN
          ELSE
            IF (KCOUN.GT.0) THEN
              YOFFN = YOFFN - GAPV - HFRAM
              Q(KOFUCD+LREFCD(1)+IPNNU+MYFNCD) = YOFFN
            ELSE
              LCOUN = LCOUN + 1
            ENDIF
          ENDIF
          IPNNU = IQ(KOFUCD+LREFCD(1)+IPNNU+MPNLCD)
   65   CONTINUE
        YOFFN = YOFFP
        IPNNU = IQ(KOFUCD+LREFCD(1)+IPNL+MPNNCD)
        DO 70 I = 1, LCOUN
          Q(KOFUCD+LREFCD(1)+IPNNU+MYFNCD) = YOFFP
     +                                     + (LCOUN-I+1)*(HFRAM+GAPV)
          IPNNU = IQ(KOFUCD+LREFCD(1)+IPNNU+MPNLCD)
   70   CONTINUE
        KPNL  = IPNL
   75 CONTINUE
*
* *** Now draw the tree structure for DBL3
*
      IPNL = IQ(KOFUCD+LREFCD(1)+MPNLCD)
      DO 95 ILEV = 1, NLEV
        IPNN  = IQ(KOFUCD+LREFCD(1)+IPNL+MPNNCD)
        NODES = IQ(KOFUCD+LREFCD(1)+IPNL+MNODCD)
        XOFF  =  Q(KOFUCD+LREFCD(1)+IPNL+MXOFCD)
        XWID  =  Q(KOFUCD+LREFCD(1)+IPNL+MXWDCD)
        IF (ILEV.LT.NLEV) THEN
          LPNL  = IQ(KOFUCD+LREFCD(1)+IPNL+MPNLCD)
          XOFFL =  Q(KOFUCD+LREFCD(1)+LPNL+MXOFCD)
          NODEH = IQ(KOFUCD+LREFCD(1)+LPNL+MNODCD)
        ELSE
          XOFFL = 0.
          NODEH = 0
        ENDIF
        DO 90 INOD = 1, NODES
          NCH   = IQ(KOFUCD+LREFCD(1)+IPNN+MNCHCD)
          NDOWN = IQ(KOFUCD+LREFCD(1)+IPNN+MNDWCD)
          YOFF  =  Q(KOFUCD+LREFCD(1)+IPNN+MYFNCD)
          X(1)  = XOFF
          X(2)  = X(1) + XWID
          X(3)  = X(2)
          X(4)  = X(1)
          X(5)  = X(1)
          XCEN  = 0.5 * (X(1) + X(2))
          Y(1)  = YOFF
          Y(2)  = Y(1)
          Y(3)  = Y(1) + HFRAM
          Y(4)  = Y(3)
          Y(5)  = Y(1)
          YCEN  = Y(1) + 0.5*(HFRAM-SIZC)
          CALL IPL (5,X,Y)
          CALL UHTOC (IQ(KOFUCD+LREFCD(1)+IPNN+MNAMCD), 4, CHVAR, NCH)
          CALL IGTEXT (XCEN, YCEN, CHVAR(1:NCH), SIZC, 0., 'C')
          IF (ILEV.LT.NLEV.AND.NDOWN.GT.0) THEN
            IPNND = IQ(KOFUCD+LREFCD(1)+LPNL+MPNNCD)
            YUP   =  Q(KOFUCD+LREFCD(1)+IPNN+MYFNCD) + HFRAM*0.5
            XUP   = XOFF + XWID
            YDOW1 = -1
            XDOWN = XOFFL
            XLINE = XDOWN - GAPH/6.
            DO 80 IDOWN = 1, NODEH
              ITEMP   =IQ(KOFUCD+LREFCD(1)+IPNND+MNFNCD)
              IF (INOD.EQ.ITEMP) THEN
                YDOW2 = Q(KOFUCD+LREFCD(1)+IPNND+MYFNCD) + HFRAM*0.5
                IF (YDOW1.LT.0.) THEN
                  YDOW1 = YDOW2
                  YFRST = YDOW1
                  Y(1)  = YDOW1
                  Y(2)  = Y(1)
                  X(1)  = XDOWN
                  X(2)  = XLINE
                  CALL IPL (2, X, Y)
                ELSE
                  Y(1)  = YDOW1
                  Y(2)  = YDOW2
                  Y(3)  = Y(2)
                  X(1)  = XLINE
                  X(2)  = X(1)
                  X(3)  = XDOWN
                  CALL IPL (3, X, Y)
                  YDOW1 = YDOW2
                ENDIF
              ENDIF
              IPNND = IQ(KOFUCD+LREFCD(1)+IPNND+MPNLCD)
   80       CONTINUE
            YEND  = YDOW1
   85       IF (YDOW1.GE.0) THEN
              Y(1)  = YUP
              Y(2)  = (YFRST+YEND)*0.5
              X(1)  = XUP
              X(2)  = XLINE
              CALL IPL (2, X, Y)
            ENDIF
          ENDIF
          IPNN  = IQ(KOFUCD+LREFCD(1)+IPNN+MPNLCD)
   90   CONTINUE
        IPNL  = IQ(KOFUCD+LREFCD(1)+IPNL+MPNLCD)
   95 CONTINUE
*                                                             END CDPLBK
  999 END
