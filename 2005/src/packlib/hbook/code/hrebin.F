*
* $Id: hrebin.F,v 1.2 1996/03/27 10:52:43 couet Exp $
*
* $Log: hrebin.F,v $
* Revision 1.2  1996/03/27 10:52:43  couet
* The test ILAS.LE.IFIRS is now a LT this allow to rebin only one channel.
*
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 24/01/95  11.09.42  by  Julian Bunn
*-- Author :
      SUBROUTINE HREBIN(ID,X,Y,DX,DY,NNR,IFIRST,ILAST)
*.==========>
*.    CHANNELS OF ID BETWEEN IFIRST,ILAST ARE CUMULATED INTO
*.       NNR BINS
*.    X  ARRAY OF NEW ABCISSA VALUES
*.    Y  ARRAY OF CUMULATED CONTENTS
*.    DX ARRAY OF X ERRORS
*.    DY ARRAY OF Y ERRORS
*.
*.     If NNR<0 Errors in X are computed assuming a flat
*.     distribution in one bin
*.
*.     If IFIRST<0 do not normalize result in Y
*.
*.     Modifications
*.     =============
*.     JJB 16/1/95   Allow for variable width bins. Changed error calc.
*..=========> ( R.Brun )
#include "hbook/hcmail.inc"
      DIMENSION X(1),Y(1),DX(1),DY(1)
      LOGICAL LSIGX,NORMAL
*.___________________________________________
      NARG=8
      CALL NOARG(NARG)
      LSIGX = .FALSE.
      NN=NNR
*
*             GET HISTOGRAM SPECIFICATIONS
*
      CALL HGIVE(ID,CHMAIL,NCX,XLOW,XUP,NCY,YLOW,YUP,NWT,IAD)
*
      IF (NCX.LE.0)                              GO TO 99
      N=NCX
 
      IFIRS=1
      ILAS=NCX
      IF(NARG.GE.6) THEN
         IF (NN .LT. 0.) THEN
            LSIGX = .TRUE.
            NN = -NN
         ENDIF
         N=NN
      ENDIF
      IF(NARG.GE.7)IFIRS=IFIRST
      IF(NARG.GE.8)ILAS=ILAST
      IF(IFIRS.LT.0)THEN
         IFIRS=-IFIRS
         NORMAL=.FALSE.
      ELSE
         NORMAL=.TRUE.
      ENDIF
      IF(IFIRS.LE.0)IFIRS=1
      IF(IFIRS.GT.NCX)IFIRS=1
      IF(ILAS.GT.NCX)ILAS=NCX
      IF(ILAS.LT.IFIRS)ILAS=NCX
      IF(N.LE.0)N=NCX
      IF(N.GT.NCX)N=NCX
      NTOT   = ILAS - IFIRS + 1
      IF (N.GT.NTOT)N = NTOT
      NEW    = NTOT / N
*
*             FILL NEW BINS,ETC.
*
      DO 20 J = 1,N
         I1     = IFIRS + NEW * (J - 1)
         I2     = I1 + NEW - 1
         IF (I2.GT.ILAS)I2 = ILAS
*
         Y(J)   = 0.
         DY(J)  = 0.
         XMEAN  = 0.
         XRMS   = 0.
*
*             LOOP ON ALL CHANNELS GOING INTO NEW BIN "J"
*
         numnew = 0
         DO 10 I = I1,I2
            numnew = numnew + 1
c
c Get lower edge of bin I
c
            call hix(id,i,xlower)
c
c Get upper edge of bin I (= low edge of bin I+1)
c
            xupper = xup
            if(i+1.le.ncx) call hix(id,i+1,xupper)
c
c Calculate centre of bin
c
            xbin = 0.5*(xupper+xlower)
c
c Calculate width of new bin
c
            if(i.eq.i1) xstart = xlower
            if(i.eq.i2) binwid = xupper-xstart
c
            XMEAN  = XMEAN + XBIN
            Y(J)   = Y(J) + HI(ID,I)
            EY     = HIE(ID,I)
            DY(J)  = DY(J) + EY**2
   10    CONTINUE
*
         X(J)   = XMEAN / numnew
         IF (LSIGX) THEN
            DX(J) = binwid /sqrt(12.)
         ELSE
            DX(J) = 0.5*binwid
         ENDIF
         IF(NORMAL)THEN
            DY(J)  = SQRT(DY(J)) / numnew
            Y(J)   = Y(J) / numnew
         ELSE
            DY(J)  = SQRT(DY(J))
         ENDIF
   20 CONTINUE
*
   99 RETURN
      END
