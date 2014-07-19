*
* $Id: plotisto.F,v 1.2 1996/08/30 15:19:17 cremel Exp $
*
* $Log: plotisto.F,v $
* Revision 1.2  1996/08/30 15:19:17  cremel
* Bug correction: "rebin" in the HISTO Style panel of Paw++.
*
* Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 02/09/93  19.43.09  by  Alfred Nathaniel
*-- Author :    O.Couet   03/09/92
      SUBROUTINE PLOT_HISTO(CHIDD,CHOPT,ICX1,ICX2,ICY1,ICY2)
#include "paw/pawcom.inc"
*
      CHARACTER*(*) CHIDD,CHOPT
*
      CHID=CHIDD
      CALL HGETID(CHID)
      CALL PAPLOT(ID,CHOPT,CHCASE,NUM,1,ICX1,ICX2,ICY1,ICY2)
      CALL IGTERM
*
      END

      SUBROUTINE REBIN(ID,IFIRST,ILAST,NG,CHDIR,CHOPT)
#include "hbook/hcbook.inc"
*
*     ID     = histogram to be rebin
*     NG     = number of bins in the rebined histo for one bin in ID
*     IFIRST = first bin
*     ILAST  = last bin
*
      COMMON/PCBUFF/PAWBUF(2004)
      DIMENSION X(500),Y(500),EX(500),EY(500)
      EQUIVALENCE (X(1),PAWBUF(1))
      EQUIVALENCE (Y(1),PAWBUF(501))
      EQUIVALENCE (EX(1),PAWBUF(1001))
      EQUIVALENCE (EY(1),PAWBUF(1501))
      CHARACTER*(*) CHDIR,CHOPT
      CHARACTER*80 CHTIT,CHTEMP
      LOGICAL HEXIST
*
*     Rebin
*
      IF(.NOT.HEXIST(ID))RETURN
      ILEN1=LEN(CHDIR)
      CHTEMP=CHDIR(1:ILEN1)
      CHTEMP(ILEN1+1:ILEN1+1)='/'
      CALL IZITOC(ID,CHTEMP(ILEN1+2:))
      CALL HGETID(CHTEMP)
      CALL HGIVE(ID,CHTIT,NX,XMI,XMA,NY,YMI,YMA,NWT,LOC)
      N=(ILAST-IFIRST+1)/NG
      IF(N.GT.500)THEN
         PRINT*, 'Too many channels for rebinning (>500)'
         RETURN
      ENDIF
      NENTRY = IQ(LCONT+KNOENT)
      CALL HREBIN(ID,X,Y,EX,EY,N,IFIRST,ILAST)
*
*     Create the new rebined histo ID
*
  20  CONTINUE
      IDR=1000001
      IF(HEXIST(IDR))CALL HDELET(IDR)
C**   CALL HBOOK1(IDR,CHTIT,N,X(1),X(N),0.)
      CALL HBOOK1(IDR,CHTIT,N,X(1)-EX(1),X(N)+EX(N),0.)
      CALL HPAK(IDR,Y)
      IQ(LCONT+KNOENT) = NENTRY
      CALL PAPLOT(IDR,CHOPT,' ',0,1,1,N,0,0)
      CALL IGTERM
      NG=N
*
      END
