*
* $Id: cdvect.F,v 1.1.1.1 1996/02/28 16:24:43 mclareni Exp $
*
* $Log: cdvect.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:43  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDVECT(CHPATH,IVECT,LVECT,LBANK,CHOPT,IRC)
 
      CHARACTER*(*) CHPATH
      DIMENSION     IVECT(LVECT)
 
      PARAMETER       (JBIAS=2)
      DIMENSION       LBD(9)
      CHARACTER*4     CHID
#include "hepdb/cdcblk.inc"
#include "hepdb/hdbopts.inc"
 
      IRC = 0
*
*     I/O characteristic
*
      IF(IOPTB.NE.0) IFORM = 1
      IF(IOPTI.NE.0) IFORM = 2
      IF(IOPTR.NE.0) IFORM = 3
      IF(IOPTD.NE.0) IFORM = 4
      IF(IOPTH.NE.0) IFORM = 5
*
*     Get or put?
*
      IF(IOPTG.EQ.0.AND.IOPTP.EQ.0) THEN
         IF(IDEBCD.GE.0) WRITE(LPRTCD,9001)
9001  FORMAT(' CDVECT. please specify one of options G or P')
         IRC = -1
         RETURN
      ENDIF
*
*     Get vector
*
      IF(IOPTG.NE.0)     THEN
 
         ND = IQ(KOFUCD+LBANK-1)
         IF(ND.GT.LVECT) IRC = 1
         CALL UCOPY(IQ(KOFUCD+LBANK+1),IVECT,MIN(ND,LVECT))
*
*     Put vector
*
      ELSEIF(IOPTP.NE.0) THEN
         IF (LBFXCD.NE.0) THEN
            CALL UHTOC (IQ(KOFUCD+LBFXCD-4), 4, CHID, 4)
            IF (CHID.EQ.'USER') CALL MZDROP (IDIVCD, LBFXCD, ' ')
         ENDIF
         NDATA = LVECT
         CALL CDBANK (IDIVCD, LBFXCD, LBFXCD, JBIAS, 'USER', 0, 0,
     +   NDATA, IFORM, -1, IRC)
         IF (IRC.NE.0) GO TO 999
         CALL UCOPY(IVECT,IQ(KOFUCD+LBFXCD+1),NDATA)
         LBANK  = LBFXCD
         LBFXCD = 0
      ENDIF
 
  999 CONTINUE
      END
