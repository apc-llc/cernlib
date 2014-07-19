*
* $Id: cdvwpr.F,v 1.1.1.1 1996/02/28 16:24:53 mclareni Exp $
*
* $Log: cdvwpr.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDVWPR (PATHN, IDATM, NI, CHTI, IOTI, ICONI, KEYI,
     +                                 NO, CHTO, IOTO, ICONO, NOBO, IRC)
*     ==================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDVWPR (PATHN, IDATM, NI*, CHTI*, IOTI*, ICONI, KEYI,   *
*                      NO*, CHTO*, IOTO*,ICONO*, NOBO*, IRC*)          *
*                                                                      *
*   Routine to Prepare a View from a Table for Presentation or to      *
*              Join to a another Table                                 *
*   The values of the Keys to be presented/joined can be found at      *
*                 IQ(LVIWCX+1,..,NO,NO+1,...,NO*NOBO)                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Pathname of the Table                                   *
*     IDATM    Packed Date and Time (or Run #) at which the Table is   *
*              valid                                                   *
*     NI       Number of Keys to search                                *
*     CHTI     Name of Keys to search                                  *
*     IOTI     Type of Keys to search                                  *
*     ICONI    Number of concatenated keys in search                   *
*     KEYI     Value of the Keys to search                             *
*     NOBO     Number of Objects to be presented                       *
*     NO       Number of Keys to be Presented                          *
*     CHTO     Name of Keys to be presented                            *
*     IOTO     Type of Keys to be presented                            *
*     ICONO    Number of concatenated to be presented                  *
*     NOBO     Number of Objects to be presented                       *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDVIEW                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =101 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/cviews.inc"
#include "hepdb/cxlink.inc"
      DIMENSION       IOTI(9), IOTO(9), KEYI(9), ICONI(9), ICONO(9)
      DIMENSION       IFLAG(100), IDATM(9)
      CHARACTER       PATHN*(*), CHTI(*)*8, CHTO(*)*8
      CHARACTER       KYNAM*8, KYVAL*100, BLANK*20, KY10*10
      DATA            BLANK /' '/
*
*     ------------------------------------------------------------------
*
*   ** Set the current directory
*
      CALL RZCDIR (PATHN, ' ')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 101
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          KYVAL  = PATHN
          NCH    = LENOCC (KYVAL)
          CALL CDPRNT (LPRTCD, '(/,'' CDVWPR : Illegal Path Name '//
     +         KYVAL(1:NCH)//''')', IARGCD, 0)
        ENDIF
#endif
        GO TO 999
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      CALL CDKYTG
*
*  ** Fill concatenation flag (if non-zero: number of concatenated keys
*
      CALL CDCONC (IOTYCK(1), NWKYCK, ICONCK(1), NSKPCK)
*
*  ** Get Key-Names and Key-Values to Search
*
      CALL VZERO (KEYSCV, NWKYCK)
      CALL VZERO (MASKCX, NWKYCK)
      CALL VZERO (IFLAG,  NWKYCK)
      IK = 0
   10 CONTINUE
      CALL KUPROC ('Key-Name to Search (if no more - |) ?', KYNAM, LKEY)
      IF (KYNAM.EQ.'|')     GO TO 20
      I = NSYSCK+1
   11 IF (I.GT.NWKYCK)      GO TO 13
        IF (KYNAM.EQ.CTAGCK(I).AND.IFLAG(I).EQ.0) THEN
          IFLAG(I)   = 1
          IKEY       = I
          IK         = IK + 1
          IFKYCV(IK) = I
          MASKCX(I)  = 1
          CHTI(IK)   = CTAGCK(I)
          IOTI(IK)   = IOTYCK(I)
          ICONI(IK)  = ICONCK(I)
          NREP       = 0
          IF (ICONI(IK).GT.0) THEN
            NREP = ICONI(IK)
            DO 12 J = 1, NREP
              I            = I + 1
              IFKYCV(IK+J) = I
              MASKCX(I)    = 1
              CHTI(IK+J)   = CTAGCK(I)
              IOTI(IK+J)   = IOTYCK(I)
              ICONI(IK+J)  = ICONCK(I)
              IFLAG(I)     = 1
   12       CONTINUE
          ENDIF
          GO TO 15
        ENDIF
        I = I + 1
        GO TO 11
   13 CONTINUE
      GO TO 10
   15 CONTINUE
*    *      Key-Vector for CDUSEDB
      CALL KUPROC ('Key-Value ?', KYVAL, LKEY)
      IF (IOTYCK(IKEY).EQ.5) THEN
*  *        Hollerith
        LCDAT = (NREP+1)*4
        CALL UCTOH (KYVAL(1:LCDAT), KEYSCV(IKEY), 4, LCDAT)
        KEYI(IK) = KEYSCV(IKEY)
        IF (NREP.GT.0) THEN
          DO 16 J = 1, NREP
            IK = IK + 1
            KEYI(IK) = KEYSCV(IKEY+J)
   16     CONTINUE
        ENDIF
      ELSE IF (IOTYCK(IKEY).EQ.2) THEN
*  *        Integer
        LEN  = MIN (10,LKEY)
        KY10 = BLANK(1:10-LEN)//KYVAL(1:LEN)
        READ (KY10(1:10), 1002) KEYSCV(IKEY)
        KEYI(IK) = KEYSCV(IKEY)
      ELSE IF (IOTYCK(IKEY).EQ.1) THEN
*  *        Bit-string
        LEN  = MIN (8,LKEY)
        KY10 = BLANK(1:8-LEN)//KYVAL(1:LEN)
        READ (KY10(1:8), 1003) KEYSCV(IKEY)
        KEYI(IK) = KEYSCV(IKEY)
      ENDIF
      GO TO 10
   20 CONTINUE
      NI = IK
*
*  ** Find the Objects Satisfying the Search
*
      CALL CDUSEDB (PATHN, LKVWCX(1), IDATM, MASKCX, KEYSCV, 'VS', IRC)
      IF (IRC.NE.0)         GO TO 100
*
*  ** Get the Key-Names to Present
*
      CALL VZERO (JFKYCV, NWKYCK)
      CALL VZERO (IFLAG, NWKYCK)
      IP = 1
      ICONO(I) = 0
      CHTO(1)  = CTAGCK(1)
      IOTO(1)  = IOTYCK(1)
      JFKYCV(1) = 1
   25 CALL KUPROC ('Key-Name to Present (if no more: |) ?', KYNAM, LKEY)
      IF (KYNAM.EQ.'|')     GO TO 30
      I = NSYSCK+1
   26 IF (I.GT.NWKYCK)      GO TO 28
        IF (KYNAM.EQ.CTAGCK(I).AND.IFLAG(I).EQ.0) THEN
          IFLAG(I) = 1
          IP = IP + 1
          CHTO(IP)  = CTAGCK(I)
          IOTO(IP)  = IOTYCK(I)
          ICONO(IP) = ICONCK(I)
          JFKYCV(IP) = I
          IF (ICONO(IP).LE.0) THEN
            GO TO 25
          ELSE
            NREP = ICONO(IP)
            DO 27 J = 1,NREP
              IP = IP + 1
              I = I + 1
              IFLAG(I) = 1
              CHTO(IP)  = CTAGCK(I)
              IOTO(IP)  = IOTYCK(I)
              ICONO(IP) = ICONCK(I)
              JFKYCV(IP) = I
   27       CONTINUE
          ENDIF
          GO TO 25
        ENDIF
        I = I + 1
        GO TO 26
   28 CONTINUE
      GO TO 25
   30 CONTINUE
*
*  ** Lift Bank for Output
*
      NO = IP
      IF (NO.LE.0)          GO TO 100
      NOBO = NZBANK (IDIVCD, LKVWCX(1))
      IF (NOBO.LE.0)        GO TO 100
      NDAT = NO *  NOBO
      CALL CDBANK (IDIVCD, LVIWCX, LVIWCX, 2, 'VIEW', 0, 0, NDAT, 2, 0,
     +             IRC)
      IF (IRC.NE.0)         GO TO 100
*
*  ** Fill-Up 'VIEW' Bank and free LKVWCX
*
      IPOIN = LVIWCX
      NKOBJ = NZBANK (IDIVCD, LKVWCX(1))
      IF (NKOBJ.GT.0) THEN
        LFRSCX = LKVWCX(1)
        DO 40 K = 1, NKOBJ
          IF (LFRSCX.GT.0) THEN
            DO 35 J = 1, NO
              IPOIN = IPOIN + 1
              IQ(KOFUCD+IPOIN) = IQ(KOFUCD+LFRSCX+JFKYCV(J))
   35       CONTINUE
          ENDIF
          LFRSCX = LQ(KOFUCD+LFRSCX)
   40   CONTINUE
      ENDIF
  100 CALL MZDROP (IDIVCD, LKVWCX(1), 'L')
*
 1001 FORMAT (I2)
 1002 FORMAT (I10)
 1003 FORMAT (Z8)
*                                                             END CDVWPR
  999 END
