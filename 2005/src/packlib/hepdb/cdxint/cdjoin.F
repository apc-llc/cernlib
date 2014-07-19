*
* $Id: cdjoin.F,v 1.1.1.1 1996/02/28 16:24:50 mclareni Exp $
*
* $Log: cdjoin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:50  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDJOIN (PATHN, IDATM, NI, NOBI, CHTI, IOTI, ICONI,
     +                      KEYI, NO, NOBO, CHTO, IOTO, ICONO, IRC)
*     =============================================================
*
************************************************************************
*                                                                      *
*        SUBR.CDJOIN (PATHN, IDATM, NI, NOBI, CHTI, IOTI, ICONI,       *
*                     KEYI, NO*, NOBO*, CHTO*, IOTO*, ICONO, IRC*)     *
*                                                                      *
*   Routine to Join a Table of Name PATHN                              *
*   The values of the Keys to be presented can be found at             *
*                 IQ(LJOICX+1,..,NO,NO+1,...,NO*NOBO)                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Pathname of the Table                                   *
*     IDATM    Packed Date and Time (or Run #) at which the table is   *
*              valid                                                   *
*     NI       Number of Coloumns to join                              *
*     NOBI     Number of Rows (Data) to join                           *
*     CHTI     Name of Keys to which to join                           *
*     IOTI     Type of Keys to which to join                           *
*     ICONI    Number of concatenated keys to join                     *
*     KEYI     Value of Keys to which to join                          *
*     NO       Number of Keys to be Presented                          *
*     NOBO     Number of Objects to be presented                       *
*     CHTO     Name of Keys used for Presentation                      *
*     IOTO     Type of Keys used for Presentation                      *
*     ICONO    Number of concatenated keys to present                  *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDVIEW                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/cviews.inc"
#include "hepdb/cxlink.inc"
      DIMENSION       IOTI(NI), IOTO(9), KEYI(NI, NOBI)
      DIMENSION       ICONI(9), ICONO(9), IFLAG(100), IDATM(9)
      CHARACTER       PATHN*(*), CHTI(NI)*8, CHTO(*)*8, CHTG(100)*8
      CHARACTER       KYNAM*8, KYVAL*20, BLANK*20
      CHARACTER       KY10*10, CHPRO*50
      DATA            BLANK /' '/
*
*     ------------------------------------------------------------------
*
* *** Set the current directory
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)         GO TO 999
      CALL CDKYTG
*
*  ** Fill concatenation flag (if non-zero: number of concatenated keys
*
      CALL CDCONC (IOTYCK(1), NWKYCK, ICONCK(1), NSKPCK)
*
*  ** Get the Coloumn Names in the Table to Join
*
      CALL VZERO (IFKYCV, NWKYCK)
      CALL VZERO (IFLAG, NWKYCK)
      CALL VZERO (MASKCX, NWKYCK)
      I  = 1
    1 I  = I + 1
      IF (I.GT.NI)          GO TO 7
    2 I1 = I-1
      CHPRO = 'Give name for '//CHTI(I)
      CALL KUPROC (CHPRO(1:23), CHTG(I1), LCHT)
      IOTYCV(I1) = IOTI(I)
      DO 3 J = NSYSCK+1, NWKYCK
        IF (CHTG(I1).EQ.CTAGCK(J)) THEN
          JST        = J
          IFKYCV(I1) = J
          MASKCX(J)  = 1
          GO TO 5
        ENDIF
    3 CONTINUE
      GO TO 2
    5 CONTINUE
      IF (ICONI(I).EQ.0)    GO TO 1
      NREP = ICONI(I)
      DO 6 J = 1, NREP
        I          = I + 1
        K          = JST + J
        I1         = I - 1
        IFKYCV(I1) = K
        IFLAG(K)   = 1
        MASKCX(K)  = 1
        IOTYCV(I1) = IOTYCK(K)
        CHTG(I1)   = CTAGCK(K)
    6 CONTINUE
      GO TO 1
    7 CONTINUE
*
*  ** Get additional Key-Names and Key-Values to Search
*
      CALL VZERO (KEYSCV, NWKYCK)
      IK = NI-1
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
        CHTG(IK)   = CTAGCK(I)
        IOTYCV(IK) = IOTYCK(I)
        ICONI(IK)  = ICONCK(I)
        NREP       = 0
        IF (ICONI(IK).GT.0) THEN
          NREP = ICONI(IK)
          DO 12 J = 1, NREP
            I = I + 1
            IFKYCV(IK+J) = I
            MASKCX(I)    = 1
            CHTG(IK+J)   = CTAGCK(I)
            IOTYCV(IK+J) = IOTYCK(I)
            ICONI(IK+J)  = ICONCK(I)
            IFLAG(I)     = 1
   12     CONTINUE
        ENDIF
        GO TO 15
      ENDIF
      I = I + 1
      GO TO 11
   13 CONTINUE
      GO TO 10
   15 CONTINUE
      CALL KUPROC ('Key-Value ?', KYVAL, LKEY)
      IF (IOTYCK(IKEY).EQ.5) THEN
*  *        Hollerith
        LCDAT = (NREP+1)*4
        CALL UCTOH (KYVAL(1:LCDAT), KEYSCV(IKEY), 4, LCDAT)
        IK = IK + NREP
      ELSE IF (IOTYCK(IKEY).EQ.2) THEN
*  *        Integer
        LEN  = MIN (10,LKEY)
        KY10 = BLANK(1:10-LEN)//KYVAL(1:LEN)
        READ (KY10(1:10), 1002) KEYSCV(IKEY)
      ELSE IF (IOTYCK(IKEY).EQ.1) THEN
*  *        Bit-string
        LEN  = MIN (8,LKEY)
        KY10 = BLANK(1:8-LEN)//KYVAL(1:LEN)
        READ (KY10(1:8), 1003) KEYSCV(IKEY)
      ENDIF
      GO TO 10
   20 CONTINUE
*
*  ** Loop on Input Objects
*
      LKJNCX(1) = 0
      LKJNCX(2) = 0
      DO 23 I = 1, NOBI
*    *      Key-Vector for CDUSEDB
        DO 22 J = 2, NI
          INDX = IFKYCV(J-1)
*    *  The following equality can be later replaced by other relation:
          KEYSCV(INDX) = KEYI(J,I)
   22   CONTINUE
*
        IF (I.EQ.1) THEN
          CALL CDUSEDB (PATHN, LKJNCX(1),IDATM, MASKCX,KEYSCV,'VS', IRC)
        ELSE
          CALL CDUSEDB (PATHN, LKJNCX(2),IDATM, MASKCX,KEYSCV,'VS', IRC)
          IF (LKJNCX(1).EQ.0) LKJNCX(1) = LKJNCX(2)
        ENDIF
   23 CONTINUE
*
*  ** Get the Key-Names to Present
*
      CALL VZERO (JFKYCV, NWKYCK)
      CALL VZERO (IFLAG, NWKYCK)
      IP        = 1
      ICONO(I)  = 0
      CHTO(1)   = CTAGCK(IDHKSN)
      IOTO(1)   = IOTYCK(IDHKSN)
      JFKYCV(1) = IDHKSN
   25 CALL KUPROC ('Key-Name to Present (if no more: |) ?', KYNAM, LKEY)
      IF (KYNAM.EQ.'|')     GO TO 30
      I = NSYSCK+1
   26 IF (I.GT.NWKYCK)      GO TO 28
      IF (KYNAM.EQ.CTAGCK(I).AND.IFLAG(I).EQ.0) THEN
        IFLAG(I)   = 1
        IP         = IP + 1
        CHTO(IP)   = CTAGCK(I)
        IOTO(IP)   = IOTYCK(I)
        ICONO(IP)  = ICONCK(I)
        JFKYCV(IP) = I
        IF (ICONO(IP).LE.0) THEN
          GO TO 25
        ELSE
          NREP = ICONO(IP)
          DO 27 J = 1,NREP
            IP         = IP + 1
            I          = I + 1
            IFLAG(I)   = 1
            CHTO(IP)   = CTAGCK(I)
            IOTO(IP)   = IOTYCK(I)
            ICONO(IP)  = ICONCK(I)
            JFKYCV(IP) = I
   27     CONTINUE
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
      IF (NO.LE.0)          GO TO 999
      NOBO =  NZBANK (IDIVCD, LKJNCX(1))
      IF (NOBO.LE.0)        GO TO 999
      NDAT = NO *  NOBO
      CALL CDBANK (IDIVCD, LJOICX, LJOICX, 2, 'JOIN', 0, 0, NDAT, 2, 0,
     +             IRC)
      IF (IRC.NE.0)   GO TO 999
*
*  ** Fill-Up 'JOIN' Bank and free LKJNCX
*
      IPOIN = LJOICX
      NKOBJ = NZBANK (IDIVCD, LKJNCX(1))
      IF (NKOBJ.GT.0) THEN
        LFRSCX = LKJNCX(1)
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
      CALL MZDROP (IDIVCD, LKJNCX(1), 'L')
*
 1001 FORMAT (I2)
 1002 FORMAT (I10)
 1003 FORMAT (Z8)
*                                                             END CDJOIN
  999 END
