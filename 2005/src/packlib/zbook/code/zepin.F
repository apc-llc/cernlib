*
* $Id: zepin.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zepin.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEPIN (IZ,LUN,KEYS,NKEYS,IDCAT,IERR)
C
C     ******************************************************************
C     *                                                                *
C     *                                                                *
C     *       READS NEXT LOGICAL EVENT IDENTIFIED BY THE               *
C     *       LIST OF NKEY  KEYWORDS                                   *
C     *       THE CURRENT CATALOG IDENTIFIER IS RETURNED IN IDCAT      *
C     *       IF ZEPCAT WAS CALLED PRIOR TO ZEPIN THEN                 *
C     *       ZEPIN STARTS TO READ AT THE FIRST KEY                    *
C     *       UNLESS IT READS THE NEXT CATALOG TOO                     *
C     *                                                                *
C     *                                                                *
C     ******************************************************************
C
C--- Inserted by HG
      COMMON/ZCFORM/NLFORM,JD(60)
      DIMENSION IFORBF(50)
      EQUIVALENCE (IFORBF(1),JD(11))
C---
      DIMENSION IZ(1) , KEYS(1), IH(60)
      DIMENSION JL(10) , JM(10)
      DIMENSION MODSEP(8),NUM(40),LL(4)
C
      DATA MODSEP/11,12,13,13,13,12,13,13/
C
#include "zbook/wlength.inc"
      DATA IFIRST/0/
C
C     ------------------------------------------------------------------
C
C--- Inserted by HG
      NAMEL=0
      NWFORL=0
      NLFORM=10
      IF(IFIRST.EQ.0)  THEN
         IFIRST=1
         CALL ZLOCAL(IZ,NLFORM)
         CALL UCTOH('****',MISS,4,4)
         CALL UCTOH('    ',IBLANK,4,4)
         CALL UCTOH1(' 1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ*+-'
     +   ,NUM(1),40)
      ENDIF
C---
      NKEY   = NKEYS
      IERR   = 0
      IDS    = IZ(1)
      IZ(IDS+6)= 0
C
C     DEFINE LASOPD (LAST OPERATION DONE)
C
      JBUF   = IZ(IDS - 3)
      IF (JBUF.LE.0)                             GO TO 10
C
      NUNIT  = IZ(JBUF) - 2
      LBUF=1
      DO 5 I=1,NUNIT
      IF(IZ(JBUF+I).NE.LUN)GO TO 5
      LBUF=I
      GO TO 7
   5  CONTINUE
      CALL ZERROR(IZ,900,'ZEPIN ',IDS)
      GO TO 180
C
   7  JBUF   = IZ(JBUF - LBUF) + 3
      LASOPD = IZ(JBUF - 2)
C
      IF (LASOPD.EQ.0)                           GO TO 10
      IF (LASOPD.EQ.3)                           GO TO 10
C
      IDCAT  = IZ(JBUF - 1)
      GO TO 20
C
C     READ IN NEXT CATALOG HEADER
C
   10 CALL ZEPNXT (IZ,LUN,IDCAT,IERR)
      IF (IERR.NE.0.AND.IERR.NE.6)               GO TO 180
C
C     PERFORM SYSTEMATIC GARBAGE COLLECTION
C
  20  IF(IZ(IDS+5).LE.0)GO TO 50
      CALL ZGARB(IZ)
   50 CONTINUE
      IDS=IZ(1)
      IF(IZ(IDS+6).NE.0)GO TO 180
      JBUF=IZ(IDS-3)
      JBUF=IZ(JBUF-LBUF)+3
C
      CALL EPFHDR (LUN,7,IH,IZ(JBUF),IERR)
      IF (IERR.NE.0.AND.IERR.NE.6)               GO TO 180
C
   60 IF (IH(2).EQ.4)                            GO TO 170
      IF (IH(2).NE.2)                            GO TO 50
      IF (NKEY.LE.0)                             GO TO 50
C
C     KEY HEADER
C
      CALL EPFRD (LUN,13,NW,IH,IZ(JBUF),IERR)
      IF (IERR.NE.0)                             GO TO 180
      CALL ZFRIBM(IH,NW,2)
C
      KEY    = IH(1)
C
      DO 62 I=1,NKEY
      IF(KEY.EQ.KEYS(I))GO TO 63
  62  CONTINUE
      GO TO 50
C
  63  CALL ZEPLOC (IZ,KEY,NID,NMEMB,ICODE)
      IF (NID.EQ.0)                              GO TO 50
      IF (NMEMB.EQ.0)                            GO TO 50
C
      ICODE  = MOD (IH(2),10)
      MODEP  = MODSEP(ICODE + 1)
      NMB    = 1
C
   70 NST    = 1
      JD(1)  = NID + NMB
      JL(1)  = 2
      JM(1)  = 1
C
   80 CONTINUE
      JBUF=IZ(IDS-3)
      JBUF=IZ(JBUF-LBUF)+3
      CALL EPFHDR (LUN,60,IH,IZ(JBUF),IERR)
      IF (IERR.NE.0.AND.IERR.NE.6)               GO TO 180
      IF (IH(2).NE.5)                            GO TO 83
C
C     SET OF MISSING BANKS
C
      NMSS   = IH(4)
      IF (NST.NE.1)                              GO TO 82
C
      NMB    = NMB + NMSS
      IF (NMB.GT.NMEMB)                          GO TO 50
      GO TO 70
C
   82 JL(NST) = JL(NST) - NMSS
      IF (JL(NST).GT.0)                          GO TO 80
C
      NST    = NST - 1
      GO TO 80
C
   83 IF (IH(2).NE.3)                            GO TO 60
C
C     BANK HEADER
C
      NWH    = IH(3)
      NDATA     = IH(1) - IH(3)
C--- Inserted by HG
      NWFOR=0
      IF(NWH.LE.7)  GOTO 831
      NWFOR=IH(NWH)
      NWH=NWH-NWFOR-1
  831 CONTINUE
C---
      ISIT   = NWH - 2
      IF (ISIT.GT.2) ISIT = ISIT - 1
C
      GO TO (84,85,86,87), ISIT
C
   84 NLINKS = 0
      NAME=IBLANK
      NUMB=0
      GO TO 89
C
   85 NLINKS = IH(4)
      NAME=IBLANK
      NUMB=0
      GO TO 89
C
   86 NLINKS = 0
      NUMB   = IH(6)
      JJ1=MOD(IH(4),256)
      JJ2=MOD(IH(4)/256,256)
      JJ3=MOD(IH(5),256)
      JJ4=MOD(IH(5)/256,256)
      LL(1)=NUM(JJ1)
      LL(2)=NUM(JJ2)
      LL(3)=NUM(JJ3)
      LL(4)=NUM(JJ4)
      CALL UBUNCH(LL,NAME,4)
      GO TO 89
C
   87 NLINKS = IH(4)
      NUMB   = IH(7)
      JJ1=MOD(IH(5),256)
      JJ2=MOD(IH(5)/256,256)
      JJ3=MOD(IH(6),256)
      JJ4=MOD(IH(6)/256,256)
      LL(1)=NUM(JJ1)
      LL(2)=NUM(JJ2)
      LL(3)=NUM(JJ3)
      LL(4)=NUM(JJ4)
      CALL UBUNCH(LL,NAME,4)
C
      IF (NAME.EQ.MISS)                          GO TO 130
C
   89 IF (ICODE.EQ.1)                            GO TO 100
      IF (ICODE.EQ.5)                            GO TO 100
      IF (ICODE.EQ.0)                            GO TO 90
C
      NDATA     = NDATA / 2
      GO TO 100
C
   90 NDATA     = (NDATA*16-1) / NBIT + 1
C
  100 J      = JD(NST) - JM(NST) + JL(NST) - 1
      IZ(IDS+17) = 1
      CALL ZBOOKN (IZ,IZ(J),NDATA,NLINKS,NAME,NUMB)
      IF(IZ(IDS+6).EQ.200)IZ(IDS+6)=0
      IF (IZ(IDS + 6).NE.0)                      GO TO 180
C
      J      = JD(NST) - JM(NST) + JL(NST) - 1
      K      = IZ(J)
      CALL EPFRD (LUN,MODEP,NW,IZ(K+1),IZ(JBUF),IERR)
      IF (IERR.NE.0)                             GO TO 180
C--- Inserted by HG
      IF(NWFOR.NE.0)  THEN
C--- BANK HAS A FORMAT - STORE IT AWAY
      CALL ZPTFOR(IZ,NAME,NWFOR,IH(NWH+1))
      J      = JD(NST) - JM(NST) + JL(NST) - 1
      K      = IZ(J)
      NWFORL=NWFOR
      CALL UCOPY(IH(NWH+1),IFORBF,NWFOR)
      ELSEIF(NWFORL.EQ.0.OR.NAME.NE.NAMEL)  THEN
      NWFORL=0
      GOTO 120
      ENDIF
      IF(NDATA.GT.0.AND.ICODE.NE.0)
     + CALL ZFCONV(IFORBF,NWFORL,IZ(K+1),NDATA,0)
      GOTO 130
  120 CONTINUE
C---
      IF (ICODE.EQ.5.OR.ICODE.EQ.0)              GO TO 130
C
      J      = JD(NST) - JM(NST) + JL(NST) - 1
      K      = IZ(J)
      CALL ZFRIBM (IZ(K+1),NDATA,ICODE)
C
  130 NAMEL=NAME
      JL(NST) = JL(NST) - 1
      IF (JL(NST).NE.0)                          GO TO 140
C
      NST    = NST - 1
C
  140 IF (NLINKS.NE.0)                           GO TO 150
      IF (NST.NE.1)                              GO TO 80
      GO TO 160
C
  150 NST    = NST + 1
      JD(NST) = K
      JL(NST) = NLINKS
      JM(NST) = NLINKS
      GO TO 80
C
  160 NMB    = NMB + 1
      IF (NMB.GT.NMEMB)                          GO TO 50
      GO TO 70
C
  170 JF     = JBUF - 2
      IZ(JF) = 3
C
  180 IERR   = IERR + IZ(IDS + 6)
C
      NLFORM=0
      RETURN
      END
