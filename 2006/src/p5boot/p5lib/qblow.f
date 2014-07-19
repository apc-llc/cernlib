CDECK  ID>, QBLOW.
      SUBROUTINE QBLOW (LN)

C-    Blow the name of the bank at LN, name-word adr

      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCN/  IQLST,IQLNA,IQID,IQTY,IQLI,IQNL,IQNS,IQND
     +,              IQLNX,IQFOUL
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------

      IAND (IZV,IZW) = AND (IZV, IZW)
      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)


      IQLNA = LN

      M = LQ(IQLNA)
                       IQTY= IAND (M,63)
      M = ISHFTR(M,6)
                       IQLI= IAND (M,1)
      M = ISHFTR(M,1)
                       IQNL= IAND (M,63)
      M = ISHFTR(M,6)
                       IQNS= IAND (M,63)
      M = ISHFTR(M,6)
                       IQND= IAND (M,2047)

      IQLST = IQLNA + IQNL + 1
      NL = ISHFTR(IQ(IQLST),26)
      IF (NL.NE.IQNL)              GO TO 91
      IF (IQNL.LT.IQNS)            GO TO 91
      IF (IQLST+IQND.GE.NQMAX)     GO TO 91
      IF (IQTY.EQ.0)               GO TO 91
      IF (IQTY.GT.NBANKS)          GO TO 91
      IF (IQNL+IQND.EQ.0)          GO TO 91

      IQLNX = IQLST + IQND + 1
      IQID  = MMBANK(1,IQTY)
      RETURN
   91 CALL DQCLOBB (IQLNA,0)
      RETURN
      END
