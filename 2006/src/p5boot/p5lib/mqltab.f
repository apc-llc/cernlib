CDECK  ID>, MQLTAB.
      SUBROUTINE MQLTAB

C-    Garbage collector - relocation table building, line slot numbers
C-    run through all banks of the linear garbage structure, which
C-    at this point contains only banks with text pointers
C.    started  8-oct-91

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCT/  LQTA,LQTB,LQTE,LQMTB,LQMTE,LQMTH
     +,              JQDIVG,NQRESV(3),IQPART,NQFREE(3)
      COMMON /MQCN/  IQLST,IQLNA,IQID,IQTY,IQLI,IQNL,IQNS,IQND
     +,              IQLNX,IQFOUL
      PARAMETER      (IQDROP=31, IQMARK=30, KMDROP=1073741824)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------

      IAND (IZV,IZW) = AND (IZV, IZW)


C----              Decide where to put the table

      CALL MQGETT
      LIMIT = LQTE - 8
      LQTB  = LQTA + 3
      LQTE  = LQTA

      NLFREE = 0

      JSLL = 1
      LNX  = LQGARB

C------            Do all banks

   22 LS  = LNX
      IF (LS.EQ.0)                 GO TO 37
      LNX  = LQ(LS-1)
      JSLA = IQ(LS+1)
      JSLE = JSLA + IQ(LS+2)
      IF (JSLA.LE.JSLL)  THEN
          JSLL = JSLE
          GO TO 22
        ENDIF

C--       start of new dead region

      IF (LQTE.GE.LIMIT)           GO TO 31
      LQ(LQTE)   = JSLL
      LQ(LQTE+1) = JSLA
      JSLL = JSLE
      LQTE = LQTE + 3
      GO TO 22

C------            End of table

C--       Not enough table space, partial collection

   31 LQGARB = LS

   34 CALL QNAME (LS)
      LQ(IQLNA) = IAND (LQ(IQLNA), NOT(KMDROP))
      LS = LQ(LS-1)
      IF (LS.NE.0)                 GO TO 34
      LS = LQGARB

C--       Normal end

   37 LQGARB = LS
      LQ(LQTE) = JSLL
      IF (JSLL.NE.LQLEND(1))  THEN
          LQ(LQTE+1) = LQLEND(1)
          LQTE = LQTE + 3
          LQ(LQTE) = LQLEND(1)
        ENDIF
      LQ(LQTA+2) = 0
      LQ(LQTE+1) = 0

C--                compute relocation constants

      DO 44 JTB=LQTB,LQTE,3
      NLFREE = NLFREE + LQ(JTB) - LQ(JTB-2)
   44 LQ(JTB+2) = -NLFREE

      NQRESV(2) = NQRESV(2) + NLFREE
      NQFREE(2) = NLFREE
      RETURN
      END
