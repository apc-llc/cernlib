CDECK  ID>, MQLIFT.
      SUBROUTINE MQLIFT (L,K,N,JN,JD)

C-    Satisfy bank request
C-    Linkage control:  if N.LE.0  connect at LQ(K-N)
C-                         N.EQ.1  connect at K (top level)
C-                         N.GE.2  no connection (stand alone)
C-    JN  is the bank name index,  JD  is the division number
C-    return in L the adr of the bank lifted
C.    started  8-oct-91

      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCT/  LQTA,LQTB,LQTE,LQMTB,LQMTE,LQMTH
     +,              JQDIVG,NQRESV(3),IQPART,NQFREE(3)
      COMMON /MQCL/  NQLST,NQLNA,NQID,NQTY,NQLI,NQNL,NQNS,NQND
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      DIMENSION    L(9), K(9), JN(9), JD(9)


      KK    = K(1)
      JBKIX = JN(1)
      JDIV  = JD(1)
      IFORW = 3 - JDIV
      IF ((JBKIX.LE.0).OR.(JBKIX.GT.NBANKS))  THEN
          CALL P_CRASH ('MQLIFT - faulty bank name index')
        ENDIF
      IF ((JDIV.LE.0).OR.(JDIV.GT.3))  THEN
          CALL P_CRASH ('MQLIFT - faulty division index')
        ENDIF

      NQTY = JBKIX
      NQID = MMBANK(1,JBKIX)
      NQLI = MMBANK(2,JBKIX)
      NQNL = MMBANK(3,JBKIX)
      NQNS = MMBANK(4,JBKIX)
      NQND = MMBANK(5,JBKIX)
      NT   = NQNL + NQND + 2
      IF ((NQNL.GE.64).OR.(NQNS.GT.NQNL).OR.(NQND.GE.2048))  THEN
          PRINT *, ' MQLIFT: NL/NS/ND=',NQNL,NQNS,NQND
          CALL P_CRASH ('MQLIFT - faulty bank parameters')
        ENDIF
      IF (IFORW.NE.0)  THEN
          NQLNA  = LQCEND(JDIV)
          NQRESV(1) = LQCSTA(JDIV+1) - NQLNA - NT
        ELSE
          NQLNA  = LQCSTA(JDIV) - NT
          NQRESV(1) = NQLNA - LQCEND(JDIV-1)
        ENDIF

      IF (NQRESV(1).LE.0)  THEN
          SLERRM = 'no space in control division x'
C-                  _:.=+=.: 1_:.=+=.: 2_:.=+=.: 3
          CALL CSETDI (JDIV,SLERRM,30,30)
          CALL NOSPACE (SLERRM(1:30))
        ENDIF

      IF (IFORW.NE.0)  THEN
          LQCEND(JDIV) = LQCEND(JDIV) + NT
        ELSE
          LQCSTA(JDIV) = LQCSTA(JDIV) - NT
        ENDIF

      NQLST = NQLNA + NQNL + 1
      CALL VZERO (LQ(NQLNA),NT)
      CALL QLUMP
      IF (N.LE.0)  THEN
          KKN = KK + N
          LNX = LQ(KKN)
          LQ(KKN) = NQLST
        ELSEIF (N.EQ.1)  THEN
          LNX = KK
          K(1) = NQLST
        ELSE
          LNX = 0
        ENDIF
      IF (NQNS.GT.0)  LQ(NQLST-1) = LNX
      L(1) = NQLST
      RETURN
      END
