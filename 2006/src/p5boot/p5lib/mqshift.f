CDECK  ID>, MQSHIFT.
      SUBROUTINE MQSHIFT

C-    Shift the TEXT material in division 2 (PAM) supported
C-    by the banks of control division 1 starting at LQHOLD
C-    from text division 1 to division 1
C-    Called just before reading a new PAM (or PAM instalment)
C.    started 14-oct-91

C-        LQHOLD   is the adr of the first bank created after
C-                 the last shift operation
C-                 it decribes the trailing lines in text division 2
C-                 to be maintained in division 2, if non-zero

      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      PARAMETER      (IQDROP=31, IQMARK=30, KMDROP=1073741824)
      PARAMETER (KQGARB=1,KQARRV=3,KQKEEP=4,KQPREP=5,KQMAIN=6,KQPAST=8)
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
      COMMON /MQCN/  IQLST,IQLNA,IQID,IQTY,IQLI,IQNL,IQNS,IQND
     +,              IQLNX,IQFOUL
C--------------    End CDE              --------------------------------

      IOR  (IZV,IZW) =  OR (IZV, IZW)



C----         Remove dead material if any

      IF (LQGARB.NE.0)  CALL MQGARB

C----         Shift new text material from text division 2 to 1

C--               is the HOLD bank the last bank in division 1 ?
C-                in which case there is no new text material

      CALL QNAME (LQHOLD)
      IF (IQLNX.EQ.LQCEND(1))  THEN
          IF (IQ(LQHOLD+2).EQ.0)  THEN
              JSLTO = LQLEND(1)
              GO TO 41
            ENDIF
        ENDIF

C--       create the new HOLD bank

      LSAVE  = LQHOLD
      CALL MQLIFT (LQHOLD,0,7,JBKHOL,1)

      IQ(LQHOLD+1) = IQ(LSAVE+1)
      IQ(LQHOLD+2) = IQ(LSAVE+2)
      IQ(LQHOLD+3) = IQ(LSAVE+3)

      IQ(LSAVE+1) = 0
      IQ(LSAVE+2) = 0

C----         Create the linear structure of GARB banks

      LQCEND(2) = LQCSTA(2)
      LQPREP   = 0
      KJOIN    = KQPREP

      CALL QNAME (LSAVE)
      LQ(IQLNA) = IOR (LQ(IQLNA),KMDROP)
      LNX = IQLNX

C--       do the next bank, does it have lines attached?

   22 IF (LNX.GE.LQCEND(1))       GO TO 31
      CALL QBLOW (LNX)
      LNX = IQLNX
      LST = IQLST
   24 IF (IQLI.EQ.0)               GO TO 22
      JSLFR = IQ(LST+1)
      IF (JSLFR.LT.LQLSTA(2))      GO TO 22
      IF (JSLFR.GE.LQLEND(2))      GO TO 22
      NSL = IQ(LST+2)
      IF (NSL.EQ.0)  THEN
          IQ(LST+1) = 0
          GO TO 22
        ENDIF

      CALL MQLIFT (LP,KJOIN,0,JBKGAR,2)
      KJOIN = LP - 1
      IQ(LP+1) = JSLFR
      IQ(LP+2) = LST
      GO TO 22

C----         Copy the material

   31 JSLTO = LQLEND(1)
      JTXTO = MLIAD(JSLTO)
      IF (LQPREP.EQ.0)             GO TO 41

      CALL QSORTI (1,KQPREP)

      LP = KQPREP + 1
   32 LP = LQ(LP-1)
      IF (LP.EQ.0)                 GO TO 41
      LST   = IQ(LP+2)
      JSLFR = IQ(LST+1)
      NSL   = IQ(LST+2)

      IQ(LST+1) = JSLTO

      JTXFR = MLIAD(JSLFR)
      NTX   = MLIAD(JSLFR+NSL) - JTXFR
      NTXRE = JTXTO - JTXFR

      DO 34  J=1,NSL
   34 MLIAD(JSLTO+J) = MLIAD(JSLFR+J) + NTXRE

      CALL CCOPYL (TEXT(JTXFR),TEXT(JTXTO),NTX)
      JTXTO = JTXTO + NTX
      JSLTO = JSLTO + NSL
      GO TO 32

C--       reset end of TEXT division 1 and division 2

   41 IF (IQ(LQHOLD+2).EQ.0)  THEN
          LQLEND(1) = JSLTO
          LQLSTA(2) = JSLTO
          LQLEND(2) = JSLTO
          IQ(LQHOLD+1) = JSLTO
        ELSE
          LQLEND(1) = IQ(LQHOLD+1)
          LQLSTA(2) = LQLEND(1)
          LQLEND(2) = LQLSTA(2) + IQ(LQHOLD+2)
        ENDIF
      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)
      LQCEND(2) = LQCSTA(2)
      RETURN
      END
