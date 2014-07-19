CDECK  ID>, NA_REIN.
      SUBROUTINE NA_REIN

C-    Re-initialize the name stack
C-    for c/l  +NAMES, length, slots, text
C.    started  1-oct-91

      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
      COMMON /NAMEC/ NA_OCC, NA_CUT, NA_MXSL, NA_MXTX
C--------------    End CDE              --------------------------------


      IF (NCCPN.EQ.0)              RETURN

      NLE = MCCPAR(JCCPN+1)
      NSL = 0
      NTX = 0
      IF (NCCPN.GE.2)  NSL = MCCPAR(JCCPN+4)
      IF (NCCPN.GE.3)  NTX = MCCPAR(JCCPN+7)

      IF (NLE.GT.0)  NA_CUT  = NLE
      IF (NSL.GT.0)  NA_MXSL = NSL

      NA_CUT  = MAX (NA_CUT, 8)
      NA_CUT  = MIN (NA_CUT, 32)
      NA_MXSL = MAX (NA_MXSL,200)

      IF (NTX.GT.0)  THEN
          NA_MXTX = NTX
        ELSE
          NA_MXTX = 8 * NA_MXSL
        ENDIF
      NA_MXTX = MAX (NA_MXTX,1200)

      JSLN = LQLEND(5) - NA_MXSL
      JTXN = NSIZETX - NA_MXTX

      JSLO = LQLSTA(5)
      JTXO = MLIAD(JSLO)
      NSL  = NA_OCC +1 - JSLO
      NTX  = MLIAD(NA_OCC+1) - JTXO
      NSH  = JTXN - JTXO

      CALL UCOPY2 (MLIAD(JSLO),MLIAD(JSLN), NSL+1)

      IF (NSH.EQ.0)                GO TO 29

      IF (NSH.LT.0)  THEN
          CALL CCOPYL (TEXT(JTXO),TEXT(JTXN),NTX)
        ELSE
          CALL CCOPYR (TEXT(JTXO),TEXT(JTXN),NTX)
        ENDIF

      DO 24  J=JSLN,JSLN+NSL
   24 MLIAD(J) = MLIAD(J) + NSH

   29 LQLSTA(5)  = JSLN
      LQLEND(4)  = JSLN
      LQLSTA(4)  = JSLN
      NA_OCC = JSLN + NSL -1

      RETURN
      END
