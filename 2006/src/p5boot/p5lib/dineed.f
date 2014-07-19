CDECK  ID>, DINEED.
      SUBROUTINE DINEED (JFILE,LPAT)

C-    The patch pointed to by LPAT from file JFILE is needed in memory
C.    started 15-june-94

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
     +,         LEXP,LLPAST,LQPAST, LQUSER(4), LHASM,LRPAM,LPAM, LQINCL
     +,         LACRAD,LARRV, LPCRA,LDCRAB, LEXD,LDECO, LCRP,LCRD, LSERV
     +, INCRAD, IFLGAR, JANSW, IFMODIF, IFALTN
     +, JDKNEX,JDKTYP, JSLZER,NSLORG,JSLORG
     +, MOPTIO(34), MOPUPD, NCLASH, IFLMERG,IFLDISP, NSLFRE,NTXFRE
     +, NVGAP(4), NVGARB(6), NVIMAT(4), NVUTY(4),  LASTWK
     +,   INMEM(2),NSLTOT(2),NSLMAX(2),NTXMAX(2),JSLTTF(2), IFSTRUC
     +,   NOBJTT(3,2),NOBJMAT, MDELADD(2,2), LUNCRA,NLSENT,   LASTDI
C--------------    End CDE              --------------------------------


      IF (IQ(LPAT+1).NE.0)         RETURN
      JFI  = JFILE
      JDIV = 3 - JFI
      JDVN = JDIV + 1

      JSLLIM = LQLSTA(JDVN)
      JTXLIM = MLIAD(JSLLIM)

      NSLTAK = IQ(LPAT+2)
      NTXTAK = IQ(LPAT+3)
      IFLOOP = 0

      IF (NSLTAK.LE.0)  THEN
          IQ(LPAT+1) = 1
          RETURN
        ENDIF

   22 JSLGO = LQLEND(JDIV)
      JTXGO = MLIAD(JSLGO)

      IF     (JSLGO+NSLTAK+2 .LT.JSLLIM) THEN
          IF (JTXGO+NTXTAK+2 .LT.JTXLIM) GO TO 24
        ENDIF

      IF (IFLOOP.NE.0)               GO TO 91
   23 IFLOOP = 7

      CALL DICLFI (LQUSER(JFI))
      LQLEND(JDIV) = LQLSTA(JDIV)
      GO TO 22

C--       read the patch into memory

   24 LARRV = LQARRV
      IF (JFI.EQ.2)  LARRV= LQ(LARRV-1)

      CALL DINEIN (JSLGO,LPAT)

C--       set the starting slot number of each deck

      IQ(LPAT+1) = JSLGO
      JSL = JSLGO
      LDK = LQ(LPAT-4)
   27 IQ(LDK+1) = JSL
      JSL = JSL + IQ(LDK+2)
      LDK = LQ(LDK-1)
      IF (LDK.NE.0)          GO TO 27
      LQLEND(JDIV) = JSL
      JSLE = JSLGO + NSLTAK
      JTXE = JTXGO + NTXTAK
      IF (JSL.NE.JSLE)         CALL P_CRASH ('Trouble in DINEED')
      IF (MLIAD(JSL).NE.JTXE)  CALL P_CRASH ('Trouble in DINEED')
      RETURN

C--                trouble

   91 CALL P_KILL ('Crash DINEED - large patch too big')
      RETURN
      END
