CDECK  ID>, TOGARB.
      SUBROUTINE TOGARB (KDO,IFLAG)

C-    Shunt the single bank connected at KDO (if IFLAG.EQ.0)
C-    or the linear structure at KDO (if IFLAG.NE.0)
C-    to the garbage structure
C.    started  5-dec-91

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
C--------------    End CDE              --------------------------------


      LDO = LQ(KDO)
      IF (LDO.EQ.0)                RETURN

C--       single bank

      IF (IFLAG.NE.0)              GO TO 21
      LQ(KDO) = LQ(LDO-1)
      IF (LDO.GE.LQCSTA(2))        RETURN

      LQ(LDO-1) = LQGARB
      LQGARB    = LDO
      RETURN

C--       linear structure

   21 KL = KDO
   22 LL = LQ(KL)
      IF (LL.GT.0)         THEN
          IF (LL.GE.LQCSTA(2))  THEN
              LQ(KL) = LQ(LL-1)
              GO TO 22
            ELSE
              KL = LL - 1
              GO TO 22
            ENDIF
        ENDIF

      LDO = LQ(KDO)
      IF (LDO.EQ.0)                RETURN

      LQ(KL) = LQGARB
      LQGARB = LDO

      LQ(KDO) = 0
      RETURN
      END
