CDECK  ID>, ARRTM.
      SUBROUTINE ARRTM

C-    Read cradle lines from the on-line user
C.    started 10-feb-92

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
      COMMON /ARRCOM/IN_LUN, IN_FD,  IN_FIO, IN_EOF
     +,              IN_DOX, IN_DO1, IN_DO2
C--------------    End CDE              --------------------------------


      JSLGO = IQ(LQHOLD+1)

      JSLU = LQLEND(2)
      JTXU = MLIAD(JSLU)
      JSLE = JSLU + IN_DO1
      JTXE = JTXU + IN_DO2

   24 JSLF  = JSLU
      IF (JSLU.GE.JSLE)            GO TO 37
      IF (JTXU.GE.JTXE)            GO TO 37
      CALL TMPRO (' ny> ')
      CALL TMREAD (512,TEXT(JTXU),NTXR,IN_EOF)

      IF (IN_EOF.NE.0)              GO TO 31

      JTXF  = JTXU
      JTXU  = JTXU + NTXR
      TEXT(JTXU) = CHAR(NEWLN)
      JTXU = JTXU + 1
      JSLU = JSLU + 1
      MLIAD(JSLU) = JTXU

      IF (NTXR.LT.4)               GO TO 24
      IF (TEXT(JTXF).NE.'+')       GO TO 24

      CALL NEXTSI (JSLF,JDKNEX,INCRAD)

      IF (JDKNEX.EQ.0)             GO TO 24
      IF (JSLF.NE.JSLGO)           GO TO 37
      IF (JDKNEX.LT.0)             GO TO 37
      JDKTYP = JDKNEX
      JDKNEX = 0
      JSLF   = JSLU
      GO TO 37

C----     EoF

   31 IF (IN_EOF.GT.0)   CALL P_KILLM ('terminal read fails')

      IQ(LARRV+8) = -1
      JDKNEX = 6
      IF (JSLU.EQ.JSLGO)  JDKTYP = 6

C--       Done

   37 IQ(LARRV+12) = IQ(LARRV+12) + (JSLU-LQLEND(2))
      LQLEND(2) = JSLU
      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)
      IQ(LQHOLD+2) = JSLU - JSLGO
      IQ(LQHOLD+3) = JSLF - JSLGO
      RETURN
      END
