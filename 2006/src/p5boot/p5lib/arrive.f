CDECK  ID>, ARRIVE.
      SUBROUTINE ARRIVE (MODEIN)

C-    Read input material to memory
C-    MODE = 0 : read the blank deck of the cradle
C-           1 : make sure the beginning of the next deck is in memory
C-           2 : make sure the complete deck is in memory
C-           3 : read as much as you can
C.    started 11-mar-92

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER  (MCCNIL=1,  MCCKIL=2,  MCCINC=3,  MCCCDE=4,  MCCSEQ=5,
     + MCCXSQ=6,  MCCTRU=7,  MCCFAL=8,  MCCELS=9,  MCCEND=10,
     +            MCCSEL=11, MCCSES=12, MCCFAU=13, MCCSKI=14,
     +            MCCKEE=15, MCCDEL=16, MCCREP=17, MCCADB=18, MCCADD=19,
     + MCCUSE=20, MCCXDI=21, MCCDIV=22, MCCLIS=23, MCCEXE=24, MCCIMI=25,
     + MCCASM=26, MCCUPD=27, MCCNAM=28, MCCGAP=29, MCCMOR=30, MCCONL=31,
     + MCCFOR=32, MCCSUS=33, MCCOPT=34, MCCOP2=35, MCCSHO=36, MCCPAM=37,
     + MCCQUI=38, MCCEOD=39, MCCDEC=40, MCCPAT=41, MCCTIT=42)
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
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
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
C--------------    End CDE              --------------------------------


      MODE = MODEIN

C--       collect garbage before reading next instalment from PAM

      IF (INCRAD.EQ.0)  THEN
          IF (LQLEND(2).NE.LQLSTA(2))    GO TO 25
        ENDIF
      IF (MODE.NE.0)               GO TO 21

C----          Start reading the blank deck of the cradle

      LARRV = LACRAD
      IQ(LARRV+1) = IQREAD
      IQ(LARRV+2) = IQRFD
      IQ(LARRV+6) = IQRSIZ
      JDKTYP = 0

      IF (IQRFD .NE.0)             GO TO 21
      IF (IQRSIZ.NE.0)             GO TO 21
      IQ(LARRV+7) = -2
      IF (IQRTTY.NE.0)  IQ(LARRV+7)= -1

C----          Check enough space

   21 NWFREE = LQCSTA(3) - LQCEND(2)
      JSLOCC = LQLSTA(4)
      JSLFRE = LQLEND(2)
      NSLFRE = JSLOCC - JSLFRE - 8
      NTXFRE = MLIAD(JSLOCC) - MLIAD(JSLFRE)  - 400

      NSLRSV = NVGAP(2)
      NTXRSV = NSLRSV * NVGAP(4)

      NSLEXP = NVGAP(3)
      NTXEXP = NSLEXP * NVGAP(4)

      NSLMIN = 40
      NTXMIN = NSLMIN * NVGAP(4)

      IF (IFMODIF.NE.0)  THEN
          NSLRSV = NSLRSV + NSLEXP
          NTXRSV = NTXRSV + NTXEXP
        ENDIF

      NSLWAN = NSLFRE - NSLRSV
      NTXWAN = NTXFRE - NTXRSV

      IF (NWFREE.LT.NVGAP(1))      GO TO 24
      IF (NSLFRE.LT.NSLRSV+NSLEXP) GO TO 24
      IF (NTXFRE.GE.NTXRSV+NTXEXP) GO TO 31

   24 IF (IFLGAR.NE.0)             GO TO 27
   25 CALL MQSHIFT
      IFLGAR = 7
      GO TO 21

C--           not plenty of space, try to do with half the reservation

   27 IF   (NSLFRE.GE.NSLRSV+NSLEXP) THEN
        IF (NTXFRE.GE.NTXRSV+NTXEXP) GO TO 31
       ENDIF
      NSLWAN = NSLFRE - NSLRSV/2
      NTXWAN = NTXFRE - NTXRSV/2

      IF (NSLWAN.LT.NSLMIN)
     +           CALL NOSPACE ('no line slots left for reading')
      IF (NTXWAN.LT.NTXMIN)
     +           CALL NOSPACE ('no text store left for reading')


C----          Handle pending material from the ASAV bank
C-             Note: this can happen only at the moment
C-                   when resuming the cradle or a Pam file

   31 LASAV  = LQ(LARRV-2)
      IF (LASAV.EQ.0)              GO TO 41

      IQ(LQHOLD+1)= LQLEND(2)

      JSLTR = IQ(LASAV+1)
      NSLTR = IQ(LASAV+2)
      JSLE  = JSLTR + NSLTR
      NTXTR = MLIAD(JSLE) - MLIAD(JSLTR)

      IF (NSLTR.GT.NSLWAN)
     +           CALL NOSPACE ('no line slots left for reading')
      IF (NTXTR.GT.NTXWAN)
     +           CALL NOSPACE ('no text store left for reading')

C--       transfer all pending material

      JSLTR = LN_COP2 (JSLTR,NSLTR)
      CALL TOGARB (LARRV-2,0)

      IQ(LQHOLD+2) = NSLTR
      CALL ARRNXD (-1)

      IF (JDKNEX.NE.0)             RETURN
      IF (MODE.EQ.1)               RETURN
      GO TO 21

C------        Read from the file

   41 IF (IQ(LQHOLD+2).EQ.0)  THEN
          IQ(LQHOLD+1) = LQLEND(2)
          JDKTYP = 0
        ENDIF

      IN_DOX = 2
C--       if file not seekable, dont read too much

      IF (IQ(LARRV+7).LT.-1)  THEN
          NSLWAN = NSLMIN
          NTXWAN = NTXMIN
        ENDIF
      IN_DO1 = NSLWAN
      IN_DO2 = NTXWAN
      JDKNEX = 0
      IF (IQ(LARRV+7).EQ.-1)  THEN
          CALL ARRTM
        ELSE
          CALL ARRIN
        ENDIF

      IF (JDKNEX.LT.0)             GO TO 61
      IF (MODE.NE.1)  THEN
          IF (JDKNEX.EQ.0)         GO TO 21
        ENDIF
      IF (MODE.NE.0)               RETURN

C--           having read the blank deck of the cradle

      IF (IFLAUX.NE.0)             RETURN
      IF (JDKTYP.LT.2)             RETURN
      CALL P_KILL ('illegal start of the cradle')

C----     Having read the start of the blank deck of the cradle
C--       until a line +MORE : read the cradle continuation file

   61 JDKNEX = 0
      JSLM   = IQ(LQHOLD+1) + IQ(LQHOLD+3)
      JCCTYP = MCCMOR
      CALL CCKRAK (JSLM)

      IF (NCHCCD.GE.NCHCCT)  JCCBAD= 7
      IF (JCCBAD.NE.0)  CALL P_KILL ('faulty +MORE line')

C--       close previous cradle file, if not terminal

      IF (IQ(LARRV+7).NE.-1)  THEN
          IN_DOX = -1
          CALL ARRIN
        ENDIF

C--       attach the +MORE file

      IQREAD = IQRSAV
      IQRTTY = 0
      CALL FLKRAK (1,1)
      CALL FLINK  (IQREAD, 1, -1, 0)
      IQRFD  = LUNFD
      IQRSIZ = LUNSIZ

      IQ(LARRV+1) = IQREAD
      IQ(LARRV+2) = IQRFD
      IQ(LARRV+4) = IXFLUN
      IQ(LARRV+6) = IQRSIZ
      IQ(LARRV+7) = 0
      IQ(LARRV+8) = 0

      LQLEND(2) = JSLM + 1
      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)
      GO TO 21
      END
