CDECK  ID>, NA_DUMP.
      SUBROUTINE NA_DUMP

C-    Dump the NAME stack

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
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
      CHARACTER     NAMEOP*80


      WRITE (IQPRNT,9011) LQLSTA(5), NA_OCC, NA_CUT
 9011 FORMAT (/'1Dump the NAME stack, start/end/cutat =',2I7,I3)

      IF (NA_OCC.EQ.0)             RETURN

      IF (LQLSTA(5).LT.LQLEND(4))  GO TO 84
      IF (NA_OCC.LT.LQLSTA(5))     GO TO 84
      IF (NA_CUT.LT.6)             GO TO 84
      IF (NA_CUT.GT.32)            GO TO 84

      JSL  = LQLSTA(5)
      JSLE = NA_OCC + 1
      JTX  = MLIAD(JSL)
      JTXE = MLIAD(JSLE)

      NSLOCC = JSLE - JSL
      NTXOCC = JTXE - JTX
      NSLAV  = LQLEND(5) - JSL
      NTXAV  = NSIZETX - JTX

      NSLPRO = (100 * NSLOCC) / NSLAV
      NTXPRO = (100 * NTXOCC) / NTXAV

      WRITE (IQPRNT,9017) NTXOCC,NTXPRO,NTXAV,JTX,
     +                    NSLOCC,NSLPRO,NSLAV
 9017 FORMAT (/
     F8X,'Name text ',I7,' or',I3,'% used of',I7,' bytes maximum at',I9/
     F8X,'Name slots',I7,' or',I3,'% used of',I7,' maximum'//
     F7X,'J    JSL      JTX  N  name'/)

      JNA = 0
   44 JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX
      IF (NTX.LE.0)                GO TO 81

      NAMEOP = ' '
      CALL NA_GET (JNA,NAMEOP,1)
      IF (NDSLAT.NE.NTX)           GO TO 82

      WRITE (IQPRNT,9044) JNA,JSL,JTX,NTX,NAMEOP(1:NTX+1)
 9044 FORMAT (1X,2I7,I9,I3,2X,A)

      IF (JSL.EQ.NA_OCC)           RETURN
      JSL = JSL + 1
      JNA = JNA + 1
      GO TO 44

C----     Trouble

   81 WRITE (IQPRNT,9081) JNA,JSL,JTX,NTX
      RETURN

   82 WRITE (IQPRNT,9082) JNA,JSL,JTX,NTX,NDSLAT
      RETURN

   84 WRITE (IQPRNT,9084)
      RETURN

 9081 FORMAT (1X,2I7,I9,I3,'  ***!!! faulty !!!***')
 9082 FORMAT (1X,2I7,I9,2I3,'  ***!!! faulty !!!***')
 9084 FORMAT (40X,'***!!! Faulty !!!***')
      END
