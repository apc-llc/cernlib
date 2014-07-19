CDECK  ID>, NA_FUL.
      SUBROUTINE NA_FUL

C-    Name stack full
C.    started 22-jan-92

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

      WRITE (IQPRNT,9082) NTXOCC,NTXPRO,NTXAV,
     +                    NSLOCC,NSLPRO,NSLAV
 9082 FORMAT (/' ***!!! Name space full !!!***'/
     F5X,'Name text ',I7,' or',I3,'% used of',I7,' bytes maximum'/
     F5X,'Name slots',I7,' or',I3,'% used of',I7,' maximum')

      CALL NOSPACE ('-')
      END
