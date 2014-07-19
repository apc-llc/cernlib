CDECK  ID>, NA_INIT.
      SUBROUTINE NA_INIT

C-    Pre-initialize the name stack
C.    started  1-oct-91

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


      NA_CUT  = 32
      NA_MXSL = 6000
      NA_MXTX = 8 * NA_MXSL

      JSLA = LQLEND(5) - NA_MXSL
      JTXA = NSIZETX - NA_MXTX

      LQLSTA(5) = JSLA
      LQLEND(4) = JSLA
      LQLSTA(4) = JSLA

C--   preset slot  0  one blank
C-                 1  unknown

      TEXT(JTXA)    = ' '
      MLIAD(JSLA)   = JTXA
      MLIAD(JSLA+1) = JTXA + 1

      JTXA = JTXA + 1
      JSLA = JSLA + 1

      CALL CCOPYL ('unknown', TEXT(JTXA), 7)
      MLIAD(JSLA+1) = JTXA + 7
      NA_OCC = JSLA
      RETURN
      END
