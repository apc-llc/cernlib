CDECK  ID>, MQWORK.
      SUBROUTINE MQWORK (LREF,IDATA,IDATAL)

C-    Set working space limits and clear wsp to zero

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
C--------------    End CDE              --------------------------------
      DIMENSION    IDATA(9),IDATAL(9)


      NS = IQLOCF(LREF)   - 1
      NL = IQLOCF(IDATA)  - 1
      NW = IQLOCF(IDATAL) + 1

      NQSYSS = NS
      NQLINK = NL

      LQCSTA(1) = NW
      LQCEND(1) = NW
      CALL VZERO (IQUEST, NW+200)
      RETURN
      END
