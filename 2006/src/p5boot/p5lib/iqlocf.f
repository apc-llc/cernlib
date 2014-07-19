CDECK  ID>, IQLOCF.
      FUNCTION IQLOCF (VAR)

C-    Return the address of VAR in LQ

      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      DIMENSION    VAR(9)


      IQLOCF = LOCF(VAR(1)) - LQADR0
      RETURN
      END
