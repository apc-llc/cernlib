CDECK  ID>, LQSTEP.
      FUNCTION LQSTEP (LBK,NSTEP)

C-    Step LBK by NSTEP steps through the linear structure
C.    started 17-june-94

      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------


      N = NSTEP
      L = LBK
   24 IF (N.LE.0)                  GO TO 29
      N = N - 1
      IF (L.EQ.0)  CALL P_KILL ('LQSTEP is in trouble')
      L = LQ(L-1)
      GO TO 24

   29 LQSTEP = L
      RETURN
      END
