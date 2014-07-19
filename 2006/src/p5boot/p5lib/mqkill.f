CDECK  ID>, MQKILL.
      SUBROUTINE MQKILL

C-    Turn the garbage d/s into a flat linear structure
C-    mark banks in division 1 to be dead
C-    keep in the structure only banks in division 1 with line-numbers
C.    started  8-oct-91

      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCN/  IQLST,IQLNA,IQID,IQTY,IQLI,IQNL,IQNS,IQND
     +,              IQLNX,IQFOUL
      PARAMETER      (IQDROP=31, IQMARK=30, KMDROP=1073741824)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      PARAMETER    (KQGARB=1)

      IOR  (IZV,IZW) =  OR (IZV, IZW)


      KCUR = KQGARB
   21 LCUR = LQ(KCUR)
      IF (LCUR.EQ.0)               RETURN
      IF (LCUR.GE.LQCEND(1))       GO TO 49

      CALL QNAME (LCUR)
      LQ(IQLNA) = IOR (LQ(IQLNA),KMDROP)
      IF (IQNS.LT.2)               GO TO 41

C----              Bank with structural links reached

      KUSE = LCUR - IQNS -1

C--                Look at next link

   24 KUSE = KUSE + 1
      IF (KUSE.EQ.LCUR-1)          GO TO 41
      LNEW = LQ(KUSE)
      IF (LNEW.EQ.0)               GO TO 24
      IF (LNEW.GE.LQCEND(1))  THEN
          LQ(KUSE) = 0
          GO TO 24
        ENDIF

C--                Shunt the linear structure at KUSE to LCUR-1

      CALL QSHLIN (KUSE,LCUR-1)
      GO TO 24

C----              Keep current bank connected only if with line-number

   41 IF (IQLI.EQ.0)               GO TO 49
      IF (IQ(LCUR+1).EQ.0)         GO TO 49
      IF (IQ(LCUR+1).GE.LQLSTA(2)) GO TO 49
      IF (IQ(LCUR+2).EQ.0)         GO TO 49
      KCUR = LCUR - 1
      GO TO 21

C--       bridge unwanted bank

   49 LQ(KCUR) = LQ(LCUR-1)
      GO TO 21
      END
