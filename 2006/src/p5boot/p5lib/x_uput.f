CDECK  ID>, X_UPUT.
      SUBROUTINE X_UPUT (MTARG)

C-    Update the activation bits in MTARG for X_USE

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      COMMON //      IQUEST(100)
C--------------    End CDE              --------------------------------
      DIMENSION    MTARG(9)
      DIMENSION    MV(4)
      EQUIVALENCE (MV(1),IQUEST(1))
      EQUIVALENCE (MXNEW,IQUEST(11)), (MODE,IQUEST(12))
     +,           (MXTRAN,IQUEST(13))

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)
      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)

C--       unpack MV( 1 self+forg  2 inhibit  3 trans  4 self only

      MXD   = MTARG(1)
      MV(1) = IAND        (MXD,     KM6-1)
      MV(2) = IAND (ISHFTR(MXD,5),  KM6-1)
      MV(3) = IAND (ISHFTR(MXD,10), KM5-1)
      MV(4) = IAND (ISHFTR(MXD,14), KM5-1)

C-    MODE:  1 ONLY,  2 TRANS,  4 INHIBIT

      IF (MODE.EQ.4)               GO TO 28

C----              Activation

      IF (MODE.NE.1)  MV(1) = IOR (MV(1), MXNEW)
      IF (MODE.EQ.2)  MV(3) = IOR (MV(3), MXNEW)
                      MV(4) = IOR (MV(4), MXNEW)

C--       "TRANS" attachment to USE

      IF (MXTRAN.NE.0)  THEN
      IF (MXNEW.GE.16)  THEN
          MV(1) = IOR (MV(1), MXTRAN)
          MV(3) = IOR (MV(3), MXTRAN)
          MV(4) = IOR (MV(4), MXTRAN)
        ENDIF
        ENDIF

C--       mask against inhibition bits

   24 MV(1) = IAND (MV(1), MV(2))
      MV(3) = IAND (MV(3), MV(2))
      MV(4) = IAND (MV(4), MV(2))

C--       store

      MXWK = IOR (ISHFTL(MV(2),5),  MV(1))
      MXWK = IOR (ISHFTL(MV(3),10), MXWK)
      MXWK = IOR (ISHFTL(MV(4),14), MXWK)
      CALL SBYT  (MXWK,MTARG(1),1,18)
      RETURN

C----              Inhibition

   28 MXWK  = 127 - MXNEW
      MV(2) = IAND (MV(2), MXWK)
      GO TO 24
      END
