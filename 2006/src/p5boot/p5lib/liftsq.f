CDECK  ID>, LIFTSQ.
      FUNCTION LIFTSQ (KJOIN,JTYPE,JSLCL,JDIV)

C-    Lift a sequence call bank
C-        KJOIN  K-adr of the bank to be lifted
C-        JTYPE  CC type to be stored
C-        JSLCL  slot number of the control-line
C-               if this is non-zero the line will be cracked
C-                       but in this case it must be +SEQ
C-         JDIV  division number for lifting
C-
C-    Return adr of the bank lifted as function value
C-    and IQUEST(1) the number of sequence calls lost:
C-    Note that no bank may have more than 63 links!
C#    started 18-dec-91

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256, KMJ=512, KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
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
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)


      IF (JSLCL.NE.0)  THEN
          JCCTYP = MCCSEQ
          CALL CCKRAK (JSLCL)
        ENDIF

      NPZORG = NCCPZ
      NCCPZ  = MIN (NCCPZ,61)

      MMBANK(3,JBKXSQ) = NCCPZ + 2
      MMBANK(5,JBKXSQ) = NCCPZ + 4
      CALL MQLIFT (LX,KJOIN,0,JBKXSQ,JDIV)

      IQ(LX) = IOR (IQ(LX), ISHFTL(JTYPE,8))
      IQ(LX) = IOR (IQ(LX), IAND(MCCPAR(JCCPT+1),KMP))

      IQ(LX+4) = NCCPZ
      CALL UCOCOP (MCCPAR(JCCPZ+1),IQ(LX+5),NCCPZ,1,3,1)

      IQUEST(1) = NPZORG - NCCPZ
      LIFTSQ = LX
      RETURN
      END
