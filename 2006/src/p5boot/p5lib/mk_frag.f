CDECK  ID>, MK_FRAG.
      SUBROUTINE MK_FRAG (KJOINX,NSTEP)

C-    Fragment the contents of the bank at LDOAN for DOFORG
C-    on the boundaries and according to the delayed control-lines
C.    started  15-jul-93

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
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
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------

      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)


      KJOIN = KJOINX
      JSLA  = IQ(LDOAN+1)
      JSLE  = JSLA + IQ(LDOAN+2)
      JSLA  = JSLA + NSTEP
      JCPUT = JCCTYP
      IF (JCCTYP.LT.MCCCDE)        GO TO 20
      IF (JCCTYP.GT.MCCSEQ)        GO TO 20

C--           starting with +CDE or +SEQ, lift a call bank

      CALL CCKRAK (JSLA)
      GO TO 37

C--           Look for the next delayed c/line

   20 JCLAST = 0
      JSLG   = JSLA

   21 CALL NEXTCC ('-',JSLG,JSLE,JSLF,JCCTYP)

   22 NLDO = JSLF - JSLA
      IF (NLDO.EQ.0)               GO TO 27

C--           store the pending material

      CALL MQLIFT (LMAT,KJOIN,0,JBKMAT,1)

      MMBANK(3,JBKMAT) = 2

      LQ(LMAT-2) = LDECO
      IQ(LMAT)   = IOR (IQ(LMAT), MEXAN)
      IQ(LMAT+1) = JSLA
      IQ(LMAT+2) = NLDO
      IQ(LMAT+3) = JSLA - JSLORG

      IF (JCPUT.NE.0)  THEN
          IQ(LMAT) = IOR (IQ(LMAT), ISHFTL(JCPUT,8))
        ELSEIF (JCLAST.NE.0)  THEN
          IQ(LMAT) = IOR (IQ(LMAT), KM15)
        ENDIF
      KJOIN = LMAT - 1

   27 JCPUT = 0
      JSLA  = JSLF
      IF (JCCTYP.LT.0)             GO TO 20
      IF (JCCTYP.EQ.0)             GO TO 99

C----         handle the new c/line

      JCLAST = JCCTYP
      JSLG   = JSLA + 1
      CALL CCKRAK (JSLA)
      IF (JCCBAD.NE.0)  THEN
          NUM = JSLA - JSLORG
          CALL FAILLN (1, 0, NUM, JSLA, 'Bad syntax for delayed c/l')
        ENDIF


C--       control-lines with associated material are:
C--       +_xxx  +SELF +SKIP +KEEP

C--           soft c/lines

      IF (JCCTYP.GE.MCCSEL)        GO TO 41
      JTX = MLIAD(JSLA)
      TEXT(JTX) = '+'
      JCPUT = JCCTYP

      IF (JCCTYP.GE.MCCTRU)        GO TO 21
      IF (JCCTYP.LT.MCCCDE)        GO TO 54

C--           do +CDE, +SEQ, lift a call bank

   37 LMAT = LIFTSQ (KJOIN,JCCTYP,0,1)
      LQ(LMAT-2) = LDECO
      IQ(LMAT)   = IOR (IQ(LMAT), MEXAN)
      IQ(LMAT+1) = JSLA
      IQ(LMAT+2) = 1
      IQ(LMAT+3) = JSLA - JSLORG
      KJOIN = LMAT - 1
      JSLA  = JSLA + 1
      JCPUT = 0
      GO TO 20

C--           hard control lines

   41 IF (JCCTYP.GE.MCCUSE)        GO TO 54
      IF (JCCTYP.GE.MCCKEE)        GO TO 51
      IF (JCCTYP.NE.MCCSEL)        GO TO 21

C--           +SELF, sname

      IF (NCCPZ.EQ.0)              GO TO 21
      LMAT = LIFTSQ (KJOIN,MCCSES,0,1)
      LQ(LMAT-2) = LDECO
      IQ(LMAT)   = IOR (IQ(LMAT), MEXAN)
      IQ(LMAT+1) = JSLA
      IQ(LMAT+3) = JSLA - JSLORG

      CALL NEXTCC ('-',JSLG,JSLE,JSLF,JCCTYP)
      IQ(LMAT+2) = JSLF - JSLA
      KJOIN = LMAT - 1
      GO TO 27

C--           Action line +ADD etc, MAT bank needs 3 links

   51 MMBANK(3,JBKMAT) = 3
      IF (JCCTYP.EQ.MCCKEE)        GO TO 21

C--           stand-alone control-line

   54 JCCTYP = -1
      JSLF = JSLA + 1
      GO TO 22

C----         done

   99 KJOINX = KJOIN
      RETURN
      END
