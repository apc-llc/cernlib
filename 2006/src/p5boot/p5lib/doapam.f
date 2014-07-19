CDECK  ID>, DOAPAM.
      SUBROUTINE DOAPAM

C-    Gather the parameters of the c/line +PAM currently in CCKARD
C-    and store them into the PAM bank; position the wanted ARRV bank
C-    at LQARRV
C.    started 3-aug-93

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /ARRCOM/IN_LUN, IN_FD,  IN_FIO, IN_EOF
     +,              IN_DOX, IN_DO1, IN_DO2
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      PARAMETER (KQGARB=1,KQARRV=3,KQKEEP=4,KQPREP=5,KQMAIN=6,KQPAST=8)
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
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
C--------------    End CDE              --------------------------------

      PARAMETER    (NPOSOP=10)
      CHARACTER    POSOP(NPOSOP)*6

      DATA POSOP   / 'A*TTAC'
     +,              'H*OLD '
     +,              'R*ESUM'
     +,              'U*PDAT'
     +,              'MER*GE'
     +,              'DIS*PL'
     +,              'LIS*T '
     +,              'IND*IV'
     +,              'C*AR* '
     +,              'D*ETAC' /

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)


C--   Analyse +PAM, LAB=lab, N=x,y, T=opt, RETURN=pname .fnname
C-
C-                  L=lab    lab (integer) identify the file
C-                                         default: zero
C-                  N=x      do x files
C-                  N=x,y    skip x files, do y files
C-
C-                  T=ATTACH   attach file fname
C-                    HOLD     do not release the file
C-                    RESUME   continue file "lab"
C-                    UPDATE   process this file in update mode
C-                    (old: CARDS, DETACH)
C-
C-                  R=pname    stop after P=pname complete


      CALL CCOPT (POSOP,NPOSOP)
      IF (JCCBAD.NE.0)             RETURN

      LASTLU = IQ(LPAM+1)
      CALL VZERO (IQ(LPAM+1),8)

      LABEL = MCCPAR(JCCPC+1)

      IF (NCCPN.GE.2)  THEN
          IQ(LPAM+4) = ABS(MCCPAR(JCCPN+1))
          IQ(LPAM+5) =     MCCPAR(JCCPN+4)
        ELSEIF (NCCPN.EQ.1)  THEN
          IQ(LPAM+5) = ABS(MCCPAR(JCCPN+1))
        ELSE
          IQ(LPAM+5) = 999
        ENDIF

      IF (NCCPT.GT.0)  IQ(LPAM+6) = MCCPAR(JCCPT+2)
      IQ(LPAM+7) = MCCPAR(JCCPD+1)

C--       find a free logical unit number

      LUNX = 11

C--       if labelled, find its ARRV bank

      IFLPRE = 1
      IF (LABEL.NE.0)  THEN
          LXX = KQFIND (LABEL,3,KQARRV,KXX)
          IF (LXX.NE.0)            GO TO 31
        ENDIF

C--       get an empty ARRV bank

      IFLPRE = 0
      KXX = KQARRV
   24 LXX = LQ(KXX)
      IF (LXX.EQ.0)                   GO TO 26
      IF (IQ(LXX+1)+IQ(LXX+3).EQ.0)   GO TO 27
      KXX = LXX - 1
      GO TO 24

   26 CALL MQLIFT (LXX, KXX,0, JBKARR,3)
   27 CALL VZERO (IQ(LXX+1), 11)

C------       New file to be opened        --------------

   31 IXFLUN = 0
      IF (IAND(IQ(LPAM+6),KM1).NE.0)  GO TO 34
      IF (LASTLU.NE.0)                GO TO 41
      IQ(LPAM+6) = IOR (IQ(LPAM+6),KM1)
      IXFLUN = IXLUN(1)
      GO TO 35

   34 CALL FLKRAK (1,2)
   35 IF (IXFLUN.EQ.0)             GO TO 91
      IF (IQ(LXX+1).EQ.0)          GO TO 47

C--       the file connected is still active, close it

      LUNX  = IQ(LXX+1)
      LSAVE = LARRV
      LARRV = LXX
      IN_DOX = -1
      CALL ARRIN
      LARRV = LSAVE
      GO TO 47

C------       File to be resumed           --------------

   41 IF (LABEL.EQ.0)  THEN
          KXX = KQARRV
          LXX = LQARRV
        ELSE
          IF (IFLPRE.EQ.0)         GO TO 91
        ENDIF

      IF (IQ(LXX+1).NE.0)  LUNX= IQ(LXX+1)

   47 IF (KXX.NE.KQARRV)  CALL QSHUNT (KXX,KQARRV)
      IQ(LPAM+1) = LUNX
      IQ(LPAM+3) = LABEL
      IQ(LPAM+8) = IXFLUN
      RETURN

C--           Trouble

   91 JCCBAD = 7
      RETURN
      END
