CDECK  ID>, CCKRAK.
      SUBROUTINE CCKRAK (JSLOT)

C-    Analyse control-card into parameter-list & index

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      CHARACTER      CQBLAN*128, CQEQS*128, CQCETA*32
      CHARACTER      CQAPO*1, CQBSL*1, CQTAB*1
      COMMON /QCHAR/ CQBLAN, CQEQS, CQCETA, CQAPO, CQBSL, CQTAB
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      PARAMETER     (JSPSEQ1=3, JSPDATE=9, JSPSEQL=15)
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
C--------------    End CDE              --------------------------------
      CHARACTER    CCOL(256)*1
      EQUIVALENCE (CCOL,CCKARD)

      DIMENSION    JVECT(7),         NVECT(7)
      EQUIVALENCE (JVECT(1),JCCPP), (NVECT(1),NCCPP)
      EQUIVALENCE (LSEP,JCCWK(1)), (IFSEEN,JCCWK(2)), (MSEEN,JCCWK(3))

      CHARACTER*7  MSEPS, MSEP2
      PARAMETER   (MSEP2 = 'SRFX&LN')
      PARAMETER   (MSEPS = 'PDZTICN')
C-                          1234567
C-                              136
C-                          1248624

      PARAMETER    (NTYPES=42)
      DIMENSION    MCXDEF(NTYPES), MCXMUS(NTYPES)
     +,            MCXMAY(NTYPES), MCXMUL(NTYPES)

C.                                        translated by table_cctype
C.                default key  must   may   mult  def must  may mult
C.  04 +NIL     MCCNIL=01   -     -   -      -      0    0    0    0
C.  39 +KILL    MCCKIL=02   I     -   -      I      5    0   16   16
C.  05 +INC     MCCINC=03   Z     Z   T      IZ     3    4   28   28
C.  01 +CDE     MCCCDE=04   Z     Z   T      I      3    4   28   24
C.  02 +SEQ     MCCSEQ=05   Z     Z   T      I      3    4   28   24
C. \40          MCCXSQ=06   -     -   -      -      0    0    0    0
C. |06 +IF,     MCCTRU=07    I    I   -      I      5   16   16   16
C.  07 +IFNOT   MCCFAL=08    I    I   -      I      5   16   16   16
C.  08 +ELSE    MCCELS=09    I    -   -      I      5    0   16   16
C.  09 +ENDIF   MCCEND=10   -     -   -      -      0    0    0    0
C.  03 +SELF   |MCCSEL=11    Z    -   T      IZ     3    0   28   28
C.  41          MCCSES=12    Z    -   T      IZ     3    0   28   28
C.  42          MCCFAU=13   -     -   -      -      0    0    0    0
C.  19 +SKIP    MCCSKI=14   -     -   -      I      0    0   16   16
C. |10 +KEEP   |MCCKEE=15    Z    Z   PDT    I      3    4   31   24
C.  14 +DEL     MCCDEL=16   P     C   D      IC     1   32   51   48
C.  11 +REPL    MCCREP=17   P     C   DZ     IZ     1   32   55   20
C.  13 +ADBEF   MCCADB=18   P     C   DZ     IZ     1   32   55   20
C.  12 +ADD     MCCADD=19   P     C   DZ     IZ     1   32   55   20
C. |20 +USE     MCCUSE=20   P     -   T      IPD    1    0   27   27
C.  24 +XDIV    MCCXDI=21   P     -   T      IPD    1    0   27   27
C.  23 +DIV     MCCDIV=22   P     -   T      IPD    1    0   27   27
C.  22 +LIST    MCCLIS=23   P     -   T      IPD    1    0   27   27
C.  21 +EXE     MCCEXE=24   P     -   T      IPD    1    0   27   27
C.  25 +IMITAT  MCCIMI=25   P     P   -      IP     1    1   17   17
C. |29 +ASM     MCCASM=26    D    D   PTC    ID     2    2   59   26
C.  35 +UPDATE  MCCUPD=27   -     -   -      -      0    0    0    0
C. |33 +NAMES   MCCNAM=28     N   N   -      N      7   64   64   64
C.  34 +GAP     MCCGAP=29     N   N   -      N      7   64   64   64
C.  36 +MORE    MCCMOR=30   -     -   -      -      0    0    0    0
C.  30 +ONLY    MCCONL=31   F     F   -      IF     3    4   20   20
C.  31 +FORCE   MCCFOR=32    T    T   -      I      4    8   24   24
C.  32 +SUPEND  MCCSUS=33    T    T   -      I      4    8   24   24
C. |26 +OPTION  MCCOPT=34    T    -   -      I      4    0   24   24
C.  27 +PARAM   MCCOP2=35    T    TN  -      IN     4   72   88   88
C.  28 +SHOW    MCCSHO=36    T    -   -      I      4    0   24   24
C. |37 +PAM     MCCPAM=37     L   -   RTN    IN     6    0  122   88
C.  38 +QUIT    MCCQUI=38   -     -   -      I      0    0   16   16
C.  18 +EOD     MCCEOD=39   -     -   -      -      0    0    0    0
C. |15 +DECK    MCCDEC=40    D    D   PT     I      2    2   27   24
C.  16 +PATCH   MCCPAT=41   P     P   T      I      1    1   25   24
C.  17 +TITLE   MCCTIT=42   -     -   -      -      0    0    0    0

C-                    1    2    3    4    5    6    7    8    9   10

      DATA MCXDEF /   0,   5,   3,   3,   3,   0,   5,   5,   5,   0
     +,               3,   3,   0,   0,   3,   1,   1,   1,   1,   1
     +,               1,   1,   1,   1,   1,   2,   0,   7,   7,   0
     +,               3,   4,   4,   4,   4,   4,   6,   0,   0,   2
     +,               1,   0 /

      DATA MCXMUS /   0,   0,   4,   4,   4,   0,  16,  16,   0,   0
     +,               0,   0,   0,   0,   4,  32,  32,  32,  32,   0
     +,               0,   0,   0,   0,   1,   2,   0,  64,  64,   0
     +,               4,   8,   8,   0,  72,   0,   0,   0,   0,   2
     +,               1,   0 /

      DATA MCXMAY /   0,  16,  28,  28,  28,   0,  16,  16,  16,   0
     +,              28,  28,   0,  16,  31,  51,  55,  55,  55,  27
     +,              27,  27,  27,  27,  17,  59,   0,  64,  64,   0
     +,              20,  24,  24,  24,  88,  24, 122,  16,   0,  27
     +,              25,   0 /

      DATA MCXMUL /   0,  16,  28,  24,  24,   0,  16,  16,  16,   0
     +,              28,  28,   0,  16,  24,  48,  20,  20,  20,  27
     +,              27,  27,  27,  27,  17,  26,   0,  64,  64,   0
     +,              20,  24,  24,  24,  88,  24,  88,  16,   0,  24
     +,              24,   0 /

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)

      CALL VZERO (NCCPAR,24)
      LOC = 4

C----              Copy control-line and find useful length

      JCCSL  = JSLOT
      JTX    = MLIAD(JCCSL)
      NCHCCT = MLIAD(JCCSL+1) - JTX - NCHNEWL
      CALL CCOPYL (TEXT(JTX),CCKORG,NCHCCT)

C--           environment variable substitution

      IF (INCRAD.EQ.0)                        GO TO 18
      IF (INDEX(CCKORG(1:NCHCCT),'${').EQ.0)  GO TO 18
      CALL CENVIR (TEXT(JTX),NCHCCT, CCKORG,1,LEN(CCKORG), 7)
      NCHCCT = NESLAT - 1
      ICCSUB = NGSLAT

   18 CCKORG(NCHCCT+1:NCHCCT+2) = '  '
      NCHCCT = LNBLNK (CCKORG(1:NCHCCT))
      CCKARD(1:NCHCCT+2) = CCKORG(1:NCHCCT+2)
      IF (JCCTYP.EQ.MCCTIT)        RETURN
      IF (JCCTYP.EQ.MCCNIL)        RETURN

C--       check terminating dot is obscured

      NCHCCC = -1
      NCHCCD = ICFIND ('.',CCKARD,5,NCHCCT)
      IF (NCHCCD.LT.NCHCCT)  THEN
        IF     (CCKARD(NCHCCD+1:NCHCCD+2).NE.'  ')   THEN
          IF   (CCKARD(NCHCCD-2:NCHCCD-1).NE.'  ')   THEN
            IF (CCKARD(NCHCCD-1:NCHCCD+1).NE.' . ')  JCCWAR=1
           ENDIF
         ENDIF
       ENDIF

      CALL CLEFT (CCKARD,1,NCHCCD-1)
      NCHU = NDSLAT
      CALL CLTOU (CCKARD(1:NCHU))

   19 IF (CCOL(NCHU).EQ.',')  THEN
          JCCWAR = 2
          NCHU = NCHU - 1
          GO TO 19
        ENDIF

      IFLERR = 0
      IFLACT = 0
      IF (JCCTYP.GE.MCCDEL .AND. JCCTYP.LE.MCCADD)  IFLACT= 1

C-----------       Read next parameter

      JCOM = ICFIND (',',CCKARD,4,NCHU)
      IF (JCOM.GE.NCHU)            GO TO 51

      JEQU = ICFIND ('=',CCKARD,JCOM,NCHU)
      JMIN = ICFIND ('-',CCKARD,JCOM,NCHU)

   21 JGO  = JCOM + 1
      JCOM = ICFIND (',',CCKARD,JGO,NCHU)
      MCCPAR(LOC) = 1

      IF (JEQU.EQ.JGO)             GO TO 96
      IF (JEQU.LT.JCOM)            GO TO 24

   22 NEGAT = 0
      IF (JGO.GT.NCHU)             GO TO 30
      IF (JGO.EQ.JMIN)  THEN
          JGO = JGO + 1
          JMIN = ICFIND ('-',CCKARD,JGO,NCHU)
          IF (JMIN.LT.JCOM)        GO TO 96
          NEGAT = -1
        ENDIF

      JEND = MIN (JCOM,JMIN)
      NCU  = JEND - JGO
      GO TO 31

C----              Parameter key seen

   24 JSEP = INDEX (MSEPS,CCOL(JGO))
      IF (JSEP.EQ.0)  JSEP= INDEX (MSEP2,CCOL(JGO))
      IF (JSEP.EQ.0)               GO TO 96

      JGO  = JEQU + 1
      JEQU = ICFIND ('=',CCKARD,JGO,NCHU)
      IF (JEQU.LT.JCOM)            GO TO 96

      IF (JVECT(JSEP).NE.0)        GO TO 27
      IF (IFSEEN.NE.0)             GO TO 96
      IF (JSEP.EQ.5)  IFSEEN= 7
      LSEP  = JSEP
      JVECT(JSEP) = LOC
      IF (JSEP.EQ.4)  THEN
          MCCPAR(LOC)   = 5
          MCCPAR(LOC+1) = 0
          MCCPAR(LOC+2) = 0
          LOC = LOC + 3
        ENDIF
   26 MCCPAR(LOC) = JSEP + 1
      GO TO 22

C--                Same key again

   27 IF (JSEP.NE.LSEP)  THEN
          IFLERR = 7
          GO TO 22
        ENDIF
      IF (JSEP.NE.5)               GO TO 22
      GO TO 26

C-----------       Digest current parameter

   30 JEND = JGO
      NCU  = 0

   31 IF (LSEP.NE.0)               GO TO 41

C--       First parameter without parameter-key

      IF (IFLACT.NE.0)             GO TO 33
      IF (NCU.EQ.0)                GO TO 96
      JSEP = MCXDEF(JCCTYP)
      IF (JSEP.EQ.0)  THEN
          IFLERR = 7
          JSEP   = 1
        ENDIF
      IF (JSEP.NE.5)               GO TO 36
      IFSEEN = 7
      GO TO 36

C--       Action card with implicite parameter-key

   33 JSEP = IFLACT
      IF (JSEP.EQ.3)               GO TO 35
      IFLACT = IFLACT + 1
      IF (NCU.EQ.0)                GO TO 49
      GO TO 37

   35 IF (NCU.EQ.0)                GO TO 49
      JSEP = 6

   36 LSEP = JSEP
   37 JVECT(JSEP) = LOC
      IF (JSEP.EQ.4)  THEN
          MCCPAR(LOC)   = 5
          MCCPAR(LOC+1) = 0
          MCCPAR(LOC+2) = 0
          LOC = LOC + 3
        ENDIF
      MCCPAR(LOC) = JSEP + 1
      GO TO 42

C-----             Normal parameter processing

   41 JSEP  = LSEP
   42 IF (JSEP.LT.6)               GO TO 46

C--                Store numeric parameter value

      IF (NCU.EQ.0)                GO TO 96
      NUM = ICDECI (CCKARD,JGO,JEND-1)
      IF (NGSLAT.NE.0)             GO TO 96
      IF (NEGAT.NE.0)  THEN
          IF (JSEP.NE.7)           GO TO 96
          NUM = -NUM
        ENDIF

      MCCPAR(LOC+1) = NUM
      MCCPAR(LOC+2) = NUM
      IF (JEND.NE.JMIN)            GO TO 48
      IF (JMIN.GT.NCHU)            GO TO 48

      JGO  = JEND + 1
      JMIN = ICFIND ('-',CCKARD,JGO,NCHU)
      IF (JMIN.LT.JCOM)            GO TO 96

      NUM2 = ICDECI (CCKARD,JGO,JCOM-1)
      IF (NGSLAT.NE.0)             GO TO 96
      MCCPAR(LOC+2) = NUM2
      IF (NUM2.LT.NUM)             GO TO 96
      GO TO 48

C--                Store text parameter value

   46 IF (NCU.NE.0)                GO TO 47
      IF (JSEP.EQ.1)  THEN
          IF (JCCTYP.EQ.MCCASM)    GO TO 47
          IF (JCCTYP.EQ.MCCUSE)    GO TO 47
        ELSEIF (JSEP.EQ.2)  THEN
          IF (JCCTYP.NE.MCCDEC)    GO TO 47
        ENDIF
      GO TO 96

   47 IF (JSEP.NE.4)  THEN
          IXNAME = NA_NEW (CCKARD,JGO,JEND)
          MCCPAR(LOC+1) = IXNAME
          MCCPAR(LOC+2) = IXNAME
        ELSE
          MCCPAR(LOC+1) = JGO
          MCCPAR(LOC+2) = NCU
        ENDIF

      IF (NEGAT.NE.0)  THEN
          IF (JSEP.NE.5)           GO TO 96
          MCCPAR(LOC) = -MCCPAR(LOC)
        ENDIF

   48 NCCPAR = NCCPAR + 1
      NVECT(JSEP) = NVECT(JSEP) + 1
      LOC = LOC + 3
   49 IF (JCOM.LT.NCHU)            GO TO 21

C------            Analysis finished

   51 JGO = NCHU + 1
      MCCPAR(LOC) = 0
      JCCEND = LOC
      IF (IFLERR.NE.0)             GO TO 94

C--       Compress values for  TYPE=...

      MVALT  = 0
      IF (NCCPT.EQ.0)              GO TO 56
      IF (JCCTYP.EQ.MCCPAT)        GO TO 56
      IF (JCCTYP.EQ.MCCDEC)        GO TO 56
      LOC = JCCPT
      NTH = NCCPT
   52 IF (NTH.GT.0)  THEN
          LOC = LOC + 3
          JXX = MCCPAR(LOC+1)
          J   = INDEX (CQCETA(1:32),CCOL(JXX))
          IF (J.NE.0)    CALL SBIT1 (MVALT,J)
          NTH = NTH - 1
          GO TO 52
        ENDIF
      MCCPAR(JCCPT+1)= MVALT
      MCCPAR(JCCPT+2)= MVALT

C--       Check parameters which may or must be present, may be multiple

   56 MUST = MCXMUS(JCCTYP)
      MMAY = MCXMAY(JCCTYP)
      MULT = MCXMUL(JCCTYP)

      MSEEN = 0
      MSEMU = 0
      IONE  = 1

      DO 57 J=1,7
      IF (NVECT(J).NE.0)  THEN
          MSEEN = MSEEN + IONE
          IF (NVECT(J).GE.2)  MSEMU = MSEMU + IONE
        ENDIF
   57 IONE = 2*IONE

      IF (IAND(MSEEN, MUST)     .NE.MUST)   GO TO 91
      IF (IAND(MSEEN, NOT(MMAY)).NE.0)      GO TO 63
      IF (IAND(MSEMU, NOT(MULT)).NE.0)      GO TO 63

C--       Check for Action lines, fix for +ADD

      IF (IFLACT.EQ.0)             GO TO 59
      NUM = MCCPAR(JCCPC+1)

      IF (JCCTYP.EQ.MCCADD)  THEN
          MCCPAR(JCCPC+1) = NUM + 1
        ELSEIF (JCCTYP.EQ.MCCADB)  THEN
          IF (NUM.EQ.0)            GO TO 96
        ENDIF

   59 RETURN

C--       check legal parameters on special +CDE, +SEQ or +SELF

C-    PARAMETER   (MSEP2 = 'SRFX&LN')
C-    PARAMETER   (MSEPS = 'PDZTICN')
C-                          1234567

   63 IF (JCCTYP.LT.MCCCDE)        GO TO 94
      IF (JCCTYP.GT.MCCSEQ)  THEN
          IF (JCCTYP.LT.MCCSEL)    GO TO 94
          IF (JCCTYP.GT.MCCSES)    GO TO 94
          IF (NCCPZ.EQ.0)          GO TO 94
        ENDIF
      JC = JCCPZ
      DO 67 JL=1,NCCPZ
      JREL = MCCPAR(JC+1) - JSPSEQ1
      IF (JREL.LT.0)  JREL= 99
      MMAYX = MMAY
      MULTX = MULT

      IF      (JREL.GE.12) THEN
                          MULTX= IOR (MULTX, KM3)
       ELSEIF (JREL.GE.10) THEN
                          MMAYX= IOR (MMAYX, KM1)
       ELSEIF (JREL.GE.4)  THEN
                          MMAYX= IOR (MMAYX, KM1+KM6+KM7)
       ELSEIF (JREL.GE.1)  THEN
                          MMAYX= IOR (MMAYX, KM7)
       ELSE
                          MMAYX= IOR (MMAYX, KM2)
       ENDIF

      IF (IAND(MSEEN, NOT(MMAYX)).NE.0)      GO TO 94
      IF (IAND(MSEMU, NOT(MULTX)).NE.0)      GO TO 94
   67 JC = JC + 3
      GO TO 59

C----         Missing parameter, allowed only on +SEQ, T=DUMMY

   91 IF   (JCCTYP.LT.MCCCDE)      GO TO 94
      IF   (JCCTYP.GT.MCCSEQ)   THEN
        IF (JCCTYP.NE.MCCKEE)      GO TO 94
       ENDIF
      IF (IAND(MVALT,KM4).NE.0)    GO TO 59

C----         Faulty c/line, for good syntax but unexpected
C--           or missing parameter give no error in c/line
C--           is IF de-selected

   94 IF (IFLAUX.EQ.0)  THEN
          CALL CCPROC
          IF (JCCIFV.NE.0)         RETURN
        ENDIF

   96 JCCBAD = JGO
      RETURN
      END
