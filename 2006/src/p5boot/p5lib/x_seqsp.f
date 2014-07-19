CDECK  ID>, X_SEQSP.
      SUBROUTINE X_SEQSP (JSL,IXSEQ,IFSEND)

C-    Ready special sequence called
C-     input:  JSL    slot number of the calling control-line
C-             IXSEQ  sequence name index
C-    output:  IFSEND  = 0  no output to be sent
C-                     > 0  output
C.    split off from X_SEQ 23-aug-95

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      CHARACTER      CQBLAN*128, CQEQS*128, CQCETA*32
      CHARACTER      CQAPO*1, CQBSL*1, CQTAB*1
      COMMON /QCHAR/ CQBLAN, CQEQS, CQCETA, CQAPO, CQBSL, CQTAB
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      CHARACTER      CHEXPD*68
      COMMON /CHEXC/ IXEXPAM, IXEXPAT,IXEXDEC,IXEXID, NCHEPD, CHEXPD
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      PARAMETER     (JSPSEQ1=3, JSPDATE=9, JSPSEQL=15)
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      PARAMETER      (NFIMAX=100)
      COMMON /TITLEC/ NFILET, JTIPAM(NFIMAX)
      CHARACTER       CHTTNA*10, CHTTDT*20
      COMMON /USETTC/  JTTNAM,JTTVER,JTTSLA,JTTDAT,JTTCOM
     +,                NTTNA(5),  NTTDT,  NTTNORM, NTTALL
     +,               CHTTNA(5), CHTTDT
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
C--------------    End CDE              --------------------------------

      CHARACTER    LINE*128, LIMSK*128, LITIT*128, CHESC*1
      EQUIVALENCE (LINE,TEXT), (LIMSK,CCCOMF), (LITIT,SLERRM)


      JTX    = MLIAD(JSL)
      JCCTYP = JPTYPE (TEXT(JTX))
      CALL CCKRAK (JSL)
      NNN   = MCCPAR(JCCPN+1)
      LLL   = MCCPAR(JCCPC+1)
      JCASE = IXSEQ - JSPSEQ1 - 1
      IF (JCASE.GE.1)              GO TO 51
      IF (JCASE.EQ.0)              GO TO 44

C--                handle  +SEQ, QCARD1, R=name

      IF (JCCPD.EQ.0)              GO TO 49
      IX = MCCPAR(JCCPD+1)
      IF (IX.LE.2)                 GO TO 49
      IF (NVEXDK(1).NE.0)  THEN
          IF (JD_NEW.EQ.0)  CALL DPEXTM (-1)
        ENDIF
      IXEXID  = IX
      GO TO 49

C--                handle  +SEQ, QEJECT, N=n

   44 IF (NVEXDK(2).EQ.0)          GO TO 49
      IF (MOPTIO(5).EQ.0)          GO TO 49
      IF (MOPTIO(3).NE.0)          GO TO 49
      IF (NNN.LT.0)  NNN = NQLMAX
      IF (NNN.EQ.0)  NNN = NQLMAX - 6
      IF (NQUSED+NNN.LE.NQLMAX)    GO TO 49
      NQUSED = NQLMAX

   49 IFSEND = 0
      RETURN

C------            handle  +SEQ, QFxxx, QDATE, QTIME

   51 CHESC  = '?'

C----     ready LIMSK, the mask line,
C--             for QFTITLE and QFTITLCH

      IF (JCASE.GE.3)              GO TO 53
      IF (JCASE.EQ.1)  THEN
          LIMSK = '     +  8H'
        ELSE
          LIMSK = '     +  ' // CQAPO
        ENDIF
      NMSK = LNBLNK (LIMSK(1:12))
      LLL  = MAX (NNN,LLL)
      IF (LLL.LE.0)  LLL= 8
      NNN = 0
      GO TO 54

C--             for all others

   53 CALL CCCOMG (0)
      NMSK = NCHCCC
      IF (NMSK.LE.0)               GO TO 49
      IF (JCASE.EQ.11)             GO TO 76

      IF (JCCPP.NE.0) THEN
          JTX   = NA_JTX (MCCPAR(JCCPP+1))
          CHESC = TEXT(JTX)
        ENDIF

C--       find the escape symbol

   54 LINE = LIMSK(1:NMSK)
      JESC = ICFIND (CHESC,LIMSK,1,NMSK)
      NAFT = NMSK - JESC

      IF (JCASE.GE.JSPDATE)        GO TO 81

C----     ready the PAM file title

      JFILE = NFILET
      IF (NNN.GT.0)  JFILE= NNN
      IF (NNN.LT.0)  JFILE= MAX (1,NFILET+NNN)
      JFILE = MIN (JFILE,NFILET)
      JSL = 0
      IF (JFILE.GT.0)  JSL = JTIPAM(JFILE)
      CALL USETT (JSL)

      IF (JCASE.GE.4)              GO TO 71
      GO TO (61,63,65), JCASE

C--           QFTITLE   gives:  +nnHWYLBUR 1.21 /77 930630 22.01

   61 LLL = MIN (LLL,62)
      CALL CSETDI (LLL,LINE,8,9)
      LINE(11:10+LLL) = LITIT(1:LLL)
      LINE(77:80) = 'HOLD'
      NFIN = 80
      GO TO 89

C--           QFTITLCH  gives:  + 'WYLBUR 1.21 /77 930630 22.01'

   63 LLL = MIN (LLL,62)
      LINE(10:10+LLL) = LITIT(1:LLL) // CQAPO
      NFIN = 10+LLL
      GO TO 89

C--           QFHEAD    gives:  txb"WYLBUR 1.21 /77 930630 22.01"txa

   65 IF (LLL.LE.0)  LLL= NTTNORM
      LLL = MIN (LLL,NTTALL)
      LINE(JESC:JESC+LLL-1) = LITIT(1:LLL)
      NFIN = JESC+LLL-1
      GO TO 88

C--       4   QFNAME    gives:  txb"WYLBUR"txa
C--       5   QFVERS    gives:  txb"1.21 /77"txa
C--       6   QFVSNUM   gives:  txb"12177"txa
C--       7   QFVPRIM   gives:  txb"1.21"txa
C--       8   QFVSEC    gives:  txb"77"txa

   71 JNA = JCASE - 3

      IF (LLL.LE.0)  LLL= NTTNA(JNA)
      LLL = MIN (LLL,NTTNA(JNA))
      LINE(JESC:JESC+LLL-1) = CHTTNA(JNA)(1:LLL)
      NFIN = JESC+LLL-1
      GO TO 88

C----         QENVIR    gives  text with substitution

   76 CALL CENVIR (LIMSK,NMSK, LINE,1,256, 1)
      NFIN = NDSLAT
      GO TO 89

C----         QDATE     gives:  txb"19930802"txa

   81 IF (JCASE.NE.JSPDATE)        GO TO 83
      LLL = LEN(CQDATE)
      LINE(JESC:JESC+LLL-1) = CQDATE
      NFIN = JESC+LLL-1
      GO TO 88

C--           QTIME     gives:  txb"1107"txa

   83 LINE(JESC:JESC+3) = CQTIME(1:2) // CQTIME(4:5)
      NFIN = JESC+3

C--       set the "after" text

   88 IF (NAFT.GT.0)  THEN
          LINE(NFIN+1:NFIN+NAFT)= LIMSK(JESC+1:JESC+NAFT)
          NFIN = NFIN + NAFT
        ENDIF

C--       finalize the line in slot 1

   89 JTX = NFIN + 1
      TEXT(JTX) = CHAR (NEWLN)
      JTX = JTX + 1
      MLIAD(2) = JTX
      IFSEND = 7
      RETURN
      END
