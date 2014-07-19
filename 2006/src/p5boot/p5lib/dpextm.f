CDECK  ID>, DPEXTM.
      SUBROUTINE DPEXTM (JTERM)

C-    End of deck or routine
C-        JTERM = +1  end of deck
C-                -1  about to start a new routine in the same deck
C.    started 6-oct-93

      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      CHARACTER      CHEXPD*68
      COMMON /CHEXC/ IXEXPAM, IXEXPAT,IXEXDEC,IXEXID, NCHEPD, CHEXPD
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
      COMMON /INCLC/ N_INCL, IX_INCL(100)
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
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
      EQUIVALENCE (LASML,JD_SML), (LASMP,JD_SMP), (LXASM,JD_SMX)
      CHARACTER    LINE*128
      EQUIVALENCE (LINE,SLLINE)

      IOR  (IZV,IZW) =  OR (IZV, IZW)


      IF (JD_SPL.LT.2)             GO TO 41

C------       SPLIT or MODIFY mode: close split file
C--       if MODIFY: check EoF reached
      IF (JD_MOD.NE.0)  CALL DPEXMO (0,0)

C--       register the name of the include file
      IF (JD_DTD.EQ.JIN_TYP)  THEN
          CALL MQLIFT (L, LQINCL,1, JBKINC,3)
          IQ(L)  = IOR (IQ(L), MX_SELF)
          IQ(L+1) = IXEXID
          IF (JD_MOD.EQ.0)  IQ(L)= IOR (IQ(L),KM5)
        ENDIF

      CALL CICLOS (JD_LUN)
C--       Write the entry into the log file

      IF (JD_LOG.LE.0)             GO TO 37
      LINE = ' '
      CALL STRMID (JD_DTD, JD_DIV, LINE,2)
      N = MAX(NESLAT,12) + 1

      CALL NA_GET (IXEXID, LINE,N)
      N = NESLAT

      IF (IX_EXT.NE.0)  THEN
          CALL NA_GET (IX_EXT, LINE,N)
          N = NESLAT
        ENDIF

C--       add the names of the called include files

      IF (JD_MOD.EQ.0)             GO TO 33
      LINE(N+3:N+6) = 'same'
      N   = N + 6

   33 JTK = 0
      IF (N_INCL.EQ.0)             GO TO 36
   34 JTK = JTK + 1
      CALL NA_GET (IX_INCL(JTK),LINE,N+2)
      N = NESLAT + 1
      LINE(N-1:N) = '.h'
      IF (JTK.EQ.N_INCL)           GO TO 36
      IF (N.LT.72)                 GO TO 34
      N = N + 2
      LINE(N:N) = '+'

   36 CALL CUTOL (LINE(1:N))
      N = N + 1
      LINE(N:N) = CHAR(NEWLN)
      CALL CIPUT (JD_LOG, LINE,N, ISTAT)
      IF (ISTAT.NE.0)  CALL P_FATAM ('CIO write fails')
      IF (JTK.LT.N_INCL)  THEN
          LINE = ' '
          N = 10
          GO TO 34
        ENDIF

   37 IF (JD_MOD.NE.0)             GO TO 79

C------       Accounting

   41 IQ(LXASM+4)  = IQ(LXASM+4)  + JD_NLI
      IQ(LASML+10) = IQ(LASML+10) + JD_NLI

          IQ(LXASM+3) = IQ(LXASM+3) + 1
      IF (JD_MULT.EQ.0)  THEN
          IQ(LASML+9) = IQ(LASML+9) + 1
        ENDIF

C--           Print the log line for option MAPASM

      IF (MOPTIO(13).EQ.0)         GO TO 78
      LINE = ' Written'
      CALL CSETDI (NQDKNO, LINE,10,17)
      LINE(19:20) = 'p='
      CALL NA_GET (IXEXPAT,LINE,21)
      JP = MAX (NESLAT,29)
      LINE(JP+1:JP+2) = 'd='
      CALL NA_GET (IXEXDEC,LINE,JP+3)
      JP = MAX (NESLAT,44)
      CALL CSETDI (JD_NLI, LINE,JP+1,JP+5)

      LINE(JP+7:JP+14) = 'lines to'
      JP = JP + 15
      IF (JD_SPL.LT.2)  THEN
          LINE(JP+1:JP+7) = 'logical'
          JP = JP + 9
          CALL STRMID (JD_DTD, JD_DIV, LINE,JP)
          N = NESLAT - 1
        ELSE
          LINE(JP+1:JP+4) = 'file'
          JP = JP + 5
          LINE(JP+1:JP+NLIFI) = CHLIFI(1:NLIFI)
          N = JP + NLIFI
        ENDIF
      WRITE (IQPRNT,9000)  LINE(1:N)
      NQUSED = NQUSED + 1
      NQLLBL = 0

   78 JD_MULT = JD_MULT + 1
   79 JD_NLI  = 0
      JD_NEW  = JTERM
      RETURN

 9000 FORMAT (A)
      END
