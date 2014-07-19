CDECK  ID>, MQINIT.
      SUBROUTINE MQINIT

C-    Initialise memory manager and some other COMMONs

      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      CHARACTER      CQBLAN*128, CQEQS*128, CQCETA*32
      CHARACTER      CQAPO*1, CQBSL*1, CQTAB*1
      COMMON /QCHAR/ CQBLAN, CQEQS, CQCETA, CQAPO, CQBSL, CQTAB
      PARAMETER      (NQBITW=32, NQBITC=8, NQCHAW=4)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCT/  LQTA,LQTB,LQTE,LQMTB,LQMTE,LQMTH
     +,              JQDIVG,NQRESV(3),IQPART,NQFREE(3)
      COMMON /MQCN/  IQLST,IQLNA,IQID,IQTY,IQLI,IQNL,IQNS,IQND
     +,              IQLNX,IQFOUL
      COMMON /MQCL/  NQLST,NQLNA,NQID,NQTY,NQLI,NQNL,NQNS,NQND
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
     +,              MUSER(9)
C--------------    End CDE              --------------------------------
      COMMON /SLATE/ ISLATE(40)
      LOGICAL      INTRAC

      CHARACTER    MMTOBK(NBANKS)*20

      DATA MMTOBK  / 'PAT    0   4   3   2'
     +,              'DECK   0   4   3   1'
     +,              'ORG    0   0   0   2'
     +,              'INCL   0   1   1   1'
     +,              'HOLD   1   1   1   5'
     +,              'KEEP   1   3   2   4'
     +,              'ACT    1   3   2   5'
     +,              'MAT    1   3   1   3'
     +,              'XSQ    1   3   1   5'
     +,              'PREP   1   3   1   2'
     +,              'GARB   0   1   1   2'
     +,              'ASMH   0  63   0   2'
     +,              'ASMT   0   5   1  11'
     +,              'ASML   0   1   1  11'
     +,              'ASMX   0   1   1   4'
     +,              'ARRV   0   2   2  14'
     +,              'ASAV   1   1   1   2'
     +,              'PAM    0   0   0  15'
     +,              'RPAM   0   1   1   4' /
C-                    _:.=+=.: 1_:.=+=.: 2
C-                      ID  LI  NL  NS  ND



C--                Ready common /QCHAR/

      CQBLAN = ' '
      CALL CFILL ('====', CQEQS, 1, LEN(CQEQS))
      CQCETA = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ012345'
      CQAPO  = CHAR(39)
      CQBSL  = CHAR(92)
      CQTAB  = CHAR(9)

C--                Ready common /QPAGE/

      NQLMAX = 110
      NQCMAX = 120
      NQCPGH = 110
      CALL MQPAGE
      NQNEWH = 0
      NQJOIN = 0
      NQDKNO = 0
      NQDKPG = 1
      NQPAGE = 1
      NQWYL  = 0
      NQWYLDO= 0

C--                Ready common /QSTATE/

      CALL VZERO  (NQERR,6)
      CALL DATIME (IQDATE,IQTIME)
      IQDATE = (ISLATE(1)*100 + ISLATE(2))*100 + ISLATE(3)
      ISL4 = ISLATE(4)
      ISL5 = ISLATE(5)

      CQDATE = '00000000'
      CALL CSETDI (IQDATE,CQDATE,1,8)
      CQDATEM = CQDATE(1:4) // '/' // CQDATE(5:6) // '/' // CQDATE(7:8)

      CQTIME = ' 0.0 '
      CALL CSETDI (ISL4, CQTIME,1,2)
      CALL CSETDI (ISL5, CQTIME,4,5)

C--                Ready common /QUNIT/

      IQREAD = 2
      IQPRNT = 3
      IQTTIN = 5
      IQTYPE = 6
      IQRSAV = IQREAD
      IQRFD  = 0
      IQRRD  = 0
      IQRSIZ = 0
      NQLPAT = 0
      NQUSED = 0
      NQLLBL = 0
      NQINIT = 7

      IF (INTRAC())  THEN
          IQOFFL = 0
          IQRTTY = IQTTIN
        ELSE
          IQOFFL = 1
          IQRTTY = 0
        ENDIF

C--                Ready common /MQCM/

      LQADR  = LOCF (LQ(1))
      LQADR0 = LQADR - 1
      NQOFFS = LOCF (MLIAD(1)) - LQADR
      NQSYSS = 12
      NQLINK = 24
      NQMAX  = NSIZEQ - 2

C--                Clear commons

      CALL VZERO (IQUEST,4000)
      CALL VZERO (LQTA,14)
      CALL VZERO (NQLST,8)
      CALL VZERO (IQLST,10)

      IFLAUX = 0
      CALL VZERO (IXLUN,12)

      CALL VZERO (NCHCCD,269)
      CCKORG = ' '
      CCKARD = ' '
      MCCPAR(2) = -1

C--                Ready control divisions

      LQCEND(3) = NQMAX - 1
      LQCSTA(3) = LQCEND(3)

      LQCSTA(1) = NQLINK + 1
      LQCEND(1) = LQCSTA(1)

      LQCSTA(2) = (LQCSTA(3)+LQCEND(1)) / 2
      LQCEND(2) = LQCSTA(2)

C--                Ready text divisions

      JTXL = NSIZETX - 12000
      JSLL = NSIZELN -  2000
      MLIAD(JSLL) = JTXL

      LQLSTA(4) = JSLL
      LQLEND(4) = JSLL
      LQLSTA(5) = JSLL
      LQLEND(5) = NSIZELN - 2
      MLIAD(LQLEND(5)) = NSIZETX

C--       slots 1+2 are used for constructing the line
C--       for each special sequence

      DO 42 J=1,999
   42 TEXT(J) = ' '
      MLIAD(1) = 1
      MLIAD(2) = 257
      MLIAD(3) = 513

      LQLSTA(1) = 1
      LQLEND(1) = 3
      LQLSTA(2) = LQLEND(1)
      LQLEND(2) = LQLSTA(2)
      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)

C--                Ready /QBANKS/

      DO 47  J=1,NBANKS
      CALL UCTOH (MMTOBK(J),MMBANK(1,J),4,4)
      MMBANK(2,J) = ICDECI (MMTOBK(J), 6, 8)
      MMBANK(3,J) = ICDECI (MMTOBK(J),10,12)
      MMBANK(4,J) = ICDECI (MMTOBK(J),14,16)
   47 MMBANK(5,J) = ICDECI (MMTOBK(J),18,20)

C--                Initialize the name handler

      CALL NA_INIT
      CALL NA_INPY
      RETURN
      END
