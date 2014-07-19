CDECK  ID>, SMDEF.
      SUBROUTINE SMDEF

C-    Load the default compiler instructions for nyshell and nymake
C.    started 20-jan-94

      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER     (NTYSZ=4, NACSZ=4, NLNSZ=4,  NRTSZ=10000)
      CHARACTER      CHNLOG*80,    CODEX*8, CHTYP*8
      COMMON /SHMKC/ NNLOG,CHNLOG, CODEX,   CHTYP(17)
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
     +,    NLINST(4,NACSZ,NTYSZ), IXINST(NLNSZ,4,NACSZ,NTYSZ)
     +,    NCOUNTR(4,NTYSZ), NCSUMR(NTYSZ)
     +,    NCOUNTH(4,NTYSZ), NCSUMH(NTYSZ), IXOPTN(4,3), IXOPTP(4,3)
     +,    NRXQT, NROUT, NNAME(NRTSZ),  NRALLC, NRDEL
     +,    LUNSH, IXOBJ, IXSDIR,  JXTYP, JXACT, JXSTR,  LASTSH
C--------------    End CDE              --------------------------------
      CHARACTER    LINEWK*128
      EQUIVALENCE (LINEWK, SLERRM)


      CHTYP(1)  =  'fo:     '
      CHTYP(2)  =  'fopt    '
      CHTYP(3)  =  'fc      '
      CHTYP(4)  =  'fort:   '
      CHTYP(5)  =  'co:     '
      CHTYP(6)  =  'copt    '
      CHTYP(7)  =  'cc      '
      CHTYP(8)  =  'cc:     '
      CHTYP(9)  =  'ao:     '
      CHTYP(10) =  'aopt    '
      CHTYP(11) =  'as      '
      CHTYP(12) =  'as:     '
      CHTYP(13) =  'start:  '
      CHTYP(14) =  'start   '
      CHTYP(15) =  'end:    '
      CHTYP(16) =  'end     '
      CHTYP(17) =  'sdir    '

      CALL SMCIL (-1, 'fo:3'  , '=:1')
      CALL SMCIL (-1, 'fo:4'  , '=:2')
      CALL SMCIL (-1, 'co:3'  , '=:1')
      CALL SMCIL (-1, 'co:4'  , '=:2')
      CALL SMCIL (-1, 'ao:2'  , '=:1')
      CALL SMCIL (-1, 'ao:3'  , '=:1')
      CALL SMCIL (-1, 'ao:4'  , '=:2')

      CALL SMCIL (-1, 'fort:3', '=:1')
      CALL SMCIL (-1, 'fort:4', '=:2')
      CALL SMCIL (-1, 'cc:3'  , '=:1')
      CALL SMCIL (-1, 'cc:4'  , '=:2')
      CALL SMCIL (-1, 'as:2'  , '=:1')
      CALL SMCIL (-1, 'as:3'  , '=:1')
      CALL SMCIL (-1, 'as:4'  , '=:2')

      IXOBJ = NA_LONG ('.o')
      CALL SMCIL (-1, 'fo:1'  , '-O')
      CALL SMCIL (-1, 'fopt'  , '-c')
      CALL SMCIL (-1, 'co:1'  , '-O')
      CALL SMCIL (-1, 'copt'  , '-c')

      CALL SMCIL (-1, 'fort:1', '${FC} ${FO_1} ${s}$*.f')
      CALL SMCIL (-1, 'fort:2', '${FC} ${FO_2} ${s}$*.f')
      CALL SMCIL (-1, 'cc:1'  , '${CC} ${CO_1} ${s}$*.c')
      CALL SMCIL (-1, 'cc:2'  , '${CC} ${CO_2} ${s}$*.c')
      CALL SMCIL (-1, 'as:1'  , '${AS} ${AO_1} ${s}$*.s')

      CALL SMCIL (-1, 'fc'    , 'f77')
      CALL SMCIL (-1, 'cc'    , 'cc')
      CALL SMCIL (-1, 'as'    , 'as')
      CALL SMCIL (-1, 'fopt'  , '-c -w66')
      CALL SMCIL (-1, 'as:1'  , '${AS} -o $*.o ${s}$*.s')

      RETURN
      END
