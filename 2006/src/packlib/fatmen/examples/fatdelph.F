*
* $Id: fatdelph.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: fatdelph.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
*=======================================================================
*
*  Program to copy DELPHI FATMEN catalogue, suppressing L0nnn level
*  This level, indicating the LEP fill number, is stored in user word 5
*
*=======================================================================
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
      COMMON /QUEST/IQUEST(100)
      PARAMETER (MAXDIR=100)
      CHARACTER*255 CHDIR(MAXDIR),CHSAVE,CHOLD
      CHARACTER*20  FNAME
*
* Start of FATMEN sequence FATPARA
*
** ***     Data set bank mnemonics
*
*          Keys
      PARAMETER ( MKSRFA= 1, MKFNFA= 2, MKCLFA=7, MKMTFA=8
     1           ,MKLCFA= 9, MKNBFA=10, NKDSFA=10 )
*
** ***     Bank offsets
*
      PARAMETER ( MFQNFA=  1, MHSNFA= 65, MCPLFA= 67, MMTPFA= 68
     1           ,MLOCFA= 69, MHSTFA= 70, MHOSFA= 74
     2           ,MVSNFA= 77, MVIDFA= 79, MVIPFA= 81, MDENFA= 82
     3           ,MVSQFA= 83, MFSQFA= 84, MSRDFA= 85, MERDFA= 86
     4           ,MSBLFA= 87, MEBLFA= 88, MRFMFA= 89, MRLNFA= 90
     5           ,MBLNFA= 91, MFLFFA= 92, MFUTFA= 93, MCRTFA= 94
     6           ,MCTTFA= 95, MLATFA= 96, MCURFA= 97, MCIDFA= 99
     7           ,MCNIFA=101, MCJIFA=103, MFPRFA=105, MSYWFA=106
     8           ,MUSWFA=116, MUCMFA=126, NWDSFA=145
     9           ,MFSZFA=MSYWFA,MUSCFA=MSYWFA+1)
 
* End of FATMEN sequence FATPARA
*KEEP,FATBUG.
      COMMON /FATUSE/ IDEBFA, IDIVFA, IKDRFA, KOFSFA, KOFUFA, LBFXFA
     +              , LSAVFA, LTOPFA, LBBKFA, LBGNFA, LTDSFA, LBDSFA
     +              , LPRTFA, NTOPFA, LUFZFA, IOUPFA, IOBKFA, IODSFA
     +              , LLNLFA, LLNHFA
*KEND.
      COMMON /MZCC/  LQPSTO,NQPFEN,NQPSTR,NQPREF,NQPLK,NQPMIN,LQP2E
     +,              JQPDVL,JQPDVS,NQPLOG,NQPNAM(6)
     +,              LQSYSS(10), LQSYSR(10), IQTDUM(22)
     +,              LQSTA(21), LQEND(20), NQDMAX(20),IQMODE(20)
     +,              IQKIND(20),IQRTO(20), IQRNO(20), NQDINI(20)
     +,              NQDWIP(20),NQDGAU(20),NQDGAF(20),NQDPSH(20)
     +,              NQDRED(20),NQDSIZ(20),IQDN1(20), IQDN2(20)
     +,              KQFT, LQFSTA(21)
                                       DIMENSION    IQTABV(16)
                                       EQUIVALENCE (IQTABV(1),LQPSTO)
C
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
                   EQUIVALENCE (LQRS,LQSYSS(7))
C
      PARAMETER (NLPATM=100)
      COMMON /RZDIRN/NLCDIR,NLNDIR,NLPAT
      COMMON /RZDIRC/CHCDIR(NLPATM),CHNDIR(NLPATM),CHPAT(NLPATM)
      CHARACTER*16   CHNDIR,    CHCDIR,    CHPAT
C
      COMMON /RZCH/  CHWOLD,CHL
      CHARACTER*128  CHWOLD,CHL
C
      PARAMETER (KUP=5,KPW1=7,KNCH=9,KDATEC=10,KDATEM=11,KQUOTA=12,
     +           KRUSED=13,KWUSED=14,KMEGA=15,KIRIN=17,KIROUT=18,
     +           KRLOUT=19,KIP1=20,KNFREE=22,KNSD=23,KLD=24,KLB=25,
     +           KLS=26,KLK=27,KLF=28,KLC=29,KLE=30,KNKEYS=31,
     +           KNWKEY=32,KKDES=33,KNSIZE=253,KEX=6,KNMAX=100)
 
      PARAMETER    (JBIAS=2)
      PARAMETER    (ICYCLE=999)
      PARAMETER    (LKEYFA=10)
      DIMENSION    KEYS(LKEYFA)
      CHARACTER*8  CHTAG(LKEYFA)
      CHARACTER*10 CHFOR
      DATA CHTAG/'Num.Id.',5*'Fname'
     +          ,'cp.level','loc.code','medium','nm.banks'/
      DATA CHFOR/'IHHHHHIIII'/
*
*
*     Initialise ZEBRA
*
      CALL MZEBRA(-3)
      CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(LURCOR))
      CALL MZLOGL(IXSTOR,-3)
 
*
* *** Define user division and link area like:
*
      CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')
      CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)
*
*     Units for FATMEN RZ/FZ files
*
      LUNRZ = 1
      LUNFZ = 2
*
*     Initialise FATMEN
*
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/DELPHI',IRC)
*
*     Set log level
*
      CALL FMLOGL(1)
*
*     Create output RZ file
*
      CALL RZOPEN(20,'//KERN','/fatmen/fmdelphi/delphi.fatrz',
     +   'N1',1024,IRC)
      CALL RZMAKE (20,'KERN',LKEYFA,CHFOR,CHTAG,65000,' ')
      CALL RZMDIR('DELPHI',LKEYFA,CHFOR,CHTAG)
*
*     Get list of directories in input file
*
      NTOTAL = 0
      K      = 0
      NOBJ   = 0
      ICONT  = 0
 
10    CONTINUE
 
      CALL FMLDIR('//CERN/DELPHI/*',
     +CHDIR,NFOUND,MAXDIR,ICONT,IRC)
      IF(IDEBFA.GE.3) PRINT *,NFOUND,' directories found'
      IF(IRC.NE.0.AND.IDEBFA.GT.0)
     +   PRINT *,'return code ',IRC,' from FMLDIR'
 
      CALL RZCDIR(CHOLD,'R')
      LOLD   = LENOCC(CHOLD)
 
      NTOTAL = NTOTAL + NFOUND
 
      DO 20 J=1,NFOUND
      LEND   = LENOCC(CHDIR(J))
      IF(IDEBFA.GE.3) PRINT *,'Processing ',CHDIR(J)(1:LEND)
*
*     Exclusion list:
*
      IF(INDEX(CHDIR(J)(1:LEND),
     +   '//CERN/DELPHI/P01_ALLD/CDST/LEPT/Y91V02').NE.0) GOTO 20
      IF(INDEX(CHDIR(J)(1:LEND),
     +   '//CERN/DELPHI/P01_ALLD/CDST/LEPT/Y91V03').NE.0) GOTO 20
      IF(INDEX(CHDIR(J)(1:LEND),
     +   '//CERN/DELPHI/P01_ALLD/CDST/LEPT/Y91V04').NE.0) GOTO 20
      IF(INDEX(CHDIR(J)(1:LEND),
     +   '//CERN/DELPHI/P01_ALLD/CDST/S2PR/Y91V02').NE.0) GOTO 20
      IF(INDEX(CHDIR(J)(1:LEND),
     +   '//CERN/DELPHI/P01_ALLD/CDST/S2PR/Y91V03').NE.0) GOTO 20
      IF(INDEX(CHDIR(J)(1:LEND),
     +   '//CERN/DELPHI/P01_ALLD/CDST/S2PR/Y91V04').NE.0) GOTO 20
      IF(INDEX(CHDIR(J)(1:LEND),
     +   '//CERN/DELPHI/P01_ALLD/DSTO/LEPT/Y91V03').NE.0) GOTO 20
      IF(INDEX(CHDIR(J)(1:LEND),
     +   '//CERN/DELPHI/P01_ALLD/DSTO/PHYS/Y91V02').NE.0) GOTO 20
      IF(INDEX(CHDIR(J)(1:LEND),
     +   '//CERN/DELPHI/P01_ALLD/DSTO/PHYS/Y91V04').NE.0) GOTO 20
*
      CALL RZCDIR(CHSAVE,'R')
      LSAVE = LENOCC(CHSAVE)
*
*     Skip all L0* directories
*
      LSLASH = INDEXB(CHDIR(J)(1:LEND),'/')
      IF(CHDIR(J)(LSLASH:LSLASH+2).EQ.'/L0') GOTO 30
      K = K + 1
*
*     Convert CDST to MDST
*
      LDST = INDEX(CHDIR(J)(1:LEND),'CDST')
      IF(LDST.NE.0) CHDIR(J)(LDST:LDST) = 'M'
      IF(IDEBFA.GE.2) PRINT *,'Creating directory # ',K,CHDIR(J)(1:LEND)
      CHDIR(J)(1:6) = '//KERN'
 
      CALL RZCDIR(CHDIR(J)(1:LSLASH-1),' ')
      CALL RZMDIR(CHDIR(J)(LSLASH+1:LEND),LKEYFA,CHFOR,CHTAG)
      CALL RZCDIR(CHSAVE(1:LSAVE),' ')
      GOTO 20
30    CONTINUE
*
*     Process all files in this directory - copy to output file
*
      IF(CHDIR(J)(LSLASH:LSLASH+2).EQ.'/L0') THEN
         READ(CHDIR(J)(LSLASH+1:LEND),'(1X,I4)') LEVEL
      ELSE
         LEVEL = -1
      ENDIF
 
      IF(IDEBFA.GE.3) PRINT *,'Processing data directory ',
     +   CHDIR(J)(1:LEND)
      CALL RZCDIR(CHDIR(J)(1:LEND),' ')
      NWK        = IQ(KQSP+LCDIR+KNWKEY)
      NKEYS      = IQ(KQSP+LCDIR+KNKEYS)
      LK         = IQ(KQSP+LCDIR+KLK)
 
      DO 40 I=1,NKEYS
*
*     Number of this key vector
*
         KK=LK+(NWK+1)*(I-1)
         DO 50 JJ=1,NWK
            IKDES=(JJ-1)/10
            IKBIT1=3*JJ-30*IKDES-2
            IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
               KEYS(JJ)=IQ(KQSP+LCDIR+KK+JJ)
            ELSE
               CALL ZITOH(IQ(KQSP+LCDIR+KK+JJ),KEYS(JJ),1)
            ENDIF
   50    CONTINUE
         CALL UHTOC(KEYS(MKFNFA),4,FNAME,20)
         LF = LENOCC(FNAME)
         CALL RZIN(IXDIV,LUSRLS,JBIAS,KEYS,ICYCLE,'D')
         IF(IQUEST(1).NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'return code from RZIN = ',IQUEST(1)
            GOTO 40
         ENDIF
         IF(LEVEL.GT.0) IQ(LUSRLS+MUSWFA+4) = LEVEL
         CHDIR(J)(1:6) = '//KERN'
         LDST = INDEX(CHDIR(J)(1:LEND),'CDST')
         IF(LDST.NE.0) CHDIR(J)(LDST:LDST) = 'M'
         CALL RZCDIR(CHDIR(J)(1:LSLASH-1),' ')
         CALL RZOUT(IXDIV,LUSRLS,KEYS,ICYC,'SW')
         CHDIR(J)(1:6) = '//CERN'
         IF(LDST.NE.0) CHDIR(J)(LDST:LDST) = 'C'
         CALL RZCDIR(CHDIR(J)(1:LEND),' ')
         NOBJ = NOBJ + 1
         IF(MOD(NOBJ,100).EQ.0.AND.IDEBFA.GE.2)
     +      PRINT *,'Processed ',NOBJ,' objects ,cwd = ',
     +      CHDIR(J)(1:LEND)
40    CONTINUE
20    CONTINUE
      IF(IRC.EQ.-1) THEN
         ICONT = 1
         CALL RZCDIR(CHOLD(1:LOLD),' ')
         GOTO 10
      ENDIF
 
      CALL RZEND('KERN')
      CALL FMEND(IRC)
      PRINT *,'Total of ',NTOTAL,' directories found in input file'
      PRINT *,'Total of ',K,' directories created in output file'
 
      END
 
