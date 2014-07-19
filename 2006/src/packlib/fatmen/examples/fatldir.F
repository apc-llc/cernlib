*
* $Id: fatldir.F,v 1.1.1.1 1996/03/07 15:17:59 mclareni Exp $
*
* $Log: fatldir.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:59  mclareni
* Fatmen
*
*
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
      COMMON /QUEST/IQUEST(100)
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
      CHARACTER*6  DENS
      CHARACTER*8  LIB
      CHARACTER*4  LABTYP
      CHARACTER*1  MNTTYP
      CHARACTER*8  MODEL
      CHARACTER*7  ROBMAN(2)
      DATA         ROBMAN(1)/'-Robot '/,ROBMAN(2)/'-Manual'/
      PARAMETER (LKEYFA=10)
      PARAMETER (MAXKEY=5000)
      PARAMETER (MAXDIR=500)
      DIMENSION KEYS(LKEYFA,MAXDIR)
      CHARACTER*255 CHDIR(MAXDIR)
      CHARACTER*255 CHFILE(MAXKEY)
      CHARACTER*8   THRONG
      CHARACTER*255 TOPDIR
      CHARACTER*26  CHOPT
      CHARACTER*8   DSN
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
      CALL FMLOGL(0)
*
*     Get list of file names
*
      CALL FMLDIR('//CERN/DELPHI/*',
     +CHDIR,NFOUND,MAXDIR,ICONT,IRC)
      PRINT *,NFOUND,' directories found'
 
      NFILES = 0
      DO 10 I=1,NFOUND
      LEND = LENOCC(CHDIR(I))
      PRINT *,'Directory ',CHDIR(I)(1:LEND)
      CALL FMFNMS(CHDIR(I)(1:LEND),CHFILE,KEYS,NKEYS,MAXKEY,IRC)
      NFILES = NFILES + NKEYS
      DO 20 J=1,NKEYS
      PRINT *,CHFILE(J)(1:LENOCC(CHFILE(J)))
20    CONTINUE
10    CONTINUE
      PRINT *,'Total of ',NFILES,' files in ',NFOUND,' directories'
*
*     Terminate cleanly
*
      CALL FMEND(IRC)
 
      END
 
