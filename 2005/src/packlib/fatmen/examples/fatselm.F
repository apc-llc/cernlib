*
* $Id: fatselm.F,v 1.1.1.1 1996/03/07 15:17:59 mclareni Exp $
*
* $Log: fatselm.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:59  mclareni
* Fatmen
*
*
*=======================================================================
*
*  Example of using the FMSELM routine.
*  This routine performs selection according to a user specified
*  selection matrix.
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
      CHARACTER*255 CHDIR(MAXDIR)
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
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA),KEYM(LKEYFA,2)
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
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/OPAL',IRC)
      CALL FMLOGL(3)
 
      KEYM(MKMTFA,1) = 1
      KEYM(MKCLFA,1) = -1
      KEYM(MKLCFA,1) = 31
      KEYM(MKMTFA,2) = 2
      KEYM(MKCLFA,2) = -1
      KEYM(MKLCFA,2) = 1
 
      NK = 2
 
      CALL FMSELM('//CERN/OPAL/DDST/PASS3/FYZ1/P20/R02222C01',
     +   LBANK,KEYS,KEYM,NK,'NM',IRC)
 
      PRINT *,'Return code from FMSELM = ',IRC
 
      IF(IRC.EQ.0)
     +   CALL FMSHOW('//CERN/OPAL/DDST/PASS3/FYZ1/P20/R02222C01',
     +                LBANK,KEYS,'A',IRC)
 
      END
 
