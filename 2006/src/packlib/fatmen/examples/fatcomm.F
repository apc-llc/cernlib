*
* $Id: fatcomm.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: fatcomm.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
*
*     Look through catalogue looking for generic names
*     which have more than one entry. For each of these
*     cases, update the comment string using the comment
*     string from the first entry with a non-blank comment
*
*     Illustrates the use of:
*        FMSCAN to loop over directories
*        FMUNIQ to return a list of unique names in a directory
*        FMEXST to count the number of entries of a given generic name
*        FMFILS to retrieve the keys vectors for given generic name
*        FMGETK to get the bank corresponding to a given generic name &
*               keys vector combination
*        FMGETC to read a string from a bank
*        FMPUTC to put a string into a bank
*        FMMOD  to update the catalogue
*
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
      CHARACTER*8   DSN
      EXTERNAL      UROUT
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
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/NA44',IRC)
      CALL FMLOGL(0)
 
      CALL FMSCAN('//CERN/NA44/*',-1,UROUT,IRC)
      END
 
      SUBROUTINE UROUT(PATH,IRC)
      CHARACTER*(*) PATH
      PARAMETER     (MAXFIL=1000)
      PARAMETER     (MAXCOP=10)
      CHARACTER*255 FILES(MAXFIL),COPIES(MAXCOP),COMM,CHCOMM
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYSIN(LKEYFA),KEYS(LKEYFA,MAXCOP)
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      COMMON/QUEST/IQUEST(100)
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
 
      PARAMETER  (NUCMFA = (NWDSFA-MUCMFA)*4)
 
      PARAMETER ( MKSRFA= 1, MKFNFA= 2, MKCLFA=7, MKMTFA=8
     1           ,MKLCFA= 9, MKNBFA=10, NKDSFA=10 )
      LP = LENOCC(PATH)
      PRINT *,'Processing ',PATH(1:LENOCC(PATH))
      IRC = 0
*
*     Any files in this directory?
*
      IF(IQUEST(14).EQ.0) RETURN
*
*     Get list of unique file names in this directory
*
      KEYSIN(MKMTFA) = -1
      KEYSIN(MKLCFA) = -1
      KEYSIN(MKCLFA) = -1
      CALL FMUNIQ(PATH(1:LP),KEYSIN,FILES,MAXFIL,NFILES,' ',IRC)
      PRINT *,NFILES,' files found in ',PATH(1:LP)
      DO 10 I=1,NFILES
*
*     Find those files for which more than one entry exists
*
         CALL FMEXST(FILES(I),NCOPIES)
         IF(NCOPIES.LT.2) GOTO 10
         LENF = LENOCC(FILES(I))
         PRINT *,NCOPIES,' copies found for ',
     +      FILES(I)(1:LENF)
*
*     Now get the comment (if any)
*
          COMM = ' '
          CALL FMFILS(FILES(I)(1:LENF),COPIES,KEYS,NCOPY,MAXCOP,
     +    ICONT,IRC)
          LC   = 0
          DO 20 J=1,NCOPY
             CALL FMGETK(COPIES,LBANK,KEYS(1,J),IRC)
             IF(LC.EQ.0) THEN
                CALL FMGETC(LBANK,COMM,MUCMFA,NUCMFA,IRC)
                LC = LENOCC(COMM)
                IF(LC.GT.0) THEN
                   PRINT *,'Comment found for ',FILES(I)(1:LENF),
     +                     ' copy # ',J
                   PRINT *,COMM(1:LC)
                   IF(J.GT.1) PRINT *,'*** WARNING *** comment ',
     +                'field not updated for previous copies!'
                ENDIF
             ELSE
*
*     Check that a comment does not already exist for this copy...
*
                CALL FMGETC(LBANK,CHCOMM,MUCMFA,NUCMFA,IRC)
                LCH = LENOCC(CHCOMM)
                IF(LCH.EQ.0) THEN
*
*     Set the comment and update...
*
                   CALL FMPUTC(LBANK,COMM,MUCMFA,LC,IRC)
                   CALL FMSHOW(FILES(I)(1:LENF),LBANK,KEYS(1,J),
     +                            'CG',IRC)
                   CALL FMMOD(FILES(I)(1:LENF),LBANK,1,IRC)
                ENDIF
             ENDIF
             CALL MZDROP(IXSTOR,LBANK,' ')
20    CONTINUE
10    CONTINUE
      END
