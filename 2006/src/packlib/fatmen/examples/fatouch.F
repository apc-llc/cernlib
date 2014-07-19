*
* $Id: fatouch.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: fatouch.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
*------------------------------------------------------------------------
*
* Program FATOUCH - 'touch' all files in all directories in file
*                   read from unit 3.
*                   FATMEN is initialised automatically after the
*                   first name is read in.
* +USE,TOUCHF. if the names read in a full generic names
* Default is to assume directory names, in which case all files
* in the specified directories are touched.
*------------------------------------------------------------------------
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
#include "fatmen/quest.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fmnkeys.inc"
*
      PARAMETER (MAXFIL=2000)
      DIMENSION KEYS(LKEYFA,MAXFIL)
      CHARACTER*255 FILES(MAXFIL)
      CHARACTER*80  GENAM
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
*     Open input file
*
      NDIRS = 0
      NFILS = 0
#if defined(CERNLIB_IBM)
      OPEN(3,FORM='UNFORMATTED',STATUS='OLD')
#endif
#if !defined(CERNLIB_IBM)
      OPEN(3,FORM='FORMATTED',STATUS='OLD')
#endif
1     CONTINUE
#if defined(CERNLIB_IBM)
      READ(3,NUM=LGN,END=99) GENAM
#endif
#if !defined(CERNLIB_IBM)
      READ(3,'(A)',END=99) GENAM
      LGN = LENOCC(GENAM)
#endif
      IF(NDIRS.EQ.0) THEN
*
*     Initialise FATMEN
*
         LEND = INDEX(GENAM(3:LGN),'/') + 2
         LEND = LEND + INDEX(GENAM(LEND+1:LGN),'/') -1
         CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,GENAM(1:LEND),IRC)
         CALL FMLOGL(0)
      ENDIF
      NDIRS = NDIRS + 1
      PRINT *,'Processing ',GENAM(1:LGN)
*
*     Get list of file names
*
      ICONT = 0
20    CONTINUE
#if defined(CERNLIB_TOUCHF)
*
*     To touch individual files
*
      CALL FMLFIL(GENAM(1:LGN),
#endif
#if !defined(CERNLIB_TOUCHF)
*
*     To touch directories (i.e. all files in the specified dirs)
*
      CALL FMLFIL(GENAM(1:LGN)//'/*',
#endif
     +FILES,KEYS,NFOUND,MAXFIL,ICONT,IRC)
 
      NFILS = NFILS + NFOUND
      DO 10 I=1,NFOUND
      LENF = LENOCC(FILES(I))
      LBANK1 = 0
      PRINT 9001,FILES(I)(1:LENF),KEYS(1,I)
      CALL FMGETK(FILES(I)(1:LENF),LBANK,KEYS(1,I),IRC)
      CALL FMMOD(FILES(I)(1:LENF),LBANK,0,IRC)
      CALL MZDROP(IXSTOR,LBANK,' ')
9001  FORMAT(1X,A,I10)
10    CONTINUE
      IF(ICONT.NE.0) GOTO 20
*
*     Any more directories?
*
      GOTO 1
99    CLOSE(3)
#if defined(CERNLIB_TOUCHF)
      PRINT *,'Processed ',NDIRS,' generic names and ',
     +   NFILS,' FATMEN entries'
#endif
#if !defined(CERNLIB_TOUCHF)
      PRINT *,'Processed ',NDIRS,' directory names and ',
     +   NFILS,' file names'
#endif
*
*     Terminate cleanly
*
      CALL FMEND(IRC)
 
      END
