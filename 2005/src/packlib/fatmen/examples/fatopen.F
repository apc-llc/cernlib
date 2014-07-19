*
* $Id: fatopen.F,v 1.1.1.1 1996/03/07 15:17:59 mclareni Exp $
*
* $Log: fatopen.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:59  mclareni
* Fatmen
*
*
      PROGRAM FATOPEN
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
      COMMON /QUEST/IQUEST(100)
*
      PARAMETER       (LKEYFA=10)
      DIMENSION KEY(LKEYFA)
*
*
*     Initialise ZEBRA
*
      CALL MZEBRA(0)
      CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(LURCOR))
      CALL MZLOGL(IXSTOR,-3)
 
*
* *** Define user division and link area like:
*
      CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')
      CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)
*
*     Unit for RZ database
*
      LUNRZ = 1
      LUNFZ = 2
 
*         read mode
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/OPAL',IRC)
      CALL FMLOGL(3)
      CALL FUOPEN
      END
 
      SUBROUTINE FUOPEN
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
     9           ,MFSZFA=MSYWFA)
 
 
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
      COMMON /QUEST/IQUEST(100)
      CHARACTER*8  DDNAME/'FT11F000'/
      CHARACTER*80 GENAME/
     +'//CERN/OPAL/DDST/PASS1/FYZ1/P25/R02354C01'/
      CHARACTER*4  CHOPT
 
      DIMENSION IUHEAD(1000)
      CALL FMLOGL(3)
      LG = LENOCC(GENAME)
*
*     Open options: R - read
*                   N - don't put DSN on stage command
*                   V - verify file size (catalogue versus staged file)
*
      CALL FMOPEN(GENAME(1:LG),
     +'11',LBANK,'RNV',IRC)
      PRINT *,LBANK
      IF(IRC.NE.0) THEN
         PRINT *,'Return code ',IRC,' from FMOPEN'
         GOTO 10
         ENDIF
*
*     Close options: D - drop staging disk (or dismount tape, drop
*                        mini-disk, deassign logical name etc.)
*                    Z - MZDROP bank at LBANK
*
      CALL FMCLOS(GENAME,
     +                   '11',LBANK,'DZ',IRC)
      IF(IRC.NE.0) PRINT *,'Return code ',IRC,' from FMCLOS'
1     CONTINUE
10    CONTINUE
*
      END
 
