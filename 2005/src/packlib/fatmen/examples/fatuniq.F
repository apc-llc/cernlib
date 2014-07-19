*
* $Id: fatuniq.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: fatuniq.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
      PARAMETER (LURCOR=200000)                                         FAT00010
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)       FAT00020
      DIMENSION    LQ(999),IQ(999),Q(999)                               FAT00030
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)                        FAT00040
*                                                                       FAT00050
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS                               FAT00060
*                                                                       FAT00070
      COMMON /QUEST/IQUEST(100)                                         FAT00080
*                                                                       FAT00090
* Start of FATMEN sequence FATPARA                                      FAT00100
*                                                                       FAT00110
** ***     Data set bank mnemonics                                      FAT00120
*                                                                       FAT00130
*          Keys                                                         FAT00140
      PARAMETER ( MKSRFA= 1, MKFNFA= 2, MKCLFA=7, MKMTFA=8              FAT00150
     1           ,MKLCFA= 9, MKNBFA=10, NKDSFA=10 )                     FAT00160
*                                                                       FAT00170
** ***     Bank offsets                                                 FAT00180
*                                                                       FAT00190
      PARAMETER ( MFQNFA=  1, MHSNFA= 65, MCPLFA= 67, MMTPFA= 68        FAT00200
     1           ,MLOCFA= 69, MHSTFA= 70, MHOSFA= 74                    FAT00210
     2           ,MVSNFA= 77, MVIDFA= 79, MVIPFA= 81, MDENFA= 82        FAT00220
     3           ,MVSQFA= 83, MFSQFA= 84, MSRDFA= 85, MERDFA= 86        FAT00230
     4           ,MSBLFA= 87, MEBLFA= 88, MRFMFA= 89, MRLNFA= 90        FAT00240
     5           ,MBLNFA= 91, MFLFFA= 92, MFUTFA= 93, MCRTFA= 94        FAT00250
     6           ,MCTTFA= 95, MLATFA= 96, MCURFA= 97, MCIDFA= 99        FAT00260
     7           ,MCNIFA=101, MCJIFA=103, MFPRFA=105, MSYWFA=106        FAT00270
     8           ,MUSWFA=116, MUCMFA=126, NWDSFA=145                    FAT00280
     9           ,MFSZFA=MSYWFA,MUSCFA=MSYWFA+1)                        FAT00290
                                                                        FAT00300
* End of FATMEN sequence FATPARA                                        FAT00310
      CHARACTER*6  DENS                                                 FAT00320
      CHARACTER*8  LIB                                                  FAT00330
      CHARACTER*4  LABTYP                                               FAT00340
      CHARACTER*1  MNTTYP                                               FAT00350
      CHARACTER*8  MODEL                                                FAT00360
      CHARACTER*7  ROBMAN(2)                                            FAT00370
      DATA         ROBMAN(1)/'-Robot '/,ROBMAN(2)/'-Manual'/            FAT00380
      PARAMETER (LKEYFA=10)                                             FAT00390
      PARAMETER (MAXFIL=1000)                                           FAT00400
      DIMENSION KEYSIN(LKEYFA)                                          FAT00410
      CHARACTER*255 FILES(MAXFIL)                                       FAT00420
      CHARACTER*8   THRONG                                              FAT00430
      CHARACTER*26  CHOPT                                               FAT00440
*                                                                       FAT00450
*                                                                       FAT00460
*     Initialise ZEBRA                                                  FAT00470
*                                                                       FAT00480
      CALL MZEBRA(-3)                                                   FAT00490
      CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),   FAT00500
     +            BLVECT(5000),BLVECT(LURCOR))                          FAT00510
      CALL MZLOGL(IXSTOR,-3)                                            FAT00520
                                                                        FAT00530
*                                                                       FAT00540
* *** Define user division and link area like:                          FAT00550
*                                                                       FAT00560
      CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')          FAT00570
      CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)          FAT00580
*                                                                       FAT00590
*     Units for FATMEN RZ/FZ files                                      FAT00600
*                                                                       FAT00610
      LUNRZ = 1                                                         FAT00620
      LUNFZ = 2                                                         FAT00630
*                                                                       FAT00640
*     Initialise FATMEN                                                 FAT00650
*                                                                       FAT00660
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/NA31',IRC)                 FAT00670
      CALL FMLOGL(3)                                                    FAT00680
*                                                                       FAT00690
*     Get list of unique file names                                     FAT00700
*                                                                       FAT00710
      KEYSIN(MKMTFA) = -1                                               FAT00720
      KEYSIN(MKCLFA) = -1                                               FAT00730
      KEYSIN(MKLCFA) = -1                                               FAT00740
      CALL FMUNIQ('//CERN/NA31/883/MIN8',KEYSIN,FILES,MAXFIL,NMATCH,    FAT00750
     +            ' ',IC)                                               FAT00760
      IF(IC.NE.0) PRINT *,'Return code ',IC,' from FMUNIQ'              FAT00770
      DO 10 I=1,NMATCH                                                  FAT00780
      PRINT *,FILES(I)(1:LENOCC(FILES(I)))                              FAT00790
10    CONTINUE                                                          FAT00800
*                                                                       FAT00810
*     Terminate cleanly                                                 FAT00820
*                                                                       FAT00830
      CALL FMEND(IRC)                                                   FAT00840
                                                                        FAT00850
      END                                                               FAT00860
                                                                        FAT00870
