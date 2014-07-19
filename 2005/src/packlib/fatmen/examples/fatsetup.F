*
* $Id: fatsetup.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: fatsetup.F,v $
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
      PARAMETER       (LKEYFA=10)                                       FAT00100
      DIMENSION KEYS(LKEYFA)                                            FAT00110
*                                                                       FAT00120
*                                                                       FAT00130
*     Initialise ZEBRA                                                  FAT00140
*                                                                       FAT00150
      CALL MZEBRA(-3)                                                   FAT00160
      CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),   FAT00170
     +            BLVECT(5000),BLVECT(LURCOR))                          FAT00180
      CALL MZLOGL(IXSTOR,-3)                                            FAT00190
                                                                        FAT00200
*                                                                       FAT00210
* *** Define user division and link area like:                          FAT00220
*                                                                       FAT00230
      CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')          FAT00240
      CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)          FAT00250
*                                                                       FAT00260
*     Unit for RZ database                                              FAT00270
*                                                                       FAT00280
      LUNRZ = 1                                                         FAT00290
      LUNFZ = 2                                                         FAT00300
                                                                        FAT00310
*         read mode                                                     FAT00320
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/CNDIV',IRC)                FAT00330
      CALL FMLOGL(3)                                                    FAT00340
*
*     Use Tape directly (i.e. issue SETUP rather than STAGE)
*
      CALL FMOPEN('//CERN/CNDIV/JAMIE/TEST2','10',LBANK,'RTU',IRC)      FAT00350
      PRINT *,'Return code from FMOPEN = ',IRC                          FAT00360
      END                                                               FAT00380
                                                                        FAT00390
