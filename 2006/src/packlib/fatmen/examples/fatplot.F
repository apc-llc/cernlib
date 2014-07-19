*
* $Id: fatplot.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: fatplot.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
*=======================================================================
*
*  Example of using HBOOK to plot various FATMEN catalogue values.
*  This program histograms the file size, number of days since last
*  access, medium type etc.
*=======================================================================
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
      COMMON /QUEST/IQUEST(100)
      CHARACTER*8   THRONG
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
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
      CALL GETENVF('THRONG',THRONG)
      LTH = LENOCC(THRONG)
*
*     Initialise FATMEN
*
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/'//THRONG(1:LTH),IRC)
      CALL FMLOGL(0)
*
*     Initialise HBOOK
*
      CALL HLIMIT(-20000)
*
*     Book histograms
*
      CALL HBOOK1(1,'File Size (MB)',50,0.,200.,0.)
      CALL HBOOK1(2,'Number of accesses',50,0.,50.,0.)
      CALL HBOOK1(3,'Number days since last access',50,0.,300.,0.)
      CALL HBOOK1(4,'Number days since catalogued',50,0.,300.,0.)
      CALL HBOOK1(5,'Number days since created',50,0.,300.,0.)
      CALL HBOOK1(6,'Medium',5,0.,5.,0.)
      CALL HIDOPT(0,'BLAC')
*
*     Loop over all files
*
      CALL FMLOOP('//CERN/*/*',-1,UROUT,IRC)
*
*     Print and store the histograms
*
      CALL HPRINT(0)
      CALL HRPUT(0,'FATTUPLE.'//THRONG(1:LTH),'N')
*
*     Terminate cleanly
*
      CALL FMEND(IRC)
      END
 
      SUBROUTINE UROUT(PATH,KEYS,IRC)
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
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      CHARACTER*(*) PATH
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYS(LKEYFA)
      DIMENSION     NDAYS(3)
      COMMON/QUEST/IQUEST(100)
      IRC   = 0
      LBANK = 0
      LP    = LENOCC(PATH)
      CALL FMGETK(PATH(1:LP),LBANK,KEYS,IRC)
*
*     Fill histograms
*
      IF(IQ(LBANK+MFSZFA).NE.0)
     +CALL HFILL(1,FLOAT(IQ(LBANK+MFSZFA)),0.,1.)
      IF(IQ(LBANK+MUSCFA).NE.0)
     +CALL HFILL(2,FLOAT(IQ(LBANK+MUSCFA)),0.,1.)
      CALL FMDAYS(PATH(1:LP),LBANK,KEYS,NDAYS,' ',IRC)
      CALL HFILL(3,FLOAT(NDAYS(3)),0.,1.)
      CALL HFILL(4,FLOAT(NDAYS(2)),0.,1.)
      CALL HFILL(5,FLOAT(NDAYS(1)),0.,1.)
      CALL HFILL(6,FLOAT(IQ(LBANK+MMTPFA)),0.,1.)
      CALL MZDROP(IXSTOR,LBANK,' ')
      END
