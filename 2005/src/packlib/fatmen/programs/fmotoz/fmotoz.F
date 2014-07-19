*
* $Id: fmotoz.F,v 1.1.1.1 1996/03/07 15:17:48 mclareni Exp $
*
* $Log: fmotoz.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:48  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMOTOZ(GENAM,IFLAG)
*
*     This routine calls FOGETx, according to the generic name
*     If IFLAG = 0, send via FMPUT to server
*                1, write directly to RZ file
*
      CHARACTER*20  FILEN
      CHARACTER*240 GNAME
#include "fatmen/fatsys.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fofile.inc"
#include "fatmen/fovars.inc"
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      COMMON /FATUSE/ IDEBFA, IDIVFA, IKDRFA, KOFSFA, KOFUFA, LBFXFA
     +              , LSAVFA, LTOPFA, LBBKFA, LBGNFA, LTDSFA, LBDSFA
     +              , LPRTFA, NTOPFA, LUFZFA, IOUPFA, IOBKFA, IODSFA
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LADDR,LUSRLS
      COMMON/QUEST/IQUEST(100)
      CHARACTER*(*) GENAM
      CHARACTER*16  EXPER
      CHARACTER*1   MEDTYP(4)
      CHARACTER*3   MONTH,MONTHS(12)
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      CHARACTER*8 CHTAG(LKEYFA)
      CHARACTER*10 CHFOR
      DATA CHTAG/'Num.Id.',5*'Fname'
     +          ,'cp.level','loc.code','medium','nm.banks'/
      DATA CHFOR/'IHHHHHIIII'/
 
      DATA MONTHS(1)/'JAN'/,
     +     MONTHS(2)/'FEB'/,
     +     MONTHS(3)/'MAR'/,
     +     MONTHS(4)/'APR'/,
     +     MONTHS(5)/'MAY'/,
     +     MONTHS(6)/'JUN'/,
     +     MONTHS(7)/'JUL'/,
     +     MONTHS(8)/'AUG'/,
     +     MONTHS(9)/'SEP'/,
     +     MONTHS(10)/'OCT'/,
     +     MONTHS(11)/'NOV'/,
     +     MONTHS(12)/'DEC'/
 
      DATA MEDTYP(1)/'D'/,MEDTYP(2)/'C'/,MEDTYP(3)/'T'/,MEDTYP(4)/'X'/
 
      DATA NPROC/0/,NDEL/0/
 
      LTOP   = LENOCC(TOPDIR)
      CALL FMWORD(EXPER,3,'/',GENAM,IRC)
      LENEXP = LENOCC(EXPER)
*
*     Loop until all information has been returned
*
      ITOTAL = 0
1     CONTINUE
 
      CALL FOGET(GENAM,ITOTAL,IRC)
      IF (IRC .LT. 0) THEN
         WRITE(LPRTFA,*) 'Error code ',IRC,' from FOGET'
         RETURN
         ENDIF
 
      DO 10 I=1,ITOTAL
 
      JBIAS = 2
      NCH = LENOCC(GENAME(I))
      CALL FMBOOK(GNAME(1:NCH),KEYS,LSUP,LADDR,JBIAS,IRC)
      GNAME = TOPDIR(1:LTOP)//'/'//
     +        EXPER(1:LENEXP)//'/'//GENAME(I)(1:NCH)
*
*     Find file name
*
      ICH = INDEXB(GENAME(I)(1:NCH),'/')
      FILEN = GENAME(I)(ICH+1:NCH)
*
*     Check delete flag...
*
      IF(ACTIVE(I) .EQ. 'N') THEN
         WRITE(LPRTFA,*) 'Skipping ',GENAME(I)(1:NCH),' (deleted) ...'
         NDEL = NDEL + 1
         GOTO 10
         ENDIF
 
      WRITE(LPRTFA,*) 'Processing ',GENAME(I)(1:NCH)
      NPROC = NPROC + 1
 
#if defined(CERNLIB_OLD)
      IF(IFLAG.EQ.0) THEN
*
*     Pretend we're a disk file - going to change it anyway
*
         CALL FMLIFT(GNAME(1:LENOCC(GNAME)),KEYS,'DISK',' ',IRC)
         CALL FMLINK(GNAME(1:LENOCC(GNAME)),LADDR,' ',IRC)
 
         ELSE
*
*     Lift stand-alone bank
*
         CALL MZBOOK(IDIVFA,LADDR,L,2,'LFAT',0,0,NWDSFA,IODSFA,0)
*
*     Zero/blank it according to I/O characteristic
*
         CALL DZZERO(IDIVFA,LADDR)
 
         ENDIF
#endif
*
*     Now fill it with data from COMMON blocks
*
      IQ(LADDR+MCPLFA) = CPLVL(I)
      IQ(LADDR+MLOCFA) = LOCAT(I)
      CALL UCTOH(FNAME(I),IQ(LADDR+MFQNFA),4,240)
      CALL UCTOH(HNAME(I),IQ(LADDR+MHSNFA),4,8)
      CALL UCTOH(HTYPE(I),IQ(LADDR+MHSTFA),4,16)
      CALL UCTOH(OPSYS(I),IQ(LADDR+MHOSFA),4,12)
      CALL UCTOH(FFORMT(I),IQ(LADDR+MFLFFA),4,4)
      CALL UCTOH(USRFMT(I),IQ(LADDR+MFUTFA),4,4)
      IQ(LADDR+MSRDFA) = SRTREC(I)
      IQ(LADDR+MERDFA) = ENDREC(I)
      IQ(LADDR+MSBLFA) = SRTBLK(I)
      IQ(LADDR+MEBLFA) = ENDBLK(I)
      CALL UCTOH(RECFMT(I),IQ(LADDR+MRFMFA),4,4)
      IQ(LADDR+MRLNFA) = RECLGH(I)
      IQ(LADDR+MBLNFA) = BLKLGH(I)
*
*     date/time are stored in format DD-MON-YY HH:MM
*     for ORACLE. IDATE = YYMMDD, ITIME = HHMM
*
      CALL CLTOU(CREDAT(I))
      READ(CREDAT(I),9001) IDAY,MONTH,IYEAR,IHOU,IMIN
9001  FORMAT(I2,1X,A3,1X,I2,1X,I2,1X,I2)
      IMONTH = ICNTH(MONTH,MONTHS,12)
      IDATE  = IYEAR * 10000 + IMONTH * 100 + IDAY
      ITIME  = IHOU  * 100   + IMIN
      CALL FMPKTM(IDATE,ITIME,IQ(LADDR+MCRTFA),IRC)
 
      CALL CLTOU(CATDAT(I))
      READ(CATDAT(I),9001) IDAY,MONTH,IYEAR,IHOU,IMIN
      IMONTH = ICNTH(MONTH,MONTHS,12)
      IDATE  = IYEAR * 10000 + IMONTH * 100 + IDAY
      ITIME  = IHOU  * 100   + IMIN
      CALL FMPKTM(IDATE,ITIME,IQ(LADDR+MCTTFA),IRC)
 
      CALL CLTOU(ACSDAT(I))
      READ(ACSDAT(I),9001) IDAY,MONTH,IYEAR,IHOU,IMIN
      IMONTH = ICNTH(MONTH,MONTHS,12)
      IDATE  = IYEAR * 10000 + IMONTH * 100 + IDAY
      ITIME  = IHOU  * 100   + IMIN
      CALL FMPKTM(IDATE,ITIME,IQ(LADDR+MLATFA),IRC)
 
      CALL UCTOH(CRENAM(I),IQ(LADDR+MCURFA),4,8)
      CALL UCTOH(CREACC(I),IQ(LADDR+MCIDFA),4,8)
      CALL UCTOH(CRENOD(I),IQ(LADDR+MCNIFA),4,8)
      CALL UCTOH(CREJOB(I),IQ(LADDR+MCJIFA),4,8)
 
 
      IQ(LADDR+MFPRFA)   = PROTEC(I)
      IQ(LADDR+MUSWFA)   = USRWD0(I)
      IQ(LADDR+MUSWFA+1) = USRWD1(I)
      IQ(LADDR+MUSWFA+2) = USRWD2(I)
      IQ(LADDR+MUSWFA+3) = USRWD3(I)
      IQ(LADDR+MUSWFA+4) = USRWD4(I)
      IQ(LADDR+MUSWFA+5) = USRWD5(I)
      IQ(LADDR+MUSWFA+6) = USRWD6(I)
      IQ(LADDR+MUSWFA+7) = USRWD7(I)
      IQ(LADDR+MUSWFA+8) = USRWD8(I)
      IQ(LADDR+MUSWFA+9) = USRWD9(I)
 
      IQ(LADDR+MSYWFA)   = SYSWD0(I)
      IQ(LADDR+MSYWFA+1) = SYSWD1(I)
      IQ(LADDR+MSYWFA+2) = SYSWD2(I)
      IQ(LADDR+MSYWFA+3) = SYSWD3(I)
      IQ(LADDR+MSYWFA+4) = SYSWD4(I)
      IQ(LADDR+MSYWFA+5) = SYSWD5(I)
      IQ(LADDR+MSYWFA+6) = SYSWD6(I)
      IQ(LADDR+MSYWFA+7) = SYSWD7(I)
      IQ(LADDR+MSYWFA+8) = SYSWD8(I)
      IQ(LADDR+MSYWFA+9) = SYSWD9(I)
 
      CALL UCTOH(COMMTS(I),IQ(LADDR+MUCMFA),4,80)
      IQ(LADDR+MMTPFA) = ICNTH(MEDIA(I),MEDTYP,4)
      IQ(LADDR+MFSQFA) = FLSQNO(I)
      IQ(LADDR+MVSQFA) = VLSQNO(I)
      CALL UCTOH(VSN(I),IQ(LADDR+MVSNFA),4,6)
      CALL UCTOH(VID(I),IQ(LADDR+MVIDFA),4,6)
      IQ(LADDR+MVIPFA) = PREF(I)
      IQ(LADDR+MDENFA) = DENS(I)
      CALL VBLANK(KEYS(2),5)
      CALL UCTOH(FILEN,KEYS(2),4,NCH-ICH)
      KEYS(MKCLFA) = IQ(LADDR+MCPLFA)
      KEYS(MKMTFA) = IQ(LADDR+MMTPFA)
      KEYS(MKLCFA) = IQ(LADDR+MLOCFA)
      KEYS(MKNBFA) = LKEYFA
 
      IF(IFLAG.EQ.0) THEN
 
         CALL FMPUT(GNAME,LADDR,IRC)
 
      ELSE
*
*     Check if new directories have to be made
*
         LEND  = INDEXB(GNAME,'/') - 1
         CALL FACDIR (GNAME(1:LEND), ' ')
         IF (IQUEST(1).NE.0)
     +   CALL FATMDI(GNAME(1:LEND),LKEYFA,CHFOR,CHTAG)
         CALL FACDIR(GNAME(1:LEND), ' ')
*        NKEYFA = IQUEST(7)
*        KEYS(1) = NKEYFA + 1
         CALL FMALLK(KEYS(1),IRC)
*
*     Display what we've got so far...
*
*        CALL DZSHOW('SQL->RZ conversion',IDIVFA,LADDR,'L',0,0,0,0)
*
*     Verify bank contents
*
         CALL FMVERI(GNAME(1:NCH),LADDR,KEYS,'A',IRC)
         IF(IRC.NE.0) GOTO 10
*
*     Write new bank to RZ file
*
         ICYCLE = 9999
         CALL RZOUT (IDIVFA, LADDR, KEYS, ICYCLE, 'SW')
         CALL RZSAVE
#if defined(CERNLIB_OLD)
*
*     Drop bank...
*
         CALL MZDROP(IDIVFA,LADDR,'.')
         LADDR = 0
#endif
      ENDIF
 
10    CONTINUE
 
      IF (IRC .EQ. 1) THEN
         ITOTAL = 1
         GOTO 1
         ENDIF
 
      WRITE(LPRTFA,9002) NPROC,NDEL
9002  FORMAT(//' FMOTOZ. Total of ',I5,' entries processed ',
     +         ' including ',I5,' deleted files ')
 
      END
 
