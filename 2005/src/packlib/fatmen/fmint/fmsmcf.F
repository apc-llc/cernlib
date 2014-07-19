*
* $Id: fmsmcf.F,v 1.1.1.1 1996/03/07 15:18:10 mclareni Exp $
*
* $Log: fmsmcf.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:10  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSMCF(GENAME,LBANK,IRC)
*
*     Routine to make a copy of the dataset STAGEd in into the robot
*     using FMCOPY option 'S' (STAGE CHANGE)
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      CHARACTER*(*) GENAME
      CHARACTER*6   VSN,VID
      CHARACTER*15  XVID
      CHARACTER*8     VIP
      CHARACTER*8   CHACC
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA),KEYSR(LKEYFA)
#include "fatmen/fmaxcop.inc"
      DIMENSION KEYSOU(LKEYFA,MAXCOP),KEYSIN(LKEYFA)
      INTEGER   FMACNT
#include "fatmen/tmsdef.inc"
*
      LGN = LENOCC(GENAME)
      IRC = 0
      IF(IDEBFA.GE.3) THEN
         PRINT *,'FMSMCF. input bank...'
         CALL FMSHOW(GENAME(1:LGN),LBANK,KEYS,'A',IC)
      ENDIF
*
*     Save old bank address
*
      LOLDFA = LBANK
      LTDSFA = 0
*
*     First, check that a robot copy does not already exist
*
      CALL UCOPY(KEYS,KEYSIN,10)
*
*     Don't compare copy level or location code
*
      KEYSIN(MKCLFA) = -1
      KEYSIN(MKLCFA) = -1
*
*     Restrict search to 3480s
*
      KEYSIN(MKMTFA) =  2
      CALL FMSELK(GENAME(1:LGN),KEYSIN,KEYSOU,NMATCH,MAXCOP,IRET)
*     IF(IDEBFA.GE.2)
      IF(IDEBFA.GE.0)
     +PRINT *,'FMSMCF. found ',nmatch,' matches for media type 2'
      DO 10 I=1,NMATCH
         CALL FMGETK(GENAME(1:LGN),LBANKR,KEYSOU(1,I),IC)
         IF(IC.NE.0) THEN
            PRINT *,'FMSMCF. error in FMGETK for candidate # ',I
            GOTO 10
         ENDIF
         PRINT *,'FMSMCF. candidate # ',I
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMSMCF. candidate # ',I
            CALL FMSHOW(GENAME(1:LGN),LBANKR,KEYS,'A',IC)
         ENDIF
         CALL UHTOC(IQ(LBANKR+KOFUFA+MVIDFA),4,VID,6)
         LVID = LENOCC(VID)
         IF(IDEBFA.GE.0) PRINT *,'FMSMCF. candidate # ',I,' VID = ',
     +   VID(1:LVID)
#if defined(CERNLIB_PREFIX)
         CALL FMXVID(VID,IQ(LBANKR+KOFUFA+MVIPFA),XVID,VIP,'C',IC)
         LXVID = LENOCC(XVID)
         CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if !defined(CERNLIB_PREFIX)
         CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
         IF(IDEBFA.GE.0) PRINT *,'FMSMCF. candidate # ',I,' MNTTYP = ',
     +   MNTTYP
 
         IF(MNTTYP.EQ.'R') THEN
            IF(IDEBFA.GE.0) PRINT *,'FMSMCF. robot copy already '
     +      //'exists'
            IRC = 1
            RETURN
         ENDIF
 
   10 CONTINUE
*
*     Lift new bank for the robot copy
*
*     CALL FMLIFT(GENAME(1:LGN),KEYSR,'DISK',' ',IRC)
*     CALL FMLINK(GENAME(1:LGN),LBANKR,' ',IRC)
      JBIAS = 2
      LSUP  = 0
      CALL FMBOOK(GENAME(1:LGN),KEYSR,LBANKR,LSUP,JBIAS,IRC)
*
*     Blindly copy old bank into new...
*
      CALL UCOPY(IQ(LOLDFA+KOFUFA+MFQNFA),IQ(LBANKR+KOFUFA+MFQNFA),
     +           NWDSFA)
*
*     and the keys...
*
      CALL UCOPY(KEYS,KEYSR,10)
*
*     Set last access date, date of cataloging and use count
*
      CALL DATIME(IDATE,ITIME)
      CALL FMPKTM(IDATE,ITIME,IPACK,IRC)
      IQ(LBANKR+KOFUFA+MCTTFA) = IPACK
      IQ(LBANKR+KOFUFA+MLATFA) = IPACK
      IQ(LBANKR+KOFUFA+MUSCFA) = 1
*
*     Now, allocate new tape
*
      IC = FMACNT(CHACC)
*
*     SMCF_1 changed to 3495_2 March 1993
*
      CALL FMALLO('3480','38K',' ','3495_2',CHACC(5:6)//'_FAT1',
     +LBANKR,' ',VSN,VID,IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMSMCF. Cannot allocate robot tape'
         RETURN
      ELSE
         IF(IDEBFA.GE.0) PRINT *,'FMSMCF. allocated ',VSN,' ',VID,
     +                           ' (VSN/VID)'
      ENDIF
*
*     Do the copy
*
      CALL FMCOPY(GENAME,LOLDFA,KEYS,GENAME,LBANKR,KEYSR,'ACSW',IRC)
      IF(IRC.NE.0) PRINT *,'FMSMCF. return code ',IRC,' from FMCOPY'
*
*     Restore bank address
*
      LBANK = LOLDFA
      END
