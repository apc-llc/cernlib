*
* $Id: fmqvol.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmqvol.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMQVOL(GENAM,LBANK,KEYS,LIB,MODEL,DENS,MNTTYP,LABTYP,
     +                  IRC)
*
*     Routine to interface to the TMS. Check if:
*             1) Volume is available (F)
*             2) Volume is in manual/robot library
*             3) Volume is readable/writable by current account
*
*     Return codes: 0   ok
*                   8   Syntax error
*                   12  Access denied
*                   100 Volume does not exist
*                   312 Volume unavailable on current system
*                   315 Volume unavailable on any system
*
#include "fatmen/faust.inc"
      CHARACTER*(*) GENAM
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYS(LKEYFA)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      CHARACTER*15  VID,XVID
      CHARACTER*8   VIP
#include "fatmen/fattyp.inc"
#include "fatmen/tmsrep.inc"
      CHARACTER*132 LINE,CHLINE
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatvidp.inc"
#include "fatmen/tmsdef1.inc"
 
      NFQVOL = NFQVOL + 1
 
      LGN = LENOCC(GENAM)
 
      IQUEST(1) = 0
      JMEDIA = KEYS(MKMTFA)
#if defined(CERNLIB_TMS)
      IF(LBANK.EQ.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMQVOL. get bank for ',GENAM(1:LGN)
         CALL FMGETK(GENAM(1:LGN),LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FMQVOL. Return code ',IRC,' from FMGETK'
            IRC = 1
            RETURN
            ENDIF
         ELSE
         IF(IDEBFA.GE.0)
     +      PRINT *,'FMQVOL. enter for user supplied bank for ',
     +         GENAM(1:LGN)
         ENDIF
 
      VID    = ' '
      CALL FMGETC(LBANK,VID,MVIDFA,6,IRC)
      LVID   = LENOCC(VID)
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_TMS))
*
*        Generate eXtended VID - with VID prefix
*
         JP = IQ(LBANK+KOFUFA+MVIPFA)
            IF(JP.NE.0) THEN
            LVIP   = LENOCC(PREVID(JP))
            VIP    = PREVID(JP)(1:LVIP)
            XVID   = PREVID(JP)(1:LVIP) // '.' // VID(1:LVID)
            LXVID  = LENOCC(XVID)
 
            ELSE
            XVID   = VID
            LXVID  = LVID
            LVIP   = 0
            ENDIF
 
         VID   = XVID
         LVID  = LXVID
#endif
#if !defined(CERNLIB_TMS)
        IF(IDEBFA.GE.3) PRINT *,'FMQVOL. TMS option not installed.',
     +     ' Default values for LIB/MODEL/DENS/LABTYP/MNTTYP taken'
        IRC    = 0
        IQUEST(1) = -1
        LIB    = '*Unknown'
        MODEL  = 'CT1 '
        DENS   = '38K'
        LABTYP = 'SL'
        MNTTYP = 'M'
*
*       Take values from sequence FATTYP is media type is known
*
        IF(JMEDIA.NE.0) THEN
           MODEL  = CHMGEN(JMEDIA)
           DENS   = CHMDEN(JMEDIA)
           LABTYP = CHMLAB(JMEDIA)
           MNTTYP = CHMMNT(JMEDIA)
#endif
#if (defined(CERNLIB_CERN))&&(!defined(CERNLIB_TMS))
*
*     The following test is CERN specific!!!
*
           IF(JMEDIA.EQ.2.AND.
     +        (VID(1:1).EQ.'I').AND.(ICNUM(VID,2,6).EQ.7)) THEN
              LIB = '3485_2'
              MODEL = 'SMCF'
              MNTTYP= 'R'
           ENDIF
#endif
#if !defined(CERNLIB_TMS)
        ENDIF
#endif
#if (!defined(CERNLIB_TMS))&&(defined(CERNLIB_VMTAPE))&&(defined(CERNLIB_VMTMC))
*
*     Check and see if this tape is in the VMTAPE catalogue
*
      CALL VMCMS('VMTAPE LIST '//VID(1:LVID)
     +           //' (SHORT STACK LIFO',IRC))
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMQVOL. error ',IRC,' from VMTAPE '
     +      'command.'
      ELSE
         CALL VMRTRM(CHLINE,LLINE)
         IF(IDEBFA.GE.0) PRINT *,'FMQVOL. reply from VMTAPE = ',
     +      CHLINE(1:LLINE)
      ENDIF
#endif
#if !defined(CERNLIB_TMS)
*
*     Now call user exit to allow user to override these values
*
        CALL FMUVOL(GENAM,LBANK,KEYS,LIB,MODEL,DENS,MNTTYP,LABTYP,IRC)
        RETURN
#endif
#if defined(CERNLIB_TMS)
        IRC    = 0
*
*VID   Library  Slot     Model     Dens  R R R R A A csn      sstr  estr  labtyp
*                                        i o e a v l
*                                        n b a c a l
*                                        g o l k i o
*                                        ? t ? e l w
*                                          ?   d ? ?
*                                        W M L U U D
*28901 3485_2   00000000 3480      38000 W R R U A A 00000000 00000 00000 SL
10    CONTINUE
*
      I = LENREP
*
      CALL FMSREQ('TMS     ',
#endif
#if (defined(CERNLIB_TMS))&&(!defined(CERNLIB_QVID))
     +            'QVOL '//VID(1:LVID)
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_SHIFT))&&(defined(CERNLIB_CERN))
     +            //' (GENERIC SHIFT'
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_QVID))
     +            'Q VID '//VID(1:LVID)
#endif
#if defined(CERNLIB_TMS)
     +           ,IRC,TMSREP,I)
#endif
#if (defined(CERNLIB_TMS))&&(!defined(CERNLIB_TMSTEST))
      IF(IRC.EQ.100) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMQVOL. volume ',VID,
     +      ' unknown to TMS'
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_TMSTEST))
      IF(IRC.EQ.100) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMQVOL. volume ',VID,
     +      ' unknown to TMS - defaults assumed'
        LIB    = '*Unknown'
        MODEL  = 'CT1 '
        IF(JMEDIA.NE.0) MODEL  = CHMGEN(JMEDIA)
        DENS   = '38K'
        LABTYP = 'SL'
        MNTTYP = 'M'
        IF((VID(1:1).EQ.'I').AND.(ICNUM(VID,2,6).EQ.7)) THEN
           LIB = 'SMCF'
           MNTTYP= 'R'
           ENDIF
        IRC = 0
#endif
#if defined(CERNLIB_TMS)
        RETURN
        ELSE
#endif
#if (defined(CERNLIB_TMS))&&(!defined(CERNLIB_QVID))
        LIB    = TMSREP(1)(8:15)
        MODEL  = TMSREP(1)(26:33)
        DENS   = TMSREP(1)(35:40)
        LABTYP = TMSREP(1)(75:76)
        MNTTYP = TMSREP(1)(44:44)
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_QVID))
        LINE   = TMSREP(1)
        CALL CSQMBL(LINE  ,1,LENOCC(LINE))
        CALL FMWORD(LIB   ,5,' ',LINE,IRC)
        CALL FMWORD(MODEL ,2,' ',LINE,IRC)
        CALL FMWORD(DENS  ,3,' ',LINE,IRC)
        CALL FMWORD(LABTYP,4,' ',LINE,IRC)
        MNTTYP = 'M'
        IF(LIB(1:2).EQ.'CR') MNTTYP = 'R'
*
*     Libraries beginning * are by definition unavailable (apparently)
*
        IF(LIB(1:1).EQ.'*') THEN
           IRC = 312
           RETURN
        ENDIF
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_QVID))&&(defined(CERNLIB_DESPARATE))
*
*     To determine if volume is available, must issue TMS Q LIBRARY...
*     e.g.
*
*SYSREQ TMS Q LIBRARY HP_LPOOL
*Library  Czar     Group    R A L S M Target   Retain Racks  Slots    Spare
*-------- -------- -------- - - - - - -------- ------ ------ -------- --------
*HP_LPOOL *None    *None    N N N N N *None         0      0    10000     9521
*123456789_123456789_123456789_123456789_123456789_123456789_123456789_123456789
*CP Lear Production Data at Liverpool
 
      I = LENREP
*
      CALL FMSREQ('TMS     ',
     +            'Q LIBRARY '//LIB
     +            IRC,TMSREP,I)
      IF(I.NE.3) THEN
         PRINT *,'FMQVOL. unexpected reply from TMS QUERY LIBRARY'
         DO 11 J=1,I
            PRINT *,'FMQVOL. ',TMSREP(J)(1:LENOCC(TMSREP(J)))
11       CONTINUE
      ELSE
*
*     Look for ACTIVE:N
*
         IF(TMSREP(3)(30:30).EQ.'N') IRC = 312
 
      ENDIF
#endif
#if defined(CERNLIB_TMS)
        IF(INDEX(DENS,'38000').NE.0) DENS = '38K'
        ENDIF
*
#endif
      END
 
