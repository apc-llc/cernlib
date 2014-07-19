*
* $Id: fmqtms.F,v 1.1.1.1 1996/03/07 15:18:14 mclareni Exp $
*
* $Log: fmqtms.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:14  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMQTMS(VID,LIB,MODEL,DENS,MNTTYP,LABTYP,IRC)
*     SUBROUTINE FMQTMS(VID,LIB*,MODEL*,DENS*,MNTTYP*,LABTYP*,IRC*)
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
*                   312 Volume unavailable on system issuing QVOL
*                   315 Volume unavailable on any system
*
#include "fatmen/faust.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*(*) VID
#include "zebra/quest.inc"
#include "fatmen/fattyp.inc"
#include "fatmen/tmsrep.inc"
      CHARACTER*132 LINE,CHLINE
      CHARACTER*15  CHVID
#include "fatmen/tmsdef.inc"
 
      NFQVOL = NFQVOL + 1
 
      IF(IDEBFA.GE.3) PRINT *,'FMQTMS. enter for VID = ',VID
      LVID   = LENOCC(VID)
      CHVID  = VID(1:LVID)
      JMEDIA = IQUEST(11)
      IQUEST(1) = 0
#if !defined(CERNLIB_TMS)
        IF(IDEBFA.GE.3) PRINT *,'FMQTMS. TMS option not installed.',
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
        IF(JMEDIA.NE.0.AND.JMEDIA.LE.NFMTYP) THEN
           IF(IDEBFA.GE.3) PRINT *,'FMQTMS. using information in ',
     +        'FATMEN catalogue for MODEL/DENS/LABTYP/MNTTYP'
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
        ELSE
           IF(IDEBFA.GE.-3) WRITE(LPRTFA,9001) JMEDIA,JMEDIA
 9001   FORMAT(' FMQTMS. invalid media type specified via IQUEST(11)',
     +         ' - ',I10,' (',Z8,'). See documentation.')
           IRC = 3
           GOTO 999
        ENDIF
#endif
#if (!defined(CERNLIB_TMS))&&(defined(CERNLIB_VMTAPE))&&(defined(CERNLIB_VMTMC))
*
*     Check and see if this tape is in the VMTAPE catalogue
*
      CALL VMCMS('VMTAPE LIST '//CHVID(1:LVID)
     +           //' (SHORT STACK LIFO',IRC))
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMQTMS. error ',IRC,' from VMTAPE '
     +      'command.'
      ELSE
         CALL VMRTRM(CHLINE,LLINE)
         IF(IDEBFA.GE.0) PRINT *,'FMQTMS. reply from VMTAPE = ',
     +      CHLINE(1:LLINE)
      ENDIF
#endif
#if !defined(CERNLIB_TMS)
*
*     Now call user exit to allow user to override these values
*
        CALL FMUTMS(VID,LIB,MODEL,DENS,MNTTYP,LABTYP,IRC)
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
   10 CONTINUE
*
      I = LENREP
*
      CALL FMSREQ('TMS     ',
#endif
#if (defined(CERNLIB_TMS))&&(!defined(CERNLIB_QVID))
     +            'QVOL '//CHVID(1:LVID)
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_SHIFT))&&(defined(CERNLIB_CERN))
     +            //' (GENERIC SHIFT'
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_QVID))
     +            'Q VID '//CHVID(1:LVID)
#endif
#if defined(CERNLIB_TMS)
     +           ,IRC,TMSREP,I)
 
#endif
#if (defined(CERNLIB_TMS))&&(!defined(CERNLIB_TMSTEST))
      IF(IRC.EQ.100) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMQTMS. volume ',VID,
     +      ' unknown to TMS'
#endif
#if (defined(CERNLIB_TMS))&&(defined(CERNLIB_TMSTEST))
      IF(IRC.EQ.100) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMQTMS. volume ',VID,
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
         PRINT *,'FMQTMS. unexpected reply from TMS QUERY LIBRARY'
         DO 20 J=1,I
            PRINT *,'FMQTMS. ',TMSREP(J)(1:LENOCC(TMSREP(J)))
   20    CONTINUE
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
  999 END
 
