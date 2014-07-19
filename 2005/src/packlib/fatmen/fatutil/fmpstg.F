*
* $Id: fmpstg.F,v 1.1.1.1 1996/03/07 15:18:08 mclareni Exp $
*
* $Log: fmpstg.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:08  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPSTG(GNAME,NNAMES,NFILES,NTAPES,CHOPT,IRC)
*
*     CHOPT: F - stage only first tape
*            H - wHole volume staging, as for FMOPEN
*            L - override label with DCB information in catalogue
*
      CHARACTER*(*)     GNAME(NNAMES),CHOPT
      CHARACTER*255     GENAM,FSQSTR,CHCOMM
 
#if defined(CERNLIB_UNIX)
      INTEGER SYSTEMF
#endif
 
      CHARACTER*6       VID,VSN,VIDOLD,VSNOLD,FSEQ
 
      CHARACTER*40  DCB,DCBOLD
      CHARACTER*4   RECFM,RECFMO,LABOLD
 
      CHARACTER*8   CHOPTX,MODOLD
 
#include "fatmen/fmnkeys.inc"
      DIMENSION         KEYS(LKEYFA)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatopt0.inc"
#include "fatmen/tmsdef.inc"
#include "fatmen/fatopt1.inc"
 
      IRC    = 0
      NDONE  = 0
      NFILES = 0
      NTAPES = 0
      VIDOLD = ' '
      RECFMO = ' '
      LFLSTR = 0
      LMXSTR = LEN(FSQSTR)
 
      IF(IDEBFA.GE.1) WRITE(LPRTFA,9001) NNAMES,CHOPT
 9001 FORMAT(' FMPSTG. enter for ',I6,' generic names, CHOPT = ',A)
 
#if defined(CERNLIB_UNIX)
      IF(IOPTL.EQ.0) THEN
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9002)
 9002 FORMAT(' FMPSTG. option L turned on for Unix systems')
         IOPTL = 1
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
      IF(IOPTL.NE.0) THEN
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9003)
 9003 FORMAT(' FMPSTG. option L not currently supported on VMS systems')
         IOPTL = 0
      ENDIF
#endif
 
      DO 20 I=1,NNAMES
 
         LGN = LENOCC(GNAME(I))
         GENAM = GNAME(I)(1:LGN)
         CALL CLTOU(GENAM(1:LGN))
 
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9004) I,GENAM(1:LGN)
 9004 FORMAT(' FMPSTG. processing generic name # ',I6,/,
     +       ' (',A,')')
 
         LBANK = 0
         CALL FMGET(GENAM,LBANK,KEYS,IRC)
         IF (IRC.NE.0) GOTO 999
*
*     Is this entry a link?
*
         IF(KEYS(MKLCFA).EQ.0) THEN
            CALL UHTOC(IQ(LBANK+KOFUFA+MFQNFA),4,GENAM,NFQNFA)
            NCH = LENOCC(GENAM)
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9005) GNAME(I)(1:LGN),
     +           GENAM(1:NCH)
 9005       FORMAT(' FMPSTG. ',A,' --> ',A)
            NCH = LGN
            CALL VZERO(KEYS,LKEYFA)
            CALL MZDROP(IDIVFA,LBANK,'B')
            LBANK = 0
            CALL FMGET(GENAM,LBANK,KEYS,IRC)
            IF (IRC.NE.0) GOTO 999
         ENDIF
*
*     Ignore disk files
*
         IF(KEYS(MKMTFA).EQ.1) THEN
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9006) I
 9006 FORMAT(' FMPSTG. generic name # ',I6,' points to a disk file',
     +       ' - ignored')
            GOTO 10
         ENDIF
 
         NFILES = NFILES + 1
 
         CALL UHTOC(IQ(LBANK+KOFUFA+MVIDFA),4,VID,6)
         LVID = LENOCC(VID)
         CALL CLTOU(VID(1:LVID))
 
         CALL UHTOC(IQ(LBANK+KOFUFA+MVSNFA),4,VSN,6)
         LVSN = LENOCC(VSN)
         CALL CLTOU(VID(1:LVSN))
 
         CALL FMITOC(IQ(LBANK+KOFUFA+MFSQFA),FSEQ,LFSEQ)
 
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9007) VID(1:LVID),VSN(1:LVSN),
     +      FSEQ(1:LFSEQ)
 9007 FORMAT(' FMPSTG. processing VID: ',A,' VSN: ',A,' FSEQ: ',A)
*
*     Option H - ignore if we have already seen this volume
*
         IF(IOPTH.NE.0.AND.VID.NE.VIDOLD.AND.VSN.NE.VSNOLD) GOTO 10
 
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
         IQUEST(11) = IQ(LBANK+KOFUFA+MMTPFA)
         CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMPSTG. return from FMQTMS with ', VID,'/',LIB,'/'
     +      ,MODEL,'/',DENS,'/',MNTTYP,'/', LABTYP,'/',IC
         ENDIF
 
         LLAB = LENOCC(LABTYP)
*
*     Get DCB information
*
         CALL UHTOC(IQ(LBANK+KOFUFA+MRFMFA),4,RECFM,4)
         LRECL = IQ(LBANK+KOFUFA+MRLNFA)*4
         LBLOCK = IQ(LBANK+KOFUFA+MBLNFA)*4
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_SHIFT))
         IF(RECFM.EQ.'U') RECFM = 'F'
#endif
*
*     Issue stage request is this is the last generic name or if
*     we have switched to a new volume
*
         IF(I.EQ.NNAMES.OR.
     +     (VIDOLD.NE.' '.AND.VID.NE.VIDOLD.AND.VSN.NE.VSNOLD)) THEN
 
            NTAPES = NTAPES + 1
 
            IF(IOPTL.NE.0) THEN
*
*     Add DCB information
*
#if !defined(CERNLIB_VAXVMS)
               WRITE(DCB,9008) RECFM,LRECL,LBLOCK
#endif
#if defined(CERNLIB_UNIX)
 9008          FORMAT(' -F ',A,' -L ',I5,' -b ',I5)
#endif
#if defined(CERNLIB_IBMVM)
 9008          FORMAT(' RECFM ',A,' LRECL ',I5,' BLKSIZE ',I5)
#endif
            ENDIF
*
*      Issue stage request for the previous volume
*
#if defined(CERNLIB_IBMVM)
            IF(IOPTH.NE.0) THEN
               FSQSTR = '1-E'
               LFLSTR = 3
            ENDIF
 
            CHCOMM = 'EXEC STAGE IN FT00F001 '//VSNOLD(1:LVSNO)//
     +               '.'//FSQSTR(1:LFLSTR)//'.'//LABOLD(1:LLABO)//
     +               '.'//VIDOLD(1:LVIDO)//' (NOWAIT)'
            LCOMM  = LENOCC(CHCOMM)
 
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9011) CHCOMM(1:LCOMM)
 
            CALL VMCMS(CHCOMM(1:LCOMM),IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.0) WRITE(LPRTFA,9009) CHCOMM(1:LCOMM)
 9009 FORMAT(' FMPSTG. return code ',I6,' from VMCMS for ',A)
               GOTO 10
            ENDIF
#endif
#if defined(CERNLIB_UNIX)
            IF(IOPTH.NE.0) THEN
               FSQSTR = '1-'
               LFLSTR = 2
            ENDIF
 
            CHCOMM = 'stagein  -G  -v '//VSNOLD(1:LVSNO) // ' -q ' //
     +      FSQSTR(1:LFLSTR) //' -V '//VIDOLD(1:LVIDO)//
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_TMS))
     +         ' -l '//LABOLD(1:LLABO)  // ' -g '//MODOLD(1:LMODO)//
#endif
#if defined(CERNLIB_UNIX)
     +      DCBOLD(1:LDCBO) // ' -w  T'//VIDOLD(1:LVIDO)//' &'
            LCOMM = LENOCC(CHCOMM)
 
            IF(IDEBFA.GE.0) PRINT 9010,CHCOMM(1:LCOMM)
 9010 FORMAT(' FMPSTG. executing ',A)
 
            IRC = SYSTEMF(CHCOMM(1:LCOMM))
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.0) WRITE(LPRTFA,9009) CHCOMM(1:LCOMM)
 9009 FORMAT(' FMPSTG. return code ',I6,' from SYSTEMF for ',A)
               GOTO 10
            ENDIF
 
#endif
#if defined(CERNLIB_VAXVMS)
            IF(IOPTH.EQ.0) THEN
               CHOPTX = 'Q'
            ELSE
               CHOPTX = 'HQ'
            ENDIF
*
*      call FMOPEN with option Q (assume VAXTAP in server mode)
*
            CALL FMOPEN(GENAM(1:LGN),'FMPSTG',LBANK,CHOPTX,IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.0) WRITE(LPRTFA,9009) GENAM(1:LGN)
 9009 FORMAT(' FMPSTG. return code ',I6,' from FMOPEN for ',A)
               GOTO 10
            ENDIF
#endif
 9011 FORMAT(' FMPSTG. executing ',A)
*
*      IOPTF: just first volume
*
            IF(IOPTF.NE.0) THEN
               GOTO 10
            ENDIF
 
            NDONE = 0
 
         ENDIF
*
*    Build file sequence string
*
         IF(IOPTH.EQ.0) THEN
            IF(NDONE.EQ.0) THEN
               FSQSTR = FSEQ(1:LFSEQ)
               LFLSTR = LFSEQ
            ELSE
               IF(LFLSTR+1+LFSEQ.GT.LMXSTR) THEN
                  IF(IDEBFA.GE.-3) WRITE(LPRTFA,9012) LMXSTR
 9012 FORMAT(' FMPSTG. error - list of files cannot exceed ',I6,
     +       ' characters')
                  IRC = -1
                  GOTO 10
               ENDIF
               FSQSTR(LFLSTR+1:) = ','//FSEQ(1:LFSEQ)
               LFLSTR = LFLSTR + LFSEQ + 1
            ENDIF
         ENDIF
 
         VIDOLD = VID(1:LVID)
         VSNOLD = VSN(1:LVSN)
         RECFMO = RECFM
         JRECL  = LRECL
         JBLOCK = LBLOCK
         LABOLD = LABTYP(1:LLAB)
         MODOLD = MODEL
#if !defined(CERNLIB_VAXVMS)
         DCBOLD = DCB
#endif
         LVIDO  = LVID
         LVSNO  = LVSN
         LLABO  = LLAB
         LMODO  = LENOCC(MODOLD)
         LDCBO  = LENOCC(DCBOLD)
 
   10    CONTINUE
 
         CALL MZDROP(IDIVFA,LBANK,'B')
         LBANK = 0
 
   20 CONTINUE
 
  999 END
