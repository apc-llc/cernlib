*
* $Id: igwkty.F,v 1.4 1998/01/30 15:22:22 couet Exp $
*
* $Log: igwkty.F,v $
* Revision 1.4  1998/01/30 15:22:22  couet
* - APOLLO version removed
*
* Revision 1.3  1998/01/28 14:34:23  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:57:21  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:41  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/01 08/05/95  11.37.33  by  O.Couet
*-- Author :    O.Couet   04/04/89
      SUBROUTINE IGWKTY(IWKTY)
*.===========>
*.
*.   This routine get the workstation type on the standard input
*.
*. _Output parameters:
*.
*.  INTEGER IWKTY : Workstation identifier choosen interactively
*.
*..==========> (O.Couet)
#if (!defined(CERNLIB_BATCH))
#include "higz/hiatt.inc"
#endif
#if !defined(CERNLIB_BATCH)
#include "higz/hiques.inc"
#endif
#if defined(CERNLIB_X11)||defined(CERNLIB_MSDOS)
#include "higz/hikern.inc"
#endif
#if !defined(CERNLIB_BATCH)
      CHARACTER*48 CHMESS
      CHARACTER*24 CHREP
#endif
#if defined(CERNLIB_CCREAD)
      CHARACTER*24 CHCC
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB_AIX370)
      PARAMETER (LUNIN=5,LUNOUT=6)
#endif
#if (defined(CERNLIB_DI3000))&&(defined(CERNLIB_VAX))
      CHARACTER*6 DRIVER
      CHARACTER*40 STRING
      INCLUDE '($SSDEF)'
#endif
#if (defined(CERNLIB_DI3000))&&(defined(CERNLIB_IBM))
      INTEGER ARGLEN
      CHARACTER*100 ARGS
#endif
#if (defined(CERNLIB_DECGKS))&&(defined(CERNLIB_VAX))
      CHARACTER*12 STRING
      INCLUDE '($SSDEF)'
#endif
#if (defined(CERNLIB_DECGKS))&&(defined(CERNLIB_DECS))
      CHARACTER*12 STRING
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      CHARACTER*3 CHLUN
#endif
#if defined(CERNLIB_X11)
      CHARACTER*80 CHWKTY(10)
      LOGICAL LOHOST
#endif
#if (!defined(CERNLIB_BATCH))
      SAVE IFIRST
      DATA IFIRST /0/
#endif
*.______________________________________
*
#if (!defined(CERNLIB_BATCH))
      IF(IFIRST.NE.0)THEN
         IWKTY = IDTY
         RETURN
      ELSE
         IFIRST = 1
      ENDIF
#endif
#if (defined(CERNLIB_DI3000))&&(defined(CERNLIB_IBM))
      CALL GOPARM(ARGLEN,ARGS)
#endif
*
      CALL IGDWK(IWKTY)
      CHMESS = 'Workstation type (?=HELP) <CR>='
      LENC   = LENOCC(CHMESS)+1
      CALL IZITOC(IWKTY,CHMESS(LENC:))
*
#if !defined(CERNLIB_BATCH)
      LENC = LENOCC(CHMESS)
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_AIX370))&&(!defined(CERNLIB_BATCH))
   10 PRINT 10002, CHMESS(1:LENC)
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB_AIX370)
   10 WRITE (LUNOUT,'('' '',A,'' :'')') CHMESS(1:LENC)
#endif
#if !defined(CERNLIB_BATCH)
      CHREP = ' '
#endif
#if defined(CERNLIB_CCREAD)
      CALL IXGETL(LCC,CHCC)
      CHREP = CHCC(1:LCC)
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_BATCH))
      READ 10003, CHREP
#endif
#if defined(CERNLIB_IBM)
      READ (LUNIN,'(A)',END=20) CHREP
#endif
#if !defined(CERNLIB_BATCH)
      IF(CHREP.EQ.' ')RETURN
      IF(INDEX(CHREP,'?').NE.0)THEN
         PRINT 10000
#endif
#if (defined(CERNLIB_DI3000))&&(defined(CERNLIB_VAX))
         DO 100 I=1,9
           WRITE(DRIVER,'(A2,Z1,A3)') 'YD',I,'DRV'
           IF (SS$_NORMAL.EQ.LIB$SYS_TRNLOG(DRIVER,ICHAR,STRING)) THEN
              IF (STRING(ICHAR-6:ICHAR-4).NE.'DRV')
     +           PRINT 10001, I,STRING(ICHAR-6:ICHAR-4)
           ENDIF
  100    CONTINUE
#endif
#if (defined(CERNLIB_DI3000))&&(defined(CERNLIB_IBM))
         CALL TRE0EU(ARGS,ARGLEN)
         I = 1
         J = 1
  100    CONTINUE
            INDX = INDEX(ARGS(J:),'DRV')
            IF (INDX.NE.0) THEN
               PRINT 10001, I,ARGS(J+INDX+2:J+INDX+4)
               I = I+1
               J = J+INDX
               GOTO 100
            ELSE
               PRINT 10001, I,'DI3000 driver menu'
            ENDIF
  200    CONTINUE
#endif
#if !defined(CERNLIB_BATCH)
         GOTO 10
      ENDIF
#endif
#if defined(CERNLIB_X11)
      IP     = INDEX(CHREP,'.')
      LOHOST = .FALSE.
      IF(IP.NE.0)THEN
         LOHOST = .TRUE.
         IF(IP+1.GT.LENOCC(CHREP))THEN
            FX11 = ' '
         ELSE
            FX11 = CHREP(IP+1:LENOCC(CHREP))
         ENDIF
         CHREP(IP:) = ' '
      ENDIF
      IF(CHREP(1:1).EQ.'M'.OR.CHREP(1:1).EQ.'m')THEN
         CALL IXSETHN(LENOCC(FX11),FX11(1:LENOCC(FX11)))
         IWKTY = 999
         RETURN
      ENDIF
#endif
#if !defined(CERNLIB_BATCH)
      CALL IZCTOI(CHREP,IWK)
      IF(IQUEST(1).NE.0)THEN
         PRINT*, ' Invalid workstation type'
         GOTO 10
      ENDIF
      IWKTY = IWK
#endif
#if defined(CERNLIB_X11)
      IF(LOHOST)THEN
         CALL IKFILE(IWK)
         OPEN(UNIT=LUNWIN,FILE=FILEN,FORM='FORMATTED'
     +,       STATUS='OLD',IOSTAT=ISTA)
         DO 30 I=1,10
            READ(LUNWIN,'(A)')CHWKTY(I)
   30    CONTINUE
         CLOSE(LUNWIN,STATUS='DELETE')
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
         WRITE(CHLUN,'(I3)')LUNWIN
         FILEN='FILEDEF '//CHLUN//'  CLEAR'
         CALL VMCMS(FILEN,ISTAT)
         FILEN = '/HIGZWIN DATA A'
#endif
#if defined(CERNLIB_X11)
         OPEN(UNIT=LUNWIN,FILE=FILEN,FORM='FORMATTED'
     +,       STATUS='NEW',IOSTAT=ISTA)
         DO 40 I=1,10
            IF(I.EQ.IWK)CHWKTY(I)(22:)=FX11
            WRITE(LUNWIN,'(A)')CHWKTY(I)(1:LENOCC(CHWKTY(I)))
   40    CONTINUE
         CLOSE(LUNWIN)
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
         WRITE(CHLUN,'(I3)')LUNWIN
         FILEN = 'FILEDEF '//CHLUN//'  CLEAR'
         CALL VMCMS(FILEN,ISTAT)
         FILEN = '/HIGZWIN DATA A'
#endif
#if defined(CERNLIB_X11)
      ENDIF
#endif
#if !defined(CERNLIB_BATCH)
      RETURN
#endif
#if defined(CERNLIB_IBM)
*
   20 REWIND(LUNIN)
#endif
#if !defined(CERNLIB_BATCH)
*
10000 FORMAT(' ',/,' List of valid workstation types:',/,
#endif
#if (!defined(CERNLIB_DI3000))&&(!defined(CERNLIB_BATCH))
     +'       0:  Alphanumeric terminal',/,
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_NEWLIB))
     +'    4714:  X-Terminal'/
     +'    4725:  Workstation/X-Terminal (2 Terminal Mode)'/
     +'    5003:  IBM Graphics Terminal (e.g. 3192 G)',/,
     +'    7878:  Falco Infinity Terminal',/,
     +'  470352:  Atari-Workstation via 7171',/,
     +'  471352:  Atari-Workstation via 7171 (full window)',/,
     +'  470353:  Atari-Workstation via Terminal Server',/,
     +'  471353:  Atari-Workstation via Terminal Server(full window)',/,
     +'    5005:  Macintosh IBM Emulator',/,
     +'    5010:  IBM 3820 Laserprinter LI1  (portrait-format)',/,
     +'    5020:  IBM 3820 Laserprinter LI1  (landscape-format)',/,
     +'    5011:  IBM Matrix-Printer PRINTER2',/,
     +'   13001:  QMS Laserprinter L1  (portrait-Format)',/,
     +'   13002:  QMS Laserprinter L1  (landscape-format)',/,
#endif
#if (defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_SUNGKS))&&(!defined(CERNLIB_ATCGKS))&&(!defined(CERNLIB_NEWLIB))
     +'     101:  Tektronix 4010, 4014',/,'     102:  Tektronix 4012',/,
     +'     103:  Tektronix 4014 with enhanced graphics option',/,
     +'     121:  Tektronix 4107, 4207, Pericom MX2000',/,
     +'     122:  Tektronix 4109',/,'     123:  Tektronix 4111',/,
     +'     125:  Tektronix 4113',/,'     127:  Tektronix 4115, Pericom
     +MX8000',/,'    7800:  MG600, MG200',/,'    7878:  Falco, Pericom
     +Graph Pac (old Pericom)',/,'    1020:  VT240',/,'    1030:  VT340'
     +,/,'  8601-6:  Vaxstation GPX',/,' 32120-9:  X-Window',/,
#endif
#if defined(CERNLIB_DI3000)
     +'      -1:  Alphanumeric terminal',/,
     +'       0:  DI3000 metafile',
#endif
#if defined(CERNLIB_DECGKS)
     +'       2:  GKSM Output metafile',/,
     +'       7:  CGM Output metafile',/,
     +'      13:  VT240 color',/,
     +'      14:  VT240 monochrome',/,
     +'      16:  VT330 ',/,
     +'      17:  VT340 color',/,
     +'      82:  Tek 4107, 4207, Pericom MX2000',/,
     +'      41:  Vaxstation',/,
     +'     211:  Vaxstation running DECWindows',/,
     +'      38:  LN03 Plus Laser Printer (Assign GKS$CONID)',/,
     +'      72:  Tektronix 4014 Emulators',/,
     +'      61:  Postscript Printers (Assign GKS$CONID)',/,
#endif
#if defined(CERNLIB_GK2000)
     +'     445:  Vaxstation',/,
     +'     102:  Tektronix 4014 Emulators',/,
     +'     105:  Macintosh Tektronix 4014 Emulator',/,
     +'     191:  Ramtek R25 4014 Emulator',/,
     +'     324:  LN03 Plus Laser Printer',/,
     +'     601:  Talaris Printers',/,
     +'     700:  Postscript Printers',/,
#endif
#if (defined(CERNLIB_SUNGKS))&&(!defined(CERNLIB_ATCGKS))
     +'       3:  GKS Metafile Output (ASCII)',/,
     +'       4:  SUN Workstation',/,
     +'       6:  HP-GL Output',/,
     +'       7:  PostScript Output',/,
     +'       8:  CGM Output',/,
     +'      10:  GKS Metafile Output (binary)',/,
#endif
#if defined(CERNLIB_ATCGKS)
     +'    15nn:  QMS Lasergrafix (TALARIS) 8.5x11',/,
     +'           for TALARIS: Use PRINT/NOFORM/PASSALL <file>',/,
     +'    19nn:  POSTSCRIPT 8.5x11 Printer',/,
     +'    23nn:  Tektronix 4105 Terminals',/,
     +'    2400:  Tektronix 4014 Terminal',/,
     +'    2501:  Tektronix 4010 Terminal',/,
     +'    2502:  C-ITOH Terminals (201,414)',/,
     +'    2503:  Retrographics VT640',/,
     +'    2506:  GRAPHON 140, 230 Terminal',/,
     +'    25nn:  other Tektronix 4010 Types',/,
     +'    2600:  DEC VT125 Terminal',/,
     +'    2602:  DEC VT240 Terminal',/,
     +'    2603:  DEC VT330 Terminal',/,
     +'    2605:  DEC VT340 Color Terminal',/,
     +'    3100:  Tektronix 4107 Terminal',/,
     +'    3102:  Tektronix 4205 Terminal',/,
     +'    3104:  Tektronix 4208 Terminal',/,
     +'    315n:  4107-4208 (software segments)',/,
     +'    4300:  VAXstation II (not in Version 3.2)',/,
     +'    5300:  X-Windows',/,
     +'    5350:  X-Windows with refresh',/,
     +'    63nn:  IMAGEN 8.5x11 Printer',/,
     +'    66nn:  Tektronix 4510 Color Rasterizer',/,
     +'   10100:  Binary Output CG Metafile',/,
     +'   10110:  Character Output CG Metafile',/,
     +'   10120:  Clear Text Output CG Metafile',/,
#endif
#if (defined(CERNLIB_GL)||defined(CERNLIB_X11)||defined(CERNLIB_MACMPW))&&(!defined(CERNLIB_MSDOS))
     +'    1-10:  Describe in file higz_windows.dat',/,
#endif
#if (defined(CERNLIB_MSDOS))&&(defined(CERNLIB_NDP))
     +'       4:   320x200,  4 colors'/
     +'       5:   320x200,  4 colors'/
     +'       6:   640x200,  2 colors'/
     +'      13:   320x200, 16 colors'/
     +'      14:   640x200, 16 colors'/
     +'      15:   640x350,  2 colors'/
     +'      16:   640x350, 16 colors'/
     +'      17:   640x480,  2 colors'/
     +'      18:   640x480, 16 colors'/
     +'      19:   320x200,256 colors'/
     +    /
     +' SuperVGA:   800x600  16 colors:'//
     +'       41:   Tseng chipset            98:   Video Seven       '/
     +'       88:   Paradise                100:   Renaissance GRX   '/
#endif
#if (defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_NDP))
     +'      >0:  Graphics option is pre-selected by the'/,
     +'           GO32 environment variable'/
#endif
#if defined(CERNLIB_X11)
     +'  n.host:  Open the display on host (1 < n < 10)',/,
#endif
#if defined(CERNLIB_FALCO)
     +'    7878:  FALCO terminal',/,
     +'    7879:  xterm',/,
#endif
#if defined(CERNLIB_GKSBX)
     +'    8811:  GKSBx-X11',/,
#endif
#if defined(CERNLIB_PHIGS)
     +'    8887:  GPHIGS-X11',/,'    7176:  GPHIGS-GL',/,
     +'    8384:  GPHIGS-Starbase',/,'    8871:  GPHIGS-Xgl',/,
     +'    7188:  GPEX',/,
#endif
#if !defined(CERNLIB_BATCH)
     +' ')
#endif
#if defined(CERNLIB_DI3000)
10001 FORMAT(I8,':  ',A)
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_AIX370))&&(!defined(CERNLIB_BATCH))
10002 FORMAT(' ',A,' : ',$)
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_BATCH))
10003 FORMAT(A)
#endif
      END
