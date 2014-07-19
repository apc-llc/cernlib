*
* $Id: irqst.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: irqst.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.10/00 15/01/90  09.38.41  by  O.Couet
*-- Author :
      SUBROUTINE IRQST(WKID,STDNR,STATUS,L,STR)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Request and return input from a STRING input device.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JKEYBS (DEVICE, PHYDEV, ECHOLV, MAXCHR, STRING, ACTUAL)
*.
*.   Parameters:
*.   -----------
*.     DEVICE
*.      - Currently selected display device
*.
*.     PHYDEV
*.      - Integer defining the physical input device on DEVICE that is to be
*.        used for obtaining KEYBOARD input
*.
*.     ECHOLV
*.      - Echo level for echoing KEYBOARD input (=0 No echo, =1 KEYBOARD string
*.        is displayed, =2 KEYBOARD string is output onto a display DEVICE
*.        other than the display device of DEVICE
*.
*.      MAXCHR
*.       - Maximum number of characters that will fit into the text array
*.         STRING, must be passed from the calling program, max=254
*.
*.      STRING
*.       - The character string returned by the KEYBOARD input function
*.
*.      ACTUAL
*.       - The actual number of characters read in by the KEYBOARD input
*.         function
*.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/hiflag.inc"
#endif
      INTEGER WKID,STATUS,L
      CHARACTER*(*) STR
      INTEGER DEVICE, PHYDEV, ECHOLV, MAXCHR, ACTUAL, INPFCT
      CHARACTER *254 STRING

      INTEGER NKEYBD
*.______________________________________
*
#if defined(CERNLIB_MAIL)
*
*              MAIL option
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(3I3)') 556,IWKID,ISTD
         CALL IMWRIT(1)
         READ (5,'(2I5,A)') ISTAT,L,STR
         CALL IMWRIT(5)
         RETURN
      ENDIF
#endif
*
      CALL IGSG(0)
C
C     ***** Enable KEYBOARD device
C
      DEVICE = ABS(WKID)
      CALL JIQDEV(DEVICE,45,NKEYBD)
      IF (NKEYBD.NE.0) THEN

         INPFCT = 4
         PHYDEV = 1
         ECHOLV = 0
         MAXCHR = 254

         CALL JIENAB(DEVICE,INPFCT,PHYDEV)

C        ***** Get STRING from device

         STRING = ' '
         CALL JKEYBS (DEVICE, PHYDEV, ECHOLV, MAXCHR, STRING, ACTUAL)
         L = ACTUAL
         STR = STRING
         STATUS = 1
      ENDIF
*
      END
#endif
