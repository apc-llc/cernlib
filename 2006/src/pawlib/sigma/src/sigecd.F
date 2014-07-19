*
* $Id: sigecd.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sigecd.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIGECD
C    ..................................................
C         PURPOSE
C            TO GET THE CHARACTERISTICS OF A NAME
C            FOLLOWING ALL CHAINS TO THE END IF NECESSARY
C         USAGE
C            CALL SIGECD
C
C         COMM. BLOCKS USED
C            COM1
C
C         REMARKS
C            GET WILL BE SATISFIED WITH
C                     A DECLARED GLOBAL NAME (SINGLE LINK TO LEVEL 0)
C                     A FORMAL PARAMETER (ITYPE=16)
C                     A SUBPROGRAM NAME (DEFINED ON ZERO LEVEL ONLY)
C            OTHERWISE GET WILL GIVE AN ERROR MESSAGE
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SEE
C            SINERR
C            MESSC
C
C         AUTHOR.   C.E.VANDONI & J.REINFELDS DATE 15/06/74
C
C...  PAW VERSION  ...  MAY 1988
C
C    ..................................................
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
C
      CHARACTER CNATA (64)*8
      SAVE CNATA
      DATA CNATA( 1)/'ACOS    '/
      DATA CNATA( 2)/'ASIN    '/
      DATA CNATA( 3)/'TAN     '/
      DATA CNATA( 4)/'SINH    '/
      DATA CNATA( 5)/'COSH    '/
      DATA CNATA( 6)/'ERF     '/
      DATA CNATA( 7)/'ERFC    '/
      DATA CNATA( 8)/'FREQ    '/
      DATA CNATA( 9)/'DILOG   '/
      DATA CNATA(10)/'GAMMA   '/
      DATA CNATA(11)/'ELLICK  '/
      DATA CNATA(12)/'ELLICE  '/
      DATA CNATA(13)/'BESJ0   '/
      DATA CNATA(14)/'BESJ1   '/
      DATA CNATA(15)/'BESY0   '/
      DATA CNATA(16)/'BESY1   '/
      DATA CNATA(17)/'BESI0   '/
      DATA CNATA(18)/'BESI1   '/
      DATA CNATA(19)/'BESK0   '/
      DATA CNATA(20)/'BESK1   '/
      DATA CNATA(21)/'EBESI0  '/
      DATA CNATA(22)/'EBESI1  '/
      DATA CNATA(23)/'EBESK0  '/
      DATA CNATA(24)/'EBESK1  '/
      DATA CNATA(25)/'SININT  '/
      DATA CNATA(26)/'COSINT  '/
      DATA CNATA(27)/'EXPINT  '/
      DATA CNATA(28)/'DAWSON  '/
      DATA CNATA(29)/'ALOGAM  '/
      DATA CNATA(30)/'RNDM    '/
      DATA CNATA(31)/'CGAMMA  '/
      DATA CNATA(32)/'CDIGAM  '/
      DATA CNATA(33)/'CLOGAM  '/
      DATA CNATA(34)/'CWERF   '/
      DATA CNATA(35)/'ADIGAM'/
      DATA CNATA(36)/'ASINH'/
      DATA CNATA(37)/'ATANI'/
      DATA CNATA(38)/'ATG'/
      DATA CNATA(39)/'FRSIN'/
      DATA CNATA(40)/'FRCOS'/
      DATA CNATA(41)/'GAMMF'/
      DATA CNATA(42)/'GAUSIN'/
      DATA CNATA(43)/'RANGAM'/
      DATA CNATA(44)/'STRH0'/
      DATA CNATA(45)/'STRH1'/
C
      CALL SIGSEE
      IF(KIT.LT.0)RETURN
      DO 33 I=1,45
      IF(CNATA(I).EQ.CNAME) GOTO 1
33    CONTINUE
      RETURN
1     CONTINUE
      ITYPE=5
      KIT=-I
      KLASS=I
      END
