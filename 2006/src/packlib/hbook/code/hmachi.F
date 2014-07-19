*
* $Id: hmachi.F,v 1.6 2000/01/26 09:58:08 couet Exp $
*
* $Log: hmachi.F,v $
* Revision 1.6  2000/01/26 09:58:08  couet
* - To be Y2K compliant this routine has been changed to use the new DATIME.
*   The calling sequence used was CALL DATIME(ID,IT). This is the one used is
*   the manual. ID and IT are not used because after the call to DATIME, the
*   time and date are taken in a COMMON block. Unfortunately, the variable name
*   ID is used in the common block hcflag. So this variable was corrupted by
*   the call to DATIME.
*
* Revision 1.5  1998/11/10 09:18:54  couet
* - mods for Y2K
*
* Revision 1.3  1998/11/09 14:16:51  couet
* - update version number
*
* Revision 1.2  1998/09/25 09:27:01  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.1.1.1  1996/01/16 17:07:43  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/05 29/11/93  09.45.20  by  Rene Brun
*-- Author :
      SUBROUTINE HMACHI
*.==========>
*.           INITIALISATION SUBROUTINE
*.           ROUTINE called by HLIMIT at the start of HBOOK
*.           DEFINE CONTANTS FOR ONE MACHINE
*.
*.    ****************************************************************
*.
*.           DESCRIPTION OF VARIABLES IN COMMONS
*.
*.                  A, COMMON/HCFLAG/
*.
*.           ID     CURRENT IDENTIFIER
*.           IDLAST LAST IDENTIFIER FILLED
*.           IDHOLD LAST ID ACCESSED BY HI,HIJ,ETC.
*.           LID    LAST IDENTIFIER SEARCHED IN ADDRESS TABLE
*.           IDBADD ADDRESS OF ID IN ADDRESS TABLE (LIBRARY)
*.           NBIT   NUMBER OF BITS IN A MACHINE WORD
*.           NBITCH NUMBER OF BITS PER CHARACTER
*.           NCHAR  NUMBER OF CHARACTERS IN A MACHINE WORD
*.           IERR   0,1 NO ERROR,ERROR AFTER ANY MEMORY ACTION
*.           NV     1 FOR HTABLE     2 FOR HBOOK2
*.           LCID FIRST LOCATION OF HIST ID  IN /PAWC/
*.
*.
*.                  B, COMMON/HCPRIN/
*.
*.           IFW    POINTER TO NUMBER OF ENTRIES FOR CURRENT ID
*.           NW     NUMBER OF WORDS FOR SUM OF WEIGHTS
*.           NB     NUMBER OF BITS PER CHANNEL
*.           IH     CURRENT NUMBER OF HISTOGRAM PRINTED
*.           NHT    NUMBER OF PIECES IN WHICH ID IS SPLITTED
*.           ICN    NUMBER OF CHANNELS
*.           NH     NUMBER OF HISTOGRAMS PRINTED
*.           MSTEP  NUMBER OF PRINTER COLUMNS (HBIGBI)
*.           NOENT  SEVERAL MEANINGS
*.           NOLD   USED TO PRINT TITLE
*.           IDOLAR 1H$ INTEGER VALUE
*.           IBLANC 1H
*.           KBINSZ =1 IF BIN ADJUSTEMENT REQUIRED AT BOOKING
*.           KSQUEZ =1 IF NO PAGE EJECT AT PRINTING
*.           NCOLMA MAXIMUM NUMBER OF COLUMNS FOR PRINTING
*.           NCOLPA NUMBER OF COLUMNS IN ONE PAGE
*.           NLINPA NUMBER OF LINES IN ONE PAGE
*.           MAX    POWERS OF 2
*.           INO    2HNO
*.           BIGP   VERY BIG NUMBER
*.
*.
*.                  C, COMMON/HCBITS/
*.
*.           I1     HBOOK1
*.           I2     HBOOK2
*.           I3     HTABLE
*.           I4     NTUPLE
*.           I5     AUTOMATIC BINNING
*.           I6     VARIABLE BIN SIZE HISTOGRAM
*.           I7     HBSTAT
*.           I8     PROFILE HISTOGRAM
*.           I9     HBARX
*.           I10    HBARY
*.           I11    HERROR
*.           I12    HFUNC
*.           I13    HROTAT
*.           I14    HPRFUN
*.           I15    HPRLOW
*.           I16    HPRHIS
*.           I17    HBIGBI
*.           I18    HNORMA
*.           I19    HSCALE
*.           I20    HMAXIM
*.           I21    HMINIM
*.           I22    HINTEG
*.           I23    H2PAGE
*.           I24    H1EVLI
*.           I25    HPRSTA
*.           I26    HLOGAR
*.           I27    HBLACK
*.           I28    HSTAR
*.           I29    HPRCHA
*.           I30    HPRCON
*.           I31    HPRERR
*.
*.           I32,I33,I35  NOT YET USED
*.           I34 USED NOT AS A BIT BUT A FLAG AT PRINTING
*.           I230=I2+I3
*.           I123=I1+I230
*.
*.
*.                  D, COMMON/HCUNIT/
*.
*.           LOUT   LOGICAL UNIT FOR PRINTING OUTPUT
*.           LERR   LOGICAL UNIT FOR ERROR MESSAGES
*.           LINFIT LOGICAL UNIT FOR MINUIT INPUT
*.
*.
*.                  E, COMMON/HCNT/
*.
*.           IBIPW   Bits per word (=MBIT)
*.           IBIPB   Bits per byte (=MBITCH)
*.           IBYPW   Bytes per word (=NCHAR)
*.           ISHBIT  Bits to shift to devide by IBIPW (shifting is faster
*.                   than dividing)
*.
*.                  F, COMMON/HCOPT/
*.           ISTAF   to fill statistics at filling time
*            ICOPT   not used
*.
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcpar0.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcfitr.inc"
#include "hbook/hcvers.inc"
#include "hbook/hcnt.inc"
#include "hbook/hcset.inc"
#include "hbook/hcrecv.inc"
#include "hbook/hmachine.inc"
#include "hbook/hcminpu.inc"
#include "hbook/hcpiaf.inc"
#include "hbook/czcbuf.inc"
#include "hbook/hcopt.inc"
      COMMON /SLATE/ ISL(40)
      CHARACTER*1 IDGTDA(42)
      CHARACTER*4 IPROJ(9)
      SAVE IDGTDA,IPROJ
      DATA IDGTDA/'0','1','2','3','4','5','6','7','8','9',
     +            'A','B','C','D','E','F','G','H','I','J',
     +            'K','L','M','N','O','P','Q','R','S','T',
     +            'U','V','W','X','Y','Z','*','.','-','+',
     +            ' ','/'/
*
      DATA IPROJ/'HIST','HIST','PROX','PROY','SLIX',
     +           'SLIY','BANX','BANY','FUNC'/
*.___________________________________________
*
*             Get date of day
*
      CALL DATIMH(IVERS,INSTAL)
      CALL UBLOW(IVERS,IDATE,8)
      CALL DATIME(IIDATE,IITIME)
      IYEAR  = ISL(1)
      IMONTH = ISL(2)
      IDAY   = ISL(3)
      CHDATE = ' '
      WRITE (CHDATE(1:2),'(I2.2)') IDAY
      CHDATE(3:3) = '/'
      WRITE (CHDATE(4:5),'(I2.2)') IMONTH
      CHDATE(6:6) = '/'
      WRITE (CHDATE(7:10),'(I4.4)') IYEAR
*
*             Set version number
*
      HVERSN = 4.25
      CALL UCTOH('4.25 ',IVERS,1,5)
#if defined(CERNLIB_NEWLIB)
      CALL UCTOH1 ( 'IBM  DESY    ', INSTAL, 14 )
#else
      CALL UCTOH('HBOOK  CERN         ',INSTAL,1,20)
#endif
*
      IZCBUF = 0
      CONNPF = .FALSE.
      SERVPF = .FALSE.
*
      NCMINP = 0
      IADINP = 0
      NBIT   = MBIT
      NBITCH = MBITCH
      LINFIT = 5
      LOUT   = MOUT
      BIGP   = HMBIGP
      LERR   = LOUT
      NHT    = 1
      MSTEP  = 1
      NOLD   = 4
      NCHAR  = NBIT/NBITCH
      NCOLPA = 128
      NCOLMA = 100
#if defined(CERNLIB_NEWLIB)
      NLINPA = 85
#else
      NLINPA = 61
#endif
      IDHOLD = 0
      IDLAST = 0
      NV     = 2
      KBINSZ = 0
      KSQUEZ = 0
      LID    = 0
      NRHIST = 0
      IERR   = 0
      IH     = 0
      NH     = 0
      IPONCE = 0
      PNEF   = 0.
      PNBF   = 0.
      PSEL   = -1.
      PFLV   = -1.
      PLUN   = -1.
      PNBX   = -1.
      PNCX   = -1.
      FNIX   = -1.
      FEPS   = -1.
      IFTRNG = 0
      IFXLOW = 1
      IFXUP  = 99999
      IFYLOW = 1
      IFYUP  = 99999
      ISTAF  = 0
      CALL VZERO(ICOPT,9)
*
      CALL VZERO(I1,37)
      K = (NBIT+1)/2
      MAXBIT(1) = 2
      DO 10 I=2,K
         MAXBIT(I)   = MAXBIT(I-1)*2
         MAXBIT(I-1) = MAXBIT(I-1)-1
 10   CONTINUE
      MAXBIT(K) = MAXBIT(K)-1
      CALL VBLANK(IDG,42)
      CALL UCTOH(IDGTDA,IDG,1,42)
      ICSTAR = IDG(37)
      ICBLAC = IDG(34)
      ICFUNC = IDG(37)
      CALL UCTOH(IPROJ,IDENT,4,36)
      CALL UCTOH('NO  ',INO,4,4)
*
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||defined(CERNLIB_MSDOS)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))||defined(CERNLIB_WINNT)
      L2 = 1
#else
      L2 = NBIT-NBITCH+1
#endif
      CALL UCTOH('$   ',IDOL,4,4)
      IDOLAR = JBYT(IDOL,L2,NBITCH)
      IBLANC = JBYT(IDG(41),L2,NBITCH)
*
*-- set Ntuple recover flag off
*
      NRECOV = .FALSE.
*
*-- buffer size of new N-tuple
*-- Should be set to Record_length - 15 words for ZEBRA headers
*
      IBSIZE = 1009
*
*-- no. of bits/word, bits/byte, byte/word and no. of bits to shift to
*-- divide by bits/word (used by new n-tuples)
*
      IBIPW  = MBIT
      IBIPB  = MBITCH
      IBYPW  = NCHAR
      ISHBIT = 0
      DO 20 I = 1, 10
         IF (2**I .EQ. IBIPW) THEN
            ISHBIT = I
         ENDIF
 20   CONTINUE
*
      END
