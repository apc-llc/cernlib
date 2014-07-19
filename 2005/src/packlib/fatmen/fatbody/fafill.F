*
* $Id: fafill.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fafill.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAFILL  (LADDR, PATHN, FNAME, KEYS, MEDIA, CHOPT)
*
************************************************************************
*        Borrowed from JDS by JJG on 7-18-89
*        SUBR. FAFILL (LADDR, PATHN, FNAME, *KEYS*, MEDIA, CHOPT)
*
*   Fill the data set data bank(s)
*
*   Arguments :
*
*     LADDR    Bank Address
*     GENEN    File generic name
*     KEYS     Keys vector
*     MEDIA    Media type
*     CHOPT    Character option - U = user will provide media
*
*   Called by FMLIFT
*
*   Error Condition :
*
*     IQUEST(1) =  0 : No error
*
************************************************************************
*
*
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      CHARACTER DSN*256, COMM*80
      CHARACTER*(*) FNAME,PATHN,MEDIA,CHOPT
*
*     Fill in as much information automatically
*     In particular, ensure that all mandatory information is present
*
      CHARACTER*8 CNAME,CTYPE,CSYS,CUSER,CJOB,CACCT
      CHARACTER*6 VSN,VID
      INTEGER FMTIME,FMUSER,FMHOST,FMJOB,FMACNT
      PARAMETER  (LKEYFA=10)
      DIMENSION  KEYS(LKEYFA)
*
*     Bank has already been preset to zeroes - fill hollerith
*     fields with blanks
*
      CALL VBLANK(IQ(LADDR+MFQNFA),MCPLFA-MFQNFA)
      CALL VBLANK(IQ(LADDR+MHSTFA),MVIPFA-MHSTFA)
      CALL VBLANK(IQ(LADDR+MRFMFA),MRLNFA-MRFMFA)
      CALL VBLANK(IQ(LADDR+MFLFFA),MCRTFA-MFLFFA)
      CALL VBLANK(IQ(LADDR+MCURFA),MFPRFA-MCURFA)
      CALL VBLANK(IQ(LADDR+MUCMFA),NWDSFA-MUCMFA+1)
 
      IC = FMTIME(IDATE,ITIME)
*     PACK TIME INTO ONE WORD
      CALL FMPKTM(IDATE,ITIME,IDATM,IRC)
*
      IC = FMHOST(CNAME,CTYPE,CSYS)
      IC = FMJOB(CJOB)
      IC = FMUSER(CUSER)
      IC = FMACNT(CACCT)
*
*     Fill KEYS vector
*
      CALL VBLANK(KEYS(2),5)
      LF = LENOCC(FNAME)
      CALL UCTOH(FNAME(1:LF),KEYS(2),4,LF)
      KEYS(MKSRFA) = 0
      KEYS(MKCLFA) = 0
      KEYS(MKLCFA) = 1
      KEYS(MKMTFA) = 1
      KEYS(MKNBFA) = MKNBFA
*
*     fill data
*
      IQ(LADDR+MCPLFA) = 0
      IQ(LADDR+MLOCFA) = 1
      DSN = ' '
      CALL FMFNM(DSN)
*
*     Default media is disk
*
      IF (MEDIA(1:4) .NE. 'DISK') THEN
*
*        Update to indicate a tape file...
*
         KEYS(MKMTFA) = 2
         IQ(LADDR+MMTPFA) = 2
 
         CALL UCTOH(VSN,IQ(LADDR+MVSNFA),4,6)
         CALL UCTOH(VID,IQ(LADDR+MVIDFA),4,6)
         IQ(LADDR+MDENFA) = 38000
         IQ(LADDR+MFSQFA) = 1
         IQ(LADDR+MVSQFA) = 1
 
         ELSE
 
         IQ(LADDR+MMTPFA) = 1
 
         ENDIF
 
      CALL UCTOH(DSN,IQ(LADDR+MFQNFA),4,LENOCC(DSN))
*             .....
      CALL UCTOH('UN  ',IQ(LADDR+MFLFFA),4,4)
      COMM = 'Added via FORTRAN interface'
      LENCOM = LENOCC(COMM)
      CALL UCTOH(COMM,IQ(LADDR+MUCMFA),4,LENCOM)
*
      CALL VBLANK(IQ(LADDR+MHSNFA),NHSNFA/4)
      CALL VBLANK(IQ(LADDR+MHSTFA),NHSTFA/4)
      CALL VBLANK(IQ(LADDR+MHOSFA),NHOSFA/4)
 
      CALL UCTOH(CNAME,IQ(LADDR+MHSNFA),4,LENOCC(CNAME))
      CALL UCTOH(CTYPE,IQ(LADDR+MHSTFA),4,LENOCC(CTYPE))
      CALL UCTOH(CSYS, IQ(LADDR+MHOSFA),4,LENOCC(CSYS))
*
      IQ(LADDR+MCRTFA) = IDATM
      IQ(LADDR+MCTTFA) = IDATM
*
      CALL VBLANK(IQ(LADDR+MCURFA),NCURFA/4)
      CALL VBLANK(IQ(LADDR+MCNIFA),NCNIFA/4)
      CALL VBLANK(IQ(LADDR+MCIDFA),NCIDFA/4)
      CALL VBLANK(IQ(LADDR+MCJIFA),NCJIFA/4)
 
      CALL UCTOH(CUSER,IQ(LADDR+MCURFA),4,LENOCC(CUSER))
      CALL UCTOH(CNAME,IQ(LADDR+MCNIFA),4,LENOCC(CNAME))
      CALL UCTOH(CACCT,IQ(LADDR+MCIDFA),4,LENOCC(CACCT))
      CALL UCTOH(CJOB, IQ(LADDR+MCJIFA),4,LENOCC(CJOB))
 
*
*                                                            END FAFILL
  999 END
 
