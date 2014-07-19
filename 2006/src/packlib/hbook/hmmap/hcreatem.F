*
* $Id: hcreatem.F,v 1.2 1996/03/13 10:13:20 couet Exp $
*
* $Log: hcreatem.F,v $
* Revision 1.2  1996/03/13 10:13:20  couet
* Mods for ALPHA_OSF: William Badgett, Univ of Wisconsin <BADGETT@vxdesy.desy.de>
*
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          14/09/95  10.00.05  by  Julian Bunn
*-- Author :    Fons Rademakers   20/03/91
*-- Modified :  Wojtek Burkot     02/03/91
      INTEGER FUNCTION HCREATEM(MFILE, IBASE, ISIZE, IOFFST)
************************************************************************
*                                                                      *
* HCREATEM                                                             *
*                                                                      *
* Create a global section. This routine causes the pages at ICOMAD     *
* and continuing for at most ISIZE words (1 word = 4 bytes) to be      *
* mapped from file MFILE. MFILE will be created in the /tmp directory. *
* IOFFST is the offset between the address of the common starting at   *
* IBASE and the address of ICOMAD. The space in ICOMAD can then be     *
* addressed like: IBASE(IOFFST+1,...).                                 *
* On successful completion this function returns 0. In case of an      *
* error -ERRNO is returned.                                            *
* HCREATEM is an interface routine that calls the C routine HCREATEI.  *
*                                                                      *
* After a global section has been created by this function, other      *
* processes can use the data in this global section via the functions  *
* HMAPM and HFREEM.                                                    *
* On BSD machines, the shared memory is automatically deleted when     *
* the process who has created it terminates.                           *
* On non-BSD machines, the user has to delete the shared memory via    *
*     CALL HFREEM(0)                                                   *
*                                                                      *
************************************************************************
*
      CHARACTER*(*) MFILE
#if !defined(CERNLIB_ALPHA_OSF)
      INTEGER       ICOMAD(1), ISIZE, IBASE(1), HCREATEI
      SAVE ICOMAD
#endif
#if defined(CERNLIB_ALPHA_OSF)
      INTEGER       ISIZE, IBASE(1), HCREATEI
      INTEGER*8     ICOMAD(1)
      SAVE ICOMAD
#endif
*
#if defined(CERNLIB_BSD)||defined(CERNLIB_WINNT)
      MFLEN = LENOCC(MFILE)
      HCREATEM = HCREATEI(MFILE, MFLEN, IBASE, ISIZE, ICOMAD)
#endif
#if defined(CERNLIB_SYS5)
      PARAMETER (MFEN=100000000)
      CALL UCTOH(MFILE,MKEY,4,4)
************************************************************************
* modification: change ICOMAD(1) on input to required address of       *
* mapping. MUST! be higher in memory than PAW common to assure         *
* proper action of MZSTORE                                             *
************************************************************************
      ICOMAD(1) = 0
#endif
#if (defined(CERNLIB_DECS))&&(!defined(CERNLIB_ALPHA))
      ICOMAD(1) = 4*LOCF(ICOMAD(1)) + MFEN
      ICOMAD(1) = ((ICOMAD(1)+4095)/4096)*4096
#endif
#if (defined(CERNLIB_DECS))&&(defined(CERNLIB_ALPHA))
      ICOMAD(1) = 4*LOCF(ICOMAD(1)) + MFEN
C Pages on the AXP are 8k !
      ICOMAD(1) = ((ICOMAD(1)+8191)/8192)*8192
#endif
#if defined(CERNLIB_SGI)
      ICOMAD(1) = 4*LOCF(IBASE(1)) + MFEN
      ICOMAD(1) = ((ICOMAD(1)+4095)/4096)*4096
#endif
#if defined(CERNLIB_ALPHA_OSF)
      ICOMAD(1) = %loc(IBASE(1)) + MFEN
      ICOMAD(1) = ((ICOMAD(1)+8191)/8192)*8192
      HCREATEM = HCREATEI(MKEY, ISIZE, %val(ICOMAD(1)) )
#endif
#if defined(CERNLIB_HPUXBUG)
      HCREATEM  = -1
      CALL HBUG('Cannot use HLIMAP on HPUX: Waiting for f77 fix',
     +          'HLIMAP',0)
#endif
#if defined(CERNLIB_SYS5) && !defined(CERNLIB_ALPHA_OSF)
      HCREATEM = HCREATEI(MKEY, ISIZE, ICOMAD)
#endif

#if !defined(CERNLIB_ALPHA_OSF)
      IOFFST = ICOMAD(1) - LOCF(IBASE(1))
#endif
#if defined(CERNLIB_ALPHA_OSF)
      IOFFST = ( ICOMAD(1) - %LOC(IBASE(1)) ) / 4
#endif
*
      END
