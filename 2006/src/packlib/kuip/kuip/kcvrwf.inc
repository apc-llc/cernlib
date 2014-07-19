*
* $Id: kcvrwf.inc,v 1.1 2000/01/10 10:30:56 couet Exp $
*
* $Log: kcvrwf.inc,v $
* Revision 1.1  2000/01/10 10:30:56  couet
* - new common block kcvrwf shared by kxvrea and kxvwri to store the file
*   name in which the vector in red or written.
*
* Revision 1.1.1.1  2000/10/01 11:00:00 couet
* Kuip
*
*
#ifndef CERNLIB_KUIP_KCVRWF_INC
#define CERNLIB_KUIP_KCVRWF_INC
*
*
* kcvrwf.inc
*
      CHARACTER*160 FNAME
      COMMON /KCVRWF/ FNAME
#endif
