*
* $Id: linout.inc,v 1.1.1.1 1996/03/04 16:12:59 mclareni Exp $
*
* $Log: linout.inc,v $
* Revision 1.1.1.1  1996/03/04 16:12:59  mclareni
* Dzdoc/Zebpack
*
*
#ifndef CERNLIB_DZDOC_LINOUT_INC
#define CERNLIB_DZDOC_LINOUT_INC
*
*
* linout.inc
*
C--     Parameters and commons defining the output line
 
C--     Output vector
      PARAMETER (NOUTQ=80)
      CHARACTER COUT*(NOUTQ)
C--     Definition of fields on output line
      PARAMETER (IOTAGQ=12,IOENTQ=21,NOENTQ=NOUTQ-IOENTQ+1)
      PARAMETER (INDENQ=2,NENTIQ=10)
 
      COMMON/CDLINE/COUT
      COMMON/IDLINE/IEWORK,IFLOPT(26),INLINE,INSERT,NOUTF,NWHOLL
C--     Description of the variables in common CDLINE and IDLINE
C     Identifier                     Description
C     ==========                     ===========
C     COUT         Character variable representing the output line
C     IEWORK       Pointer to the last position occupied in the work
C                  variable CWORK (Subr. DZDLIN)
C     IFLOPT (26)  Integer array with value non-zero for each option
C                  set (used by DZDOCO and DZDOCS for transmitting the
C                  options to its daughter routines)
C     INLINE       Offset of the start of the information in COUT
C     INSERT       Pointer to the current text field being treated in
C                  the ZEBRA bank
C     NOUTF        Number of columns available on the output line
C                  to contain the text information
C     NWHOLL       Number of words in the ZEBRA bank containing the
C                  text information and remaining to be treated

#endif
