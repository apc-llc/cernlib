*
* $Id: fatvaxrc.inc,v 1.1.1.1 1996/03/07 15:17:52 mclareni Exp $
*
* $Log: fatvaxrc.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:52  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATVAXRC_INC
#define CERNLIB_FATMEN_FATVAXRC_INC
*
*
* fatvaxrc.inc
*
* sequence FATVAXRC
      IF(.NOT.ISTAT) THEN
         CALL LIB$SIGNAL(%VAL(ISTAT))
         IRC = 42
      ENDIF

#endif
