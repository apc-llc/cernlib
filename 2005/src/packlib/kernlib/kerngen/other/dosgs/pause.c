/*
 * $Id: pause.c,v 1.1.1.1 1996/02/15 17:50:00 mclareni Exp $
 *
 * $Log: pause.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:00  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_WINNT)
#include <conio.h>
int pause_()
  {
     int first_char;
       first_char = _getch();
       if (first_char == 0 ^ first_char == 0xE0) first_char = -_getch();
       return first_char;
  }
#endif
