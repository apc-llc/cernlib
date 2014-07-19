#include <stdlib.h>
#include <stdio.h>
#include <cfortran.h>
#include <packlib.h>

#define PAWC_SIZE 50000

typedef float PAWC_DEF[PAWC_SIZE];
#define PAWC COMMON_BLOCK(PAWC,pawc)
COMMON_BLOCK_DEF(PAWC_DEF,PAWC);

main()
{
  int record_size=1024;
  int istat, icycle;

  HLIMIT(PAWC_SIZE);

  HROPEN(1,"MYCWN","mycwn.hbook","NX",record_size,istat);
  if (istat) {
    printf("Error in opening file ...");
    return EXIT_FAILURE;
  }

  HBNT(1,"MYCWN"," ");

  {
  /* Struct is required if defining more than one variable
     within a single call to HBNAMC or HBNAME.
     Struct may also be required in order to introduce
     an aligned variable, see 'int align' below,
     which ensures that the subsequent character arrays are aligned.
     HBNAMC requires the characters to be aligned.
   */
  struct { int align; char a[8]; char b[3][4];} m;
  struct { int align; char c[4]; } n;
  HBNAMC(1,"BLOCK",m.a,"A:C*8,B(3):C");
  HBNAMC(1,"BLOCK",n.c,"C:C");


  /* Since HFNT() gets the character info. via
     an adress stored in the HBNAMC call,
     there is no way that cfortran.h can convert
     from C strings to Fortran strings.
     The character arrays have the same number of bytes in C as they do in
     Fortran. i.e. there is no room for C's trailing '\0' character.
     So use strncpy() and blank pad until the ed of the array.
   */

  strncpy(m.a,    "hiho1   ",8);
  strncpy(m.b[0], "ba  ",4);
  strncpy(m.b[1], "bb  ",4); /* bb and bc don't show in PAW.                  */
  strncpy(m.b[2], "bc  ",4); /* Either PAW is broken, or how I use cfortran.h.*/
  strncpy(n.c,    "coco",4);
  HFNT(1);

  strncpy(m.a,    "hiho2   ",8);
  strncpy(m.b[0], "ba2 ",4);
  strncpy(m.b[1], "bb2 ",4); /* Again bb2 and bc2 don't show in PAW. */
  strncpy(m.b[2], "bc2 ",4);
  strncpy(n.c,    "coc2",4);
  HFNT(1);
  }

  HROUT(0,icycle," ");
  HREND("MYCWN");
  KUCLOS(1," ",1);
}
