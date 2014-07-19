/*
 * $Id: pawlun7.h,v 1.1.1.1 1996/03/01 11:39:13 mclareni Exp $
 *
 * $Log: pawlun7.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:13  mclareni
 * Paw
 *
 */
/*CMZ :  2.03/16 07/10/93  15.13.36  by  Fons Rademakers*/
/*-- Author :*/
/* interface to PAWLUN common block (defined in PAW) */
struct {
   long lunit[128];
   long lunchn;
} pawlun __attribute((__section(pawlun)));

