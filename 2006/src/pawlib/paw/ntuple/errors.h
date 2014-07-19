/*
 *  errors.h  -  define errors passed around in the parser.
 *
 *  Original: 13-Jul-1994 11:00
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: errors.h,v 1.8 1996/04/23 18:37:56 maartenb Exp $
 *
 *  $Log: errors.h,v $
 *  Revision 1.8  1996/04/23 18:37:56  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef _errors_h
#define _errors_h

typedef enum _return_value_ {

	R_NOERR,		/* all is fine */
	R_TYPE_ERROR,		/* wrong type in expression */
	R_NAME_UNDEF,		/* name undefined */
	R_NOT_IMPLEMENTED,	/* type of sub-expression not implemented */
	R_ALLOC_ERROR,		/* dynamic allocation failed */
	R_INTERNAL_ERROR,	/* inconsistency detected */
	R_MATH_ERROR,		/* error in math of expression */
	R_SYNTAX_ERROR,		/* error during parsing */
	R_LOAD_ERROR,		/* error during (comis) load of file */
	R_NOCUT,		/* Cut does not exist */
	R_RECURSIVECUT,		/* Cut is recursively defined */
	R_NO_HISTOGRAM,		/* id is not a histogram */
	R_SHAPE_ERROR,		/* Runtime error, not matching shapes */
	R_IOERROR,		/* Error reading or writing a file*/
	R_STOP,			/* User requested break (e.g. scan)*/
	R_NOEVT			/* No events were selected */

} ReturnValue;


#endif	/*	_errors_h	*/
