/*
 * $Id: axis.c,v 1.1.1.1 1996/03/01 11:38:54 mclareni Exp $
 *
 * $Log: axis.c,v $
 * Revision 1.1.1.1  1996/03/01 11:38:54  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.05/01 22/03/94  17.10.56  by  Fons Rademakers*/
/*-- Author :*/
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#include <Xm/Scale.h>

#include "hmotif/pawm.h"
#include "hmotif/axis.h"

/* include bitmaps used by the label orientation widget */

#define x_1_width 50
#define x_1_height 30
static char x_1_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x00,
   0x00, 0x60, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00,
   0x18, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x60, 0x00,
   0x00, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0x03, 0x18, 0x80, 0x01,
   0x60, 0x00, 0x03, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x30, 0x8e, 0x31, 0xf0, 0xf7, 0xf7, 0x00, 0x30, 0x9f, 0x33,
   0xf0, 0xf7, 0xf7, 0x01, 0xb0, 0xb1, 0x37, 0x30, 0x30, 0xb0, 0x01, 0xb0,
   0xb1, 0x37, 0x30, 0x30, 0xb0, 0x01, 0xb0, 0xb1, 0x3d, 0xf0, 0xf1, 0xf1,
   0x01, 0xb0, 0xbf, 0x3d, 0xf0, 0xf1, 0xf1, 0x03, 0xb0, 0xbf, 0x39, 0x30,
   0x30, 0x30, 0x03, 0xb1, 0xb1, 0x39, 0x30, 0x30, 0x30, 0x03, 0x9f, 0xb1,
   0x31, 0x30, 0xf0, 0xf7, 0x03, 0x8e, 0xb1, 0x31, 0x30, 0xf0, 0xf7, 0x03,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define x_2_width 50
#define x_2_height 30
static char x_2_bits[] = {
   0x00, 0x18, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x60,
   0x00, 0x00, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0x03, 0x18, 0x80,
   0x01, 0x60, 0x00, 0x03, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00,
   0xf8, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x40,
   0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x38, 0x00, 0x00,
   0x00, 0x44, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x38, 0x00, 0x00, 0x08,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x38, 0x00, 0x00, 0xf8, 0x00, 0x00, 0x00,
   0x44, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x44, 0x00, 0x00, 0x38, 0x00,
   0x00, 0x00, 0x7c, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x44, 0x00, 0x00,
   0x08, 0x00, 0x00, 0x00, 0x44, 0x00, 0x00, 0xf8, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x44, 0x00, 0x00, 0x78, 0x00, 0x00, 0x00, 0x4c, 0x00, 0x00, 0x88,
   0x00, 0x00, 0x00, 0x54, 0x00, 0x00, 0x78, 0x00, 0x00, 0x00, 0x64, 0x00,
   0x00, 0x88, 0x00, 0x00, 0x00, 0x44, 0x00, 0x00, 0x88, 0x00, 0x00, 0x00,
   0x44, 0x00, 0x00, 0x78, 0x00, 0x00};
#define x_3_width 50
#define x_3_height 30
static char x_3_bits[] = {
   0x00, 0x18, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x60,
   0x00, 0x00, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0x03, 0x18, 0x80,
   0x01, 0x60, 0x00, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x30, 0x00, 0x00, 0xe0, 0x1f, 0x00, 0x00, 0x18, 0x00, 0x00,
   0xe0, 0x1f, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x1b, 0x00, 0x00, 0x18,
   0x00, 0x00, 0x00, 0x1b, 0x00, 0x00, 0xf0, 0x07, 0x00, 0x00, 0x1b, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0xf8, 0x01, 0x00, 0xe0, 0x1f, 0x00, 0x00, 0xf8, 0x03,
   0x00, 0xe0, 0x1f, 0x00, 0x00, 0x60, 0x06, 0x00, 0x60, 0x1b, 0x00, 0x00,
   0x60, 0x06, 0x00, 0x60, 0x1b, 0x00, 0x00, 0xf8, 0x03, 0x00, 0x60, 0x18,
   0x00, 0x00, 0xf8, 0x01, 0x00, 0x60, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0xf8, 0x07, 0x00, 0xe0, 0x1f, 0x00, 0x00, 0xf8,
   0x07, 0x00, 0xe0, 0x1f, 0x00, 0x00, 0x00, 0x03, 0x00, 0x60, 0x1b, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x60, 0x1b, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x60,
   0x1f, 0x00, 0x00, 0xf8, 0x07, 0x00, 0xe0, 0x0d, 0x00, 0x00, 0xf8, 0x07,
   0x00, 0xe0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define x_4_width 50
#define x_4_height 30
static char x_4_bits[] = {
   0x00, 0x18, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x60,
   0x00, 0x00, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0x03, 0x18, 0x80,
   0x01, 0x60, 0x00, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xe0, 0x1f, 0x00, 0x00, 0x78, 0x00, 0x00, 0xe0, 0x1f, 0x00, 0x00,
   0x7b, 0x00, 0x00, 0x00, 0x03, 0x00, 0x80, 0x6f, 0x00, 0x00, 0x80, 0x01,
   0x00, 0x80, 0x6d, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x80, 0x6d, 0x00, 0x00,
   0xe0, 0x1f, 0x00, 0x80, 0x7f, 0x00, 0x00, 0xe0, 0x1f, 0x00, 0x80, 0x7f,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00,
   0x80, 0x61, 0x00, 0x00, 0xc0, 0x1f, 0x00, 0x80, 0x61, 0x00, 0x00, 0x60,
   0x06, 0x00, 0x80, 0x6d, 0x00, 0x00, 0x60, 0x06, 0x00, 0x80, 0x6d, 0x00,
   0x00, 0xc0, 0x1f, 0x00, 0x80, 0x7f, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x80,
   0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x07,
   0x00, 0x80, 0x01, 0x00, 0x00, 0xe0, 0x0f, 0x00, 0x80, 0x0d, 0x00, 0x00,
   0x00, 0x18, 0x00, 0x80, 0x0d, 0x00, 0x00, 0x00, 0x18, 0x00, 0x80, 0x0d,
   0x00, 0x00, 0x00, 0x18, 0x00, 0x80, 0x7f, 0x00, 0x00, 0x00, 0x0c, 0x00,
   0x80, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define x_5_width 50
#define x_5_height 30
static char x_5_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00, 0x80,
   0x01, 0x00, 0x00, 0x03, 0x03, 0x00, 0x80, 0x01, 0x00, 0x00, 0x03, 0x03,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x03, 0x03, 0x00, 0x80, 0x01, 0x00, 0x00,
   0x03, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0x03, 0x18, 0x80, 0x01,
   0x60, 0x00, 0x03, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x30, 0x8e, 0x31, 0xf0, 0xf7, 0xf7, 0x00, 0x30, 0x9f, 0x33,
   0xf0, 0xf7, 0xf7, 0x01, 0xb0, 0xb1, 0x37, 0x30, 0x30, 0xb0, 0x01, 0xb0,
   0xb1, 0x37, 0x30, 0x30, 0xb0, 0x01, 0xb0, 0xb1, 0x3d, 0xf0, 0xf1, 0xf1,
   0x03, 0xb0, 0xbf, 0x3d, 0xf0, 0xf1, 0xf1, 0x03, 0xb0, 0xbf, 0x39, 0x30,
   0x30, 0x30, 0x03, 0xb1, 0xb1, 0x39, 0x30, 0x30, 0x30, 0x03, 0x9f, 0xb1,
   0x31, 0x30, 0xf0, 0xf7, 0x03, 0x8e, 0xb1, 0x31, 0x30, 0xf0, 0xf7, 0x03,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define x_6_width 50
#define x_6_height 30
static char x_6_bits[] = {
   0x03, 0x00, 0x80, 0x01, 0x00, 0x00, 0x03, 0x03, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x03, 0x03, 0x00, 0x80, 0x01, 0x60, 0x00, 0x03, 0x03, 0x18, 0x80,
   0x01, 0x60, 0x00, 0x03, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00,
   0xf8, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x40,
   0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x38, 0x00, 0x00,
   0x00, 0x44, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x38, 0x00, 0x00, 0x08,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x38, 0x00, 0x00, 0xf8, 0x00, 0x00, 0x00,
   0x44, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x44, 0x00, 0x00, 0x38, 0x00,
   0x00, 0x00, 0x7c, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x44, 0x00, 0x00,
   0x08, 0x00, 0x00, 0x00, 0x44, 0x00, 0x00, 0xf8, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x44, 0x00, 0x00, 0x78, 0x00, 0x00, 0x00, 0x4c, 0x00, 0x00, 0x88,
   0x00, 0x00, 0x00, 0x54, 0x00, 0x00, 0x78, 0x00, 0x00, 0x00, 0x64, 0x00,
   0x00, 0x88, 0x00, 0x00, 0x00, 0x44, 0x00, 0x00, 0x88, 0x00, 0x00, 0x00,
   0x44, 0x00, 0x00, 0x78, 0x00, 0x00};
#define x_7_width 50
#define x_7_height 30
static char x_7_bits[] = {
   0x03, 0x00, 0x80, 0x01, 0x00, 0x00, 0x03, 0x03, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x03, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0x03, 0x18, 0x80,
   0x01, 0x60, 0x00, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x30, 0x00, 0x00, 0xe0, 0x1f, 0x00, 0x00, 0x18, 0x00, 0x00,
   0xe0, 0x1f, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x1b, 0x00, 0x00, 0x18,
   0x00, 0x00, 0x00, 0x1b, 0x00, 0x00, 0xf0, 0x07, 0x00, 0x00, 0x1b, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0xf8, 0x01, 0x00, 0xe0, 0x1f, 0x00, 0x00, 0xf8, 0x03,
   0x00, 0xe0, 0x1f, 0x00, 0x00, 0x60, 0x06, 0x00, 0x60, 0x1b, 0x00, 0x00,
   0x60, 0x06, 0x00, 0x60, 0x1b, 0x00, 0x00, 0xf8, 0x03, 0x00, 0x60, 0x18,
   0x00, 0x00, 0xf8, 0x01, 0x00, 0x60, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0xf8, 0x07, 0x00, 0xe0, 0x1f, 0x00, 0x00, 0xf8,
   0x07, 0x00, 0xe0, 0x1f, 0x00, 0x00, 0x00, 0x03, 0x00, 0x60, 0x1b, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x60, 0x1b, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x60,
   0x1f, 0x00, 0x00, 0xf8, 0x07, 0x00, 0xe0, 0x0d, 0x00, 0x00, 0xf8, 0x07,
   0x00, 0xe0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define x_8_width 50
#define x_8_height 30
static char x_8_bits[] = {
   0x03, 0x00, 0x80, 0x01, 0x00, 0x00, 0x03, 0x03, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x03, 0x03, 0x18, 0x80, 0x01, 0x60, 0x00, 0x03, 0x03, 0x18, 0x80,
   0x01, 0x60, 0x00, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xe0, 0x1f, 0x00, 0x00, 0x78, 0x00, 0x00, 0xe0, 0x1f, 0x00, 0x00,
   0x7b, 0x00, 0x00, 0x00, 0x03, 0x00, 0x80, 0x6f, 0x00, 0x00, 0x80, 0x01,
   0x00, 0x80, 0x6d, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x80, 0x6d, 0x00, 0x00,
   0xe0, 0x1f, 0x00, 0x80, 0x7f, 0x00, 0x00, 0xe0, 0x1f, 0x00, 0x80, 0x7f,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00,
   0x80, 0x61, 0x00, 0x00, 0xc0, 0x1f, 0x00, 0x80, 0x61, 0x00, 0x00, 0x60,
   0x06, 0x00, 0x80, 0x6d, 0x00, 0x00, 0x60, 0x06, 0x00, 0x80, 0x6d, 0x00,
   0x00, 0xc0, 0x1f, 0x00, 0x80, 0x7f, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x80,
   0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x07,
   0x00, 0x80, 0x01, 0x00, 0x00, 0xe0, 0x0f, 0x00, 0x80, 0x0d, 0x00, 0x00,
   0x00, 0x18, 0x00, 0x80, 0x0d, 0x00, 0x00, 0x00, 0x18, 0x00, 0x80, 0x0d,
   0x00, 0x00, 0x00, 0x18, 0x00, 0x80, 0x7f, 0x00, 0x00, 0x00, 0x0c, 0x00,
   0x80, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#define y_1_width 60
#define y_1_height 30
static char y_1_bits[] = {
   0x00, 0x20, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x98, 0x99,
   0x31, 0xbf, 0xff, 0x03, 0x00, 0x20, 0x98, 0x99, 0x33, 0xbf, 0xff, 0x03,
   0x00, 0x00, 0x98, 0x99, 0x37, 0x83, 0x01, 0x00, 0x00, 0x20, 0x98, 0x99,
   0x3d, 0x8f, 0x01, 0x00, 0x00, 0x80, 0x98, 0x99, 0x3d, 0x8f, 0x01, 0x00,
   0x00, 0xa0, 0x98, 0x99, 0x39, 0x83, 0x01, 0x00, 0x00, 0x00, 0x0f, 0x8f,
   0x31, 0xbf, 0x01, 0x00, 0x00, 0x20, 0x06, 0x86, 0x31, 0xbf, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x07, 0x00,
   0x00, 0x20, 0x00, 0x00, 0x00, 0x80, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x20, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x80, 0x31, 0x1c, 0xc3, 0x80, 0xff, 0x03, 0x00, 0xa0, 0x3b, 0x3e,
   0x66, 0x80, 0xff, 0x03, 0x00, 0x80, 0x3f, 0x63, 0x3c, 0x80, 0x01, 0x00,
   0x00, 0xa0, 0x3f, 0x63, 0x18, 0x80, 0x01, 0x00, 0x00, 0x80, 0x35, 0x7f,
   0x18, 0x80, 0x01, 0x00, 0x00, 0xa0, 0x31, 0x7f, 0x18, 0x80, 0x01, 0x00,
   0x00, 0x80, 0x31, 0x63, 0x18, 0x80, 0x01, 0x00, 0x00, 0xa0, 0x31, 0x63,
   0x18, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00};
#define y_2_width 60
#define y_2_height 30
static char y_2_bits[] = {
   0x00, 0x00, 0x08, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x33, 0x33, 0xe6,
   0x07, 0x80, 0xff, 0x01, 0x00, 0x33, 0x7b, 0xe6, 0x07, 0x80, 0xff, 0x01,
   0x00, 0x33, 0xf3, 0x66, 0x00, 0x80, 0x01, 0x00, 0x00, 0x33, 0xbb, 0xe7,
   0x01, 0x80, 0x01, 0x00, 0x10, 0x33, 0xb3, 0xe7, 0x01, 0x80, 0x01, 0x00,
   0x10, 0x33, 0x3b, 0x67, 0x00, 0x80, 0x01, 0x00, 0xe0, 0xe1, 0x31, 0xe6,
   0x07, 0x80, 0x01, 0x00, 0xc0, 0xc0, 0x38, 0xe6, 0x07, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x08, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x07, 0x00,
   0x00, 0x00, 0x08, 0x00, 0x00, 0x80, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x08, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x08, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x18, 0xc3, 0x31, 0x0c, 0x80, 0xff, 0x03, 0x00, 0xb8, 0xeb, 0x63,
   0x06, 0x80, 0xff, 0x03, 0x00, 0xf8, 0x33, 0xc6, 0x03, 0x80, 0x01, 0x00,
   0x00, 0xf8, 0x3b, 0x86, 0x01, 0x80, 0x01, 0x00, 0x00, 0x58, 0xf3, 0x87,
   0x01, 0x80, 0x01, 0x00, 0x00, 0x18, 0xfb, 0x87, 0x01, 0x80, 0x01, 0x00,
   0x00, 0x18, 0x33, 0x86, 0x01, 0x80, 0x01, 0x00, 0x00, 0x18, 0x3b, 0x86,
   0x01, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00};
#define y_3_width 60
#define y_3_height 30
static char y_3_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x10, 0x80, 0x01, 0x00, 0x00, 0x33, 0x33, 0xe6,
   0x07, 0x80, 0xff, 0x03, 0x00, 0x33, 0x73, 0xe6, 0x17, 0x80, 0xff, 0x03,
   0x00, 0x33, 0xf3, 0x66, 0x00, 0x80, 0x01, 0x00, 0x00, 0x33, 0xb3, 0xe7,
   0x11, 0x80, 0x01, 0x00, 0x10, 0x33, 0xb3, 0xe7, 0x01, 0x80, 0x01, 0x00,
   0x10, 0x33, 0x33, 0x67, 0x10, 0x80, 0x01, 0x00, 0xe0, 0xe1, 0x31, 0xe6,
   0x07, 0x80, 0x01, 0x00, 0xc0, 0xc0, 0x30, 0xe6, 0x17, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x10, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x07, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x10, 0x80, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x10, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x10, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x80, 0x01, 0x00,
   0x00, 0x8c, 0xe1, 0x18, 0x06, 0x80, 0xff, 0x01, 0x00, 0xdc, 0xf1, 0x31,
   0x13, 0x80, 0xff, 0x01, 0x00, 0xfc, 0x19, 0xe3, 0x01, 0x80, 0x01, 0x00,
   0x00, 0xfc, 0x19, 0xc3, 0x10, 0x80, 0x01, 0x00, 0x00, 0xac, 0xf9, 0xc3,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x8c, 0xf9, 0xc3, 0x10, 0x80, 0x01, 0x00,
   0x00, 0x8c, 0x19, 0xc3, 0x00, 0x80, 0x01, 0x00, 0x00, 0x8c, 0x19, 0xc3,
   0x10, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00};
#define y_4_width 60
#define y_4_height 30
static char y_4_bits[] = {
   0x00, 0x20, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x98, 0x99,
   0x31, 0xbf, 0x01, 0x00, 0x00, 0x20, 0x98, 0x99, 0x33, 0xbf, 0x07, 0x00,
   0x00, 0x00, 0x98, 0x99, 0x37, 0x83, 0x07, 0x00, 0x00, 0x20, 0x98, 0x99,
   0x3d, 0x8f, 0x01, 0x00, 0x00, 0x80, 0x98, 0x99, 0x3d, 0x8f, 0x01, 0x00,
   0x00, 0xa0, 0x98, 0x99, 0x39, 0x83, 0x01, 0x00, 0x00, 0x00, 0x0f, 0x8f,
   0x31, 0xbf, 0x01, 0x00, 0x00, 0x20, 0x06, 0x86, 0x31, 0xbf, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x20, 0x00, 0x00, 0x00, 0x80, 0xff, 0x03, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0xff, 0x03, 0x00, 0x20, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x20, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x80, 0x31, 0x1c, 0xc3, 0x80, 0x01, 0x00, 0x00, 0xa0, 0x3b, 0x3e,
   0x66, 0x80, 0x07, 0x00, 0x00, 0x80, 0x3f, 0x63, 0x3c, 0x80, 0x07, 0x00,
   0x00, 0xa0, 0x3f, 0x63, 0x18, 0x80, 0x01, 0x00, 0x00, 0x80, 0x35, 0x7f,
   0x18, 0x80, 0x01, 0x00, 0x00, 0xa0, 0x31, 0x7f, 0x18, 0x80, 0x01, 0x00,
   0x00, 0x80, 0x31, 0x63, 0x18, 0x80, 0x01, 0x00, 0x00, 0xa0, 0x31, 0x63,
   0x18, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00};
#define y_5_width 60
#define y_5_height 30
static char y_5_bits[] = {
   0x00, 0x00, 0x08, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x33, 0x33, 0xe6,
   0x03, 0x80, 0x07, 0x00, 0x00, 0x33, 0x3b, 0xe6, 0x03, 0x80, 0x07, 0x00,
   0x00, 0x33, 0x73, 0x66, 0x00, 0x80, 0x01, 0x00, 0x00, 0x33, 0xfb, 0xe6,
   0x01, 0x80, 0x01, 0x00, 0x10, 0x33, 0xb3, 0xe7, 0x01, 0x80, 0x01, 0x00,
   0x10, 0x33, 0xbb, 0x67, 0x00, 0x80, 0x01, 0x00, 0xe0, 0xe1, 0x31, 0xe7,
   0x03, 0x80, 0x01, 0x00, 0xc0, 0xc0, 0x38, 0xe6, 0x03, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x08, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0x03,
   0x00, 0x00, 0x08, 0x00, 0x00, 0x80, 0xff, 0x03, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x08, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x08, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x18, 0xc3, 0x31, 0x0c, 0x80, 0x07, 0x00, 0x00, 0xb8, 0xeb, 0x63,
   0x06, 0x80, 0x07, 0x00, 0x00, 0xf8, 0x33, 0xc6, 0x03, 0x80, 0x01, 0x00,
   0x00, 0xf8, 0x3b, 0x86, 0x01, 0x80, 0x01, 0x00, 0x00, 0x58, 0xf3, 0x87,
   0x01, 0x80, 0x01, 0x00, 0x00, 0x18, 0xfb, 0x87, 0x01, 0x80, 0x01, 0x00,
   0x00, 0x18, 0x33, 0x86, 0x01, 0x80, 0x01, 0x00, 0x00, 0x18, 0x3b, 0x86,
   0x01, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00};
#define y_6_width 60
#define y_6_height 30
static char y_6_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x10, 0x80, 0x01, 0x00, 0x00, 0x33, 0x33, 0xe6,
   0x07, 0x80, 0x07, 0x00, 0x00, 0x33, 0x73, 0xe6, 0x17, 0x80, 0x07, 0x00,
   0x00, 0x33, 0xf3, 0x66, 0x00, 0x80, 0x01, 0x00, 0x00, 0x33, 0xb3, 0xe7,
   0x11, 0x80, 0x01, 0x00, 0x10, 0x33, 0xb3, 0xe7, 0x01, 0x80, 0x01, 0x00,
   0x10, 0x33, 0x33, 0x67, 0x10, 0x80, 0x01, 0x00, 0xe0, 0xe1, 0x31, 0xe6,
   0x07, 0x80, 0x01, 0x00, 0xc0, 0xc0, 0x30, 0xe6, 0x17, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x10, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0x01,
   0x00, 0x00, 0x00, 0x00, 0x10, 0x80, 0xff, 0x01, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x10, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x10, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x80, 0x01, 0x00,
   0x00, 0x8c, 0xe1, 0x18, 0x06, 0x80, 0x07, 0x00, 0x00, 0xdc, 0xf1, 0x31,
   0x13, 0x80, 0x07, 0x00, 0x00, 0xfc, 0x19, 0xe3, 0x01, 0x80, 0x01, 0x00,
   0x00, 0xfc, 0x19, 0xc3, 0x10, 0x80, 0x01, 0x00, 0x00, 0xac, 0xf9, 0xc3,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x8c, 0xf9, 0xc3, 0x10, 0x80, 0x01, 0x00,
   0x00, 0x8c, 0x19, 0xc3, 0x00, 0x80, 0x01, 0x00, 0x00, 0x8c, 0x19, 0xc3,
   0x10, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00};

/* extern functions */
extern Widget create_axisFormDialog(void);
extern Widget create_orientationFormDialog(void);
extern Widget create_labelFormDialog(void);

/* extern widgets */
extern Widget DivAxisform;
extern Widget AxisPrimaryText;
extern Widget AxisSecondaryText;
extern Widget AxisTertiaryText;
extern Widget AxisPrimaryScale;
extern Widget AxisSecondaryScale;
extern Widget AxisTertiaryScale;
extern Widget XAxisToggleButtonGadget;
extern Widget YAxisToggleButtonGadget;
extern Widget ZAxisToggleButtonGadget;
extern Widget NoOptimiToggleButtonGadget;
extern Widget gridtoggleButtonGadget;
extern Widget logtoggleButtonGadget;
extern Widget ticktoggleButtonGadget;
extern Widget labelPushButtonGadget;
extern Widget orienPushButtonGadget;
extern Widget AxisApplyButton;
extern Widget AxisResetButton;
extern Widget AxisCloseButton;

extern Widget widget_orien;
extern Widget xdirtoggleButton1;
extern Widget xdirtoggleButton2;
extern Widget xdirtoggleButton3;
extern Widget xdirtoggleButton4;
extern Widget xdirtoggleButton5;
extern Widget xdirtoggleButton6;
extern Widget xdirtoggleButton7;
extern Widget xdirtoggleButton8;
extern Widget ydirtoggleButton1;
extern Widget ydirtoggleButton2;
extern Widget ydirtoggleButton3;
extern Widget ydirtoggleButton4;
extern Widget ydirtoggleButton5;
extern Widget ydirtoggleButton6;
extern Widget resetdirPushButton;
extern Widget closedirPushButton;

extern Widget widget_label;
extern Widget labelText1;
extern Widget labelText2;
extern Widget labelText3;
extern Widget labelText4;
extern Widget labelText5;
extern Widget labelText6;
extern Widget labelText7;
extern Widget labelText8;
extern Widget labelText9;
extern Widget labeltoggleButtonGadget0;
extern Widget labeltoggleButtonGadget1;
extern Widget labeltoggleButtonGadget2;
extern Widget labeltoggleButtonGadget3;
extern Widget labeltoggleButtonGadget4;
extern Widget labeltoggleButtonGadget5;
extern Widget labeltoggleButtonGadget6;
extern Widget labeltoggleButtonGadget7;
extern Widget labeltoggleButtonGadget8;
extern Widget labeltoggleButtonGadget9;
extern Widget labelCloseButton;
extern Widget labelApplyButton;
extern Widget DivAxisform;
extern Widget labelGadget11;
extern Widget labelrowColumn;

extern Boolean grid_active;
extern Boolean zdisac_scale;


/* global data */
Widget widget_axis  = (Widget)NULL;
Widget widget_orien = (Widget)NULL;
Widget widget_label = (Widget)NULL;
AxisStruct axis;

/* global functions */
void axis_reset_cb(Widget, XtPointer, XmAnyCallbackStruct *);
void or_reset_cb(Widget, XtPointer, XmAnyCallbackStruct *);
void xyz_string(int,int,int,int,int,char*);
void show_label(int,int );
void show_orien(void);
void unselect_list(void);
int count_labels(char *);

/* local functions */
static void popdown_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void axis_apply_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void show_division(Widget, XtPointer, XmScaleCallbackStruct *);
static void read_value(Widget, XtPointer, XmToggleButtonCallbackStruct *);
static void show_label_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void show_orien_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void show_grid_cb(Widget, XtPointer, XmToggleButtonCallbackStruct *);
static void show_log_cb(Widget, XtPointer, XmToggleButtonCallbackStruct *);
static void show_tick_cb(Widget, XtPointer, XmToggleButtonCallbackStruct *);
static void show_opt_cb(Widget, XtPointer, XmToggleButtonCallbackStruct *);
static void unselect(void);
static void set_label(Widget,int*,XmToggleButtonCallbackStruct *);
static void set_or(Widget , int *, XmToggleButtonCallbackStruct *);
static void z_disable(void);
static void initialize_widget(void);
static void insert_label(Widget,  int *, XmAnyCallbackStruct *cbs);
static void read_axis_attributes(void);
static void read_label_attributes(void);

/***********************************************************************
 *                                                                     *
 *              Create axis settings panel                             *
 *                                                                     *
 ***********************************************************************/
void axis_settings_dialog(Widget w)
{
   static MenuCbStruct *menu_button;
   XmToggleButtonCallbackStruct cbs1;

   if (!widget_axis) {

      widget_axis = create_axisFormDialog();

      menu_button = make_menuStruct(w, widget_axis);
      InstallMwmCloseCallback(XtParent(widget_axis),
                  (XtCallbackProc)cancel_cb,
                  (XtPointer)menu_button);
      XtVaSetValues(widget_axis, XmNuserData, menu_button, NULL);

      /*
       *   Make sure that if the window is popped down, the label and
       *   orientation buttons are made sensitive again and that all
       *   local popups are unmanaged.
       */
      XtAddCallback(XtParent(widget_axis), XmNpopdownCallback,
                    (XtCallbackProc)popdown_cb, NULL);

      XtAddCallback(AxisApplyButton,XmNactivateCallback,
             (XtCallbackProc)axis_apply_cb,NULL);
      XtAddCallback(AxisResetButton,XmNactivateCallback,
             (XtCallbackProc)axis_reset_cb,NULL);
      XtAddCallback(AxisCloseButton,XmNactivateCallback,
             (XtCallbackProc)cancel_cb,menu_button);
      XtAddCallback(AxisPrimaryScale, XmNvalueChangedCallback,
             (XtCallbackProc)show_division, NULL);
      XtAddCallback(AxisSecondaryScale, XmNvalueChangedCallback,
             (XtCallbackProc)show_division, NULL);
      XtAddCallback(AxisTertiaryScale, XmNvalueChangedCallback,
             (XtCallbackProc)show_division, NULL);
      XtAddCallback(AxisPrimaryScale, XmNdragCallback,
             (XtCallbackProc)show_division, NULL);
      XtAddCallback(AxisSecondaryScale, XmNdragCallback,
             (XtCallbackProc)show_division, NULL);
      XtAddCallback(AxisTertiaryScale, XmNdragCallback,
             (XtCallbackProc)show_division, NULL);
      XtAddCallback(XAxisToggleButtonGadget, XmNvalueChangedCallback,
             (XtCallbackProc)read_value, NULL);
      XtAddCallback(YAxisToggleButtonGadget, XmNvalueChangedCallback,
             (XtCallbackProc)read_value, NULL);
      XtAddCallback(ZAxisToggleButtonGadget, XmNvalueChangedCallback,
             (XtCallbackProc)read_value, NULL);
      XtAddCallback(AxisPrimaryText, XmNactivateCallback,
             (XtCallbackProc)axis_apply_cb,NULL);
      XtAddCallback(AxisSecondaryText, XmNactivateCallback,
             (XtCallbackProc)axis_apply_cb,NULL);
      XtAddCallback(AxisTertiaryText, XmNactivateCallback,
             (XtCallbackProc)axis_apply_cb,NULL);
      XtAddCallback(NoOptimiToggleButtonGadget, XmNvalueChangedCallback,
             (XtCallbackProc)show_opt_cb,NULL);
      XtAddCallback(labelPushButtonGadget, XmNactivateCallback,
             (XtCallbackProc)show_label_cb,NULL);
      XtAddCallback(orienPushButtonGadget, XmNactivateCallback,
             (XtCallbackProc)show_orien_cb,NULL);
      XtAddCallback(gridtoggleButtonGadget, XmNvalueChangedCallback,
             (XtCallbackProc)show_grid_cb, NULL);
      XtAddCallback(logtoggleButtonGadget, XmNvalueChangedCallback,
             (XtCallbackProc)show_log_cb, NULL);
      XtAddCallback(ticktoggleButtonGadget, XmNvalueChangedCallback,
             (XtCallbackProc)show_tick_cb, NULL);

      XtSetSensitive(AxisApplyButton, False);
      axis.dnp = 10;
      axis.dns = 5;
      axis.dnt = 1;
      axis.op = False;
      read_axis_attributes();
      initialize_widget();
      if (widget_axis && XtIsManaged(widget_axis)) {
        cbs1.reason = 0;
        cbs1.event = (XEvent *) NULL;
        cbs1.set = True;
        XtCallCallbacks(XAxisToggleButtonGadget,XmNvalueChangedCallback,
                        &cbs1);
      }
   }
   XtSetSensitive(w, False);
   XtManageChild(widget_axis);
}

/***********************************************************************
 *                                                                     *
 *   Unmanage dialog. This routine should be called by the             *
 *   popdown_cb routine of the parent.                                 *
 *                                                                     *
 ***********************************************************************/
void unmanage_axis_dialog(void)
{
   if (widget_axis) {
      MenuCbStruct  *item;
      XtVaGetValues(widget_axis, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
}

/***********************************************************************
 *                                                                     *
 *   Popdown callback. This callback is called when the widget_axis    *
 *   widget is popped down.                                            *
 *                                                                     *
 ***********************************************************************/
static void popdown_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   if (widget_orien) {
     if (XtIsManaged(widget_orien)) {
       XtUnmanageChild(widget_orien);
       XtSetSensitive(orienPushButtonGadget, True);
     }
   }
   if (widget_label) {
     if (XtIsManaged(widget_label)) {
       XtUnmanageChild(widget_label);
       XtSetSensitive(labelPushButtonGadget, True);
     }
   }
}

/***********************************************************************
 *                                                                     *
 *   Read primary, secondary, tertiary divisions.                      *
 *                                                                     *
 ***********************************************************************/
static void get_axis_division(int *np, int *ns, int *nt, Boolean *op)
{
   char  *pri, *sec, *ter, *s;
   int    t, i, j, result;

   s = XtCalloc(MAX_NUMBER, 1);

   pri = XmTextGetString(AxisPrimaryText);
   sec = XmTextGetString(AxisSecondaryText);
   ter = XmTextGetString(AxisTertiaryText);
   *op = XmToggleButtonGadgetGetState(NoOptimiToggleButtonGadget);

   j = strlen(pri);
   i = 0;
   t = 0;
   result = 1;
   while (result && i < j) {
      result = isdigit(pri[i]);
      t = 10 * t + pri[i] - '0';
      i++;
   }
   if (result) {
      if (t < 1) t = 1;
      if (t >MAX_P) t = MAX_P;
      *np = t;
   }
   j = strlen(sec);
   i = 0;
   t = 0;
   result = 1;
   while (result && i < j) {
      result = isdigit(sec[i]);
      t = 10 * t + sec[i] - '0';
      i++;
   }
   if (result) {
      if(t < 1) t = 1;
      if(t > MAX_S) t = MAX_S;
      *ns = t;
   }
   j = strlen(ter) ;
   i = 0;
   t = 0;
   result = 1;
   while (result && i < j) {
      result = isdigit(ter[i]);
      t = 10 * t + ter[i] - '0';
      i++;
   }
   if (result) {
      if(t < 1) t = 1;
      if(t > MAX_T) t = MAX_T;
      *nt = t;
   }

   XmScaleSetValue(AxisPrimaryScale, *np);
   XmScaleSetValue(AxisSecondaryScale, *ns);
   XmScaleSetValue(AxisTertiaryScale, *nt);
   sprintf(s,"%d", *np);
   XmTextSetString(AxisPrimaryText,s);
   sprintf(s,"%d", *ns);
   XmTextSetString(AxisSecondaryText,s);
   sprintf(s,"%d", *nt);
   XmTextSetString(AxisTertiaryText,s);

   XtFree(s);
   XtFree(pri);
   XtFree(sec);
   XtFree(ter);
}

/***********************************************************************
 *                                                                     *
 * Axis apply callback.                                                *
 *                                                                     *
 ***********************************************************************/
static void axis_apply_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   char  *comstr, *s;

   comstr = XtCalloc(MAX_CMD_LENGTH, 1);
   s      = XtCalloc(MAX_NUMBER, 1);

   if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
      get_axis_division(&axis.xnp, &axis.xns, &axis.xnt, &axis.xop);

   if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
      get_axis_division(&axis.ynp, &axis.yns, &axis.ynt, &axis.yop);

   if (XmToggleButtonGadgetGetState(ZAxisToggleButtonGadget))
      get_axis_division(&axis.znp, &axis.zns, &axis.znt, &axis.zop);

   xyz_string(axis.xnt,axis.xns,axis.xnp,axis.xnl,axis.xo,s);
   if (axis.xop)
      sprintf(comstr,"Set NDVX -%s",s);
   else
      sprintf(comstr,"Set NDVX %s",s);
   execute_kuip_cmd(comstr);

   xyz_string(axis.ynt,axis.yns,axis.ynp,axis.ynl,axis.yo,s);
   if (axis.yop)
      sprintf(comstr,"Set NDVY -%s",s);
   else
      sprintf(comstr,"Set NDVY %s", s);
   execute_kuip_cmd(comstr);

   xyz_string(axis.znt,axis.zns,axis.znp,0,0,s);
   if (axis.zop)
      sprintf(comstr,"Set NDVZ -%s",s);
   else
      sprintf(comstr,"Set NDVZ %s", s);
   execute_kuip_cmd(comstr);
   if((appres.auto_refresh)||(cbs->reason != -999))
      plot_current_histo();

   XtFree(comstr);
   XtFree(s);
}

/***********************************************************************
 *                                                                     *
 *  Axis reset callback.                                               *
 *                                                                     *
 ***********************************************************************/
void axis_reset_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   char *s;

   s = XtCalloc(MAX_NUMBER,1);

   sprintf(s,"%d",axis.dnp);
   XmTextSetString(AxisPrimaryText,s);
   XmScaleSetValue(AxisPrimaryScale,axis.dnp);

   sprintf(s,"%d",axis.dns);
   XmTextSetString(AxisSecondaryText,s);
   XmScaleSetValue(AxisSecondaryScale,axis.dns);

   sprintf(s,"%d",axis.dnt);
   XmTextSetString(AxisTertiaryText,s);
   XmScaleSetValue(AxisTertiaryScale,axis.dnt);

   XmToggleButtonGadgetSetState(NoOptimiToggleButtonGadget,axis.op,False);

   axis.xnp = axis.ynp = axis.znp = axis.dnp;
   axis.xns = axis.yns = axis.zns = axis.dns;
   axis.xnt = axis.ynt = axis.znt = axis.dnt;
   axis.xop = axis.yop = axis.zop = axis.op;
   axis.x_grid = axis.y_grid = False;

   if (widget_orien) {
     unselect();
     XmToggleButtonGadgetSetState(xdirtoggleButton1,True,False);
     XmToggleButtonGadgetSetState(ydirtoggleButton3,True,False);
   }
   if (widget_label) {
     unselect_list();
     XmToggleButtonGadgetSetState(labeltoggleButtonGadget0,True,False);
   }
   XmToggleButtonGadgetSetState(logtoggleButtonGadget,False,False);
   XmToggleButtonGadgetSetState(gridtoggleButtonGadget,False,False);
   axis.yo = 3;
   axis.xo = 1;
   axis.xnl = 0;
   axis.ynl = 0;
   execute_kuip_cmd("Option NGRI");
   execute_kuip_cmd("Option LINX");
   execute_kuip_cmd("Option LINY");
   execute_kuip_cmd("Option LINZ");
   execute_kuip_cmd("Set NDVX");
   execute_kuip_cmd("Set NDVY");
   execute_kuip_cmd("Set NDVZ");
   if (cbs->event)
      plot_current_histo();

   XtFree(s);
}

/***********************************************************************
 *                                                                     *
 *   Update the x, y or z axis divisions.                              *
 *                                                                     *
 ***********************************************************************/
static void show_axis_division(Widget w, int value,int *np, int *ns, int *nt,
                   Boolean *op, int nl, int no, char *axis)
{
   char  *comstr;
   char  *s;

   comstr = XtCalloc(MAX_CMD_LENGTH, 1);
   s      = XtCalloc(MAX_NUMBER, 1);

   *op = XmToggleButtonGadgetGetState(NoOptimiToggleButtonGadget);
   if (w == AxisPrimaryScale) {

      if (value == *np) goto end;

      *np = value;
      sprintf(s,"%d", *np);
      XmTextSetString(AxisPrimaryText,s);
      xyz_string(*nt,*ns,value,nl,no,s);
      if (*op)
       sprintf(comstr,"Set %s -%s", axis, s);
      else
       sprintf(comstr,"Set %s %s", axis, s);
   }
   if (w == AxisSecondaryScale) {

      if (value == *ns) goto end;

      *ns = value;
      sprintf(s,"%d",*ns);
      XmTextSetString(AxisSecondaryText,s);
      xyz_string(*nt,value,*np,nl,no,s);
      if (*op)
       sprintf(comstr,"Set %s -%s", axis, s);
      else
       sprintf(comstr,"Set %s %s", axis, s);
   }
   if (w == AxisTertiaryScale) {

      if (value == *nt) goto end;

      *nt = value;
      sprintf(s,"%d",*nt);
      XmTextSetString(AxisTertiaryText,s);
      xyz_string(value,*ns,*np,nl,no,s);
      if (*op)
       sprintf(comstr,"Set %s -%s", axis, s);
      else
       sprintf(comstr,"Set %s %s", axis, s);
   }

   execute_kuip_cmd(comstr);

   if (appres.auto_refresh) {
      plot_current_histo();
   }

end:
   XtFree(comstr);
   XtFree(s);
}

/***********************************************************************
 *                                                                     *
 *  Division scale value changed callback.                             *
 *                                                                     *
 ***********************************************************************/
static void show_division(Widget w, XtPointer cd, XmScaleCallbackStruct *cbs)
{

   if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
      show_axis_division(w, cbs->value, &axis.xnp, &axis.xns, &axis.xnt,
             &axis.xop, axis.xnl, axis.xo, "NDVX");

   if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
      show_axis_division(w, cbs->value, &axis.ynp, &axis.yns, &axis.ynt,
             &axis.yop, axis.ynl, axis.yo, "NDVY");

   if (XmToggleButtonGadgetGetState(ZAxisToggleButtonGadget))
      show_axis_division(w, cbs->value, &axis.znp, &axis.zns, &axis.znt,
             &axis.zop, 0, 0, "NDVZ");
}

/***********************************************************************
 *                                                                     *
 *   X, y or z axis selection toggle button callback.                  *
 *   This callback saves the values for the previous selected axis and *
 *   restores the values for the newly selected axis.                  *
 *                                                                     *
 ***********************************************************************/
static void read_value(Widget w, XtPointer cd,
                XmToggleButtonCallbackStruct *cbs)
{
   char t[MAX_NUMBER];
   char *pri, *sec, *ter;
   XmAnyCallbackStruct cbs1;

   if (w == XAxisToggleButtonGadget) {
      if (!cbs->set) {
       pri        = XmTextGetString(AxisPrimaryText);
       sec        = XmTextGetString(AxisSecondaryText);
       ter        = XmTextGetString(AxisTertiaryText);
       axis.xop   = XmToggleButtonGadgetGetState(NoOptimiToggleButtonGadget);
       axis.x_log = XmToggleButtonGadgetGetState(logtoggleButtonGadget);
       if (grid_active)
           axis.x_grid = XmToggleButtonGadgetGetState(gridtoggleButtonGadget);
       axis.xnp = atoi(pri);
       axis.xns = atoi(sec);
       axis.xnt = atoi(ter);
       XtFree(pri);
       XtFree(sec);
       XtFree(ter);
      } else {
       set_environment();
       sprintf(t,"%d",axis.xnp);
       XmTextSetString(AxisPrimaryText,t);
       sprintf(t,"%d",axis.xns);
       XmTextSetString(AxisSecondaryText,t);
       sprintf(t,"%d",axis.xnt);
       XmTextSetString(AxisTertiaryText,t);
       XmScaleSetValue(AxisPrimaryScale,axis.xnp);
       XmScaleSetValue(AxisSecondaryScale,axis.xns);
       XmScaleSetValue(AxisTertiaryScale,axis.xnt);
       XmToggleButtonGadgetSetState(NoOptimiToggleButtonGadget,
                                    axis.xop, False);
       XmToggleButtonGadgetSetState(logtoggleButtonGadget, axis.x_log, False);
       if (grid_active){
          XtSetSensitive(gridtoggleButtonGadget, True);
          XmToggleButtonGadgetSetState(gridtoggleButtonGadget,
                   axis.x_grid, False);
       }
      }
      if (widget_label) {
        if (XtIsManaged(widget_label)) {
         show_label(axis.xnl,1);
         XtSetSensitive(labelPushButtonGadget,False);
        }
      }
      if (widget_orien) {
        if (XtIsManaged(widget_orien)) {
         XtSetSensitive(orienPushButtonGadget,False);
        }
      }
   }

   if (w == YAxisToggleButtonGadget) {
      if (!cbs->set) {
       pri        = XmTextGetString(AxisPrimaryText);
       sec        = XmTextGetString(AxisSecondaryText);
       ter        = XmTextGetString(AxisTertiaryText);
       axis.yop   = XmToggleButtonGadgetGetState(NoOptimiToggleButtonGadget);
       axis.y_log = XmToggleButtonGadgetGetState(logtoggleButtonGadget);
       if (grid_active)
           axis.y_grid = XmToggleButtonGadgetGetState(gridtoggleButtonGadget);
       axis.ynp = atoi(pri);
       axis.yns = atoi(sec);
       axis.ynt = atoi(ter);
       XtFree(pri);
       XtFree(sec);
       XtFree(ter);
      } else {
       set_environment();
       sprintf(t,"%d",axis.ynp);
       XmTextSetString(AxisPrimaryText,t);
       sprintf(t,"%d",axis.yns);
       XmTextSetString(AxisSecondaryText,t);
       sprintf(t,"%d",axis.ynt);
       XmTextSetString(AxisTertiaryText,t);
       XmScaleSetValue(AxisPrimaryScale,axis.ynp);
       XmScaleSetValue(AxisSecondaryScale,axis.yns);
       XmScaleSetValue(AxisTertiaryScale,axis.ynt);
       XmToggleButtonGadgetSetState(NoOptimiToggleButtonGadget,
                     axis.yop, False);
       XmToggleButtonGadgetSetState(logtoggleButtonGadget, axis.y_log, False);
       if (grid_active){
          XtSetSensitive(gridtoggleButtonGadget,True);
          XmToggleButtonGadgetSetState(gridtoggleButtonGadget,
                   axis.y_grid, False);
       }
      }
      if (widget_label) {
        if (XtIsManaged(widget_label)) {
          show_label(axis.ynl,2);
          XtSetSensitive(labelPushButtonGadget,False);
        }
      }
      if (widget_orien) {
        if (XtIsManaged(widget_orien)) {
          XtSetSensitive(orienPushButtonGadget,False);
        }
      }
   }

   if (w == ZAxisToggleButtonGadget) {
      if (widget_label) {
        if (XtIsManaged(widget_label)) {
          cbs1.reason = 0;
          cbs1.event = (XEvent*)NULL;
          XtCallCallbacks(labelCloseButton,XmNactivateCallback,&cbs1);
        }
      }
      if (widget_orien) {
        if (XtIsManaged(widget_orien)) {
          cbs1.reason = 0;
          cbs1.event = (XEvent *)NULL;
          XtCallCallbacks(closedirPushButton,XmNactivateCallback,&cbs1);
        }
      }
      XtSetSensitive(orienPushButtonGadget,False);
      XtSetSensitive(labelPushButtonGadget,False);
      if (!cbs->set) {
         pri        = XmTextGetString(AxisPrimaryText);
         sec        =  XmTextGetString(AxisSecondaryText);
         ter        = XmTextGetString(AxisTertiaryText);
         axis.zop   = XmToggleButtonGadgetGetState(NoOptimiToggleButtonGadget);
         axis.z_log = XmToggleButtonGadgetGetState(logtoggleButtonGadget);
         axis.znp = atoi(pri);
         axis.zns = atoi(sec);
         axis.znt = atoi(ter);
         XtFree(pri);
         XtFree(sec);
         XtFree(ter);
      } else {
         sprintf(t,"%d",axis.znp);
         XmTextSetString(AxisPrimaryText,t);
         sprintf(t,"%d",axis.zns);
         XmTextSetString(AxisSecondaryText,t);
         sprintf(t,"%d",axis.znt);
         XmTextSetString(AxisTertiaryText,t);
         XmScaleSetValue(AxisPrimaryScale,axis.znp);
         XmScaleSetValue(AxisSecondaryScale,axis.zns);
         XmScaleSetValue(AxisTertiaryScale,axis.znt);
         XmToggleButtonGadgetSetState(NoOptimiToggleButtonGadget,
                  axis.zop, False);
         XmToggleButtonGadgetSetState(logtoggleButtonGadget,axis.z_log, False);
         XtSetSensitive(gridtoggleButtonGadget,False);
         XmToggleButtonGadgetSetState(gridtoggleButtonGadget,
                  False, False);
      }
   }
}

/***********************************************************************
 *                                                                     *
 *  Grid on/off toggle button callback.                                *
 *                                                                     *
 ***********************************************************************/
static void show_grid_cb(Widget w, XtPointer cd,
             XmToggleButtonCallbackStruct *cbs)
{
   XmAnyCallbackStruct cbs1;
   char *comstr;

   comstr = XtCalloc(MAX_CMD_LENGTH,1);
   if (cbs->set) {
      if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget) &&
          !axis.y_grid) {
       axis.x_grid = True;
       sprintf(comstr,"Option GRID");
       execute_kuip_cmd(comstr);
       sprintf(comstr,"Set GRID 2003");
      }
      if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget) &&
          axis.y_grid) {
       axis.x_grid = True;
       sprintf(comstr,"Option GRID");
       execute_kuip_cmd(comstr);
       sprintf(comstr,"Set GRID 3");
      }
      if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget) &&
          !axis.x_grid) {
       axis.y_grid = True;
       sprintf(comstr,"Option GRID");
       execute_kuip_cmd(comstr);
       sprintf(comstr,"Set GRID 1003");
      }
      if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget) &&
          axis.x_grid) {
       axis.y_grid = True;
       sprintf(comstr,"Option GRID");
       execute_kuip_cmd(comstr);
       sprintf(comstr,"Set GRID 3");
      }
   } else {
      if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget) &&
          !axis.y_grid) {
       axis.x_grid = False;
       sprintf(comstr,"Option NGRI");
      }
      if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget) &&
          axis.y_grid) {
       axis.x_grid = False;
       sprintf(comstr,"Option GRID");
       execute_kuip_cmd(comstr);
       sprintf(comstr,"Set GRID 1003");
      }
      if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget) &&
          !axis.x_grid) {
       axis.y_grid = False;
       sprintf(comstr,"Option NGRI");
      }
      if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget) &&
          axis.x_grid) {
       axis.y_grid = False;
       sprintf(comstr,"Option GRID");
       execute_kuip_cmd(comstr);
       sprintf(comstr,"Set GRID 2003");
      }
   }

   execute_kuip_cmd(comstr);
   XtFree(comstr);

   cbs1.reason = -999;
   cbs1.event = (XEvent *) NULL;
   XtCallCallbacks(AxisApplyButton,XmNactivateCallback, &cbs1);
}

/***********************************************************************
 *                                                                     *
 *   Logarithmic on/off toggle button callback.                        *
 *                                                                     *
 ***********************************************************************/
static void show_log_cb(Widget w, XtPointer cd,
            XmToggleButtonCallbackStruct *cbs)
{
   XmAnyCallbackStruct  cbs1;
   char                *comstr;

   comstr = XtCalloc(MAX_CMD_LENGTH,1);
   if (cbs->set) {
      if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget)) {
       axis.x_log = True;
       sprintf(comstr,"Option LOGX");
      }
      if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget)) {
       axis.y_log = True;
       sprintf(comstr,"Option LOGY");
      }
      if (XmToggleButtonGadgetGetState(ZAxisToggleButtonGadget)) {
       axis.z_log = True;
       sprintf(comstr,"Option LOGZ");
      }
   } else {
      if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget)) {
       axis.x_log = False;
       sprintf(comstr,"Option LINX");
      }
      if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget)) {
       axis.y_log = False;
       sprintf(comstr,"Option LINY");
      }
      if (XmToggleButtonGadgetGetState(ZAxisToggleButtonGadget)) {
       axis.z_log = False;
       sprintf(comstr,"Option LINZ");
      }
   }
   execute_kuip_cmd(comstr);
   XtFree(comstr);

   cbs1.reason = -999;
   cbs1.event = (XEvent *) NULL;
   XtCallCallbacks(AxisApplyButton,XmNactivateCallback, &cbs1);
}

/***********************************************************************
 *                                                                     *
 *   Ticks on/off toggle button callback.                              *
 *                                                                     *
 ***********************************************************************/
static void show_tick_cb(Widget w, XtPointer cd,
            XmToggleButtonCallbackStruct *cbs)
{
   char                *comstr;

   comstr = XtCalloc(MAX_CMD_LENGTH,1);
   if (cbs->set) {
       sprintf(comstr,"Option TIC");
   } else {
       sprintf(comstr,"Option NTIC");
   }
   execute_kuip_cmd(comstr);
   XtFree(comstr);
   if(appres.auto_refresh) plot_current_histo();
}

/***********************************************************************
 *                                                                     *
 *   Optimization on/off toggle button callback.                       *
 *                                                                     *
 ***********************************************************************/
static void show_opt_cb(Widget w, XtPointer cd,
            XmToggleButtonCallbackStruct *cbs)
{
   XmAnyCallbackStruct  cbs1;

   cbs1.reason = -999;
   cbs1.event = (XEvent *) NULL;
   XtCallCallbacks(AxisApplyButton,XmNactivateCallback, &cbs1);
}

/***********************************************************************
 *                                                                     *
 *   Create and show orientation panel.                                *
 *                                                                     *
 ***********************************************************************/
static void show_orien_cb(Widget w, XtPointer cd,
              XmAnyCallbackStruct *cbs)
{
   MenuCbStruct *menu_button;

   if (!widget_orien) {
      widget_orien = create_orientationFormDialog();
      menu_button = make_menuStruct(w, widget_orien);
      InstallMwmCloseCallback(XtParent(widget_orien),
                  (XtCallbackProc)cancel_cb,
                  (XtPointer)menu_button);
      /*if(axis.xnl != 0) {
       axis.xo = 5;
       XmToggleButtonGadgetSetState(xdirtoggleButton5,True,False);
      }
      else {
       axis.xo = 1;
       XmToggleButtonGadgetSetState(xdirtoggleButton1,True,False);
      }
      if(axis.ynl != 0) {
       axis.yo = 6;
       XmToggleButtonGadgetSetState(ydirtoggleButton6,True,False);
      }
      else {
       axis.yo = 3;
       XmToggleButtonGadgetSetState(ydirtoggleButton3,True,False);
      }*/

      XtAddCallback(xdirtoggleButton1,
                    XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)1);
      set_pixmap_to_button(xdirtoggleButton1, x_1_bits, x_1_width, x_1_height,
               True);
      XtAddCallback(xdirtoggleButton2,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)2);
      set_pixmap_to_button(xdirtoggleButton2, x_2_bits, x_2_width, x_2_height,
               True);
      XtAddCallback(xdirtoggleButton3,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)3);
      set_pixmap_to_button(xdirtoggleButton3, x_3_bits, x_3_width, x_3_height,
               True);
      XtAddCallback(xdirtoggleButton4,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)4);
      set_pixmap_to_button(xdirtoggleButton4, x_4_bits, x_4_width, x_4_height,
               True);
      XtAddCallback(xdirtoggleButton5,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)5);
      set_pixmap_to_button(xdirtoggleButton5, x_5_bits, x_5_width, x_5_height,
               True);
      XtAddCallback(xdirtoggleButton6,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)6);
      set_pixmap_to_button(xdirtoggleButton6, x_6_bits, x_6_width, x_6_height,
               True);
      XtAddCallback(xdirtoggleButton7,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)7);
      set_pixmap_to_button(xdirtoggleButton7, x_7_bits, x_7_width, x_7_height,
               True);
      XtAddCallback(xdirtoggleButton8,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)8);
      set_pixmap_to_button(xdirtoggleButton8, x_8_bits, x_8_width, x_8_height,
               True);
      XtAddCallback(ydirtoggleButton1,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)9);
      set_pixmap_to_button(ydirtoggleButton1, y_1_bits, y_1_width, y_1_height,
               True);
      XtAddCallback(ydirtoggleButton2,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)10);
      set_pixmap_to_button(ydirtoggleButton2, y_2_bits, y_2_width, y_2_height,
               True);
      XtAddCallback(ydirtoggleButton3,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)11);
      set_pixmap_to_button(ydirtoggleButton3, y_3_bits, y_3_width, y_3_height,
               True);
      XtAddCallback(ydirtoggleButton4,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)12);
      set_pixmap_to_button(ydirtoggleButton4, y_4_bits, y_4_width, y_4_height,
               True);
      XtAddCallback(ydirtoggleButton5,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)13);
      set_pixmap_to_button(ydirtoggleButton5, y_5_bits, y_5_width, y_5_height,
               True);
      XtAddCallback(ydirtoggleButton6,
             XmNvalueChangedCallback, (XtCallbackProc)set_or,(int*)14);
      set_pixmap_to_button(ydirtoggleButton6, y_6_bits, y_6_width, y_6_height,
               True);

      XtAddCallback(resetdirPushButton,XmNactivateCallback,
             (XtCallbackProc)or_reset_cb,NULL);
      XtAddCallback(closedirPushButton,XmNactivateCallback,
             (XtCallbackProc)cancel_cb, menu_button);
   }
   show_orien();
   XtSetSensitive(w, False);
   XtManageChild(widget_orien);
}

/***********************************************************************
 *                                                                     *
 *    Create and show label panel.                                     *
 *                                                                     *
 ***********************************************************************/
static void show_label_cb(Widget w, XtPointer cd,
              XmAnyCallbackStruct *cbs)
{
   MenuCbStruct *menu_button;

   if (!widget_label) {
      widget_label = create_labelFormDialog();
      menu_button  = make_menuStruct(w, widget_label);
      InstallMwmCloseCallback(XtParent(widget_label),
                  (XtCallbackProc)cancel_cb,
                  (XtPointer)menu_button);
      read_label_attributes();

      XtAddCallback(labelCloseButton, XmNactivateCallback,
             (XtCallbackProc)cancel_cb, menu_button);
      XtAddCallback(labeltoggleButtonGadget0,
                    XmNvalueChangedCallback, (XtCallbackProc)set_label,(int*)0);
      XtAddCallback(labeltoggleButtonGadget1,
                    XmNvalueChangedCallback, (XtCallbackProc)set_label,(int*)1);
      XtAddCallback(labeltoggleButtonGadget2,
                    XmNvalueChangedCallback, (XtCallbackProc)set_label,(int*)2);
      XtAddCallback(labeltoggleButtonGadget3,
                    XmNvalueChangedCallback, (XtCallbackProc)set_label,(int*)3);
      XtAddCallback(labeltoggleButtonGadget4,
                    XmNvalueChangedCallback, (XtCallbackProc)set_label,(int*)4);
      XtAddCallback(labeltoggleButtonGadget5,
                    XmNvalueChangedCallback, (XtCallbackProc)set_label,(int*)5);
      XtAddCallback(labeltoggleButtonGadget6,
                    XmNvalueChangedCallback, (XtCallbackProc)set_label,(int*)6);
      XtAddCallback(labeltoggleButtonGadget7,
                    XmNvalueChangedCallback, (XtCallbackProc)set_label,(int*)7);
      XtAddCallback(labeltoggleButtonGadget8,
                    XmNvalueChangedCallback, (XtCallbackProc)set_label,(int*)8);
      XtAddCallback(labeltoggleButtonGadget9,
                    XmNvalueChangedCallback, (XtCallbackProc)set_label,(int*)9);
      XtAddCallback(labelText1,XmNactivateCallback,
                  (XtCallbackProc)insert_label,(int*)1);
      XtAddCallback(labelText2,XmNactivateCallback,
                  (XtCallbackProc)insert_label,(int*)2);
      XtAddCallback(labelText3,XmNactivateCallback,
                  (XtCallbackProc)insert_label,(int*)3);
      XtAddCallback(labelText4,XmNactivateCallback,
                  (XtCallbackProc)insert_label,(int*)4);
      XtAddCallback(labelText5,XmNactivateCallback,
                  (XtCallbackProc)insert_label,(int*)5);
      XtAddCallback(labelText6,XmNactivateCallback,
                  (XtCallbackProc)insert_label,(int*)6);
      XtAddCallback(labelText7,XmNactivateCallback,
                  (XtCallbackProc)insert_label,(int*)7);
      XtAddCallback(labelText8,XmNactivateCallback,
                  (XtCallbackProc)insert_label,(int*)8);
      XtAddCallback(labelText9,XmNactivateCallback,
                  (XtCallbackProc)insert_label,(int*)9);
   }

   if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
      show_label(axis.xnl,1);
   if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
      show_label(axis.ynl,2);
   if (XmToggleButtonGadgetGetState(ZAxisToggleButtonGadget))
      show_label(0,3);

   XtSetSensitive(w, False);
   XtManageChild(widget_label);
}

/***********************************************************************
 *                                                                     *
 *   Orientation toggle button callback.                               *
 *                                                                     *
 ***********************************************************************/
static void set_or(Widget w, int *type, XmToggleButtonCallbackStruct *cbs)
{
   XmAnyCallbackStruct cbs1;

   switch ((int)type) {
      case 1:
       axis.xo = 1;
       break;
      case 2:
       axis.xo = 2;
       break;
      case 3:
       axis.xo = 3;
       break;
      case 4:
       axis.xo = 4;
       break;
      case 5:
       axis.xo = 5;
       break;
      case 6:
       axis.xo = 6;
       break;
      case 7:
       axis.xo = 7;
       break;
      case 8:
       axis.xo = 8;
       break;
      case 9:
       axis.yo = 1;
       break;
      case 10:
       axis.yo = 2;
       break;
      case 11:
       axis.yo = 3;
       break;
      case 12:
       axis.yo = 4;
       break;
      case 13:
       axis.yo = 5;
       break;
      case 14:
       axis.yo = 6;
       break;
      default:
       return;
   }
   cbs1.reason = -999;
   cbs1.event = (XEvent *) NULL;
   if (cbs->set)
      XtCallCallbacks(AxisApplyButton,XmNactivateCallback, &cbs1);
}

/***********************************************************************
 *                                                                     *
 *    Orientation reset callback.                                      *
 *                                                                     *
 ***********************************************************************/
void or_reset_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   XmAnyCallbackStruct cbs1;

   unselect();
   if(axis.xnl != 0) {
      axis.xo = 5;
      XmToggleButtonGadgetSetState(xdirtoggleButton5,True,False);
   } else {
      axis.xo = 1;
      XmToggleButtonGadgetSetState(xdirtoggleButton1,True,False);
   }
   if(axis.ynl != 0) {
      axis.yo = 6;
      XmToggleButtonGadgetSetState(ydirtoggleButton6,True,False);
   } else {
      axis.yo = 3;
      XmToggleButtonGadgetSetState(ydirtoggleButton3,True,False);
   }
   cbs1.reason = -999;
   cbs1.event = (XEvent *) NULL;
   XtCallCallbacks(AxisApplyButton,XmNactivateCallback, &cbs1);
}

/***********************************************************************
 *                                                                     *
 *    Set alphanumeric labels. Callback on the toggle buttons in front *
 *    of the label list.                                               *
 *                                                                     *
 ***********************************************************************/
static void set_label(Widget w, int *type, XmToggleButtonCallbackStruct *cbs)
{
   XmAnyCallbackStruct cbs1;
   char *s, *comstr, *str;
   int num;

   unselect_list();
   comstr = XtCalloc(MAX_CMD_LENGTH, 1);

   switch ((int)type) {
      case 0:

       XmToggleButtonGadgetSetState(labeltoggleButtonGadget0,True,False);
       if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
          axis.xnl = 0;
       if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
            axis.ynl = 0;
       break;

      case 1:

       XmToggleButtonGadgetSetState(labeltoggleButtonGadget1,True,False);
       str = XmTextGetString(labelText1);
       s = km_strip(str);
       if (!strempty(s)) {
          num = count_labels(s);
          if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
             axis.xnl = 1;
          if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
             axis.ynl = 1;
          sprintf(comstr,"LABEL 1 %d %s", num,s);
          execute_kuip_cmd(comstr);
       } else {
          XtFree(str);
          XtFree(s);
          XtFree(comstr);
          XmToggleButtonGadgetSetState(labeltoggleButtonGadget1,False,False);
          return;
       }
       XtFree(str);
       XtFree(s);
       break;

      case 2:

       XmToggleButtonGadgetSetState(labeltoggleButtonGadget2,True,False);
       str = XmTextGetString(labelText2);
       s = km_strip(str);
       if (!strempty(s)) {
          num = count_labels(s);
          if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
             axis.xnl = 2;
          if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
             axis.ynl = 2;
          sprintf(comstr,"LABEL 2 %d %s", num,s);
          execute_kuip_cmd(comstr);
       } else {
          XtFree(str);
          XtFree(s);
          XtFree(comstr);
          XmToggleButtonGadgetSetState(labeltoggleButtonGadget2,False,False);
          return;
       }
       XtFree(str);
       XtFree(s);
       break;

      case 3:

       XmToggleButtonGadgetSetState(labeltoggleButtonGadget3,True,False);
       str = XmTextGetString(labelText3);
       s = km_strip(str);
       if (!strempty(s)) {
          num = count_labels(s);
          if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
             axis.xnl = 3;
          if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
             axis.ynl = 3;
          sprintf(comstr,"LABEL 3 %d %s", num,s);
          execute_kuip_cmd(comstr);
       } else {
          XtFree(str);
          XtFree(s);
          XtFree(comstr);
          XmToggleButtonGadgetSetState(labeltoggleButtonGadget3,False,False);
          return;
       }
       XtFree(str);
       XtFree(s);
       break;

      case 4:

       XmToggleButtonGadgetSetState(labeltoggleButtonGadget4,True,False);
       str = XmTextGetString (labelText4);
       s = km_strip(str);
       if (!strempty (s)) {
          num = count_labels(s);
          if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
             axis.xnl = 4;
          if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
             axis.ynl = 4;
          sprintf(comstr,"LABEL 4 %d %s", num,s);
          execute_kuip_cmd(comstr);
       } else {
          XtFree(str);
          XtFree(s);
          XtFree(comstr);
          XmToggleButtonGadgetSetState(labeltoggleButtonGadget4,False,False);
          return;
       }
       XtFree(str);
       XtFree(s);
       break;

      case 5:

       XmToggleButtonGadgetSetState(labeltoggleButtonGadget5,True,False);
       str = XmTextGetString (labelText5);
       s = km_strip(str);
       if (!strempty (s)) {
          num = count_labels(s);
          if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
             axis.xnl = 5;
          if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
             axis.ynl = 5;
          sprintf(comstr,"LABEL 5 %d %s", num,s);
          execute_kuip_cmd(comstr);
       } else {
          XtFree(str);
          XtFree(s);
          XtFree(comstr);
          XmToggleButtonGadgetSetState(labeltoggleButtonGadget5,False,False);
          return;
       }
       XtFree(str);
       XtFree(s);
       break;

      case 6:

       XmToggleButtonGadgetSetState(labeltoggleButtonGadget6,True,False);
       str = XmTextGetString (labelText6);
       s = km_strip(str);
       if (!strempty (s)) {
          num = count_labels(s);
          if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
             axis.xnl = 6;
          if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
             axis.ynl = 6;
          sprintf(comstr,"LABEL 6 %d %s", num,s);
          execute_kuip_cmd(comstr);
       } else {
          XtFree(str);
          XtFree(s);
          XtFree(comstr);
          XmToggleButtonGadgetSetState(labeltoggleButtonGadget6,False,False);
          return;
       }
       XtFree(str);
       XtFree(s);
       break;

      case 7:

       XmToggleButtonGadgetSetState(labeltoggleButtonGadget7,True,False);
       str = XmTextGetString (labelText7);
       s = km_strip(str);
       if (!strempty (s)) {
          num = count_labels(s);
          if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
             axis.xnl = 7;
          if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
             axis.ynl = 7;
          sprintf(comstr,"LABEL 7 %d %s", num,s);
          execute_kuip_cmd(comstr);
       } else {
          XtFree(str);
          XtFree(s);
          XtFree(comstr);
          XmToggleButtonGadgetSetState(labeltoggleButtonGadget7,False,False);
          return;
       }
       XtFree(str);
       XtFree(s);
       break;

      case 8:

       XmToggleButtonGadgetSetState(labeltoggleButtonGadget8,True,False);
       str = XmTextGetString (labelText8);
       s = km_strip(str);
       if (!strempty (s)) {
          num = count_labels(s);
          if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
             axis.xnl = 8;
          if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
             axis.ynl = 8;
          sprintf(comstr,"LABEL 8 %d %s", num,s);
          execute_kuip_cmd(comstr);
       } else {
          XtFree(str);
          XtFree(s);
          XtFree(comstr);
          XmToggleButtonGadgetSetState(labeltoggleButtonGadget8,False,False);
          return;
       }
       XtFree(str);
       XtFree(s);
       break;

      case 9:

       XmToggleButtonGadgetSetState(labeltoggleButtonGadget9,True,False);
       str = XmTextGetString (labelText9);
       s = km_strip(str);
       if (!strempty (s)) {
          num = count_labels(s);
          if (XmToggleButtonGadgetGetState(XAxisToggleButtonGadget))
             axis.xnl = 9;
          if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget))
             axis.ynl = 9;
          sprintf(comstr,"LABEL 9 %d %s", num,s);
          execute_kuip_cmd(comstr);
       } else {
          XtFree(str);
          XtFree(s);
          XtFree(comstr);
          XmToggleButtonGadgetSetState(labeltoggleButtonGadget9,False,False);
          return;
       }
       XtFree(str);
       XtFree(s);
       break;

      default :
       break;
   }
   cbs1.reason = -999;
   cbs1.event = (XEvent *) NULL;
   XtCallCallbacks(AxisApplyButton,XmNactivateCallback,&cbs1);

   XtFree(comstr);
}

/***********************************************************************
 *                                                                     *
 *       Insert more labels in an existent label list.                 *
 *                                                                     *
 ***********************************************************************/
static void insert_label(Widget w, int* num, XmAnyCallbackStruct *cbs)
{
   XmToggleButtonCallbackStruct cbs1;

   cbs1.reason = 0;
   cbs1.event = (XEvent *) NULL;

   switch ((int) num) {
      case 1:
       if (XmToggleButtonGadgetGetState(labeltoggleButtonGadget1))
          XtCallCallbacks(labeltoggleButtonGadget1,XmNvalueChangedCallback,
                &cbs1);
       break;
      case 2:
       if (XmToggleButtonGadgetGetState(labeltoggleButtonGadget2))
          XtCallCallbacks(labeltoggleButtonGadget2,XmNvalueChangedCallback,
                &cbs1);
       break;
      case 3:
       if (XmToggleButtonGadgetGetState(labeltoggleButtonGadget3))
          XtCallCallbacks(labeltoggleButtonGadget3,XmNvalueChangedCallback,
                &cbs1);
       break;
      case 4:
       if (XmToggleButtonGadgetGetState(labeltoggleButtonGadget4))
          XtCallCallbacks(labeltoggleButtonGadget4,XmNvalueChangedCallback,
                &cbs1);
       break;
      case 5:
       if (XmToggleButtonGadgetGetState(labeltoggleButtonGadget5))
          XtCallCallbacks(labeltoggleButtonGadget5,XmNvalueChangedCallback,
                &cbs1);
       break;
      case 6:
       if (XmToggleButtonGadgetGetState(labeltoggleButtonGadget6))
          XtCallCallbacks(labeltoggleButtonGadget6,XmNvalueChangedCallback,
                &cbs1);
       break;
      case 7:
       if (XmToggleButtonGadgetGetState(labeltoggleButtonGadget7))
          XtCallCallbacks(labeltoggleButtonGadget7,XmNvalueChangedCallback,
                &cbs1);
       break;
      case 8:
       if (XmToggleButtonGadgetGetState(labeltoggleButtonGadget8))
          XtCallCallbacks(labeltoggleButtonGadget8,XmNvalueChangedCallback,
                &cbs1);
       break;
      case 9:
       if (XmToggleButtonGadgetGetState(labeltoggleButtonGadget9))
          XtCallCallbacks(labeltoggleButtonGadget9,XmNvalueChangedCallback,
                &cbs1);
       break;

      default:
       break;
   }
}

/***********************************************************************
 *                                                                     *
 *       Show labels list for the current axis.                        *
 *                                                                     *
 ***********************************************************************/
void show_label(int val, int ax)
{
   unselect_list();

   switch(val) {
      case 0:
       XmToggleButtonGadgetSetState(labeltoggleButtonGadget0,True,False);
       break;
      case 1:
       XmToggleButtonGadgetSetState(labeltoggleButtonGadget1,True,False);
       break;
      case 2:
       XmToggleButtonGadgetSetState(labeltoggleButtonGadget2,True,False);
       break;
      case 3:
       XmToggleButtonGadgetSetState(labeltoggleButtonGadget3,True,False);
       break;
      case 4:
       XmToggleButtonGadgetSetState(labeltoggleButtonGadget4,True,False);
       break;
      case 5:
       XmToggleButtonGadgetSetState(labeltoggleButtonGadget5,True,False);
       break;
      case 6:
       XmToggleButtonGadgetSetState(labeltoggleButtonGadget6,True,False);
       break;
      case 7:
       XmToggleButtonGadgetSetState(labeltoggleButtonGadget7,True,False);
       break;
      case 8:
       XmToggleButtonGadgetSetState(labeltoggleButtonGadget8,True,False);
       break;
      case 9:
       XmToggleButtonGadgetSetState(labeltoggleButtonGadget9,True,False);
       break;
      default:
       break;
   }
   switch (ax) {
      case 1:
       XtSetSensitive(labelrowColumn,True);
       break;
      case 2:
       XtSetSensitive(labelrowColumn,True);
       break;
      case 3:
       XtSetSensitive(labelrowColumn,False);
       break;
   }
}

/***********************************************************************
 *                                                                     *
 *     Set label orientation.                                          *
 *                                                                     *
 ***********************************************************************/
void show_orien()
{
   unselect();

   switch (axis.xo) {
      case 1 :
       XmToggleButtonGadgetSetState(xdirtoggleButton1,True,False);
       break;
      case 2 :
       XmToggleButtonGadgetSetState(xdirtoggleButton2,True,False);
       break;
      case 3:
       XmToggleButtonGadgetSetState(xdirtoggleButton3,True,False);
       break;
      case 4 :
       XmToggleButtonGadgetSetState(xdirtoggleButton4,True,False);
       break;
      case 5 :
       XmToggleButtonGadgetSetState(xdirtoggleButton5,True,False);
       break;
      case 6 :
       XmToggleButtonGadgetSetState(xdirtoggleButton6,True,False);
       break;
      case 7 :
       XmToggleButtonGadgetSetState(xdirtoggleButton7,True,False);
       break;
      case 8 :
       XmToggleButtonGadgetSetState(xdirtoggleButton8,True,False);
       break;
   }

   switch (axis.yo) {
      case 1 :
       XmToggleButtonGadgetSetState(ydirtoggleButton1,True,False);
       break;
      case 2 :
       XmToggleButtonGadgetSetState(ydirtoggleButton2,True,False);
       break;
      case 3 :
       XmToggleButtonGadgetSetState(ydirtoggleButton3,True,False);
       break;
      case 4 :
       XmToggleButtonGadgetSetState(ydirtoggleButton4,True,False);
       break;
      case 5 :
       XmToggleButtonGadgetSetState(ydirtoggleButton5,True,False);
       break;
      case 6 :
       XmToggleButtonGadgetSetState(ydirtoggleButton6,True,False);
       break;
   }
}

/***********************************************************************
 *                                                                     *
 *         Disable all orientation buttons.                            *
 *                                                                     *
 ***********************************************************************/
static void unselect()
{
   XmToggleButtonGadgetSetState(xdirtoggleButton1,False,False);
   XmToggleButtonGadgetSetState(xdirtoggleButton2,False,False);
   XmToggleButtonGadgetSetState(xdirtoggleButton3,False,False);
   XmToggleButtonGadgetSetState(xdirtoggleButton4,False,False);
   XmToggleButtonGadgetSetState(xdirtoggleButton5,False,False);
   XmToggleButtonGadgetSetState(xdirtoggleButton6,False,False);
   XmToggleButtonGadgetSetState(xdirtoggleButton7,False,False);
   XmToggleButtonGadgetSetState(xdirtoggleButton8,False,False);
   XmToggleButtonGadgetSetState(ydirtoggleButton1,False,False);
   XmToggleButtonGadgetSetState(ydirtoggleButton2,False,False);
   XmToggleButtonGadgetSetState(ydirtoggleButton3,False,False);
   XmToggleButtonGadgetSetState(ydirtoggleButton4,False,False);
   XmToggleButtonGadgetSetState(ydirtoggleButton5,False,False);
   XmToggleButtonGadgetSetState(ydirtoggleButton6,False,False);
}

/***********************************************************************
 *                                                                     *
 *       Permit radio behavior of label buttons                        *
 *                                                                     *
 ***********************************************************************/
void unselect_list()
{
   XmToggleButtonGadgetSetState(labeltoggleButtonGadget0,False,False);
   XmToggleButtonGadgetSetState(labeltoggleButtonGadget1,False,False);
   XmToggleButtonGadgetSetState(labeltoggleButtonGadget2,False,False);
   XmToggleButtonGadgetSetState(labeltoggleButtonGadget3,False,False);
   XmToggleButtonGadgetSetState(labeltoggleButtonGadget4,False,False);
   XmToggleButtonGadgetSetState(labeltoggleButtonGadget5,False,False);
   XmToggleButtonGadgetSetState(labeltoggleButtonGadget6,False,False);
   XmToggleButtonGadgetSetState(labeltoggleButtonGadget7,False,False);
   XmToggleButtonGadgetSetState(labeltoggleButtonGadget8,False,False);
   XmToggleButtonGadgetSetState(labeltoggleButtonGadget9,False,False);
}

/***********************************************************************
 *                                                                     *
 *        Convert list of integer arguments into a string.             *
 *                                                                     *
 ***********************************************************************/
void xyz_string(int x, int y, int z, int l, int o, char *s)
{
   if(y<10 && z<10)
      sprintf(s,"%d0%d0%d.%d%d",x,y,z,l,o);
   if(y<10 && z>=10)
      sprintf(s,"%d0%d%d.%d%d",x,y,z,l,o);
   if(y>=10 && z<10)
      sprintf(s,"%d%d0%d.%d%d",x,y,z,l,o);
   if(y>=10 && z>=10)
      sprintf(s,"%d%d%d.%d%d",x,y,z,l,o);
}

/***********************************************************************
 *                                                                     *
 *        Count number of labels in label list.                        *
 *                                                                     *
 ***********************************************************************/
int count_labels(char* t)
{
   int temp, result,i,quot,space;

   temp = strlen(t);
   i = quot = space = 0;
   result = 1;

   while(i< temp) {
      if (t[i] == '\'' && !quot)
       quot = 1;
      else {
       if (t[i] == '\'')
          quot = 0;
      }
      if (t[i] == ' ' && !quot )
       space = 1;
      if (t[i] != ' ' && space)
      {
       result++;
       space = 0;
      }
      i++;
   }
   return (result);
}

/***********************************************************************
 *                                                                     *
 *           Disable all z axis options                                *
 *                                                                     *
 ***********************************************************************/
static void z_disable()
{
   XtSetSensitive( gridtoggleButtonGadget,False);
   XtSetSensitive( labelPushButtonGadget,False);
   XtSetSensitive( orienPushButtonGadget,False);
   XmToggleButtonGadgetSetState(logtoggleButtonGadget,axis.z_log,False);
   XmToggleButtonGadgetSetState(gridtoggleButtonGadget,False,False);
   if (zdisac_scale){
      XtSetSensitive(DivAxisform,False);
   } else {
      XtSetSensitive(DivAxisform,True);
   }
}

/***********************************************************************
 *                                                                     *
 *    Initialize axis widget.                                          *
 *                                                                     *
 ***********************************************************************/
static void initialize_widget()
{
   if (!appres.auto_refresh) XtSetSensitive(AxisApplyButton, True);
   if (!grid_active) XtSetSensitive(gridtoggleButtonGadget, False);
   set_environment();
}

/***********************************************************************
 *                                                                     *
 *    Reset axis widget attributes.                                    *
 *                                                                     *
 ***********************************************************************/
static void read_axis_attributes()
{
   char   c[6], str[10];
   int    i, temp;
   float  r;

   Hplset("?NDVX",&r);
   if (r>0) {
      i = 0;
      axis.xop = False;
   } else {
      i = 1;
      axis.xop = True;
   }
   sprintf(str,"%5.2f",r);
   axis.xnt = str[0+i]-'0';
   temp = str[1+i] - '0';
   axis.xns = 10 * temp + str[2+i] - '0';
   temp = str[3+i] - '0';
   axis.xnp = 10 * temp + str[4+i] - '0';
   axis.xnl = str[6+i]-'0';
   axis.xo  = str[7+i]-'0';
   if (axis.xnl !=0 && axis.xo == 0)
      axis.xo = 5;
   if (axis.xnl ==0 && axis.xo == 0)
      axis.xo = 1;

   Hplset("?NDVY",&r);
   if (r>0) {
      i = 0;
      axis.yop = False;
   } else {
      i = 1;
      axis.yop = True;
   }
   sprintf(str,"%5.2f",r);
   axis.ynt = str[0+i] - '0';
   temp = str[1+i] - '0';
   axis.yns = 10 * temp + str[2+i] - '0';
   temp = str[3+i] - '0';
   axis.ynp = 10 * temp + str[4+i] - '0';
   axis.ynl = str[6+i]-'0';
   axis.yo  = str[7+i]-'0';
   if (axis.ynl !=0 && axis.yo == 0)
      axis.yo = 6;
   if (axis.ynl ==0 && axis.yo == 0)
      axis.yo = 3;

   Hplset("?NDVZ",&r);
   if (r>0) {
      i = 0;
      axis.zop = False;
   } else {
      i = 1;
      axis.zop = True;
   }
   sprintf(str,"%5.2f",r);
   axis.znt = str[0+i]-'0';
   temp = str[1+i] - '0';
   axis.zns = 10 * temp + str[2+i] - '0';
   temp = str[3+i] - '0';
   axis.znp = 10 * temp + str[4+i] - '0';

   strcpy(c,"GRID");
   Gethplopt(c);
   if (!strcmp(c,"NGRI"))
      r = 0;
   else {
      Hplset("?GRID",&r);
   }

   switch ((int)r) {
      case 3 :

       axis.x_grid = 1;
       axis.y_grid = 1;
       break;
      case 1003 :
       axis.x_grid = 0;
       axis.y_grid = 1;
       break;
      case 2003 :
       axis.x_grid = 1;
       axis.y_grid = 0;
       break;
      default :
       axis.x_grid = 0;
       axis.y_grid = 0;
       break;

   }

   strcpy(c,"LOGX");
   Gethplopt(c);
   if (!strcmp(c,"LOGX"))
      axis.x_log = True;
   else
      axis.x_log = False;
   strcpy(c,"LOGY");
   Gethplopt(c);
   if (!strcmp(c,"LOGY"))
      axis.y_log = True;
   else
      axis.y_log = False;
   strcpy(c,"LOGZ");
   Gethplopt(c);
   if (!strcmp(c,"LOGZ"))
      axis.z_log = True;
   else
      axis.z_log = False;
}

static void read_label_attributes()
{
   char *list;
   char *temp1;
   int n,length,num;

   for (n=1;n<=9;n++) {
      length = 0;
      temp1 = XtCalloc(MAX_CMD_LENGTH,1);
      Hplglb(&n,&num,temp1,MAX_CMD_LENGTH-1,&length);
      list = XtCalloc(length+1,1);
      strcpy(list,temp1);
      switch (n) {
       case 1:
          XmTextSetString(labelText1,list);
          break;
       case 2:
          XmTextSetString(labelText2,list);
          break;
       case 3:
          XmTextSetString(labelText3,list);
          break;
       case 4:
          XmTextSetString(labelText4,list);
          break;
       case 5:
          XmTextSetString(labelText5,list);
          break;
       case 6:
          XmTextSetString(labelText6,list);
          break;
       case 7:
          XmTextSetString(labelText7,list);
          break;
       case 8:
          XmTextSetString(labelText8,list);
          break;
       case 9:
          XmTextSetString(labelText9,list);
          break;
      }
      XtFree(list);
      XtFree(temp1);
   }
}
